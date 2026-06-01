library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
source("code/setup.R")
# Use Census API to get ZIP code level data for selected ZIP Business Patterns
# NAICS codes from 2012 to 2023.
download_years <- 2012:2023
legacy_years <- 2015:2022

download_year_label <- paste0(min(download_years), "_", max(download_years))
legacy_year_label <- paste0(min(legacy_years), "_", max(legacy_years))
api_cache_dir <- file.path("data/temp", "census_api_cache")
dir.create(api_cache_dir, recursive = TRUE, showWarnings = FALSE)

online_retailer_naics <- "4541"
local_retailer_naics <- c("4421", "4431", "4451", "4481", "4511", "4512", "4532")
warehouse_naics <- "493110"

zbp_naics_category_map <- tibble(
  naics = c(online_retailer_naics, local_retailer_naics, warehouse_naics),
  naics_category = c(
    "online retailer",
    rep("local retailer", length(local_retailer_naics)),
    "warehouse"
  )
)

Sys.setenv(CENSUS_API_KEY = "327001db07a405a83c2450f202578a6027ed0fd6")

read_census_json <- function(url, query) {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (nzchar(key)) query$key <- key
  
  res <- RETRY(
    "GET",
    url,
    query = query,
    times = 5,
    pause_base = 1,
    pause_cap = 10,
    pause_min = 0.5,
    terminate_on = c(400, 404)
  )
  stop_for_status(res)
  
  txt <- content(res, "text", encoding = "UTF-8")
  if (grepl("^\\s*<html", txt, ignore.case = TRUE)) {
    title <- sub(".*<title>(.*?)</title>.*", "\\1", txt, ignore.case = TRUE)
    stop(
      "Census API returned HTML instead of JSON: ", title,
      ". If the title is Missing Key, set CENSUS_API_KEY or try again later."
    )
  }
  x <- fromJSON(txt)
  
  # 防止 Census API 返回重复列名
  header <- x[1, ]
  keep <- !duplicated(header)
  x <- x[, keep, drop = FALSE]
  header <- header[keep]
  
  out <- as.data.frame(x[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(out) <- header
  out
}

read_cached_or_download <- function(cache_path, download_fun) {
  if (file.exists(cache_path)) {
    message("Reading cache ", cache_path)
    return(read_csv(cache_path, col_types = cols(.default = col_guess(), zipcode = col_character())))
  }
  
  out <- download_fun()
  write_csv(out, cache_path)
  out
}

clean_zbp <- function(df, year, naics_code) {
  names(df) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(df)))
  
  if ("zip_code" %in% names(df)) {
    df <- df |> rename(zipcode = zip_code)
  }
  
  if ("empszes_ttl" %in% names(df)) {
    df <- df |> rename(empszes_label = empszes_ttl)
  }
  
  df |>
    mutate(
      year = as.integer(year),
      naics = as.character(naics_code),
      estab = as.numeric(estab),
      empszes = as.character(empszes),
      zipcode = as.character(zipcode)
    )
}

aggregate_retailer_categories <- function(df) {
  out <- df |>
    left_join(zbp_naics_category_map, by = "naics")
  
  unmapped_naics <- out |>
    filter(is.na(naics_category)) |>
    distinct(naics) |>
    pull(naics)
  
  if (length(unmapped_naics) > 0) {
    stop("Unmapped NAICS code(s): ", paste(unmapped_naics, collapse = ", "))
  }
  
  out |>
    mutate(naics = naics_category) |>
    group_by(geo_id, year, zipcode, empszes, empszes_label, naics) |>
    summarise(estab = sum(estab, na.rm = TRUE), .groups = "drop")
}

get_zbp_naics_one_year <- function(year, naics_code = "4541") {
  
  if (year %in% 2012:2016) {
    url <- paste0("https://api.census.gov/data/", year, "/zbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "ESTAB",
        "EMPSZES",
        "EMPSZES_TTL",
        sep = ","
      ),
      `for` = "zipcode:*",
      NAICS2012 = naics_code
    )
    
  } else if (year %in% 2017:2018) {
    url <- paste0("https://api.census.gov/data/", year, "/zbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "ESTAB",
        "EMPSZES",
        "EMPSZES_LABEL",
        sep = ","
      ),
      `for` = "zipcode:*",
      NAICS2017 = naics_code
    )
    
  } else if (year %in% 2019:2023) {
    url <- paste0("https://api.census.gov/data/", year, "/cbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "ESTAB",
        "EMPSZES",
        "EMPSZES_LABEL",
        sep = ","
      ),
      `for` = "zip code:*",
      NAICS2017 = naics_code,
      LFO = "001"
    )
    
  } else {
    stop("Year must be between 2012 and 2023.")
  }
  
  message("Downloading ", year, "...")
  
  read_census_json(url, query) |>
    clean_zbp(year = year, naics_code = naics_code)
}

zbp_retailer <- expand_grid(
  year = download_years,
  naics_code = zbp_naics_category_map$naics
) |>
  pmap_dfr(function(year, naics_code) {
    cache_path <- file.path(api_cache_dir, paste0("zbp_retailer_", year, "_", naics_code, ".csv"))
    read_cached_or_download(
      cache_path,
      function() get_zbp_naics_one_year(year, naics_code)
    )
  }) |>
  aggregate_retailer_categories()

write_csv(
  zbp_retailer,
  file.path("data/temp", paste0("zbp_retailer_", download_year_label, ".csv"))
)

# Keep the old output available for existing clean scripts.
write_csv(
  filter(zbp_retailer, year %in% legacy_years),
  file.path("data/temp", paste0("zbp_retailer_", legacy_year_label, ".csv"))
)

# Use Census API to get ZIP code level annual payroll and employment for all sectors total (NAICS 00).
clean_zip_payroll <- function(df, year, naics_code = "00") {
  names(df) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(df)))
  
  if ("zip_code" %in% names(df)) {
    df <- df |> rename(zipcode = zip_code)
  }
  
  if ("empszes_ttl" %in% names(df)) {
    df <- df |> rename(empszes_label = empszes_ttl)
  }
  
  df |>
    filter(empszes_label == "All establishments") |>
    transmute(
      geo_id = as.character(geo_id),
      year = as.integer(year),
      naics = as.character(naics_code),
      zipcode = as.character(zipcode),
      payann = as.numeric(payann),
      emp = as.numeric(emp)
    )
}

get_zip_payroll_one_year <- function(year, naics_code = "00") {
  
  if (year %in% 2012:2016) {
    url <- paste0("https://api.census.gov/data/", year, "/zbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "PAYANN",
        "EMP",
        "EMPSZES",
        "EMPSZES_TTL",
        sep = ","
      ),
      `for` = "zipcode:*",
      NAICS2012 = naics_code
    )
    
  } else if (year %in% 2017:2018) {
    url <- paste0("https://api.census.gov/data/", year, "/zbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "PAYANN",
        "EMP",
        "EMPSZES",
        "EMPSZES_LABEL",
        sep = ","
      ),
      `for` = "zipcode:*",
      NAICS2017 = naics_code
    )
    
  } else if (year %in% 2019:2023) {
    url <- paste0("https://api.census.gov/data/", year, "/cbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "PAYANN",
        "EMP",
        "EMPSZES",
        "EMPSZES_LABEL",
        sep = ","
      ),
      `for` = "zip code:*",
      NAICS2017 = naics_code,
      LFO = "001"
    )
    
  } else {
    stop("Year must be between 2012 and 2023.")
  }
  
  message("Downloading ZIP payroll ", year, "...")
  
  read_census_json(url, query) |>
    clean_zip_payroll(year = year, naics_code = naics_code)
}

zbp_payroll <- map_dfr(
  download_years,
  function(year) {
    cache_path <- file.path(api_cache_dir, paste0("zbp_payroll_", year, "_00.csv"))
    read_cached_or_download(
      cache_path,
      function() get_zip_payroll_one_year(year, naics_code = "00")
    )
  }
)

write_csv(
  zbp_payroll,
  file.path("data/temp", paste0("zbp_payroll_", download_year_label, ".csv"))
)

# Keep the old output available for existing clean scripts.
write_csv(
  filter(zbp_payroll, year %in% legacy_years),
  file.path("data/temp", paste0("zbp_payroll_", legacy_year_label, ".csv"))
)

# Use all-sector employment size bins to calibrate industry-level employment proxies.
clean_zip_all_sector_size <- function(df, year, naics_code = "00") {
  names(df) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(df)))
  
  if ("zip_code" %in% names(df)) {
    df <- df |> rename(zipcode = zip_code)
  }
  
  if ("empszes_ttl" %in% names(df)) {
    df <- df |> rename(empszes_label = empszes_ttl)
  }
  
  df |>
    transmute(
      geo_id = as.character(geo_id),
      year = as.integer(year),
      naics = as.character(naics_code),
      zipcode = as.character(zipcode),
      empszes = as.character(empszes),
      empszes_label = as.character(empszes_label),
      estab = as.numeric(estab)
    )
}

get_zip_all_sector_size_one_year <- function(year, naics_code = "00") {
  
  if (year %in% 2012:2016) {
    url <- paste0("https://api.census.gov/data/", year, "/zbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "ESTAB",
        "EMPSZES",
        "EMPSZES_TTL",
        sep = ","
      ),
      `for` = "zipcode:*",
      NAICS2012 = naics_code
    )
    
  } else if (year %in% 2017:2018) {
    url <- paste0("https://api.census.gov/data/", year, "/zbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "ESTAB",
        "EMPSZES",
        "EMPSZES_LABEL",
        sep = ","
      ),
      `for` = "zipcode:*",
      NAICS2017 = naics_code
    )
    
  } else if (year %in% 2019:2023) {
    url <- paste0("https://api.census.gov/data/", year, "/cbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "ESTAB",
        "EMPSZES",
        "EMPSZES_LABEL",
        sep = ","
      ),
      `for` = "zip code:*",
      NAICS2017 = naics_code,
      LFO = "001"
    )
    
  } else {
    stop("Year must be between 2012 and 2023.")
  }
  
  message("Downloading ZIP all-sector size bins ", year, "...")
  
  read_census_json(url, query) |>
    clean_zip_all_sector_size(year = year, naics_code = naics_code)
}

zbp_all_sector_size <- map_dfr(
  download_years,
  function(year) {
    cache_path <- file.path(api_cache_dir, paste0("zbp_all_sector_size_", year, "_00.csv"))
    read_cached_or_download(
      cache_path,
      function() get_zip_all_sector_size_one_year(year, naics_code = "00")
    )
  }
)

write_csv(
  zbp_all_sector_size,
  file.path("data/temp", paste0("zbp_all_sector_size_", download_year_label, ".csv"))
)

# Keep the old output available for existing clean scripts.
write_csv(
  filter(zbp_all_sector_size, year %in% legacy_years),
  file.path("data/temp", paste0("zbp_all_sector_size_", legacy_year_label, ".csv"))
)
