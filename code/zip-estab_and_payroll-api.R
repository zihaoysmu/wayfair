library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
source("code/setup.R")
# Use Census API to get ZIP code level data for online and local retailer NAICS codes from 2015 to 2022.
online_retailer_naics <- "4541"
local_retailer_naics <- c("4421", "4431", "4451", "4481", "4511", "4512", "4532")

read_census_json <- function(url, query) {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (nzchar(key)) query$key <- key
  
  res <- GET(url, query = query)
  stop_for_status(res)
  
  txt <- content(res, "text", encoding = "UTF-8")
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
  df |>
    mutate(
      naics = if_else(
        naics == online_retailer_naics,
        "online retailer",
        "local retailer"
      )
    ) |>
    group_by(geo_id, year, zipcode, empszes, empszes_label, naics) |>
    summarise(estab = sum(estab, na.rm = TRUE), .groups = "drop")
}

get_zbp_naics_one_year <- function(year, naics_code = "4541") {
  
  if (year %in% 2015:2016) {
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
      `for` = "zip code:*",
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
      `for` = "zip code:*",
      NAICS2017 = naics_code
    )
    
  } else if (year %in% 2019:2022) {
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
    stop("Year must be between 2015 and 2022.")
  }
  
  message("Downloading ", year, "...")
  
  read_census_json(url, query) |>
    clean_zbp(year = year, naics_code = naics_code)
}

zbp_retailer_2015_2022 <- expand_grid(
  year = 2015:2022,
  naics_code = c(online_retailer_naics, local_retailer_naics)
) |>
  pmap_dfr(function(year, naics_code) {
    get_zbp_naics_one_year(year, naics_code)
  }) |>
  aggregate_retailer_categories()

write_csv(
  zbp_retailer_2015_2022,
  "data/temp/zbp_retailer_2015_2022.csv"
)

# Use Census API to get ZIP code level annual payroll for all sectors total (NAICS 00).
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
      payann = as.numeric(payann)
    )
}

get_zip_payroll_one_year <- function(year, naics_code = "00") {
  
  if (year %in% 2015:2016) {
    url <- paste0("https://api.census.gov/data/", year, "/zbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "PAYANN",
        "EMPSZES",
        "EMPSZES_TTL",
        sep = ","
      ),
      `for` = "zip code:*",
      NAICS2012 = naics_code
    )
    
  } else if (year %in% 2017:2018) {
    url <- paste0("https://api.census.gov/data/", year, "/zbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "PAYANN",
        "EMPSZES",
        "EMPSZES_LABEL",
        sep = ","
      ),
      `for` = "zip code:*",
      NAICS2017 = naics_code
    )
    
  } else if (year %in% 2019:2022) {
    url <- paste0("https://api.census.gov/data/", year, "/cbp")
    
    query <- list(
      get = paste(
        "GEO_ID",
        "YEAR",
        "PAYANN",
        "EMPSZES",
        "EMPSZES_LABEL",
        sep = ","
      ),
      `for` = "zip code:*",
      NAICS2017 = naics_code,
      LFO = "001"
    )
    
  } else {
    stop("Year must be between 2015 and 2022.")
  }
  
  message("Downloading ZIP payroll ", year, "...")
  
  read_census_json(url, query) |>
    clean_zip_payroll(year = year, naics_code = naics_code)
}

zbp_payroll_2015_2022 <- map_dfr(
  2015:2022,
  ~ get_zip_payroll_one_year(.x, naics_code = "00")
)

write_csv(
  zbp_payroll_2015_2022,
  "data/temp/zbp_payroll_2015_2022.csv"
)


