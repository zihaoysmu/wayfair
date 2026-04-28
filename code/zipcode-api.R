library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)

# Use Census API to get ZIP code level data for NAICS code 4541 (Electronic Shopping and Mail-Order Houses) from 2015 to 2022.
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

zbp_4541_2015_2022 <- map_dfr(
  2015:2022,
  ~ get_zbp_naics_one_year(.x, naics_code = "4541")
)

write_csv(zbp_4541_2015_2022, "data/temp/zbp_4541_2015_2022.csv")


