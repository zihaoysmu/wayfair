library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
source("code/setup.R")

# Use Census ACS 5-year API to get ZIP Code Tabulation Area population
# estimates from 2015 to 2022.
read_census_json <- function(url, query) {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (nzchar(key)) query$key <- key

  res <- GET(url, query = query)
  stop_for_status(res)

  txt <- content(res, "text", encoding = "UTF-8")
  x <- fromJSON(txt)

  header <- x[1, ]
  keep <- !duplicated(header)
  x <- x[, keep, drop = FALSE]
  header <- header[keep]

  out <- as.data.frame(x[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(out) <- header
  out
}

clean_acs_zip_population <- function(df, year) {
  names(df) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(df)))

  if ("zip_code_tabulation_area" %in% names(df)) {
    df <- df |>
      rename(zipcode = zip_code_tabulation_area)
  }

  df |>
    transmute(
      year = as.integer(year),
      geo_id = paste0("8600000US", zipcode),
      zipcode = as.character(zipcode),
      name = as.character(name),
      population = as.numeric(b01003_001e),
      population_moe = as.numeric(b01003_001m)
    )
}

get_acs_zip_population_one_year <- function(year) {
  if (!year %in% 2015:2022) {
    stop("Year must be between 2015 and 2022.")
  }

  url <- paste0("https://api.census.gov/data/", year, "/acs/acs5")

  query <- list(
    get = paste(
      "NAME",
      "B01003_001E",
      "B01003_001M",
      sep = ","
    ),
    `for` = "zip code tabulation area:*"
  )

  message("Downloading ZIP population ", year, "...")

  read_census_json(url, query) |>
    clean_acs_zip_population(year = year)
}

acs5_zipcode_population_2015_2022 <- map_dfr(
  2015:2022,
  get_acs_zip_population_one_year
)

dir.create("data/temp", recursive = TRUE, showWarnings = FALSE)

write_csv(
  acs5_zipcode_population_2015_2022,
  "data/temp/acs5_zipcode_population_2015_2022.csv"
)
