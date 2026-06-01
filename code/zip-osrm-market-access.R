required_packages <- c(
  "osrm", "sf", "tigris", "dplyr", "data.table", "readr", "stringr"
)

# Run with a local OSRM server. If the server uses OSRM's default table limit,
# reduce OSRM_MA_DESTINATION_BLOCK_SIZE or start OSRM with a larger
# --max-table-size. Each request sends origin_block_size + destination_block_size
# locations to the table service.

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running this script, e.g. install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")).",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(osrm)
  library(sf)
  library(tigris)
  library(dplyr)
  library(data.table)
  library(readr)
  library(stringr)
})

options(tigris_use_cache = TRUE)
sf::sf_use_s2(FALSE)

dir.create("data/temp", recursive = TRUE, showWarnings = FALSE)
dir.create("data/temp/tigris_cache", recursive = TRUE, showWarnings = FALSE)
options(tigris_cache_dir = normalizePath("data/temp/tigris_cache", winslash = "/"))

# Inputs and outputs ---------------------------------------------------------
path_origin_crosswalk <- Sys.getenv(
  "OSRM_MA_ORIGIN_PATH",
  "data/temp/zcta_wedge_crosswalk_10mile.csv"
)
path_payroll <- Sys.getenv(
  "OSRM_MA_PAYROLL_PATH",
  "data/temp/zbp_payroll_2015_2022.csv"
)
path_zcta_points_cache <- Sys.getenv(
  "OSRM_MA_ZCTA_POINTS_CACHE",
  "data/temp/zcta_points_lower48_dc_2020.rds"
)
path_time_blocks <- Sys.getenv(
  "OSRM_MA_BLOCK_DIR",
  "data/temp/osrm_zip_time_blocks_1000km_10mile"
)
path_output_market_access <- Sys.getenv(
  "OSRM_MA_OUTPUT",
  "data/temp/zipcode_osrm_market_access_2015_2022.csv"
)
path_output_diagnostics <- Sys.getenv(
  "OSRM_MA_DIAGNOSTICS",
  "data/temp/zipcode_osrm_market_access_diagnostics.csv"
)

# Parameters ----------------------------------------------------------------
years <- 2015:2022
zcta_year <- as.integer(Sys.getenv("OSRM_MA_ZCTA_YEAR", "2020"))
crs_projected <- as.integer(Sys.getenv("OSRM_MA_PROJECTED_CRS", "5070"))
radius_km <- as.numeric(Sys.getenv("OSRM_MA_RADIUS_KM", "1000"))
theta <- as.numeric(Sys.getenv("OSRM_MA_THETA", "1.5"))
origin_block_size <- as.integer(Sys.getenv("OSRM_MA_ORIGIN_BLOCK_SIZE", "10"))
destination_block_size <- as.integer(Sys.getenv("OSRM_MA_DESTINATION_BLOCK_SIZE", "500"))
osrm_server <- Sys.getenv("OSRM_SERVER", "http://127.0.0.1:5000/")
osrm_profile <- Sys.getenv("OSRM_PROFILE", "car")
time_source <- paste0("OSRM_OpenStreetMap_modern_", osrm_profile)

if (!grepl("/$", osrm_server)) {
  osrm_server <- paste0(osrm_server, "/")
}

options(osrm.server = osrm_server)
options(osrm.profile = osrm_profile)

if (!is.finite(radius_km) || radius_km <= 0) {
  stop("OSRM_MA_RADIUS_KM must be positive.")
}
if (!is.finite(theta) || theta <= 0) {
  stop("OSRM_MA_THETA must be positive.")
}
if (is.na(origin_block_size) || origin_block_size <= 0) {
  stop("OSRM_MA_ORIGIN_BLOCK_SIZE must be a positive integer.")
}
if (is.na(destination_block_size) || destination_block_size <= 0) {
  stop("OSRM_MA_DESTINATION_BLOCK_SIZE must be a positive integer.")
}

dir.create(path_time_blocks, recursive = TRUE, showWarnings = FALSE)

# Helpers -------------------------------------------------------------------
zip_pad <- function(x) {
  stringr::str_pad(as.character(x), width = 5, side = "left", pad = "0")
}

clean_names <- function(df) {
  names(df) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(df)))
  df
}

stop_if_missing_file <- function(path) {
  if (!file.exists(path)) {
    stop("Required input file does not exist: ", path, call. = FALSE)
  }
}

make_blocks <- function(x, block_size) {
  split(x, ceiling(seq_along(x) / block_size))
}

duration_matrix_to_long <- function(duration_matrix, src_ids, dst_ids) {
  duration_matrix <- as.matrix(duration_matrix)
  if (!identical(dim(duration_matrix), c(length(src_ids), length(dst_ids)))) {
    stop("OSRM duration matrix dimensions do not match source/destination inputs.")
  }

  dimnames(duration_matrix) <- list(src_ids, dst_ids)
  dt <- as.data.table(as.table(duration_matrix))
  setnames(dt, c("origin_zipcode", "dest_zipcode", "duration_min"))
  dt[, origin_zipcode := as.character(origin_zipcode)]
  dt[, dest_zipcode := as.character(dest_zipcode)]
  dt[, duration_min := as.numeric(duration_min)]
  dt[]
}

make_osrm_points <- function(points_sf) {
  points_ll <- sf::st_transform(points_sf, 4326)
  coords <- sf::st_coordinates(points_ll)
  data.frame(
    id = as.character(points_ll$zipcode),
    lon = coords[, "X"],
    lat = coords[, "Y"],
    stringsAsFactors = FALSE
  )
}

load_zcta_points <- function(cache_path) {
  if (file.exists(cache_path)) {
    message("Reading cached ZCTA points: ", cache_path)
    return(readRDS(cache_path))
  }

  message("Downloading/loading state and ZCTA geometries...")
  lower48_dc <- c(setdiff(state.abb, c("AK", "HI")), "DC")

  states_sf <- tigris::states(cb = TRUE, year = 2022) |>
    filter(STUSPS %in% lower48_dc) |>
    sf::st_make_valid() |>
    sf::st_transform(crs_projected) |>
    select(STATEFP, STUSPS)

  zcta_sf <- tigris::zctas(cb = TRUE, year = zcta_year) |>
    sf::st_make_valid() |>
    sf::st_transform(crs_projected)

  zcta_zip_col <- intersect(
    names(zcta_sf),
    c("ZCTA5CE20", "ZCTA5CE10", "GEOID20", "GEOID10", "ZCTA5CE")
  )[1]

  if (is.na(zcta_zip_col)) {
    stop("Cannot find a ZCTA ZIP code column in the ZCTA shapefile.")
  }

  zcta_points <- zcta_sf |>
    mutate(zipcode = zip_pad(.data[[zcta_zip_col]])) |>
    select(zipcode) |>
    sf::st_set_geometry(sf::st_point_on_surface(sf::st_geometry(zcta_sf))) |>
    sf::st_join(states_sf, left = TRUE) |>
    filter(!is.na(STATEFP)) |>
    transmute(
      zipcode,
      statefp = STATEFP,
      state_abbr = STUSPS
    ) |>
    arrange(zipcode)

  saveRDS(zcta_points, cache_path)
  zcta_points
}

candidate_pairs_for_block <- function(origin_block, dest_points, radius_m) {
  within_list <- sf::st_is_within_distance(
    origin_block,
    dest_points,
    dist = radius_m
  )

  pairs <- rbindlist(lapply(seq_along(within_list), function(i) {
    dest_index <- within_list[[i]]
    if (length(dest_index) == 0) {
      return(NULL)
    }
    data.table(
      origin_index = origin_block$origin_index[i],
      dest_index = dest_index,
      origin_zipcode = origin_block$zipcode[i],
      dest_zipcode = dest_points$zipcode[dest_index]
    )
  }))

  if (is.null(pairs) || nrow(pairs) == 0) {
    return(data.table(
      origin_index = integer(),
      dest_index = integer(),
      origin_zipcode = character(),
      dest_zipcode = character()
    ))
  }

  pairs[origin_zipcode != dest_zipcode]
}

check_osrm_connection <- function(origin_osrm, dest_osrm) {
  src <- origin_osrm[1, , drop = FALSE]
  dst <- dest_osrm[dest_osrm$id != src$id[1], , drop = FALSE]
  if (nrow(dst) == 0) {
    stop("No destination ZIP differs from the first origin ZIP.")
  }
  dst <- dst[1, , drop = FALSE]

  message("Checking OSRM server: ", osrm_server, " profile: ", osrm_profile)
  tryCatch(
    {
      osrm::osrmTable(src = src, dst = dst, measure = "duration")
      TRUE
    },
    error = function(e) {
      stop(
        "Cannot query OSRM server at ", osrm_server,
        " with profile ", osrm_profile, ". Start a local OSRM server first. ",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

summary_value <- function(x, fun) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  fun(x, na.rm = TRUE)
}

# Load inputs ----------------------------------------------------------------
stop_if_missing_file(path_origin_crosswalk)
stop_if_missing_file(path_payroll)

message("Reading origin ZIPs: ", path_origin_crosswalk)
origin_crosswalk <- fread(path_origin_crosswalk)
origin_crosswalk <- clean_names(origin_crosswalk)
if (!"zipcode" %in% names(origin_crosswalk)) {
  stop("Origin crosswalk must contain a zipcode column.")
}

origin_zips <- sort(unique(zip_pad(origin_crosswalk$zipcode)))

message("Preparing ZCTA representative points...")
zcta_points <- load_zcta_points(path_zcta_points_cache)

origin_pts <- zcta_points |>
  filter(zipcode %in% origin_zips) |>
  arrange(zipcode) |>
  mutate(origin_index = row_number())

missing_origin_zips <- setdiff(origin_zips, origin_pts$zipcode)
if (length(missing_origin_zips) > 0) {
  message("Missing origin ZIPs from lower-48/DC ZCTA points: ", length(missing_origin_zips))
}

dest_pts <- zcta_points |>
  arrange(zipcode) |>
  mutate(dest_index = row_number())

if (nrow(origin_pts) == 0) {
  stop("No origin ZIPs have matching lower-48/DC ZCTA representative points.")
}
if (nrow(dest_pts) == 0) {
  stop("No destination ZCTA representative points are available.")
}

origin_osrm <- make_osrm_points(origin_pts)
dest_osrm <- make_osrm_points(dest_pts)
check_osrm_connection(origin_osrm, dest_osrm)

message("Reading destination payroll: ", path_payroll)
payroll <- fread(path_payroll, colClasses = list(character = "zipcode"))
payroll <- clean_names(payroll)
required_payroll_cols <- c("zipcode", "year", "payann")
missing_payroll_cols <- setdiff(required_payroll_cols, names(payroll))
if (length(missing_payroll_cols) > 0) {
  stop(
    "Payroll file is missing required column(s): ",
    paste(missing_payroll_cols, collapse = ", ")
  )
}

payroll[, zipcode := zip_pad(zipcode)]
payroll[, year := as.integer(year)]
payroll[, payann := as.numeric(payann)]
payroll <- payroll[
  year %in% years & zipcode %in% dest_pts$zipcode,
  .(payann = sum(payann, na.rm = TRUE)),
  by = .(zipcode, year)
]

payroll_panel <- CJ(
  dest_zipcode = dest_pts$zipcode,
  year = years,
  unique = TRUE
)
payroll_panel <- merge(
  payroll_panel,
  payroll[, .(dest_zipcode = zipcode, year, payann)],
  by = c("dest_zipcode", "year"),
  all.x = TRUE
)
payroll_panel[is.na(payann), payann := 0]
setkey(payroll_panel, dest_zipcode)

# Compute OSRM travel-time blocks -------------------------------------------
radius_m <- radius_km * 1000
origin_blocks <- make_blocks(seq_len(nrow(origin_pts)), origin_block_size)
candidate_counts <- vector("list", length(origin_blocks))

message(
  "Computing/caching OSRM travel times for ",
  nrow(origin_pts), " origins and ", nrow(dest_pts),
  " destinations within ", radius_km, " km."
)

for (oi in seq_along(origin_blocks)) {
  idx_o <- origin_blocks[[oi]]
  origin_block <- origin_pts[idx_o, ]

  candidate_dt <- candidate_pairs_for_block(
    origin_block = origin_block,
    dest_points = dest_pts,
    radius_m = radius_m
  )

  block_count <- data.table(
    origin_zipcode = origin_block$zipcode,
    n_candidate_destinations = 0L
  )
  if (nrow(candidate_dt) > 0) {
    block_count[
      candidate_dt[, .(n_candidate_destinations = .N), by = origin_zipcode],
      on = "origin_zipcode",
      n_candidate_destinations := i.n_candidate_destinations
    ]
  }
  block_count[, origin_block := oi]
  candidate_counts[[oi]] <- block_count

  if (nrow(candidate_dt) == 0) {
    message("Origin block ", oi, "/", length(origin_blocks), ": no candidate destinations.")
    next
  }

  candidate_dest_blocks <- make_blocks(
    sort(unique(candidate_dt$dest_index)),
    destination_block_size
  )

  message(
    "Origin block ", oi, "/", length(origin_blocks),
    ": ", nrow(candidate_dt), " candidate pairs across ",
    length(candidate_dest_blocks), " destination blocks."
  )

  for (di in seq_along(candidate_dest_blocks)) {
    idx_d <- candidate_dest_blocks[[di]]
    out_file <- file.path(
      path_time_blocks,
      sprintf("time_block_o%04d_d%04d.rds", oi, di)
    )

    if (file.exists(out_file)) {
      message("Skip existing OSRM block: ", out_file)
      next
    }

    candidate_filter <- candidate_dt[
      dest_index %in% idx_d,
      .(origin_zipcode, dest_zipcode)
    ]

    src <- origin_osrm[idx_o, , drop = FALSE]
    dst <- dest_osrm[idx_d, , drop = FALSE]

    message(
      "Computing OSRM block o", oi, " d", di,
      ": ", nrow(src), " origins x ", nrow(dst), " destinations."
    )

    tab <- tryCatch(
      osrm::osrmTable(src = src, dst = dst, measure = "duration"),
      error = function(e) {
        stop(
          "OSRM failed for origin block ", oi,
          ", destination block ", di, ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    # R osrm::osrmTable() returns durations in minutes. The raw HTTP OSRM API
    # returns seconds, but the R wrapper converts the table output.
    block_dt <- duration_matrix_to_long(
      duration_matrix = tab$durations,
      src_ids = src$id,
      dst_ids = dst$id
    )

    block_dt <- block_dt[
      candidate_filter,
      on = c("origin_zipcode", "dest_zipcode"),
      nomatch = 0
    ]

    if (nrow(block_dt) != nrow(candidate_filter)) {
      stop("Filtered OSRM block does not contain all candidate pairs.")
    }

    saveRDS(block_dt, out_file)
  }
}

candidate_counts_dt <- rbindlist(candidate_counts, fill = TRUE)
n_candidate_pairs <- sum(candidate_counts_dt$n_candidate_destinations, na.rm = TRUE)

# Compute ZIP-year market access --------------------------------------------
block_files <- list.files(
  path_time_blocks,
  pattern = "^time_block_o[0-9]+_d[0-9]+\\.rds$",
  full.names = TRUE
)

if (length(block_files) == 0) {
  stop("No OSRM travel-time block files were produced.")
}

message("Aggregating market access from ", length(block_files), " OSRM block files.")
market_access <- CJ(
  zipcode = origin_pts$zipcode,
  year = years,
  unique = TRUE
)
market_access[, `:=`(
  zipcode_osrm_market_access = 0,
  n_destinations_used = 0L
)]
setkey(market_access, zipcode, year)

n_time_pairs_cached <- 0L
n_osrm_missing_duration <- 0L
n_nonpositive_duration <- 0L

for (bf in block_files) {
  dt <- readRDS(bf)
  setDT(dt)

  n_time_pairs_cached <- n_time_pairs_cached + nrow(dt)
  n_osrm_missing_duration <- n_osrm_missing_duration + sum(is.na(dt$duration_min))
  n_nonpositive_duration <- n_nonpositive_duration +
    sum(!is.na(dt$duration_min) & dt$duration_min <= 0)

  dt <- dt[
    !is.na(duration_min) &
      duration_min > 0 &
      origin_zipcode != dest_zipcode
  ]

  if (nrow(dt) == 0) {
    next
  }

  dt[, duration_hour := duration_min / 60]
  dt <- merge(
    dt,
    payroll_panel,
    by = "dest_zipcode",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  dt[is.na(payann), payann := 0]
  dt[, contribution := payann * (duration_hour ^ (-theta))]

  block_ma <- dt[, .(
    zipcode_osrm_market_access = sum(contribution, na.rm = TRUE),
    n_destinations_used = .N
  ), by = .(zipcode = origin_zipcode, year)]

  market_access[
    block_ma,
    on = c("zipcode", "year"),
    `:=`(
      zipcode_osrm_market_access =
        zipcode_osrm_market_access + i.zipcode_osrm_market_access,
      n_destinations_used =
        n_destinations_used + i.n_destinations_used
    )
  ]

  rm(dt, block_ma)
  gc()
}

market_access[, log_zipcode_osrm_market_access := fifelse(
  zipcode_osrm_market_access > 0,
  log(zipcode_osrm_market_access),
  NA_real_
)]
market_access[, `:=`(
  theta = theta,
  radius_km = radius_km,
  time_source = time_source
)]
setcolorder(market_access, c(
  "zipcode",
  "year",
  "zipcode_osrm_market_access",
  "log_zipcode_osrm_market_access",
  "n_destinations_used",
  "theta",
  "radius_km",
  "time_source"
))

balance_check <- market_access[, .(n_years = uniqueN(year)), by = zipcode]
if (any(balance_check$n_years != length(years))) {
  stop("Market access output does not contain every year for every origin ZIP.")
}
if (any(is.infinite(market_access$log_zipcode_osrm_market_access), na.rm = TRUE)) {
  stop("log_zipcode_osrm_market_access contains Inf values.")
}
if (any(is.nan(market_access$log_zipcode_osrm_market_access))) {
  stop("log_zipcode_osrm_market_access contains NaN values.")
}
if (any(market_access$zipcode_osrm_market_access < 0, na.rm = TRUE)) {
  stop("zipcode_osrm_market_access contains negative values.")
}

fwrite(market_access, path_output_market_access)

# Diagnostics ----------------------------------------------------------------
duration_success_rate <- if (n_time_pairs_cached > 0) {
  (n_time_pairs_cached - n_osrm_missing_duration) / n_time_pairs_cached
} else {
  NA_real_
}

diagnostics <- data.table(
  n_origin_zips_input = length(origin_zips),
  n_origin_zips_with_zcta_points = nrow(origin_pts),
  n_missing_origin_zcta_points = length(missing_origin_zips),
  n_destination_zcta_points = nrow(dest_pts),
  n_payroll_destination_zips = uniqueN(payroll$zipcode),
  n_years = length(years),
  first_year = min(years),
  last_year = max(years),
  radius_km = radius_km,
  theta = theta,
  osrm_server = osrm_server,
  osrm_profile = osrm_profile,
  origin_block_size = origin_block_size,
  destination_block_size = destination_block_size,
  n_candidate_pairs_excluding_self = n_candidate_pairs,
  n_time_pairs_cached = n_time_pairs_cached,
  n_osrm_missing_duration = n_osrm_missing_duration,
  osrm_duration_success_rate = duration_success_rate,
  n_nonpositive_duration = n_nonpositive_duration,
  min_candidate_destinations_per_origin = summary_value(
    candidate_counts_dt$n_candidate_destinations,
    min
  ),
  median_candidate_destinations_per_origin = summary_value(
    candidate_counts_dt$n_candidate_destinations,
    stats::median
  ),
  max_candidate_destinations_per_origin = summary_value(
    candidate_counts_dt$n_candidate_destinations,
    max
  ),
  n_output_rows = nrow(market_access),
  n_output_zips = uniqueN(market_access$zipcode),
  n_output_rows_missing_log_market_access =
    sum(is.na(market_access$log_zipcode_osrm_market_access)),
  n_output_log_market_access_inf =
    sum(is.infinite(market_access$log_zipcode_osrm_market_access), na.rm = TRUE),
  n_output_log_market_access_nan =
    sum(is.nan(market_access$log_zipcode_osrm_market_access)),
  min_market_access = summary_value(
    market_access$zipcode_osrm_market_access,
    min
  ),
  median_market_access = summary_value(
    market_access$zipcode_osrm_market_access,
    stats::median
  ),
  max_market_access = summary_value(
    market_access$zipcode_osrm_market_access,
    max
  ),
  min_log_market_access = summary_value(
    market_access$log_zipcode_osrm_market_access,
    min
  ),
  median_log_market_access = summary_value(
    market_access$log_zipcode_osrm_market_access,
    stats::median
  ),
  max_log_market_access = summary_value(
    market_access$log_zipcode_osrm_market_access,
    max
  ),
  min_destinations_used = summary_value(
    market_access$n_destinations_used,
    min
  ),
  median_destinations_used = summary_value(
    market_access$n_destinations_used,
    stats::median
  ),
  max_destinations_used = summary_value(
    market_access$n_destinations_used,
    max
  )
)

fwrite(diagnostics, path_output_diagnostics)

sample_file <- block_files[1]
sample_dt <- readRDS(sample_file)
setDT(sample_dt)
sample_dt <- sample_dt[!is.na(duration_min) & duration_min > 0]
if (nrow(sample_dt) > 0) {
  message("Sample travel times from ", basename(sample_file), ":")
  print(head(sample_dt, 10))
}

message("Done.")
message("Wrote market access: ", path_output_market_access)
message("Wrote diagnostics: ", path_output_diagnostics)
