source("code/setup.R")

test <- read.csv("data/raw/CBP2015.CB1500CBP-Data.csv",
                 colClasses = c(NAICS2012 = "character"))

# Drop the label row (row 1 of the data) and subset to NAICS 4541
test_2 <- test %>%
    slice(-1) %>%
    filter(NAICS2012 == "4541") %>%
    mutate(ESTAB = as.numeric(ESTAB))

# Distribution of ESTAB across employment-size buckets
size_dist <- test_2 %>%
    group_by(EMPSZES, EMPSZES_LABEL) %>%
    summarise(
        n_rows      = n(),
        total_estab = sum(ESTAB, na.rm = TRUE),
        mean_estab  = mean(ESTAB, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(EMPSZES)

print(size_dist, n = Inf)

# County-level sparsity if we use EMPSZES >= 241 (20+ employees) as warehouse/FC
warehouse_codes <- c("241", "242", "251", "252", "254", "260")

county_warehouse <- test_2 %>%
    filter(EMPSZES %in% warehouse_codes) %>%
    group_by(GEO_ID) %>%
    summarise(estab_warehouse = sum(ESTAB, na.rm = TRUE), .groups = "drop")

# Counties that appear in NAICS 4541 at all (using "All establishments")
county_any <- test_2 %>%
    filter(EMPSZES == "001") %>%
    select(GEO_ID, estab_all = ESTAB)

merged <- county_any %>%
    left_join(county_warehouse, by = "GEO_ID") %>%
    mutate(estab_warehouse = ifelse(is.na(estab_warehouse), 0, estab_warehouse))

cat("Total counties in NAICS 4541 file:", nrow(merged), "\n")
cat("Counties with 0 warehouse (>=20 emp) estab:",
    sum(merged$estab_warehouse == 0), "\n")
cat("Share with 0:",
    round(mean(merged$estab_warehouse == 0) * 100, 1), "%\n")

# Same check for >=50 emp threshold
warehouse_codes_50 <- c("242", "251", "252", "254", "260")
county_warehouse_50 <- test_2 %>%
    filter(EMPSZES %in% warehouse_codes_50) %>%
    group_by(GEO_ID) %>%
    summarise(estab_w50 = sum(ESTAB, na.rm = TRUE), .groups = "drop")

merged_50 <- county_any %>%
    left_join(county_warehouse_50, by = "GEO_ID") %>%
    mutate(estab_w50 = ifelse(is.na(estab_w50), 0, estab_w50))

cat("\n--- 50+ emp threshold ---\n")
cat("Counties with 0:", sum(merged_50$estab_w50 == 0),
    "(", round(mean(merged_50$estab_w50 == 0) * 100, 1), "% )\n")
