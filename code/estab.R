# library ----
source("code/setup.R")
options(tigris_use_cache = TRUE)

# import raw data ----
    gdp <- read.csv("data/temp/gdp_temp.csv", colClasses = c(GEO_ID = "character"))
    cbp <- read.csv("data/temp/cbp_temp.csv", colClasses = c(GEO_ID = "character")) 
    market <- read.csv("data/temp/market_temp.csv", colClasses = c(GEOID_i = "character")) 
    raw_state <- read.csv("data/temp/state_con_tax.csv")
    raw_pop <- read_xlsx("C:/document/SMU PhD/research/Data/Census Population Estimates Program/2010-2025 county pop.xlsx") 
    tax_raw <- read_xlsx("data/raw/combined sales tax.xlsx") 
    cit_raw <- read_xlsx("data/raw/us_state_corporate_tax.xlsx")
    payroll_raw <- read.csv("data/temp/payroll_temp.csv", colClasses = c(GEO_ID = "character"))
    data("fips_codes")

# clean state tax data ----
    state_crosswalk <- fips_codes %>%
        distinct(state_name, state_code)

    tax <- tax_raw %>%
        rename(state = GEO_ID) %>%
        left_join(state_crosswalk, by = c("state" = "state_name")) %>%
        mutate(across(starts_with("tax_"), as.numeric)) %>%
        pivot_longer(
            cols = starts_with("tax_"),
            names_to = "tax_year",
            values_to = "sales_tax"
        ) %>%
        mutate(YEAR = as.integer(str_remove(tax_year, "^tax_"))) %>%
        filter(!is.na(state_code)) %>%
        select("state", state_code, YEAR, sales_tax)

    cit <- cit_raw %>%
        rename(state = state_name, state_abbr = abbrev) %>%
        pivot_longer(
            cols = starts_with("corporate_tax_"),
            names_to = "cit_year",
            values_to = "cit"
        ) %>%
        mutate(YEAR = as.integer(str_remove(cit_year, "^corporate_tax_"))) %>%
        filter(!is.na(state_abbr)) %>%
        select("state", state_abbr, YEAR, cit)
# clean population data ----

    pop <- raw_pop
    pop$NAME <- str_remove(pop$NAME, "^\\.")
    pop <- pop %>% 
        separate(NAME, c("county", "state"), sep = ", ") %>% 
        left_join(fips_codes %>% select(state_code, state_name, county_code, county), by = c("state" = "state_name", "county" = "county")) %>%
        mutate(GEO_ID = paste0(state_code, county_code)) %>%
        drop_na()

    pop_long <- pop %>%
        pivot_longer(cols = matches("^[0-9]{4}pop$"), names_to = "year_str", values_to = "pop") %>%
        mutate(YEAR = as.integer(str_remove(year_str, "pop"))) %>%
        select(GEO_ID, YEAR, pop)

# generate cty df ----
    # 1)读county边界
    cty <- counties(cb = TRUE, year = 2022, class = "sf") %>%
        st_transform(5070) %>%  # 把坐标系换为美国专用的投影坐标系（单位：米）
        select(GEOID, STATEFP, NAME)
    # 2)建立邻接关系（touches:共享边或点）
    nb <- st_touches(cty)  # list: 每个county的邻居index
    # 3)判断是否存在“跨州邻居�?
    boundary <- vapply(seq_len(nrow(cty)), function(i) {
        nbr <- nb[[i]]
        if (length(nbr) == 0) return(FALSE)
        any(cty$STATEFP[nbr] != cty$STATEFP[i])
    }, logical(1))
    cty$is_boundary_county = boundary
    # 3.5) extract cross-state adjacent county pairs (bidirectional)
    cross_state_pairs <- rbindlist(lapply(seq_len(nrow(cty)), function(i) {
        nbr <- nb[[i]]
        cross_nbr <- nbr[cty$STATEFP[nbr] != cty$STATEFP[i]]
        if (length(cross_nbr) == 0) return(NULL)
        data.table(GEO_ID = cty$GEOID[i], GEO_ID_neighbor = cty$GEOID[cross_nbr])
    })) %>%
        as_tibble() %>%
        mutate(pair_id = paste(pmin(GEO_ID, GEO_ID_neighbor),
                               pmax(GEO_ID, GEO_ID_neighbor),
                               sep = "-"))
    # 4)costalline
    coastline <- ne_download(
        scale = "medium",
        type = "coastline",
        category = "physical",
        returnclass = "sf"
    )
    coastline <- st_transform(coastline, st_crs(cty))
    # 5)international boundary
    countries <- ne_countries(scale = "medium", returnclass = "sf")
    usa <- countries %>%
        filter(admin == "United States of America")
    neighbors <- countries %>%
        filter(admin %in% c("Canada", "Mexico"))
        # border line = intersection boundary
        border_line <- st_intersection(
        st_boundary(usa),
        st_boundary(neighbors)
        ) 
        border_line = st_transform(border_line, st_crs(cty))
    # 6)costal indicator
    cty$coastal <- as.integer(
        lengths(st_intersects(cty, coastline)) > 0
    )
    # 7)international indicator
    cty$border <- as.integer(
        lengths(st_intersects(cty, border_line)) > 0
    )
    # 8)add state name from pop
    cty <- cty %>%
        left_join(pop %>% select(GEO_ID, state), by = c("GEOID" = "GEO_ID"))

# County distance to nearest state border ----
    # Exclude AK, HI, PR, and other territories
    exclude <- c("02", "15", "60", "66", "69", "72", "78")

    counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) %>%
        rename(GEO_ID = GEOID) %>%
        filter(!STATEFP %in% exclude) %>%
        st_transform(5070)

    # State border segments (unique state-pair boundaries)
    states_pre <- states(cb = TRUE, resolution = "20m", year = 2020) %>%
        filter(!STATEFP %in% exclude) %>%
        select(STUSPS, STATEFP, NAME) %>%
        st_transform(5070)
    state_border <- st_intersection(states_pre, states_pre) %>%
        filter(STUSPS != STUSPS.1) %>%
        mutate(border_name = paste0(pmin(STUSPS, STUSPS.1), "-", pmax(STUSPS, STUSPS.1))) %>%
        group_by(border_name) %>%
        summarise(geometry = st_union(geometry), .groups = "drop")

    # Chordal distance: find nearest border point in geographic CRS,
    # then compute 3-D Euclidean distance through the Earth (chord length)
    county_cent <- st_centroid(counties_sf)
    nearest_id  <- st_nearest_feature(county_cent, state_border)

    county_cent_geo  <- st_transform(county_cent,  4326)
    state_border_geo <- st_transform(state_border, 4326)

    nearest_pts_geo <- st_nearest_points(
        county_cent_geo,
        state_border_geo[nearest_id, ],
        pairwise = TRUE
    )

    coords_mat <- st_coordinates(nearest_pts_geo)
    from_lon <- coords_mat[seq(1, nrow(coords_mat), 2), "X"] * pi / 180
    from_lat <- coords_mat[seq(1, nrow(coords_mat), 2), "Y"] * pi / 180
    to_lon   <- coords_mat[seq(2, nrow(coords_mat), 2), "X"] * pi / 180
    to_lat   <- coords_mat[seq(2, nrow(coords_mat), 2), "Y"] * pi / 180

    R_earth <- 6371   # km
    x1 <- cos(from_lat) * cos(from_lon); y1 <- cos(from_lat) * sin(from_lon); z1 <- sin(from_lat)
    x2 <- cos(to_lat)   * cos(to_lon);   y2 <- cos(to_lat)   * sin(to_lon);   z2 <- sin(to_lat)

    counties_sf$dist_to_border <- R_earth * sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)
    counties_sf$nearest_border <- state_border$border_name[nearest_id]

    # Minimum chordal distance from county polygon edge to nearest state border
    counties_geo     <- st_transform(counties_sf, 4326)
    nearest_pts_edge <- st_nearest_points(
        counties_geo,
        state_border_geo[nearest_id, ],
        pairwise = TRUE
    )
    # Cast to LINESTRING so st_coordinates works (each line = 2 rows: from, to)
    lines_edge  <- st_cast(nearest_pts_edge, "LINESTRING")
    coords_edge <- st_coordinates(lines_edge)
    e_from_lon <- coords_edge[seq(1, nrow(coords_edge), 2), "X"] * pi / 180
    e_from_lat <- coords_edge[seq(1, nrow(coords_edge), 2), "Y"] * pi / 180
    e_to_lon   <- coords_edge[seq(2, nrow(coords_edge), 2), "X"] * pi / 180
    e_to_lat   <- coords_edge[seq(2, nrow(coords_edge), 2), "Y"] * pi / 180

    ex1 <- cos(e_from_lat) * cos(e_from_lon); ey1 <- cos(e_from_lat) * sin(e_from_lon); ez1 <- sin(e_from_lat)
    ex2 <- cos(e_to_lat)   * cos(e_to_lon);   ey2 <- cos(e_to_lat)   * sin(e_to_lon);   ez2 <- sin(e_to_lat)

    counties_sf$dist_to_border_edge <- R_earth * sqrt((ex2-ex1)^2 + (ey2-ey1)^2 + (ez2-ez1)^2)

    rm(coastline, countries, border_line, nb, neighbors, usa,
       states_pre, nearest_id,
       county_cent_geo, state_border_geo, nearest_pts_geo,
       coords_mat, from_lon, from_lat, to_lon, to_lat,
       x1, y1, z1, x2, y2, z2, R_earth,
       counties_geo, nearest_pts_edge, lines_edge, coords_edge,
       e_from_lon, e_from_lat, e_to_lon, e_to_lat,
       ex1, ey1, ez1, ex2, ey2, ez2)


# combine main ----
    # add back county's own gdp to gdp_in and both market access measures
    # only keep counties in the 48 contiguous states and DC, drop AK, HI, PR, and other territories
    main <- cbp %>% 
    left_join(cty %>% select(GEOID, "state", is_boundary_county, STATEFP, coastal, border), by = c("GEO_ID" = "GEOID")) %>%
    left_join(pop_long, by = c("GEO_ID", "YEAR")) %>%
    filter(!STATEFP %in% exclude) %>% 
    left_join(gdp, by = c("YEAR" = "YEAR", "GEO_ID" = "GEO_ID")) %>%
    left_join(payroll_raw, by = c("YEAR", "GEO_ID")) %>%
    left_join(market, by = c("YEAR" = "year", "GEO_ID" = "GEOID_i")) %>%
    mutate(asinh_estab = asinh(ESTAB),
            gdp = as.numeric(gdp),
            PAYANN = as.numeric(PAYANN),
            ma_in = ma_in + PAYANN,
            ma_in_new = ma_in_new + PAYANN,
            lma_in = log(ma_in),
            lma_out = log(ma_out),
            lma = lma_in + lma_out,
            lma_in_expo = log(ma_in_new),
            lma_out_expo = log(ma_out_new),
            asinh_emp = asinh(EMP),
            asinh_estab_big = asinh(ESTAB_big),
            lpayroll = log(PAYANN),
            lma_tax_in = log(ma_tax_in),
            lma_tax_out = log(ma_tax_out)) %>%
    left_join(raw_state %>% select(state, year, expo), by = c("state" = "state", "YEAR" = "year")) %>%
    left_join(tax %>% select(state, YEAR, sales_tax), by = c("state", "YEAR")) %>%
    left_join(cit %>% select(state, YEAR, cit, state_abbr), by = c("state", "YEAR")) %>%
    mutate(
        type = ifelse(NAICS == "4541", "online", "local"),
        state_abbr = coalesce(state_abbr, state.abb[match(state, state.name)])
    )

    # drop county with only one year obs and na
    main <- main %>% 
        group_by(GEO_ID) %>% 
        filter(n() > 1) %>% 
        ungroup() %>% 
        drop_na()

    years <- 2015:2022

    county_border_lookup <- counties_sf %>%
        st_drop_geometry() %>%
        select(GEO_ID, dist_to_border, dist_to_border_edge, nearest_border) %>%
        separate(nearest_border, into = c("state1", "state2"), sep = "-", remove = FALSE)

    state_tax_lookup <- main %>%
        select(YEAR, state, state_abbr, sales_tax, cit) %>%
        distinct()

    # Distance to border is in km
    main <- main %>%
        left_join(county_border_lookup, by = "GEO_ID") %>%
        mutate(
            state_home = state_abbr,
            state_other = case_when(
                state1 == state_home ~ state2,
                state2 == state_home ~ state1,
                TRUE ~ NA_character_
            )
        ) %>%
        left_join(
            state_tax_lookup %>%
                select(state_abbr, YEAR, sales_tax) %>%
                distinct() %>%
                rename(tax_other = sales_tax),
            by = c("state_other" = "state_abbr", "YEAR" = "YEAR")
        ) %>%
        mutate(
            tax_diff = sales_tax - tax_other,
            tax_diff_abs = abs(tax_diff),
            high_tax_side = case_when(
                is.na(state_other) ~ NA_integer_,
                sales_tax > tax_other ~ 1L,
                TRUE ~ -1L
            ),
            dist_to_border_signed = dist_to_border * high_tax_side,
            dist_to_border_edge_signed = dist_to_border_edge * high_tax_side,
            high_tax_dummy = as.integer(high_tax_side == 1),
            # Wayfair adoption year of the OPPOSITE state (what removes own residents' tax arbitrage)
            adoption_year_other = case_when(
                state_other %in% c("MA", "NY", "OH", "PA") ~ 2018L,
                state_other %in% c("AZ", "AR", "NM", "OK", "RI", "TN", "TX", "VA") ~ 2020L,
                state_other == "LA" ~ 2021L,
                state_other %in% c("FL", "KS") ~ 2022L,
                state_other == "MO" ~ 2023L,
                is.na(state_other) ~ NA_integer_,
                TRUE ~ 2019L
            ),
            post_wayfair = as.integer(YEAR >= adoption_year_other)
        )

# border county pairs ----
    # Each row = focal county × one cross-state neighbor × year × NAICS type
    # Both counties in a pair appear (one as focal, one as neighbor), linked by pair_id
    border_county_pairs <- cross_state_pairs %>%
        left_join(main, by = "GEO_ID")

    # Balanced pairs: keep only pairs where BOTH counties have online obs in ALL years
    balanced_pair_ids <- border_county_pairs %>%
        filter(type == "online") %>%
        group_by(pair_id, GEO_ID) %>%
        summarise(n_years = n_distinct(YEAR), .groups = "drop") %>%
        group_by(pair_id) %>%
        filter(all(n_years == length(years))) %>%
        pull(pair_id) %>%
        unique()

    border_county_balanced <- border_county_pairs %>%
        filter(type == "online", pair_id %in% balanced_pair_ids)


# pair balance test ----
    # Within-pair balance on pre-Wayfair (YEAR < 2018) levels:
    # for each pair × year, regress characteristic on high_tax_dummy with pair FE.
    # Coef = high-tax-side mean minus low-tax-side mean within pair.
    bal_data <- border_county_balanced %>%
        filter(YEAR < 2018, !is.na(high_tax_dummy))

    bal_vars <- c("gdp", "PAYANN", "pop", "ma_in", "ma_out",
                  "lpayroll", "lma_in", "lma_out")

    bal_models <- lapply(bal_vars, function(v) {
        feols(
            as.formula(paste0(v, " ~ high_tax_dummy | YEAR^pair_id")),
            data    = bal_data,
            cluster = ~ pair_id
        )
    })
    names(bal_models) <- bal_vars

    bal_table <- do.call(rbind, lapply(bal_vars, function(v) {
        m   <- bal_models[[v]]
        ct  <- summary(m)$coeftable["high_tax_dummy", ]
        cf  <- as.numeric(ct["Estimate"])
        se_ <- as.numeric(ct["Std. Error"])
        pv  <- as.numeric(ct["Pr(>|t|)"])
        ymean <- mean(bal_data[[v]], na.rm = TRUE)
        data.frame(
            variable    = v,
            mean_y      = ymean,
            diff_high   = cf,
            se          = se_,
            p_value     = pv,
            pct_of_mean = 100 * cf / ymean,
            n_obs       = m$nobs,
            row.names   = NULL
        )
    }))
    print(bal_table, row.names = FALSE)

    etable(bal_models,
           headers      = bal_vars,
           tex          = TRUE,
           style.tex    = style.tex(main = "aer", notes.tpt.intro = ""),
           se.below     = TRUE,
           fitstat      = c("n", "r2"),
           digits       = 3,
           file         = "output/pair_balance_test.tex",
           replace      = TRUE,
           title        = "Pair balance test: within-pair difference (high-tax minus low-tax), pre-Wayfair years (2015-2017)")


    # Pair-time FE panel regression
    # post_wayfair = 1 from the year the OPPOSITE state adopted Wayfair (2019 default; FL/KS 2022, LA 2021, MO 2023)
    pair_reg_data <- border_county_balanced %>%
        filter(type == "online") %>%
        arrange(GEO_ID, YEAR) %>%
        group_by(GEO_ID) %>%
        mutate(
            d_emp = EMP - lag(EMP),
            d_asinh_emp = asinh_emp - lag(asinh_emp)
        ) %>%
        ungroup() %>%
        filter(!is.na(d_asinh_emp))

    pair_reg <- feols(
        d_emp ~
        lma_in + lma_out + sales_tax + sales_tax:post_wayfair + cit + pop | YEAR + pair_id + GEO_ID,
        data = pair_reg_data,
        cluster = ~STATEFP + nearest_border
    )
    summary(pair_reg)

    pair_reg_data %>%
    summarise(
        N = sum(!is.na(d_emp)),
        mean = mean(d_emp, na.rm = TRUE),
        sd = sd(d_emp, na.rm = TRUE),
        min = min(d_emp, na.rm = TRUE),
        p1 = quantile(d_emp, 0.01, na.rm = TRUE),
        p5 = quantile(d_emp, 0.05, na.rm = TRUE),
        p10 = quantile(d_emp, 0.10, na.rm = TRUE),
        p25 = quantile(d_emp, 0.25, na.rm = TRUE),
        median = median(d_emp, na.rm = TRUE),
        p75 = quantile(d_emp, 0.75, na.rm = TRUE),
        p90 = quantile(d_emp, 0.90, na.rm = TRUE),
        p95 = quantile(d_emp, 0.95, na.rm = TRUE),
        p99 = quantile(d_emp, 0.99, na.rm = TRUE),
        max = max(d_emp, na.rm = TRUE)
    )
    dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
    dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
    etable(pair_reg,
        tex = TRUE,
        style.tex = style.tex(main = "aer", notes.tpt.intro = ""),
        se.below = TRUE,
        fitstat = c("n", "r2"),
        digits = 3,
        file = "output/tables/pair_reg.tex"
    )

    # Dynamic event study: high_tax_dummy effect by event time (relative to opposite state's Wayfair)
    border_county_balanced_dyn <- border_county_balanced %>%
        filter(type == "online") %>%
        mutate(
            event_time = YEAR - adoption_year_other,
            high_tax_gap = high_tax_dummy * tax_diff_abs
        )

    pair_event_intercept <- feols(
        asinh_emp ~ lma_in + lma_out + cit + pop +
               i(event_time, high_tax_dummy, ref = -1) |
               YEAR^pair_id + GEO_ID,
        data = border_county_balanced_dyn,
        cluster = ~STATEFP + nearest_border
    )
    summary(pair_event_intercept)

    pair_event_gap <- feols(
        asinh_emp ~ lma_in + lma_out + cit + pop + high_tax_dummy +
               i(event_time, high_tax_gap, ref = -1) |
               YEAR^pair_id + GEO_ID,
        data = border_county_balanced_dyn,
        cluster = ~STATEFP + nearest_border
    )
    summary(pair_event_gap)

    png("output/pair_event_intercept.png", width = 1000, height = 600, res = 150)
    iplot(pair_event_intercept,
          main = "Dynamic: high_tax_dummy (intercept shift)",
          xlab = "Event time (years from opposite state's Wayfair)",
          ylab = "Coefficient")
    abline(v = -0.5, lty = 2, col = "red")
    dev.off()

    png("output/pair_event_gap.png", width = 1000, height = 600, res = 150)
    iplot(pair_event_gap,
          main = "Dynamic: high_tax_dummy x tax_diff_abs (gap scaling)",
          xlab = "Event time (years from opposite state's Wayfair)",
          ylab = "Coefficient")
    abline(v = -0.5, lty = 2, col = "red")
    dev.off()

    # Event study: lma_tax_out coefficient by year (ref = 2018)
    pair_event <- feols(
        asinh_emp ~
        lma + lma_tax_in + lma_tax_in * I(YEAR >= 2019) +
        i(YEAR, lma_tax_out, ref = 2018) +
        cit + pop | YEAR^pair_id + GEO_ID,
        data = border_county_balanced %>% filter(type == "online"),
        cluster = ~STATEFP + nearest_border
    )
    summary(pair_event)

    iplot(pair_event,
        main = "Event Study: lma_tax_out (ref = 2018)",
        xlab = "Year",
        ylab = "Coefficient on lma_tax_out"
    )
    abline(v = 2018.5, lty = 2, col = "red")

# Neighbor county year-by-year 2x2 models ----
    dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
    dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

    neighbor_county_changes <- border_county_balanced %>%
        filter(type == "online") %>%
        distinct(GEO_ID, YEAR, ESTAB, EMP) %>%
        mutate(
            ESTAB = as.numeric(ESTAB),
            EMP = as.numeric(EMP)
        ) %>%
        arrange(GEO_ID, YEAR) %>%
        group_by(GEO_ID) %>%
        mutate(
            d_estab = ESTAB - lag(ESTAB),
            d_emp = EMP - lag(EMP)
        ) %>%
        ungroup() %>%
        select(GEO_ID, YEAR, d_estab, d_emp)

    neighbor_county_year_data <- border_county_balanced %>%
        filter(type == "online") %>%
        mutate(
            ESTAB = as.numeric(ESTAB),
            EMP = as.numeric(EMP)
        ) %>%
        left_join(neighbor_county_changes, by = c("GEO_ID", "YEAR"))

    neighbor_county_outcomes <- tibble(
        outcome_family = c("Establishments", "Establishments", "Employment", "Employment"),
        outcome = c("d_estab", "ESTAB", "d_emp", "EMP"),
        outcome_label = c(
            "Change in establishments",
            "Establishments (PPML proportional effect)",
            "Change in employment",
            "Employment (PPML proportional effect)"
        ),
        model_type = c("OLS", "PPML", "OLS", "PPML")
    )

    neighbor_county_treatments <- tibble(
        treatment = c("high_tax_dummy", "sales_tax"),
        treatment_label = c("High-tax dummy", "Sales tax rate (1 pp)"),
        treatment_effect_unit = c(1, 0.01)
    )

    neighbor_county_fe_specs <- tibble(
        pair_fe = c(FALSE, TRUE),
        fe_label = c("No pair FE", "Pair FE")
    )

    run_neighbor_county_year_model <- function(outcome_family,
                                               outcome_var,
                                               outcome_label,
                                               model_type,
                                               treatment_var,
                                               treatment_label,
                                               treatment_effect_unit,
                                               pair_fe,
                                               fe_label,
                                               yr) {
        model_data <- neighbor_county_year_data %>%
            filter(YEAR == yr) %>%
            filter(
                !is.na(.data[[outcome_var]]),
                !is.na(.data[[treatment_var]]),
                !is.na(pop),
                !is.na(cit),
                !is.na(lma_in),
                !is.na(lma_out),
                !is.na(border),
                !is.na(coastal)
            )

        if (pair_fe) {
            model_data <- model_data %>% filter(!is.na(pair_id))
        }

        if (nrow(model_data) == 0) return(NULL)

        rhs <- paste(
            c(treatment_var, "pop", "cit", "lma_in", "lma_out", "border", "coastal"),
            collapse = " + "
        )
        model_formula <- as.formula(paste0(
            outcome_var, " ~ ", rhs,
            if (pair_fe) " | pair_id" else ""
        ))

        fit <- tryCatch({
            if (model_type == "PPML") {
                fepois(
                    model_formula,
                    data = model_data,
                    cluster = ~STATEFP + nearest_border
                )
            } else {
                feols(
                    model_formula,
                    data = model_data,
                    cluster = ~STATEFP + nearest_border
                )
            }
        }, error = function(e) NULL)

        if (is.null(fit)) return(NULL)

        coef_names <- names(coef(fit))
        if (!treatment_var %in% coef_names) return(NULL)

        crit <- qnorm(0.975)

        if (model_type == "PPML") {
            b <- as.numeric(coef(fit)[treatment_var])
            v <- vcov(fit)
            se_b <- as.numeric(sqrt(v[treatment_var, treatment_var]))

            estimate <- exp(treatment_effect_unit * b) - 1
            estimate_se <- treatment_effect_unit * exp(treatment_effect_unit * b) * se_b
            ci_l <- exp(treatment_effect_unit * (b - crit * se_b)) - 1
            ci_r <- exp(treatment_effect_unit * (b + crit * se_b)) - 1
            estimand <- "proportional_effect"
        } else {
            estimate <- treatment_effect_unit * as.numeric(coef(fit)[treatment_var])
            estimate_se <- treatment_effect_unit * as.numeric(se(fit)[treatment_var])
            ci_l <- estimate - crit * estimate_se
            ci_r <- estimate + crit * estimate_se
            estimand <- "coefficient"
        }

        tibble(
            outcome_family = outcome_family,
            outcome = outcome_var,
            outcome_label = outcome_label,
            model_type = model_type,
            treatment = treatment_var,
            treatment_label = treatment_label,
            treatment_effect_unit = treatment_effect_unit,
            pair_fe = pair_fe,
            fe_label = fe_label,
            estimand = estimand,
            year = yr,
            estimate = estimate,
            se = estimate_se,
            ci_l = ci_l,
            ci_r = ci_r,
            n_obs = fit$nobs
        )
    }

    neighbor_county_year_models <- bind_rows(lapply(seq_len(nrow(neighbor_county_outcomes)), function(i) {
        bind_rows(lapply(seq_len(nrow(neighbor_county_treatments)), function(j) {
            bind_rows(lapply(seq_len(nrow(neighbor_county_fe_specs)), function(k) {
                bind_rows(lapply(years, function(y) {
                    run_neighbor_county_year_model(
                        outcome_family = neighbor_county_outcomes$outcome_family[i],
                        outcome_var = neighbor_county_outcomes$outcome[i],
                        outcome_label = neighbor_county_outcomes$outcome_label[i],
                        model_type = neighbor_county_outcomes$model_type[i],
                        treatment_var = neighbor_county_treatments$treatment[j],
                        treatment_label = neighbor_county_treatments$treatment_label[j],
                        treatment_effect_unit = neighbor_county_treatments$treatment_effect_unit[j],
                        pair_fe = neighbor_county_fe_specs$pair_fe[k],
                        fe_label = neighbor_county_fe_specs$fe_label[k],
                        yr = y
                    )
                }))
            }))
        }))
    })) %>%
        mutate(
            outcome_family = factor(outcome_family, levels = c("Establishments", "Employment")),
            treatment_label = factor(treatment_label, levels = neighbor_county_treatments$treatment_label),
            fe_label = factor(fe_label, levels = neighbor_county_fe_specs$fe_label)
        ) %>%
        arrange(outcome_family, treatment, pair_fe, outcome, model_type, year)

    write_csv(
        neighbor_county_year_models,
        "output/tables/neighbor_county_year_by_year_2x2.csv"
    )

    plot_neighbor_county_year_2x2 <- function(plot_data, family_label, output_file) {
        p <- plot_data %>%
            filter(outcome_family == family_label) %>%
            ggplot(aes(x = year, y = estimate, color = outcome_label, group = outcome_label)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
            geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
            geom_line(linewidth = 0.75) +
            geom_point(size = 2.1) +
            facet_grid(treatment_label ~ fe_label, scales = "free_y") +
            scale_x_continuous(breaks = years) +
            labs(
                title = paste0("Neighbor County Year-by-Year ", family_label, " Models"),
                subtitle = "Rows: treatment variable; columns: pair fixed effects. Sales tax effects are for a 1 percentage point increase.",
                x = "Year",
                y = "Estimate",
                color = NULL
            ) +
            theme_bw() +
            theme(
                legend.position = "bottom",
                strip.background = element_rect(fill = "grey90", color = "grey60")
            )

        ggsave(output_file, p, width = 11, height = 8, dpi = 150)
        print(p)
        invisible(p)
    }

    neighbor_county_estab_2x2_plot <- plot_neighbor_county_year_2x2(
        neighbor_county_year_models,
        "Establishments",
        "output/figures/neighbor_county_establishment_year_by_year_2x2.png"
    )

    neighbor_county_emp_2x2_plot <- plot_neighbor_county_year_2x2(
        neighbor_county_year_models,
        "Employment",
        "output/figures/neighbor_county_employment_year_by_year_2x2.png"
    )
# Year-by-year OLS ----
    poly_degree <- 2
    dist_var <- "dist_to_border_edge"
    dist_poly_terms <- c(
        dist_var,
        paste0("I(", dist_var, "^", 2:poly_degree, ")")
    )
    interaction_terms <- paste0("high_tax_dummy:", dist_poly_terms)
    normalize_term <- function(x) {
        strip_I <- function(term) {
            while (grepl("^I\\(.*\\)$", term)) {
                term <- sub("^I\\((.*)\\)$", "\\1", term)
            }
            term
        }

        x <- gsub("[`[:space:]]", "", x)
        vapply(strsplit(x, ":", fixed = TRUE), function(parts) {
            parts <- vapply(parts, strip_I, character(1))
            if (length(parts) > 1) {
                paste(sort(parts), collapse = ":")
            } else {
                parts
            }
        }, character(1))
    }
    match_terms <- function(coef_names, expected_terms) {
        idx <- match(normalize_term(expected_terms), normalize_term(coef_names))
        if (any(is.na(idx))) {
            stop(
                "Cannot match model terms. Expected: ",
                paste(expected_terms, collapse = ", "),
                ". Actual coefficient names: ",
                paste(coef_names, collapse = ", ")
            )
        }
        coef_names[idx]
    }
    marginal_effect_group <- function(est, dist_grid, group_value) {
        coef_names <- names(coef(est))
        dist_terms <- match_terms(coef_names, dist_poly_terms)
        interaction_terms_matched <- match_terms(coef_names, interaction_terms)

        main_grad <- sapply(seq_along(dist_terms), function(p) {
            p * dist_grid^(p - 1)
        })
        if (!is.matrix(main_grad)) main_grad <- matrix(main_grad, ncol = 1)

        grad <- cbind(main_grad, group_value * main_grad)
        term_names <- c(dist_terms, interaction_terms_matched)
        b <- coef(est)[term_names]
        V <- vcov(est)[term_names, term_names, drop = FALSE]
        me <- as.vector(grad %*% b)
        se <- sqrt(pmax(diag(grad %*% V %*% t(grad)), 0))

        data.frame(
            distance = dist_grid,
            high_tax_dummy = group_value,
            tax_side = ifelse(group_value == 1, "High-tax side", "Low-tax side"),
            marginal_effect = me,
            ci_lo = me - 1.96 * se,
            ci_hi = me + 1.96 * se
        )
    }
    ols_formula <- as.formula(
        paste(
            "asinh_emp ~ lma_in + lma_out + cit + pop + coastal + border + high_tax_dummy +",
            paste(c(dist_poly_terms, interaction_terms), collapse = " + ")
        )
    )

    ols_by_year <- lapply(years, function(y) {
        feols(
            ols_formula,
            data = main %>% filter(type == "online", YEAR == y),
            cluster = ~STATEFP + nearest_border
        )
    })
    names(ols_by_year) <- years
    do.call(
        etable,
        c(
            ols_by_year,
            list(
                headers = paste("Year", years),
                tex = TRUE,
                style.tex = style.tex(main = "aer", notes.tpt.intro = ""),
                se.below = TRUE,
                fitstat = c("n", "r2"),
                digits = 3,
                file = "output/ols_by_year.tex",
                replace = TRUE,
                title = "Year-by-year OLS: online employment on distance polynomial and high-tax-side interactions"
            )
        )
    )

    # I tried plotting the 5th polynomial curves: no crazy things are happening, basically a negative slope curve. 
    # Only keep distance/dummy coefficients for plotting
    all_coefs <- names(coef(ols_by_year[[1]]))
    print(all_coefs)
    drop_pattern <- "^(\\(Intercept\\)|lma_in|lma_out|cit|pop|coastal|border)$"
    plot_coefs <- all_coefs[!grepl(drop_pattern, all_coefs)]

    coef_df <- do.call(rbind, lapply(years, function(y) {
        est <- ols_by_year[[as.character(y)]]
        data.frame(
            year  = y,
            var   = plot_coefs,
            coef  = as.numeric(coef(est)[plot_coefs]),
            se    = as.numeric(se(est)[plot_coefs])
        )
    }))
    coef_df$ci_lo <- coef_df$coef - 1.96 * coef_df$se
    coef_df$ci_hi <- coef_df$coef + 1.96 * coef_df$se

    n_coef <- length(plot_coefs)
    n_col  <- ceiling(n_coef / 2)
    png("output/ols_by_year.png", width = n_col * 400, height = 900, res = 150)
    par(mfrow = c(2, n_col), mar = c(4, 4, 2, 1))
    for (v in plot_coefs) {
        d <- coef_df[coef_df$var == v, ]
        plot(d$year, d$coef, type = "b", pch = 19,
             ylim = range(c(d$ci_lo, d$ci_hi)),
             main = v, xlab = "Year", ylab = "Coefficient")
        arrows(d$year, d$ci_lo, d$year, d$ci_hi,
               angle = 90, code = 3, length = 0.05, col = "grey40")
        abline(h = 0, lty = 2, col = "red")
    }
    dev.off()

    # Marginal effect of distance:
    # with a K-th order raw polynomial,
    # dE[asinh_emp]/d distance = beta_1 + 2 * beta_2 * d + ... + K * beta_K * d^(K-1).
    # Standard errors use the delta method with the clustered vcov matrix.
    dist_grid <- seq(
        quantile(main[[dist_var]][main$type == "online"], 0.01, na.rm = TRUE),
        quantile(main[[dist_var]][main$type == "online"], 0.99, na.rm = TRUE),
        length.out = 200
    )
    me_df <- bind_rows(lapply(years, function(y) {
        est <- ols_by_year[[as.character(y)]]
        bind_rows(
            marginal_effect_group(est, dist_grid, 0),
            marginal_effect_group(est, dist_grid, 1)
        ) %>%
            mutate(year = y)
    }))

    me_plot <- ggplot(me_df, aes(x = distance, y = marginal_effect,
                                 color = tax_side, fill = tax_side)) +
        geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2, color = NA) +
        geom_line(size = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        facet_wrap(~ year, ncol = 4,
                   labeller = labeller(year = function(x) paste("Year", x))) +
        scale_color_manual(values = c("Low-tax side" = "steelblue", "High-tax side" = "darkorange")) +
        scale_fill_manual(values = c("Low-tax side" = "steelblue", "High-tax side" = "darkorange")) +
        labs(
            title = "Marginal Effect of Distance to State Border",
            subtitle = "Online sector by tax side",
            x = "Distance to border edge (km)",
            y = "Marginal effect on asinh_emp",
            color = NULL,
            fill = NULL
        ) +
        theme_minimal()

    ggplot2::ggsave("output/marginal_effect_by_year.png", me_plot,
                    width = 12, height = 8, dpi = 150)

# regression ----
    # PPML (Poisson PML) �?dependent variable in levels (EMP), coefficients are semi-elasticities
    reg <- fepois(
        EMP ~
        lma_in + lma_out + lma_tax_in + lma_tax_in * I(YEAR >= 2019) + lma_tax_out + lma_tax_out * I(YEAR >= 2019) + cit + pop | YEAR + GEO_ID,
        data = main %>% filter(type == "online"),
        cluster = ~STATEFP
    )

    # Subsample: counties within 50km of a state border
    reg_border50 <- fepois(
        EMP ~
        lma_in + lma_out + lma_tax_in + lma_tax_in * I(YEAR >= 2019) + lma_tax_out + lma_tax_out * I(YEAR >= 2019) + cit + pop | YEAR + GEO_ID,
        data = main %>% filter(abs(dist_to_border_signed) <= 50 & type == "online"),
        cluster = ~STATEFP
    )

    etable(reg, reg_border50,
        headers = c("Full sample", "Border 50km"),
        tex = TRUE,
        style.tex = style.tex(main = "aer", notes.tpt.intro = ""),
        drop = "GEO_ID",
        se.below = FALSE,
        fitstat = c("n", "r2"),
        digits = 3,
        file = "output/reg_border50.tex"
    )

    # RDD
        # my approach
        panel_groups <- list(
            `2015-2018` = 2015:2018,
            `2019-2022` = 2019:2022
        )
        
        # all cluster except county in rdd cause errors (for unknown reasons), so just use county cluster and HC1 for now
        # ideally, we want to cluster on border pair
        bandwidths <- c(20, 50)

        rdd_results <- lapply(names(panel_groups), function(group_name) {
            group_years <- panel_groups[[group_name]]

            group_data <- main %>%
                filter(YEAR %in% group_years, type == "online") %>%
                select(GEO_ID, YEAR, STATEFP, asinh_emp, dist_to_border_signed,
                    lma_in, lma_out, cit, EMP, nearest_border, pop) %>%
                drop_na()

            # make sure both sides of the border are represented within the bandwidth
            in_bw <- group_data %>% filter(abs(dist_to_border_signed) <= 50)
            valid_pairs <- in_bw %>%
                mutate(side = ifelse(dist_to_border_signed > 0, "right", "left")) %>%
                group_by(nearest_border) %>%
                summarise(n_sides = n_distinct(side)) %>%
                filter(n_sides == 2) %>%
                pull(nearest_border)

            group_data <- group_data %>% filter(nearest_border %in% valid_pairs)

            year_dummies <- model.matrix(~ factor(YEAR), data = group_data)[, -1, drop = FALSE]
            pair_dummies <- model.matrix(~ factor(nearest_border), data = group_data)[, -1, drop = FALSE]

            covs_mat <- cbind(group_data$lma_in, group_data$lma_out,
                            group_data$cit, year_dummies, pair_dummies, group_data$pop)
            # drop linearly dependent covariates using QR decomposition, for now no one is dropped
            covs_mat <- covs_mat[, qr(covs_mat)$pivot[1:qr(covs_mat)$rank]]

            lapply(bandwidths, function(bw) {
                reg_rdd <- rdrobust(
                    y = group_data$asinh_emp,
                    x = group_data$dist_to_border_signed,
                    covs = covs_mat,
                    cluster = group_data$GEO_ID,
                    vce = "hc1",
                    masspoints = "adjust",
                    h = bw,
                    b = bw
                )

                tibble(
                    panel_group = group_name,
                    bandwidth = bw,
                    N = nrow(group_data),
                    N_h_l = reg_rdd$N_h[1],
                    N_h_r = reg_rdd$N_h[2],
                    bw_l = reg_rdd$bws[1, 1],
                    bw_r = reg_rdd$bws[1, 2],
                    coef = reg_rdd$coef[3],
                    se = reg_rdd$se[3],
                    p = reg_rdd$pv[3],
                    ci_l = reg_rdd$ci[3, 1],
                    ci_r = reg_rdd$ci[3, 2]
                )
            })
        })

        rdd_summary <- bind_rows(unlist(rdd_results, recursive = FALSE))
        rdd_summary <- as.data.frame(rdd_summary) %>% 
            arrange(bandwidth, panel_group)
        stargazer(
            rdd_summary,
            type = "latex",
            summary = FALSE,
            rownames = FALSE,
            out = "output/rdd_summary.tex"
        )

        # Grembi et al. 2016 approach
        grembi <- main %>%
            filter(type == "online") %>%
            mutate(S = ifelse(dist_to_border_signed > 0, 1, 0),
                   Tt = ifelse(YEAR <= 2018, 1, 0))
        grembi_50000 <- grembi %>% filter(abs(dist_to_border_signed) <= 50)
        grembi_20000 <- grembi %>% filter(abs(dist_to_border_signed) <= 20)

            # heterogeneous effect by tax difference
            grembi_rdd <- feols(
            asinh_emp ~ dist_to_border_signed + S + S:dist_to_border_signed +
                Tt + Tt:dist_to_border_signed +
                S:Tt + S:Tt:dist_to_border_signed +
                # heterogeneity terms: full interactions
                tax_diff + Tt:tax_diff + Tt:dist_to_border_signed:tax_diff + S:Tt:tax_diff + Tt:S:dist_to_border_signed:tax_diff
                + S:tax_diff + dist_to_border_signed:tax_diff + S:dist_to_border_signed:tax_diff + 
                + lma_in_expo:tax_diff + lma_out_expo:tax_diff + cit:tax_diff + pop:tax_diff
                + lma_in_expo + lma_out_expo + cit + pop |
                YEAR + nearest_border,
            data = grembi_20000,
            cluster = ~GEO_ID
            )
            # S:Tt is beta_0, the parameter of interest
            summary(grembi_rdd)
            etable(grembi_rdd,
                tex = TRUE,
                style.tex = style.tex(
                    main = "aer",
                    notes.tpt.intro = ""        
                ),
                drop = "YEAR|nearest_border",   
                se.below = FALSE,               
                fitstat = c("n", "r2"),    
                digits = 3,      
                file = "output/grembi_rdd_heter.tex"
            )

        # Dynamic (event study) Grembi approach �?mirrors grembi_rdd spec year-by-year
            # Each Tt × ... term is replaced with i(YEAR_f, ..., ref = "2018")
            # Static terms (no Tt): dist_to_border_signed, S, S:dist_to_border_signed, S:tax_diff_abs
            grembi_event <- grembi_20000 %>%
                mutate(
                    YEAR_f      = factor(YEAR),
                    S_slope     = S * dist_to_border_signed,          # Tt:S:dist  �?i(YEAR_f, S_slope)
                    dist_tax    = dist_to_border_signed * tax_diff_abs, # Tt:dist:tax_diff �?i(YEAR_f, dist_tax)
                    S_slope_tax = S * dist_to_border_signed * tax_diff_abs  # Tt:S:dist:tax_diff �?i(YEAR_f, S_slope_tax)
                )

            grembi_event_rdd <- feols(
                asinh_emp ~ dist_to_border_signed + S + S:dist_to_border_signed +
                    S:tax_diff_abs +                                        # static: S × tax_diff (no Tt)
                    i(YEAR_f, dist_to_border_signed, ref = "2018") +       # Tt:dist
                    i(YEAR_f, S,           ref = "2018") +                 # S:Tt �?key dynamic effect
                    i(YEAR_f, S_slope,     ref = "2018") +                 # S:Tt:dist
                    i(YEAR_f, tax_diff_abs, ref = "2018") +                # Tt:tax_diff
                    i(YEAR_f, dist_tax,    ref = "2018") +                 # Tt:dist:tax_diff
                    i(YEAR_f, S_slope_tax, ref = "2018") +                 # Tt:S:dist:tax_diff
                    lma_in_expo + lma_out_expo + cit + pop |
                    YEAR + nearest_border,
                data    = grembi_event,
                cluster = ~GEO_ID
            )
            summary(grembi_event_rdd)

            # Event-study plot: coefficient on i(YEAR_f, S) = S:Tt by year (ref = 2018)
            iplot(grembi_event_rdd,
                i.select = 2,          # second i() term = i(YEAR_f, S, ...)
                main  = "Dynamic Treatment Effects (Grembi RDD-DiD, bw=20km)",
                xlab  = "Year",
                ylab  = "Coefficient on S × Year (ref = 2018)"
            )
            abline(v = 2018.5, lty = 2, col = "red")  # Wayfair decision cutoff

            # Save event-study coefficients as table
            etable(grembi_event_rdd,
                tex = TRUE,
                style.tex = style.tex(
                    main = "aer",
                    notes.tpt.intro = ""
                ),
                drop = "YEAR|nearest_border",
                se.below = FALSE,
                fitstat = c("n", "r2"),
                digits = 3,
                file = "output/grembi_rdd_dynamic.tex"
            )
            
        # Butts 2023 approach ----
        # first difference
        butt <- main %>%
            filter(type == "online") %>%
            filter(YEAR %in% c(2017, 2019)) %>%
            group_by(GEO_ID) %>%
            arrange(YEAR) %>%
            mutate(d_asinh_emp = asinh_emp - lag(asinh_emp),
                   d_dist = dist_to_border_signed - lag(dist_to_border_signed),
                   d_lma_in = lma_in - lag(lma_in),
                   d_lma_out = lma_out - lag(lma_out),
                   d_cit = cit - lag(cit),
                   d_tax_diff_abs = tax_diff_abs - lag(tax_diff_abs),
                   d_pop = pop - lag(pop)) %>%
            ungroup() %>%
            drop_na()

        butt_rdd <- rdrobust(
            y = butt$d_asinh_emp,
            x = butt$dist_to_border_signed,
            covs = cbind(butt$d_lma_in, butt$d_lma_out, butt$d_cit, butt$d_pop, butt$d_tax_diff_abs),
            cluster = butt$GEO_ID,
            vce = "hc1",
            masspoints = "adjust",
            h = 50,
            b = 50
        )
        summary(butt_rdd)
        butt_summary <- as.data.frame(tibble(
            bandwidth = 50,
            N = nrow(butt),
            N_h_l = butt_rdd$N_h[1],
            N_h_r = butt_rdd$N_h[2],
            bw_l = butt_rdd$bws[1, 1],
            bw_r = butt_rdd$bws[1, 2],
            coef = butt_rdd$coef[3],
            se = butt_rdd$se[3],
            p = butt_rdd$pv[3],
            ci_l = butt_rdd$ci[3, 1],
            ci_r = butt_rdd$ci[3, 2]
        ))
        stargazer(
            butt_summary,
            type = "latex",
            summary = FALSE,
            rownames = FALSE,
            out = "output/butt_rdd.tex"
        )

        # --- expo MA spec (test) ---
        # Grembi et al. 2016 approach with exponential market access
        grembi_expo_rdd <- feols(asinh_emp ~ dist_to_border_signed + S + S:dist_to_border_signed + Tt + Tt:dist_to_border_signed + S:Tt + S:Tt:dist_to_border_signed
        + lma_in_expo + lma_out_expo + cit + pop | YEAR + nearest_border,
                data = grembi_50000,
                cluster = ~GEO_ID)
        summary(grembi_expo_rdd)
        etable(grembi_expo_rdd,
            tex = TRUE,
            style.tex = style.tex(
                main = "aer",
                notes.tpt.intro = ""
            ),
            drop = "YEAR|nearest_border",
            se.below = FALSE,
            fitstat = c("n", "r2"),
            digits = 3,
            file = "output/grembi_expo_rdd.tex"
        )

        # Butts 2023 approach with exponential market access
        butt_expo <- main %>%
            filter(type == "online") %>%
            filter(YEAR %in% c(2017, 2019)) %>%
            group_by(GEO_ID) %>%
            arrange(YEAR) %>%
            mutate(d_asinh_emp = asinh_emp - lag(asinh_emp),
                   d_lma_in_expo = lma_in_expo - lag(lma_in_expo),
                   d_lma_out_expo = lma_out_expo - lag(lma_out_expo),
                   d_cit = cit - lag(cit),
                   d_pop = pop - lag(pop)) %>%
            ungroup() %>%
            drop_na()

        butt_expo_rdd <- rdrobust(
            y = butt_expo$d_asinh_emp,
            x = butt_expo$dist_to_border_signed,
            covs = cbind(butt_expo$d_lma_in_expo, butt_expo$d_lma_out_expo, butt_expo$d_cit, butt_expo$d_pop),
            cluster = butt_expo$GEO_ID,
            vce = "hc1",
            masspoints = "adjust",
            h = 50,
            b = 50
        )
        summary(butt_expo_rdd)

# Continuous treatment reg ----
    # Y = β·T_{s(i),t} + f_p(R_i) + γ_p + δ_t + ε
    # T = tax_diff (jumps at border); f_p(R) = pair-specific local linear in dist
    # β: effect of 1pp tax difference on log employment
    cont_data <- main %>%
        filter(type == "online") %>% 
        filter(YEAR < 2019)

    cont_20 <- cont_data %>% filter(abs(dist_to_border_signed) <= 20)
    cont_50 <- cont_data %>% filter(abs(dist_to_border_signed) <= 50)

    # nearest_border[dist_to_border_signed] = pair-specific slope on distance (f_p(R))
    cont_rdd_20 <- feols(
        asinh_emp ~ tax_diff_abs + lma_in + lma_out + cit + pop + tax_diff_abs:dist_to_border_signed |
            YEAR + nearest_border[dist_to_border_signed],
        data = cont_20,
        cluster = ~GEO_ID
    )

    cont_rdd_50 <- feols(
        asinh_emp ~ tax_diff_abs + lma_in + lma_out + cit + pop + tax_diff_abs:dist_to_border_signed |
            YEAR + nearest_border[dist_to_border_signed],
        data = cont_50,
        cluster = ~GEO_ID
    )

    summary(cont_rdd_20)
    summary(cont_rdd_50)

    etable(cont_rdd_20, cont_rdd_50,
        headers = c("h = 20km", "h = 50km"),
        tex = TRUE,
        style.tex = style.tex(main = "aer", notes.tpt.intro = ""),
        drop = "YEAR|nearest_border",
        se.below = FALSE,
        fitstat = c("n", "r2"),
        digits = 3,
        file = "output/cont_treatment_rdd.tex"
    )

# Cross-sectional RDD by year ----
#   Three bandwidth specifications are compared:
#   1) Fixed 20 km  �?tight window, less bias but more variance
#   2) Fixed 50 km  �?wider window, more power but higher bias risk
#   3) Automatic     �?MSE-optimal bandwidth chosen by rdrobust (Calonico et al.)
# Each plot shows the discontinuity estimate with 90% CI across years.

# Helper: run cross-sectional RDD for one year
# bw_km = numeric �?fixed bandwidth; bw_km = NULL �?let rdrobust pick optimal
run_rdd_year <- function(yr, bw_km = NULL) {
    yr_data <- main %>%
        filter(YEAR == yr, type == "online") %>%
        select(GEO_ID, YEAR, STATEFP, asinh_emp, dist_to_border_signed,
               lma_in, lma_out, cit, nearest_border, pop) %>%
        drop_na()

    # For fixed bandwidth: keep only border pairs that have counties on both
    # sides within the bandwidth (ensures each pair contributes a contrast).
    # For auto bandwidth: use all data and let rdrobust decide the window.
    if (!is.null(bw_km)) {
        in_bw <- yr_data %>% filter(abs(dist_to_border_signed) <= bw_km)
        valid_pairs <- in_bw %>%
            mutate(side = ifelse(dist_to_border_signed > 0, "right", "left")) %>%
            group_by(nearest_border) %>%
            summarise(n_sides = n_distinct(side), .groups = "drop") %>%
            filter(n_sides == 2) %>%
            pull(nearest_border)
        yr_data <- yr_data %>% filter(nearest_border %in% valid_pairs)
    }
    if (nrow(yr_data) < 10) return(NULL)

    # Covariates: local-market access, CIT rate, population, + border-pair FE
    pair_dummies <- model.matrix(~ factor(nearest_border), data = yr_data)[, -1, drop = FALSE]
    covs_mat <- cbind(yr_data$lma_in, yr_data$lma_out,
                      yr_data$cit, yr_data$pop, pair_dummies)
    # Drop collinear columns to avoid rank-deficiency
    covs_mat <- covs_mat[, qr(covs_mat)$pivot[seq_len(qr(covs_mat)$rank)], drop = FALSE]

    # Build rdrobust arguments; omit h/b when bw_km is NULL (auto selection)
    rdd_args <- list(
        y          = yr_data$asinh_emp,
        x          = yr_data$dist_to_border_signed,
        covs       = covs_mat,
        cluster    = yr_data$nearest_border,
        masspoints = "adjust",
        level      = 90
    )
    if (!is.null(bw_km)) {
        rdd_args$h <- bw_km
        rdd_args$b <- bw_km
    }

    rdd_fit <- tryCatch(do.call(rdrobust, rdd_args), error = function(e) NULL)
    if (is.null(rdd_fit)) return(NULL)

    # Use robust bias-corrected inference (row 3 of rdrobust output)
    tibble(
        year  = yr,
        N     = nrow(yr_data),
        bw_h  = rdd_fit$bws[1, 1],   # actual bandwidth used (h)
        coef  = rdd_fit$coef[3],
        se    = rdd_fit$se[3],
        ci_l  = rdd_fit$ci[3, 1],
        ci_r  = rdd_fit$ci[3, 2],
        p     = rdd_fit$pv[3]
    )
}

# Helper: plot RDD coefficients by year 
plot_rdd_by_year <- function(df, title_label) {
    ggplot(df, aes(x = year, y = coef)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = 2018.5, linetype = "dashed", color = "red", linewidth = 0.7) +
        geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.25, linewidth = 0.7) +
        geom_point(size = 2.5) +
        annotate("text", x = 2018.5, y = Inf, label = "Wayfair", vjust = 1.5,
                 hjust = -0.1, color = "red", size = 3.5) +
        scale_x_continuous(breaks = years) +
        labs(
            title = paste0("Cross-sectional RDD Estimates by Year (", title_label, ")"),
            x     = "Year",
            y     = "Discontinuity Estimate (90% CI)"
        ) +
        theme_bw()
}

# Run and plot for each bandwidth specification

# 1) Fixed bandwidth = 20 km
rdd_20_df <- bind_rows(lapply(years, run_rdd_year, bw_km = 20))
p_rdd_20  <- plot_rdd_by_year(rdd_20_df, "bw = 20 km")
ggsave("output/rdd_by_year_20.pdf", p_rdd_20, width = 8, height = 5)
print(p_rdd_20)

# 2) Fixed bandwidth = 50 km
rdd_50_df <- bind_rows(lapply(years, run_rdd_year, bw_km = 50))
p_rdd_50  <- plot_rdd_by_year(rdd_50_df, "bw = 50 km")
ggsave("output/rdd_by_year_50.pdf", p_rdd_50, width = 8, height = 5)
print(p_rdd_50)

# 3) MSE-optimal bandwidth (automatically selected by rdrobust per year)
rdd_auto_df <- bind_rows(lapply(years, run_rdd_year, bw_km = NULL))
p_rdd_auto  <- plot_rdd_by_year(rdd_auto_df, "optimal bw")
ggsave("output/rdd_by_year_optimal.pdf", p_rdd_auto, width = 8, height = 5)
print(p_rdd_auto)
print(rdd_auto_df %>% select(year, bw_h))



