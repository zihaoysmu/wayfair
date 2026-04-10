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
    # 3)判断是否存在“跨州邻居”
    boundary <- vapply(seq_len(nrow(cty)), function(i) {
        nbr <- nb[[i]]
        if (length(nbr) == 0) return(FALSE)
        any(cty$STATEFP[nbr] != cty$STATEFP[i])
    }, logical(1))
    cty$is_boundary_county = boundary
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

# Establishment graph preparation ----
    # Generate counties and states
    counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) %>% 
        rename(GEO_ID = GEOID)
    states_sf   <- states(cb = TRUE, resolution = "20m", year = 2020)

    exclude <- c("02", "15", "60", "66", "69", "72", "78")

    counties_sf <- counties_sf %>%
        filter(!STATEFP %in% exclude)

    states_sf <- states_sf %>%
        filter(!STATEFP %in% exclude)

    # Generate distance to border and which border the county belongs to
    states_pre <- states_sf %>% 
        select(STUSPS, STATEFP, NAME) %>% 
        st_transform(5070)
    state_border <- st_intersection(states_pre, states_pre) %>% 
        filter(STUSPS != STUSPS.1) %>% 
        mutate(
        border_name = paste0(
            pmin(STUSPS, STUSPS.1),
            "-",
            pmax(STUSPS, STUSPS.1)
        )
        ) %>% 
        group_by(border_name) %>% 
        summarize(geometry = st_union(geometry), .groups = "drop")

    counties_sf  <- st_transform(counties_sf, 5070)
    state_border <- st_transform(state_border, 5070)

    county_cent <- st_centroid(counties_sf)
    nearest_id <- st_nearest_feature(county_cent, state_border)

    nearest_pts <- st_nearest_points(
        county_cent,
        state_border[nearest_id, ],
        pairwise = TRUE
    )

    counties_sf$dist_to_border <- as.numeric(st_length(nearest_pts)) / 1000
    counties_sf$nearest_border <- state_border$border_name[nearest_id]

    rm(coastline, countries, border_line, nb, neighbors, usa, states_pre)


# combine main ----
    # add back county's own gdp to gdp_in and both market access measures
    # only keep counties in the 48 contiguous states and DC, drop AK, HI, PR, and other territories
    main <- cbp %>% 
    left_join(cty %>% select(GEOID, "state", is_boundary_county, STATEFP, coastal, border), by = c("GEO_ID" = "GEOID")) %>%
    left_join(pop_long, by = c("GEO_ID", "YEAR")) %>%
    filter(!STATEFP %in% exclude) %>% 
    left_join(gdp, by = c("YEAR" = "YEAR", "GEO_ID" = "GEO_ID")) %>% 
    left_join(market, by = c("YEAR" = "year", "GEO_ID" = "GEOID_i")) %>% 
    mutate(lest = asinh(ESTAB),
            gdp = as.numeric(gdp),
            ma_in = ma_in + gdp,
            ma_in_new = ma_in_new + gdp,
            gdp_in = gdp + gdp_in,
            lma_in = log(ma_in),
            lma_out = log(ma_out),
            lma_in_expo = log(ma_in_new),
            lma_out_expo = log(ma_out_new),
            lemp = asinh(EMP),
            lgdp_tax_in = asinh(gdp_tax_in),
            lma_tax_in = asinh(ma_tax_in),
            lma_tax_out = asinh(ma_tax_out),
            lgdp_tax_out = asinh(gdp_tax_out)) %>% 
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
        select(GEO_ID, dist_to_border, nearest_border) %>%
        separate(nearest_border, into = c("state1", "state2"), sep = "-", remove = FALSE)

    state_tax_lookup <- main %>%
        select(YEAR, state, state_abbr, sales_tax, cit) %>%
        distinct()

    # Distance to border is in meters
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
            dist_to_border_signed = dist_to_border * high_tax_side
        )


# regression ----
    # FE OLS
    reg <- feols(
        lemp ~ 
        lma_in + lma_out + lma_tax_in + lma_tax_in * I(YEAR >= 2019) + lma_tax_out + lma_tax_out * I(YEAR >= 2019) + cit + pop |YEAR + GEO_ID,
        data = main,
        cluster = ~STATEFP
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
                select(GEO_ID, YEAR, STATEFP, lemp, dist_to_border_signed,
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
                    y = group_data$lemp,
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
            lemp ~ dist_to_border_signed + S + S:dist_to_border_signed +
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

        # Dynamic (event study) Grembi approach — mirrors grembi_rdd spec year-by-year
            # Each Tt × ... term is replaced with i(YEAR_f, ..., ref = "2018")
            # Static terms (no Tt): dist_to_border_signed, S, S:dist_to_border_signed, S:tax_diff_abs
            grembi_event <- grembi_20000 %>%
                mutate(
                    YEAR_f      = factor(YEAR),
                    S_slope     = S * dist_to_border_signed,          # Tt:S:dist  → i(YEAR_f, S_slope)
                    dist_tax    = dist_to_border_signed * tax_diff_abs, # Tt:dist:tax_diff → i(YEAR_f, dist_tax)
                    S_slope_tax = S * dist_to_border_signed * tax_diff_abs  # Tt:S:dist:tax_diff → i(YEAR_f, S_slope_tax)
                )

            grembi_event_rdd <- feols(
                lemp ~ dist_to_border_signed + S + S:dist_to_border_signed +
                    S:tax_diff_abs +                                        # static: S × tax_diff (no Tt)
                    i(YEAR_f, dist_to_border_signed, ref = "2018") +       # Tt:dist
                    i(YEAR_f, S,           ref = "2018") +                 # S:Tt ← key dynamic effect
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


        # Butts 2023 approach
        # first difference
        butt <- main %>%
            filter(type == "online") %>%
            filter(YEAR %in% c(2017, 2019)) %>%
            group_by(GEO_ID) %>%
            arrange(YEAR) %>%
            mutate(d_lemp = lemp - lag(lemp),
                   d_dist = dist_to_border_signed - lag(dist_to_border_signed),
                   d_lma_in = lma_in - lag(lma_in),
                   d_lma_out = lma_out - lag(lma_out),
                   d_cit = cit - lag(cit),
                   d_tax_diff_abs = tax_diff_abs - lag(tax_diff_abs),
                   d_pop = pop - lag(pop)) %>%
            ungroup() %>%
            drop_na()

        butt_rdd <- rdrobust(
            y = butt$d_lemp,
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
        grembi_expo_rdd <- feols(lemp ~ dist_to_border_signed + S + S:dist_to_border_signed + Tt + Tt:dist_to_border_signed + S:Tt + S:Tt:dist_to_border_signed
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
            mutate(d_lemp = lemp - lag(lemp),
                   d_lma_in_expo = lma_in_expo - lag(lma_in_expo),
                   d_lma_out_expo = lma_out_expo - lag(lma_out_expo),
                   d_cit = cit - lag(cit),
                   d_pop = pop - lag(pop)) %>%
            ungroup() %>%
            drop_na()

        butt_expo_rdd <- rdrobust(
            y = butt_expo$d_lemp,
            x = butt_expo$dist_to_border_signed,
            covs = cbind(butt_expo$d_lma_in_expo, butt_expo$d_lma_out_expo, butt_expo$d_cit, butt_expo$d_pop),
            cluster = butt_expo$GEO_ID,
            vce = "hc1",
            masspoints = "adjust",
            h = 50,
            b = 50
        )
        summary(butt_expo_rdd)


# Cross-sectional RDD plot - unique county + linear MA
test = main %>%
select(dist_to_border_signed) %>%
arrange(dist_to_border_signed)

# Cross-sectional RDD by year ----
    bw_cs <- 20   # bandwidth in km

    rdd_by_year <- lapply(years, function(yr) {
        yr_data <- main %>%
            filter(YEAR == yr, type == "online") %>%
            select(GEO_ID, YEAR, STATEFP, lemp, dist_to_border_signed,
                   lma_in, lma_out, cit, nearest_border, pop) %>%
            drop_na()

        # keep border pairs with counties on both sides within bandwidth
        in_bw <- yr_data %>% filter(abs(dist_to_border_signed) <= bw_cs)
        valid_pairs <- in_bw %>%
            mutate(side = ifelse(dist_to_border_signed > 0, "right", "left")) %>%
            group_by(nearest_border) %>%
            summarise(n_sides = n_distinct(side), .groups = "drop") %>%
            filter(n_sides == 2) %>%
            pull(nearest_border)

        yr_data <- yr_data %>% filter(nearest_border %in% valid_pairs)
        if (nrow(yr_data) < 10) return(NULL)

        pair_dummies <- model.matrix(~ factor(nearest_border), data = yr_data)[, -1, drop = FALSE]
        covs_mat <- cbind(yr_data$lma_in, yr_data$lma_out,
                          yr_data$cit, yr_data$pop, pair_dummies)
        covs_mat <- covs_mat[, qr(covs_mat)$pivot[seq_len(qr(covs_mat)$rank)], drop = FALSE]

        rdd_fit <- tryCatch(
            rdrobust(
                y        = yr_data$lemp,
                x        = yr_data$dist_to_border_signed,
                covs     = covs_mat,
                cluster  = yr_data$GEO_ID,
                vce      = "hc1",
                masspoints = "adjust"
            ),
            error = function(e) NULL
        )
        if (is.null(rdd_fit)) return(NULL)

        tibble(
            year  = yr,
            N     = nrow(yr_data),
            coef  = rdd_fit$coef[3],
            se    = rdd_fit$se[3],
            ci_l  = rdd_fit$ci[3, 1],
            ci_r  = rdd_fit$ci[3, 2],
            p     = rdd_fit$pv[3]
        )
    })

    rdd_by_year_df <- bind_rows(rdd_by_year)

    # Plot: coefficient + 95% CI by year
    p_rdd_year <- ggplot(rdd_by_year_df, aes(x = year, y = coef)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = 2018.5, linetype = "dashed", color = "red", linewidth = 0.7) +
        geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.25, linewidth = 0.7) +
        geom_point(size = 2.5) +
        annotate("text", x = 2018.5, y = Inf, label = "Wayfair", vjust = 1.5,
                 hjust = -0.1, color = "red", size = 3.5) +
        scale_x_continuous(breaks = years) +
        labs(
            title = paste0("Cross-sectional RDD Estimates by Year (bw = ", bw_cs, " km)"),
            x     = "Year",
            y     = "Discontinuity Estimate (95% CI)"
        ) +
        theme_bw()

    ggsave("output/rdd_by_year.pdf", p_rdd_year, width = 8, height = 5)
    print(p_rdd_year)
