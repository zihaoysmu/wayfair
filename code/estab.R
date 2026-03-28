# library ----
source("code/setup.R")
options(tigris_use_cache = TRUE)


# import raw data ----
    gdp <- read.csv("data/temp/gdp_temp.csv", colClasses = c(GEO_ID = "character"))
    cbp <- read.csv("data/temp/cbp_temp.csv", colClasses = c(GEO_ID = "character")) 
    market <- read.csv("data/temp/market_temp.csv", colClasses = c(GEOID_i = "character")) 
    state <- read.csv("data/temp/state_con_tax.csv") 
    raw_pop <- read_xlsx("C:/document/SMU PhD/research/Data/Census Population Estimates Program/co-est2020int-pop.xlsx") 
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
        select(state, state_code, YEAR, sales_tax)

    cit <- cit_raw %>%
        rename(state = state_name, state_abbr = abbrev) %>%
        pivot_longer(
            cols = starts_with("corporate_tax_"),
            names_to = "cit_year",
            values_to = "cit"
        ) %>%
        mutate(YEAR = as.integer(str_remove(cit_year, "^corporate_tax_"))) %>%
        filter(!is.na(state_abbr)) %>%
        select(state, state_abbr, YEAR, cit)
# clean population data ----

    pop <- raw_pop
    pop$NAME <- str_remove(pop$NAME, "^\\.")
    pop <- pop %>% 
        separate(NAME, c("county", "state"), sep = ", ") %>% 
        left_join(fips_codes %>% select(state_code, state_name, county_code, county), by = c("state" = "state_name", "county" = "county")) %>%
        mutate(GEO_ID = paste0(state_code, county_code)) %>% 
        drop_na()

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

    cty = cty %>% 
        left_join(pop %>% select(GEO_ID, `2018pop`, state), by = c("GEOID" = "GEO_ID"))

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

    counties_sf$dist_to_border <- as.numeric(st_length(nearest_pts))
    counties_sf$nearest_border <- state_border$border_name[nearest_id]

    rm(coastline, countries, border_line, nb, neighbors, usa, states_pre)


# combine main ----
    # add back county's own gdp to gdp_in and both market access measures
    # only keep counties in the 48 contiguous states and DC, drop AK, HI, PR, and other territories
    main <- cbp %>% 
    left_join(cty %>% select(GEOID, state, is_boundary_county, STATEFP, coastal, border, `2018pop`), by = c("GEO_ID" = "GEOID")) %>% 
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
    left_join(state %>% select(state, year, expo), by = c("state" = "state", "YEAR" = "year")) %>% 
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
            high_tax_side = case_when(
                is.na(state_other) ~ NA_integer_,
                sales_tax > tax_other ~ 1L,
                TRUE ~ -1L
            ),
            dist_to_border_signed = dist_to_border * high_tax_side
        )


# regression ----
    ## market + foreign state counties ma x tax +foreign state counties ma x tax x post 
    ## + home state counties ma x tax +home state counties ma x tax x post
    ## kansus city 地跨两州，但是税率高的county反而有更多的est

    reg <- feols(
        lemp ~ 
        lma_in + lma_out + lma_tax_in + lma_tax_in * I(YEAR >= 2019) + lma_tax_out + lma_tax_out * I(YEAR >= 2019) |YEAR + GEO_ID,
        data = main,
        cluster = ~STATEFP
    )

    panel_groups <- list(
        `2015-2018` = 2015:2018,
        `2019-2022` = 2019:2022
    )
    

    rdd_results <- lapply(names(panel_groups), function(group_name) {
        group_years <- panel_groups[[group_name]]

        year_dummies <- model.matrix(~ factor(YEAR) - 1, data = group_data)
        state_dummies <- model.matrix(~ factor(STATEFP) - 1, data = group_data)

        group_data <- main %>%
            filter(YEAR %in% group_years) %>%
            select(
                GEO_ID, YEAR, STATEFP, lemp, dist_to_border_signed,
                lma_in, lma_out, coastal, border, cit, EMP
            ) %>%
            drop_na()


        fe_reg <- fepois(
            EMP ~ lma_in + lma_out + cit | GEO_ID + YEAR,
            data = group_data,
            cluster = ~STATEFP
        )

        # Remove singletons in regression to align data and calculated residuals
        group_data <- group_data %>%
            slice(obs(fe_reg)) %>%
            mutate(emp_resid = residuals(fe_reg))

        reg_rdd <- rdrobust(
            y = group_data$emp_resid,
            x = group_data$dist_to_border_signed,
            c = 0,
            kernel = "triangular",
            p = 1,
            q = 2,
            h = 50000,
            bwselect = "mserd",
            cluster = group_data$STATEFP
        )

        tibble(
            panel_group = group_name,
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

    rdd_summary <- bind_rows(rdd_results)
    stargazer(
        as.data.frame(rdd_summary),
        type = "latex",
        summary = FALSE,
        rownames = FALSE,
        out = "output/rdd_summary.tex"
    )

# border density graph ----
    ## hard to decide which county belongs to which border

    # Unique border approach
        # assign counties to every border whose 600km buffer intersects the county centroid
        border_buffer <- st_buffer(state_border, dist = set_units(600, km))
        border_matches <- st_intersects(county_cent, border_buffer)

        # keep every county-border pair within the 600km buffer
        county_border_assignment <- tibble(
            county_idx = rep(seq_len(nrow(counties_sf)), lengths(border_matches)),
            border_idx = unlist(border_matches)
        ) %>%
            mutate(
                GEO_ID = counties_sf$GEO_ID[county_idx],
                border_name = state_border$border_name[border_idx],
                dist_to_border = as.numeric(
                    st_distance(
                        county_cent[county_idx, ],
                        state_border[border_idx, ],
                        by_element = TRUE
                    )
                )
            ) %>%
            distinct(GEO_ID, border_name, .keep_all = TRUE) %>%
            select(GEO_ID, border_name, dist_to_border) %>%
            separate(border_name, into = c("state1", "state2"), sep = "-", remove = FALSE)

        # generate a "border county-year-NAICS" df
        border_county_year_naics <- main %>%
            select(
                GEO_ID, YEAR, NAICS, type, EMP, lemp,
                lma_in, lma_out, lma_in_expo, lma_out_expo,
                coastal, border, state, state_abbr, sales_tax, cit
            ) %>%
            distinct() %>%
            left_join(county_border_assignment, by = "GEO_ID") %>%
            filter(!is.na(border_name)) %>%
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
                    rename(tax_home = sales_tax),
                by = c("state_home" = "state_abbr", "YEAR" = "YEAR")
            ) %>%
            left_join(
                state_tax_lookup %>%
                    select(state_abbr, YEAR, sales_tax) %>%
                    distinct() %>%
                    rename(tax_other = sales_tax),
                by = c("state_other" = "state_abbr", "YEAR" = "YEAR")
            ) %>%
            mutate(
                high_tax_side = case_when(
                    is.na(state_other) ~ NA_integer_,
                    tax_home > tax_other ~ 1L,
                    TRUE ~ -1L
                ),
                dist_to_border_adj = dist_to_border * high_tax_side
            )

    ma_specs <- list(
        linear = list(in_var = "lma_in", out_var = "lma_out"),
        expo = list(in_var = "lma_in_expo", out_var = "lma_out_expo")
    )

    regression_specs <- c("inh", "ppml", "raw_inh")
    summary_years <- c(2017, 2019)
    summary_plot_registry <- list()

    build_plot <- function(plot_data, x_var, biz_type, panel_title = NULL) {
        plot_df <- plot_data %>%
            mutate(
                border_side = if_else(.data[[x_var]] < 0, "left", "right"),
                distance_km = .data[[x_var]] / 1000
            )

        y_label <- if_else(
            biz_type == "online",
            "Residualized employment in online industry",
            "Residualized employment in local industry"
        )

        plot_obj <- ggplot(plot_df, aes(x = distance_km, y = plot_resid)) +
            geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
            geom_smooth(
                aes(group = border_side, fill = border_side),
                method = "loess",
                se = TRUE,
                show.legend = FALSE
            ) +
            labs(
                x = "Distance to state border (km; positive = high-tax side)",
                y = y_label
            )

        if (!is.null(panel_title)) {
            plot_obj <- plot_obj + ggtitle(panel_title)
        }

        plot_obj
    }

    generate_density_plots <- function(plot_df, approach_name, x_var) {
        for (ma_type in names(ma_specs)) {
            in_var <- ma_specs[[ma_type]]$in_var
            out_var <- ma_specs[[ma_type]]$out_var

            for (reg_type in regression_specs) {
                for (year in years) {
                    for (biz_type in c("online", "local")) {
                        model_data <- plot_df %>%
                            filter(
                                YEAR == year,
                                type == biz_type,
                                !is.na(high_tax_side)
                            ) %>%
                            select(
                                GEO_ID, YEAR, type, EMP, lemp, coastal, border, cit,
                                all_of(c(in_var, out_var, x_var))
                            ) %>%
                            drop_na()

                        if (nrow(model_data) == 0) {
                            next
                        }

                        if (reg_type == "raw_inh") {
                            plot_data <- model_data %>%
                                mutate(plot_resid = lemp)
                        } else {
                            regression_formula <- as.formula(
                                paste(
                                    ifelse(reg_type == "inh", "lemp", "EMP"),
                                    "~",
                                    paste(c(in_var, out_var, "coastal", "border", "cit"), collapse = " + ")
                                )
                            )

                            model <- if (reg_type == "inh") {
                                feols(regression_formula, data = model_data)
                            } else {
                                glm(
                                    regression_formula,
                                    family = poisson(link = "log"),
                                    data = model_data
                                )
                            }

                            plot_data <- model_data %>%
                                mutate(plot_resid = residuals(model))
                        }

                        q_resid <- quantile(plot_data$plot_resid, probs = c(.01, .99), na.rm = TRUE)
                        plot_data <- plot_data %>%
                            mutate(plot_resid = pmin(pmax(plot_resid, q_resid[1]), q_resid[2]))

                        output_dir <- file.path("output", approach_name, ma_type, reg_type, biz_type)
                        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

                        panel_title <- paste(
                            ifelse(ma_type == "linear", "Linear access", "Exponential access"),
                            ifelse(approach_name == "unique_border", "Unique border", "Unique county"),
                            sep = " + "
                        )

                        plot_obj <- build_plot(
                            plot_data = plot_data,
                            x_var = x_var,
                            biz_type = biz_type,
                            panel_title = panel_title
                        )

                        ggsave(file.path(output_dir, paste0(year, ".png")), plot = plot_obj)

                        if (reg_type == "ppml" && biz_type == "online" && year %in% summary_years) {
                            summary_key <- paste(year, ma_type, approach_name, sep = "__")
                            summary_plot_registry[[summary_key]] <<- plot_obj
                        }
                    }
                }
            }
        }
    }

    generate_density_plots(
        plot_df = border_county_year_naics,
        approach_name = "unique_border",
        x_var = "dist_to_border_adj"
    )

    # Unique county approach
    generate_density_plots(
        plot_df = main,
        approach_name = "unique_county",
        x_var = "dist_to_border_signed"
    )

    summary_output_dir <- file.path("output", "summary", "ppml", "online")
    dir.create(summary_output_dir, recursive = TRUE, showWarnings = FALSE)

    for (year in summary_years) {
        summary_plot <- (
            summary_plot_registry[[paste(year, "linear", "unique_border", sep = "__")]] +
            summary_plot_registry[[paste(year, "linear", "unique_county", sep = "__")]]
        ) / (
            summary_plot_registry[[paste(year, "expo", "unique_border", sep = "__")]] +
            summary_plot_registry[[paste(year, "expo", "unique_county", sep = "__")]]
        ) +
            plot_annotation(title = paste("PPML residualized employment in online industry,", year))

        ggsave(
            filename = file.path(summary_output_dir, paste0(year, ".png")),
            plot = summary_plot,
            width = 14,
            height = 10
        )
    }

