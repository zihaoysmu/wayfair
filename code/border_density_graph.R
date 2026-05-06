source("code/setup.R")


# border density graph ----
# Dependencies (*must have run estab.R first*):
#   state_border, county_cent, counties_sf, main, state_tax_lookup, years

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
        ) / 1000
    ) %>%
    distinct(GEO_ID, border_name, .keep_all = TRUE) %>%
    select(GEO_ID, border_name, dist_to_border) %>%
    separate(border_name, into = c("state1", "state2"), sep = "-", remove = FALSE)

# generate a "border county-year-NAICS" df
border_county_year_naics <- main %>%
    select(
        GEO_ID, YEAR, NAICS, type, EMP,
        lma_in, lma_out, lma_in_expo, lma_out_expo,
        coastal, border, state, state_abbr, sales_tax, cit, pop
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

regression_specs <- c("inh", "raw_inh")
summary_years <- c(2017, 2019)
summary_plot_registry <- list()

build_plot <- function(plot_data, x_var, biz_type, panel_title = NULL) {
    plot_df <- plot_data %>%
        mutate(
            border_side = if_else(.data[[x_var]] < 0, "left", "right"),
            distance_km = .data[[x_var]]
        )

    y_label <- if_else(
        biz_type == "online",
        "Change in employment in online industry",
        "Change in employment in local industry"
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

generate_density_plots <- function(plot_df, approach_name, x_var, group_vars) {
    change_df <- plot_df %>%
        arrange(across(all_of(c(group_vars, "YEAR")))) %>%
        group_by(across(all_of(group_vars))) %>%
        mutate(emp_change = EMP - lag(EMP)) %>%
        ungroup()

    for (ma_type in names(ma_specs)) {
        in_var <- ma_specs[[ma_type]]$in_var
        out_var <- ma_specs[[ma_type]]$out_var

        for (reg_type in regression_specs) {
            for (year in years[-1]) {
                for (biz_type in c("online", "local")) {
                    model_data <- change_df %>%
                        filter(
                            YEAR == year,
                            type == biz_type,
                            !is.na(high_tax_side),
                            !is.na(emp_change)
                        ) %>%
                        select(
                            GEO_ID, YEAR, type, emp_change, coastal, border, cit, pop,
                            all_of(c(in_var, out_var, x_var))
                        ) %>%
                        drop_na()

                    if (nrow(model_data) == 0) {
                        next
                    }

                    if (reg_type == "raw_inh") {
                        plot_data <- model_data %>%
                            mutate(plot_resid = emp_change)
                    } else {
                        regression_formula <- as.formula(
                            paste(
                                "emp_change ~",
                                paste(c(in_var, out_var, "coastal", "border", "cit", "pop"), collapse = " + ")
                            )
                        )

                        model <- feols(regression_formula, data = model_data)

                        plot_data <- model_data %>%
                            mutate(plot_resid = residuals(model))
                    }

                    q_resid <- quantile(plot_data$plot_resid, probs = c(.01, .99), na.rm = TRUE)
                    plot_data <- plot_data %>%
                        mutate(plot_resid = pmin(pmax(plot_resid, q_resid[1]), q_resid[2]))

                    output_dir <- file.path("output", "figures", approach_name, ma_type, reg_type, biz_type)
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

                    if (biz_type == "online" && year %in% summary_years) {
                        summary_key <- paste(year, ma_type, approach_name, reg_type, sep = "__")
                        summary_plot_registry[[summary_key]] <<- plot_obj
                    }
                }
            }
        }
    }
}

generate_density_plots(
    plot_df = border_county_year_naics %>% filter(dist_to_border <= 100),
    approach_name = "unique_border",
    x_var = "dist_to_border_adj",
    group_vars = c("GEO_ID", "border_name", "NAICS", "type")
)

generate_density_plots(
    plot_df = main %>% filter(dist_to_border <= 100),
    approach_name = "unique_county",
    x_var = "dist_to_border_signed",
    group_vars = c("GEO_ID", "NAICS", "type")
)

for (spec in regression_specs) {
    summary_output_dir <- file.path("output", "figures", "summary", spec, "online")
    dir.create(summary_output_dir, recursive = TRUE, showWarnings = FALSE)

    spec_label <- if (spec == "inh") "Residualized change" else "Raw change"

    for (year in summary_years) {
        summary_plot <- (
            summary_plot_registry[[paste(year, "linear", "unique_border", spec, sep = "__")]] +
            summary_plot_registry[[paste(year, "linear", "unique_county", spec, sep = "__")]]
        ) / (
            summary_plot_registry[[paste(year, "expo", "unique_border", spec, sep = "__")]] +
            summary_plot_registry[[paste(year, "expo", "unique_county", spec, sep = "__")]]
        ) +
            plot_annotation(title = paste(spec_label, "employment in online industry,", year))

        ggsave(
            filename = file.path(summary_output_dir, paste0(year, ".png")),
            plot = summary_plot,
            width = 14,
            height = 10
        )
    }
}
