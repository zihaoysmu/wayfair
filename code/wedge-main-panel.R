source("code/setup.R")

# Panel tax-gap regression:
# - Keep the same complete two-sided wedge-pair sample used in wedge-main.R.
# - Estimate the effect of a signed 1 percentage point own-minus-neighbor
# - sales tax gap in a single stacked panel.
# - Write only the panel table/figure, leaving year-by-year outputs untouched.

wedge_data_suffix <- Sys.getenv("WEDGE_DATA_SUFFIX", "")
wedge_data_suffix_part <- if (nzchar(wedge_data_suffix)) paste0("_", wedge_data_suffix) else ""

out_path <- function(dir, stem, ext) file.path(dir, paste0(stem, wedge_data_suffix_part, ".", ext))
subtitle_suffix <- function() if (nzchar(wedge_data_suffix)) paste0(" Data: ", wedge_data_suffix, ".") else ""

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

wedge_data_path <- file.path("data/temp", paste0("wedge_year_balanced_2015_2022", wedge_data_suffix_part, ".csv"))
message("Reading wedge data: ", wedge_data_path)
wedge <- read.csv(wedge_data_path)

for (estab_col in c("warehouse_estab")) {
    if (!estab_col %in% names(wedge)) wedge[[estab_col]] <- 0
}

# Require exactly two wedges/two sides for every wedge pair in every sample year.
complete_pairs <- wedge %>%
    filter(!is.na(wedge_pair_id)) %>%
    group_by(wedge_pair_id, year) %>%
    summarise(n_wedges = n_distinct(wedge_id), n_sides = n_distinct(side), .groups = "drop") %>%
    group_by(wedge_pair_id) %>%
    summarise(
        n_years = n_distinct(year),
        min_wedges_per_year = min(n_wedges),
        max_wedges_per_year = max(n_wedges),
        min_sides_per_year = min(n_sides),
        max_sides_per_year = max(n_sides),
        .groups = "drop"
    ) %>%
    filter(
        n_years == n_distinct(wedge$year),
        min_wedges_per_year == 2,
        max_wedges_per_year == 2,
        min_sides_per_year == 2,
        max_sides_per_year == 2
    )

if (nrow(complete_pairs) == 0) stop("No complete two-sided wedge pairs available in the wedge data.")

wedge <- semi_join(wedge, select(complete_pairs, wedge_pair_id), by = "wedge_pair_id")
message("Keeping complete wedge pairs: ", n_distinct(wedge$wedge_pair_id), " pairs; ", nrow(wedge), " wedge-year rows.")

# tax_other is the neighbor state's sales tax in the same year.
# tax_diff_pp is signed: +1 means own-state tax is 1 percentage point higher.
tax_lookup <- wedge %>%
    select(state_abbr, year, sales_tax) %>%
    distinct()

panel_data <- wedge %>%
    left_join(rename(tax_lookup, neighbor_state = state_abbr, tax_other = sales_tax), by = c("neighbor_state", "year")) %>%
    mutate(
        across(c(online_estab, population, cit, sales_tax, tax_other, wedge_market_potential, warehouse_estab), as.numeric),
        warehouse_estab = replace_na(warehouse_estab, 0),
        tax_diff_pp = 100 * (sales_tax - tax_other),
        log_wedge_market_potential = log(wedge_market_potential),
        log1p_warehouse_estab = log1p(warehouse_estab),
        pair_year_id = interaction(wedge_pair_id, year, drop = TRUE),
        tax_diff_pp_pre2019 = if_else(year < 2019, tax_diff_pp, 0),
        tax_diff_pp_post2019 = if_else(year >= 2019, tax_diff_pp, 0),
        tax_diff_pp_2015 = if_else(year == 2015, tax_diff_pp, 0),
        tax_diff_pp_2016 = if_else(year == 2016, tax_diff_pp, 0),
        tax_diff_pp_2017 = if_else(year == 2017, tax_diff_pp, 0),
        tax_diff_pp_2018 = if_else(year == 2018, tax_diff_pp, 0),
        tax_diff_pp_2019 = if_else(year == 2019, tax_diff_pp, 0),
        tax_diff_pp_2020 = if_else(year == 2020, tax_diff_pp, 0),
        tax_diff_pp_2021 = if_else(year == 2021, tax_diff_pp, 0),
        tax_diff_pp_2022 = if_else(year == 2022, tax_diff_pp, 0)
    ) %>%
    arrange(wedge_id, year) %>%
    group_by(wedge_id) %>%
    mutate(d_online_estab = online_estab - lag(online_estab)) %>%
    ungroup()

controls <- c("population", "cit", "log_wedge_market_potential", "log1p_warehouse_estab")
tax_gap_term <- "tax_diff_pp"

panel_specs <- tibble(
    model = c("PPML level", "OLS change"),
    outcome = c("online_estab", "d_online_estab"),
    outcome_label = c(
        "Online establishments",
        "Change in online establishments"
    ),
    model_type = c("PPML", "OLS"),
    estimand = c("proportional_effect", "coefficient"),
    y_label = c(
        "Proportional effect of +1 pp tax gap",
        "Establishment-count effect of +1 pp tax gap"
    )
)

model_sample <- function(outcome_var) {
    panel_data %>%
        filter(
            !is.na(.data[[outcome_var]]),
            !is.na(tax_diff_pp),
            !is.na(pair_year_id),
            if_all(all_of(controls), ~ !is.na(.x))
        )
}

fit_panel_model <- function(data, outcome_var, model_type) {
    model_formula <- as.formula(paste0(
        outcome_var,
        " ~ ",
        paste(c(tax_gap_term, controls), collapse = " + "),
        " | wedge_id + pair_year_id"
    ))

    if (model_type == "PPML") {
        fepois(model_formula, data = data, cluster = ~statefp + state_pair_id)
    } else {
        feols(model_formula, data = data, cluster = ~statefp + state_pair_id)
    }
}

summarise_panel_fit <- function(fit, data, spec) {
    if (!tax_gap_term %in% names(coef(fit))) {
        stop("tax_diff_pp was not estimated for ", spec$model, ".")
    }

    crit <- qnorm(0.975)
    b <- as.numeric(coef(fit)[tax_gap_term])

    if (spec$model_type == "PPML") {
        se_b <- as.numeric(sqrt(vcov(fit)[tax_gap_term, tax_gap_term]))
        estimate <- exp(b) - 1
        estimate_se <- exp(b) * se_b
        ci_l <- exp(b - crit * se_b) - 1
        ci_r <- exp(b + crit * se_b) - 1
    } else {
        estimate <- b
        estimate_se <- as.numeric(se(fit)[tax_gap_term])
        ci_l <- estimate - crit * estimate_se
        ci_r <- estimate + crit * estimate_se
    }

    estimated_data <- data[obs(fit), , drop = FALSE]

    tibble(
        model = spec$model,
        outcome = spec$outcome,
        outcome_label = spec$outcome_label,
        model_type = spec$model_type,
        estimand = spec$estimand,
        treatment = tax_gap_term,
        treatment_label = "Signed sales tax gap (own minus neighbor, pp)",
        estimate = estimate,
        se = estimate_se,
        ci_l = ci_l,
        ci_r = ci_r,
        n_obs = fit$nobs,
        n_wedges = n_distinct(estimated_data$wedge_id),
        n_wedge_pairs = n_distinct(estimated_data$wedge_pair_id)
    )
}

panel_results <- bind_rows(lapply(seq_len(nrow(panel_specs)), function(i) {
    spec <- panel_specs[i, ]
    data <- model_sample(spec$outcome)
    fit <- fit_panel_model(data, spec$outcome, spec$model_type)
    summarise_panel_fit(fit, data, spec)
})) %>%
    left_join(select(panel_specs, model, y_label), by = "model") %>%
    mutate(
        model = factor(model, levels = panel_specs$model),
        estimate_type = "Tax gap +1 pp"
    )

panel_table_path <- out_path("output/tables", "wedge_panel_tax_gap", "csv")
panel_table_write <- try(write_csv(panel_results, panel_table_path), silent = TRUE)
if (inherits(panel_table_write, "try-error")) {
    warning("Could not write existing panel table: ", panel_table_path)
}

panel_plot <- ggplot(panel_results, aes(x = estimate_type, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.12, linewidth = 0.55) +
    geom_point(size = 2.4, color = "#2b6cb0") +
    facet_wrap(~model, scales = "free_y") +
    labs(
        title = "Panel Effect of Signed Sales Tax Gap",
        subtitle = paste0(
            "Wedge FE and wedge-pair-year FE. Positive tax gap means own-state sales tax exceeds neighbor-state sales tax.",
            subtitle_suffix()
        ),
        x = NULL,
        y = "Estimate"
    ) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey90", color = "grey60"))

panel_figure_path <- out_path("output/figures", "wedge_panel_tax_gap", "png")
panel_figure_write <- try(ggsave(panel_figure_path, panel_plot, width = 8, height = 5, dpi = 150), silent = TRUE)
if (inherits(panel_figure_write, "try-error")) {
    warning("Could not write existing panel figure: ", panel_figure_path)
}


# Pre/post-2019 panel split ----
#
# This block estimates separate panel coefficients before and after 2019:
#
#   tax_diff_pp_pre2019  = tax_diff_pp * 1{year < 2019}
#   tax_diff_pp_post2019 = tax_diff_pp * 1{year >= 2019}
#
# PPML formula:
#
#   online_estab ~ tax_diff_pp_pre2019 + tax_diff_pp_post2019
#                + population + cit + log_wedge_market_potential
#                | wedge_id + pair_year_id
#
# OLS formula:
#
#   d_online_estab ~ tax_diff_pp_pre2019 + tax_diff_pp_post2019
#                  + population + cit + log_wedge_market_potential
#                  | wedge_id + pair_year_id
#
# Each coefficient is the effect of a +1 percentage point own-minus-neighbor
# sales tax gap inside that period. For the change model, 2015 is unavailable
# because d_online_estab uses the within-wedge lag.

split_terms <- c("tax_diff_pp_pre2019", "tax_diff_pp_post2019")
split_periods <- tibble(
    treatment = split_terms,
    period = factor(c("Pre-2019", "Post-2019"), levels = c("Pre-2019", "Post-2019")),
    years = c("2015-2018", "2019-2022")
)

split_sample <- function(outcome_var) {
    panel_data %>%
        filter(
            !is.na(.data[[outcome_var]]),
            !is.na(tax_diff_pp),
            !is.na(pair_year_id),
            if_all(all_of(controls), ~ !is.na(.x))
        )
}

split_formula <- function(outcome_var) {
    as.formula(paste0(
        outcome_var,
        " ~ ",
        paste(c(split_terms, controls), collapse = " + "),
        " | wedge_id + pair_year_id"
    ))
}

split_rows <- list()
split_row_i <- 0

for (i in seq_len(nrow(panel_specs))) {
    spec <- panel_specs[i, ]
    data <- split_sample(spec$outcome)
    model_formula <- split_formula(spec$outcome)

    fit <- if (spec$model_type == "PPML") {
        fepois(model_formula, data = data, cluster = ~statefp + state_pair_id)
    } else {
        feols(model_formula, data = data, cluster = ~statefp + state_pair_id)
    }

    estimated_data <- data[obs(fit), , drop = FALSE]

    for (j in seq_along(split_terms)) {
        term <- split_terms[j]

        if (!term %in% names(coef(fit))) next

        b <- as.numeric(coef(fit)[term])
        crit <- qnorm(0.975)

        if (spec$model_type == "PPML") {
            se_b <- as.numeric(sqrt(vcov(fit)[term, term]))
            estimate <- exp(b) - 1
            estimate_se <- exp(b) * se_b
            ci_l <- exp(b - crit * se_b) - 1
            ci_r <- exp(b + crit * se_b) - 1
        } else {
            estimate <- b
            estimate_se <- as.numeric(se(fit)[term])
            ci_l <- estimate - crit * estimate_se
            ci_r <- estimate + crit * estimate_se
        }

        split_row_i <- split_row_i + 1
        split_rows[[split_row_i]] <- tibble(
            model = spec$model,
            outcome = spec$outcome,
            outcome_label = spec$outcome_label,
            model_type = spec$model_type,
            estimand = spec$estimand,
            treatment = term,
            treatment_label = "Signed sales tax gap (own minus neighbor, pp)",
            period = split_periods$period[j],
            years = if (spec$outcome == "d_online_estab" && term == "tax_diff_pp_pre2019") "2016-2018" else split_periods$years[j],
            estimate = estimate,
            se = estimate_se,
            ci_l = ci_l,
            ci_r = ci_r,
            n_obs = fit$nobs,
            n_wedges = n_distinct(estimated_data$wedge_id),
            n_wedge_pairs = n_distinct(estimated_data$wedge_pair_id)
        )
    }
}

split_results <- bind_rows(split_rows) %>%
    mutate(model = factor(model, levels = panel_specs$model)) %>%
    arrange(model, period)

write_csv(split_results, out_path("output/tables", "wedge_panel_tax_gap_pre_post_2019", "csv"))

split_plot <- ggplot(split_results, aes(x = period, y = estimate, color = period)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.12, linewidth = 0.55) +
    geom_point(size = 2.4) +
    facet_wrap(~model, scales = "free_y") +
    scale_color_manual(values = c("Pre-2019" = "#2b6cb0", "Post-2019" = "#c05621")) +
    labs(
        title = "Panel Effect of Signed Sales Tax Gap Before and After 2019",
        subtitle = paste0(
            "Wedge FE and wedge-pair-year FE. Pre-2019 is 2015-2018; post-2019 is 2019-2022.",
            subtitle_suffix()
        ),
        x = NULL,
        y = "Estimate",
        color = NULL
    ) +
    theme_bw() +
    theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90", color = "grey60"))

ggsave(out_path("output/figures", "wedge_panel_tax_gap_pre_post_2019", "png"), split_plot, width = 8, height = 5, dpi = 150)


# Event-study panel plot ----
#
# This block estimates year-specific signed tax-gap effects in one stacked
# panel regression. The event-study terms are:
#
#   tax_diff_pp_2015 = tax_diff_pp * 1{year = 2015}
#   ...
#   tax_diff_pp_2022 = tax_diff_pp * 1{year = 2022}
#
# PPML formula:
#
#   online_estab ~ tax_diff_pp_2015 + tax_diff_pp_2016 + tax_diff_pp_2017
#                + tax_diff_pp_2018 + tax_diff_pp_2019 + tax_diff_pp_2020
#                + tax_diff_pp_2021 + tax_diff_pp_2022
#                + population + cit + log_wedge_market_potential
#                | wedge_id + pair_year_id
#
# OLS formula:
#
#   d_online_estab ~ tax_diff_pp_2015 + tax_diff_pp_2016 + tax_diff_pp_2017
#                  + tax_diff_pp_2018 + tax_diff_pp_2019 + tax_diff_pp_2020
#                  + tax_diff_pp_2021 + tax_diff_pp_2022
#                  + population + cit + log_wedge_market_potential
#                  | wedge_id + pair_year_id
#
# Each coefficient is the effect of a +1 percentage point own-minus-neighbor
# sales tax gap in that year.

event_terms <- c(
    "tax_diff_pp_2015",
    "tax_diff_pp_2016",
    "tax_diff_pp_2017",
    "tax_diff_pp_2018",
    "tax_diff_pp_2019",
    "tax_diff_pp_2020",
    "tax_diff_pp_2021",
    "tax_diff_pp_2022"
)

event_years <- as.integer(gsub("tax_diff_pp_", "", event_terms))

event_sample <- function(outcome_var) {
    panel_data %>%
        filter(
            !is.na(.data[[outcome_var]]),
            !is.na(tax_diff_pp),
            !is.na(pair_year_id),
            if_all(all_of(controls), ~ !is.na(.x))
        )
}

event_formula <- function(outcome_var) {
    as.formula(paste0(
        outcome_var,
        " ~ ",
        paste(c(event_terms, controls), collapse = " + "),
        " | wedge_id + pair_year_id"
    ))
}

event_rows <- list()
event_row_i <- 0

for (i in seq_len(nrow(panel_specs))) {
    spec <- panel_specs[i, ]
    data <- event_sample(spec$outcome)
    model_formula <- event_formula(spec$outcome)

    fit <- if (spec$model_type == "PPML") {
        fepois(model_formula, data = data, cluster = ~statefp + state_pair_id)
    } else {
        feols(model_formula, data = data, cluster = ~statefp + state_pair_id)
    }

    estimated_data <- data[obs(fit), , drop = FALSE]

    for (j in seq_along(event_terms)) {
        term <- event_terms[j]

        if (!term %in% names(coef(fit))) next

        b <- as.numeric(coef(fit)[term])
        crit <- qnorm(0.975)

        if (spec$model_type == "PPML") {
            se_b <- as.numeric(sqrt(vcov(fit)[term, term]))
            estimate <- exp(b) - 1
            estimate_se <- exp(b) * se_b
            ci_l <- exp(b - crit * se_b) - 1
            ci_r <- exp(b + crit * se_b) - 1
        } else {
            estimate <- b
            estimate_se <- as.numeric(se(fit)[term])
            ci_l <- estimate - crit * estimate_se
            ci_r <- estimate + crit * estimate_se
        }

        event_row_i <- event_row_i + 1
        event_rows[[event_row_i]] <- tibble(
            model = spec$model,
            outcome = spec$outcome,
            outcome_label = spec$outcome_label,
            model_type = spec$model_type,
            estimand = spec$estimand,
            treatment = term,
            treatment_label = "Signed sales tax gap (own minus neighbor, pp)",
            year = event_years[j],
            estimate = estimate,
            se = estimate_se,
            ci_l = ci_l,
            ci_r = ci_r,
            n_obs = fit$nobs,
            n_wedges = n_distinct(estimated_data$wedge_id),
            n_wedge_pairs = n_distinct(estimated_data$wedge_pair_id)
        )
    }
}

event_results <- bind_rows(event_rows) %>%
    mutate(model = factor(model, levels = panel_specs$model)) %>%
    arrange(model, year)

write_csv(event_results, out_path("output/tables", "wedge_panel_tax_gap_event_study", "csv"))

event_plot <- ggplot(event_results, aes(x = year, y = estimate, group = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.18, linewidth = 0.55) +
    geom_line(linewidth = 0.75, color = "#2b6cb0") +
    geom_point(size = 2.2, color = "#2b6cb0") +
    facet_wrap(~model, scales = "free_y") +
    scale_x_continuous(breaks = event_years) +
    labs(
        title = "Panel Event Study of Signed Sales Tax Gap",
        subtitle = paste0(
            "Year-specific +1 pp tax-gap effects from one model with wedge FE and wedge-pair-year FE.",
            subtitle_suffix()
        ),
        x = "Year",
        y = "Estimate"
    ) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey90", color = "grey60"))

ggsave(out_path("output/figures", "wedge_panel_tax_gap_event_study", "png"), event_plot, width = 9, height = 5, dpi = 150)
