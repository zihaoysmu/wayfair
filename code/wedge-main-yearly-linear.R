source("code/setup.R")

# 0. File options and output folders ----

wedge_data_suffix <- Sys.getenv("WEDGE_DATA_SUFFIX", "")
wedge_data_suffix_part <- if (nzchar(wedge_data_suffix)) paste0("_", wedge_data_suffix) else ""
subtitle_extra <- if (nzchar(wedge_data_suffix)) paste0(" Data: ", wedge_data_suffix, ".") else ""

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)


# 1. Read the balanced wedge-year data ----

wedge_data_path <- file.path(
    "data/temp",
    paste0("wedge_year_balanced_2015_2022", wedge_data_suffix_part, ".csv")
)

message("Reading wedge data: ", wedge_data_path)
wedge <- read.csv(wedge_data_path)


# 2. Keep only complete two-sided wedge pairs ----

# A complete pair has exactly two wedge_ids and two sides in every sample year.
# This matches wedge-main-yearly.R and keeps the pair comparisons balanced.
complete_pairs <- wedge %>%
    filter(!is.na(wedge_pair_id)) %>%
    group_by(wedge_pair_id, year) %>%
    summarise(
        n_wedges = n_distinct(wedge_id),
        n_sides = n_distinct(side),
        .groups = "drop"
    ) %>%
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

if (nrow(complete_pairs) == 0) {
    stop("No complete two-sided wedge pairs available in the wedge data.")
}

rows_before <- nrow(wedge)
pairs_before <- n_distinct(wedge$wedge_pair_id)

wedge <- wedge %>%
    semi_join(select(complete_pairs, wedge_pair_id), by = "wedge_pair_id")

message(
    "Keeping complete wedge pairs: ",
    n_distinct(wedge$wedge_pair_id), " of ", pairs_before,
    " pairs; ", nrow(wedge), " of ", rows_before, " wedge-year rows."
)


# 3. Build variables used by all yearly regressions ----

years <- sort(unique(wedge$year))
tax_gap_labels <- c("Low tax gap", "High tax gap")
controls <- c("population", "cit", "log_wedge_market_potential")

# Bring in the neighbor state's sales tax in the same year.
tax_lookup <- wedge %>%
    select(state_abbr, year, sales_tax) %>%
    distinct()

wedge_model_data <- wedge %>%
    left_join(
        rename(tax_lookup, neighbor_state = state_abbr, tax_other = sales_tax),
        by = c("neighbor_state", "year")
    ) %>%
    mutate(
        across(c(online_estab, population, cit, sales_tax, tax_other, wedge_market_potential), as.numeric),
        tax_diff = sales_tax - tax_other,
        tax_diff_abs = abs(tax_diff),
        tax_diff_pp = 100 * tax_diff_abs,
        high_tax_dummy = as.integer(sales_tax > tax_other),
        log_wedge_market_potential = log(wedge_market_potential)
    ) %>%
    arrange(wedge_id, year) %>%
    group_by(wedge_id) %>%
    mutate(d_online_estab = online_estab - lag(online_estab)) %>%
    ungroup()


# 4. Create low/high tax-gap bins and save the histogram ----

# The absolute tax gap should be identical for the two sides of a pair-year.
tax_gap <- wedge_model_data %>%
    filter(!is.na(wedge_pair_id), !is.na(tax_diff_abs)) %>%
    group_by(wedge_pair_id, year) %>%
    summarise(
        n_wedges = n_distinct(wedge_id),
        n_sides = n_distinct(side),
        n_tax_gaps = n_distinct(round(tax_diff_abs, 10)),
        tax_diff_abs = first(tax_diff_abs),
        tax_diff_pp = first(tax_diff_pp),
        .groups = "drop"
    )

if (any(tax_gap$n_tax_gaps != 1)) {
    stop("Expected one absolute tax gap within each wedge_pair_id-year cell.")
}

tax_gap_two_sided <- tax_gap %>%
    filter(n_wedges == 2, n_sides == 2)

if (nrow(tax_gap_two_sided) == 0) {
    stop("No complete two-sided wedge_pair_id-year cells available for tax gap bins.")
}

tax_gap_cutoff <- quantile(
    tax_gap_two_sided$tax_diff_abs,
    probs = 0.5,
    na.rm = TRUE,
    names = FALSE
)

if (any(!is.finite(tax_gap_cutoff))) {
    stop("Could not create a finite low/high tax gap median cutoff.")
}

tax_gap <- tax_gap %>%
    mutate(
        tax_diff_bin = cut(
            tax_diff_abs,
            breaks = c(-Inf, tax_gap_cutoff, Inf),
            labels = tax_gap_labels,
            include.lowest = TRUE
        )
    )

wedge_tax_gap_histogram <- ggplot(tax_gap_two_sided, aes(x = tax_diff_pp)) +
    geom_histogram(bins = 30, fill = "#2b6cb0", color = "white", linewidth = 0.2) +
    geom_vline(
        xintercept = 100 * tax_gap_cutoff,
        linetype = "dashed",
        color = "grey25",
        linewidth = 0.6
    ) +
    facet_wrap(~year) +
    labs(
        title = "Wedge Pair Sales Tax Gap Distribution",
        subtitle = paste0(
            "Complete two-sided wedge_pair_id-year cells only. Dashed line marks the full-sample median cutoff.",
            subtitle_extra
        ),
        x = "Absolute sales tax rate difference (percentage points)",
        y = "Wedge pair-years"
    ) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey90", color = "grey60"))

ggsave(
    file.path("output/figures", paste0("wedge_tax_diff_histogram", wedge_data_suffix_part, ".png")),
    wedge_tax_gap_histogram,
    width = 11,
    height = 8,
    dpi = 150
)
if (interactive()) print(wedge_tax_gap_histogram)


# 5. Add tax-gap interaction variables to the wedge-year data ----

# These two indicators make the tax-gap model report the high-tax-side effect
# directly in low-gap pair-years and high-gap pair-years.
wedge_model_data <- wedge_model_data %>%
    left_join(
        select(tax_gap, wedge_pair_id, year, tax_diff_bin),
        by = c("wedge_pair_id", "year")
    ) %>%
    mutate(
        tax_diff_bin = factor(tax_diff_bin, levels = tax_gap_labels),
        high_tax_x_gap_low = if_else(
            !is.na(high_tax_dummy) & !is.na(tax_diff_bin),
            as.integer(high_tax_dummy == 1 & tax_diff_bin == "Low tax gap"),
            NA_integer_
        ),
        high_tax_x_gap_high = if_else(
            !is.na(high_tax_dummy) & !is.na(tax_diff_bin),
            as.integer(high_tax_dummy == 1 & tax_diff_bin == "High tax gap"),
            NA_integer_
        )
    )


# 6. Model specification tables used by the repeated blocks ----

outcomes <- tibble(
    outcome = c("d_online_estab", "online_estab"),
    outcome_label = c(
        "Change in online establishments",
        "Online establishments (PPML proportional effect)"
    ),
    model_type = c("OLS", "PPML")
)

ols_outcomes <- tibble(
    outcome = c("d_online_estab", "online_estab"),
    outcome_label = c(
        "Change in online establishments (OLS)",
        "Online establishments level (OLS)"
    ),
    model_type = c("OLS", "OLS")
)

treatments <- tibble(
    treatment = c("high_tax_dummy", "sales_tax"),
    treatment_label = c("High-tax dummy", "Sales tax rate (1 pp)"),
    treatment_effect_unit = c(1, 0.01)
)

fe_specs <- tibble(
    pair_fe = c(FALSE, TRUE),
    fe_label = c("No pair FE", "Pair FE")
)

tax_gap_terms <- tibble(
    interaction_term = c("high_tax_x_gap_low", "high_tax_x_gap_high"),
    gap_bin = factor(tax_gap_labels, levels = tax_gap_labels)
)

crit <- qnorm(0.975)


# 7. Year-by-year 2x2 models: baseline OLS/PPML output ----

# For every year t, this block runs four specifications for each outcome:
#
#   y_it = a_t
#        + theta_t * high_tax_dummy_it
#        + b1_t * population_it
#        + b2_t * cit_it
#        + b3_t * log_wedge_market_potential_it
#        + e_it
#
#   y_it = a_t
#        + rho_t * sales_tax_it
#        + b1_t * population_it
#        + b2_t * cit_it
#        + b3_t * log_wedge_market_potential_it
#        + e_it
#
# and the same two specifications with wedge-pair fixed effects:
#
#   y_ipt = a_pt + treatment_ipt + controls_it + e_ipt
#
# R formulas used here are explicitly:
#
#   outcome ~ high_tax_dummy + population + cit + log_wedge_market_potential
#   outcome ~ high_tax_dummy + population + cit + log_wedge_market_potential | wedge_pair_id
#   outcome ~ sales_tax + population + cit + log_wedge_market_potential
#   outcome ~ sales_tax + population + cit + log_wedge_market_potential | wedge_pair_id
#
# Outcomes in this baseline block:
#   d_online_estab is estimated with OLS.
#   online_estab is estimated with PPML, so E[y | X] = exp(linear index).

wedge_year_model_rows <- list()
row_i <- 0

for (outcome_i in seq_len(nrow(outcomes))) {
    outcome_var <- outcomes$outcome[outcome_i]
    outcome_label <- outcomes$outcome_label[outcome_i]
    model_type <- outcomes$model_type[outcome_i]

    for (treatment_i in seq_len(nrow(treatments))) {
        treatment_var <- treatments$treatment[treatment_i]
        treatment_label <- treatments$treatment_label[treatment_i]
        treatment_effect_unit <- treatments$treatment_effect_unit[treatment_i]

        for (fe_i in seq_len(nrow(fe_specs))) {
            pair_fe <- fe_specs$pair_fe[fe_i]
            fe_label <- fe_specs$fe_label[fe_i]

            for (yr in years) {
                model_data <- wedge_model_data %>%
                    filter(
                        year == yr,
                        !is.na(.data[[outcome_var]]),
                        !is.na(.data[[treatment_var]]),
                        if_all(all_of(controls), ~ !is.na(.x))
                    )

                if (pair_fe) {
                    model_data <- model_data %>% filter(!is.na(wedge_pair_id))
                }

                if (nrow(model_data) == 0) next

                if (treatment_var == "high_tax_dummy" && !pair_fe) {
                    model_formula <- as.formula(paste0(
                        outcome_var,
                        " ~ high_tax_dummy + population + cit + log_wedge_market_potential"
                    ))
                }

                if (treatment_var == "high_tax_dummy" && pair_fe) {
                    model_formula <- as.formula(paste0(
                        outcome_var,
                        " ~ high_tax_dummy + population + cit + log_wedge_market_potential | wedge_pair_id"
                    ))
                }

                if (treatment_var == "sales_tax" && !pair_fe) {
                    model_formula <- as.formula(paste0(
                        outcome_var,
                        " ~ sales_tax + population + cit + log_wedge_market_potential"
                    ))
                }

                if (treatment_var == "sales_tax" && pair_fe) {
                    model_formula <- as.formula(paste0(
                        outcome_var,
                        " ~ sales_tax + population + cit + log_wedge_market_potential | wedge_pair_id"
                    ))
                }

                if (model_type == "PPML") {
                    fit <- try(fepois(model_formula, data = model_data, cluster = ~statefp + state_pair_id), silent = TRUE)
                } else {
                    fit <- try(feols(model_formula, data = model_data, cluster = ~statefp + state_pair_id), silent = TRUE)
                }

                if (inherits(fit, "try-error") || !treatment_var %in% names(coef(fit))) next

                b <- as.numeric(coef(fit)[treatment_var])

                if (model_type == "PPML") {
                    se_b <- as.numeric(sqrt(vcov(fit)[treatment_var, treatment_var]))
                    estimate <- exp(treatment_effect_unit * b) - 1
                    estimate_se <- treatment_effect_unit * exp(treatment_effect_unit * b) * se_b
                    ci_l <- exp(treatment_effect_unit * (b - crit * se_b)) - 1
                    ci_r <- exp(treatment_effect_unit * (b + crit * se_b)) - 1
                    estimand <- "proportional_effect"
                } else {
                    estimate <- treatment_effect_unit * b
                    estimate_se <- treatment_effect_unit * as.numeric(se(fit)[treatment_var])
                    ci_l <- estimate - crit * estimate_se
                    ci_r <- estimate + crit * estimate_se
                    estimand <- "coefficient"
                }

                row_i <- row_i + 1
                wedge_year_model_rows[[row_i]] <- tibble(
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
        }
    }
}

wedge_year_models <- bind_rows(wedge_year_model_rows) %>%
    mutate(
        treatment_label = factor(treatment_label, levels = treatments$treatment_label),
        fe_label = factor(fe_label, levels = fe_specs$fe_label)
    ) %>%
    arrange(treatment, pair_fe, outcome, model_type, year)

write_csv(
    wedge_year_models,
    file.path("output/tables", paste0("wedge_year_by_year_2x2", wedge_data_suffix_part, ".csv"))
)

wedge_year_2x2_plot <- ggplot(
    wedge_year_models,
    aes(x = year, y = estimate, color = outcome_label, group = outcome_label)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.1) +
    facet_grid(treatment_label ~ fe_label, scales = "free_y") +
    scale_x_continuous(breaks = years) +
    labs(
        title = "Wedge Year-by-Year Online Establishment Models",
        subtitle = paste0(
            "Rows: treatment variable; columns: pair fixed effects. Sales tax effects are for a 1 percentage point increase.",
            subtitle_extra
        ),
        x = "Year",
        y = "Estimate",
        color = NULL
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey90", color = "grey60")
    )

ggsave(
    file.path("output/figures", paste0("wedge_year_by_year_2x2", wedge_data_suffix_part, ".png")),
    wedge_year_2x2_plot,
    width = 11,
    height = 8,
    dpi = 150
)
if (interactive()) print(wedge_year_2x2_plot)


# 8. Tax-gap nonlinear models: baseline OLS/PPML output ----

# For every year t, this block estimates the high-tax-side effect separately
# for low-gap and high-gap wedge-pair-years.
#
# Definitions:
#   H_ipt = 1{wedge i is on the high-sales-tax side of pair p in year t}
#   G_pt  = 1{pair p is in the High tax gap bin in year t}
#   L_pt  = 1 - G_pt
#
# The two reported treatment variables are:
#   high_tax_x_gap_low  = H_ipt * L_pt
#   high_tax_x_gap_high = H_ipt * G_pt
#
# Without pair FE, the OLS linear-index version is:
#
#   y_ipt = a_t
#        + lambda_t * G_pt
#        + gamma_L,t * (H_ipt * L_pt)
#        + gamma_H,t * (H_ipt * G_pt)
#        + b1_t * population_it
#        + b2_t * cit_it
#        + b3_t * log_wedge_market_potential_it
#        + e_ipt
#
# With pair FE, it is:
#
#   y_ipt = a_pt
#        + lambda_t * G_pt
#        + gamma_L,t * (H_ipt * L_pt)
#        + gamma_H,t * (H_ipt * G_pt)
#        + controls_it
#        + e_ipt
#
# In the pair-FE version, G_pt is constant within wedge_pair_id-year cells and
# fixest will usually drop tax_diff_binHigh tax gap for collinearity. The two
# interaction terms are still the objects of interest.
#
# R formulas used here are explicitly:
#
#   outcome ~ tax_diff_bin + high_tax_x_gap_low + high_tax_x_gap_high
#             + population + cit + log_wedge_market_potential
#
#   outcome ~ tax_diff_bin + high_tax_x_gap_low + high_tax_x_gap_high
#             + population + cit + log_wedge_market_potential | wedge_pair_id
#
# For PPML, the same linear index is inside exp(.):
#   E[y_ipt | X] = exp(linear index)

wedge_tax_gap_model_rows <- list()
row_i <- 0

for (outcome_i in seq_len(nrow(outcomes))) {
    outcome_var <- outcomes$outcome[outcome_i]
    outcome_label <- outcomes$outcome_label[outcome_i]
    model_type <- outcomes$model_type[outcome_i]

    for (fe_i in seq_len(nrow(fe_specs))) {
        pair_fe <- fe_specs$pair_fe[fe_i]
        fe_label <- fe_specs$fe_label[fe_i]

        for (yr in years) {
            model_data <- wedge_model_data %>%
                filter(
                    year == yr,
                    !is.na(.data[[outcome_var]]),
                    !is.na(tax_diff_bin),
                    !is.na(high_tax_dummy),
                    if_all(all_of(controls), ~ !is.na(.x))
                )

            if (pair_fe) {
                model_data <- model_data %>% filter(!is.na(wedge_pair_id))
            }

            if (nrow(model_data) == 0) next

            if (!pair_fe) {
                model_formula <- as.formula(paste0(
                    outcome_var,
                    " ~ tax_diff_bin + high_tax_x_gap_low + high_tax_x_gap_high",
                    " + population + cit + log_wedge_market_potential"
                ))
            }

            if (pair_fe) {
                model_formula <- as.formula(paste0(
                    outcome_var,
                    " ~ tax_diff_bin + high_tax_x_gap_low + high_tax_x_gap_high",
                    " + population + cit + log_wedge_market_potential | wedge_pair_id"
                ))
            }

            if (model_type == "PPML") {
                fit <- try(fepois(model_formula, data = model_data, cluster = ~statefp + state_pair_id), silent = TRUE)
            } else {
                fit <- try(feols(model_formula, data = model_data, cluster = ~statefp + state_pair_id), silent = TRUE)
            }

            if (inherits(fit, "try-error")) next

            available_terms <- tax_gap_terms %>%
                filter(interaction_term %in% names(coef(fit)))

            if (nrow(available_terms) == 0) next

            for (term_i in seq_len(nrow(available_terms))) {
                term <- available_terms$interaction_term[term_i]
                gap_bin <- available_terms$gap_bin[term_i]
                b <- as.numeric(coef(fit)[term])

                if (model_type == "PPML") {
                    se_b <- as.numeric(sqrt(vcov(fit)[term, term]))
                    estimate <- exp(b) - 1
                    estimate_se <- exp(b) * se_b
                    ci_l <- exp(b - crit * se_b) - 1
                    ci_r <- exp(b + crit * se_b) - 1
                    estimand <- "proportional_effect"
                } else {
                    estimate <- b
                    estimate_se <- as.numeric(se(fit)[term])
                    ci_l <- estimate - crit * estimate_se
                    ci_r <- estimate + crit * estimate_se
                    estimand <- "coefficient"
                }

                row_i <- row_i + 1
                wedge_tax_gap_model_rows[[row_i]] <- tibble(
                    outcome = outcome_var,
                    outcome_label = outcome_label,
                    model_type = model_type,
                    gap_bin = gap_bin,
                    interaction_term = term,
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
        }
    }
}

wedge_tax_gap_models <- bind_rows(wedge_tax_gap_model_rows) %>%
    mutate(
        gap_bin = factor(gap_bin, levels = tax_gap_labels),
        fe_label = factor(fe_label, levels = fe_specs$fe_label)
    ) %>%
    arrange(pair_fe, outcome, model_type, year, gap_bin)

write_csv(
    wedge_tax_gap_models,
    file.path("output/tables", paste0("wedge_tax_gap_nonlinear_by_year", wedge_data_suffix_part, ".csv"))
)

wedge_tax_gap_nonlinear_plot <- ggplot(
    wedge_tax_gap_models,
    aes(x = year, y = estimate, color = gap_bin, group = gap_bin)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.1) +
    facet_grid(outcome_label ~ fe_label, scales = "free_y") +
    scale_x_continuous(breaks = years) +
    labs(
        title = "Wedge High-Tax Effects by Sales Tax Gap",
        subtitle = paste0(
            "Tax gap bins use a full-sample median split from complete two-sided wedge pairs.",
            subtitle_extra
        ),
        x = "Year",
        y = "Estimate",
        color = NULL
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey90", color = "grey60")
    )

ggsave(
    file.path("output/figures", paste0("wedge_tax_gap_nonlinear_by_year", wedge_data_suffix_part, ".png")),
    wedge_tax_gap_nonlinear_plot,
    width = 11,
    height = 8,
    dpi = 150
)
if (interactive()) print(wedge_tax_gap_nonlinear_plot)


# 9. Year-by-year 2x2 models: all-OLS robustness output ----

# Same four visible formulas as block 7, but both outcomes are estimated with OLS:
#
#   d_online_estab ~ high_tax_dummy + population + cit + log_wedge_market_potential
#   d_online_estab ~ high_tax_dummy + population + cit + log_wedge_market_potential | wedge_pair_id
#   d_online_estab ~ sales_tax + population + cit + log_wedge_market_potential
#   d_online_estab ~ sales_tax + population + cit + log_wedge_market_potential | wedge_pair_id
#
#   online_estab ~ high_tax_dummy + population + cit + log_wedge_market_potential
#   online_estab ~ high_tax_dummy + population + cit + log_wedge_market_potential | wedge_pair_id
#   online_estab ~ sales_tax + population + cit + log_wedge_market_potential
#   online_estab ~ sales_tax + population + cit + log_wedge_market_potential | wedge_pair_id

wedge_year_model_ols_rows <- list()
row_i <- 0

for (outcome_i in seq_len(nrow(ols_outcomes))) {
    outcome_var <- ols_outcomes$outcome[outcome_i]
    outcome_label <- ols_outcomes$outcome_label[outcome_i]
    model_type <- ols_outcomes$model_type[outcome_i]

    for (treatment_i in seq_len(nrow(treatments))) {
        treatment_var <- treatments$treatment[treatment_i]
        treatment_label <- treatments$treatment_label[treatment_i]
        treatment_effect_unit <- treatments$treatment_effect_unit[treatment_i]

        for (fe_i in seq_len(nrow(fe_specs))) {
            pair_fe <- fe_specs$pair_fe[fe_i]
            fe_label <- fe_specs$fe_label[fe_i]

            for (yr in years) {
                model_data <- wedge_model_data %>%
                    filter(
                        year == yr,
                        !is.na(.data[[outcome_var]]),
                        !is.na(.data[[treatment_var]]),
                        if_all(all_of(controls), ~ !is.na(.x))
                    )

                if (pair_fe) {
                    model_data <- model_data %>% filter(!is.na(wedge_pair_id))
                }

                if (nrow(model_data) == 0) next

                if (treatment_var == "high_tax_dummy" && !pair_fe) {
                    model_formula <- as.formula(paste0(
                        outcome_var,
                        " ~ high_tax_dummy + population + cit + log_wedge_market_potential"
                    ))
                }

                if (treatment_var == "high_tax_dummy" && pair_fe) {
                    model_formula <- as.formula(paste0(
                        outcome_var,
                        " ~ high_tax_dummy + population + cit + log_wedge_market_potential | wedge_pair_id"
                    ))
                }

                if (treatment_var == "sales_tax" && !pair_fe) {
                    model_formula <- as.formula(paste0(
                        outcome_var,
                        " ~ sales_tax + population + cit + log_wedge_market_potential"
                    ))
                }

                if (treatment_var == "sales_tax" && pair_fe) {
                    model_formula <- as.formula(paste0(
                        outcome_var,
                        " ~ sales_tax + population + cit + log_wedge_market_potential | wedge_pair_id"
                    ))
                }

                fit <- try(feols(model_formula, data = model_data, cluster = ~statefp + state_pair_id), silent = TRUE)

                if (inherits(fit, "try-error") || !treatment_var %in% names(coef(fit))) next

                b <- as.numeric(coef(fit)[treatment_var])
                estimate <- treatment_effect_unit * b
                estimate_se <- treatment_effect_unit * as.numeric(se(fit)[treatment_var])
                ci_l <- estimate - crit * estimate_se
                ci_r <- estimate + crit * estimate_se

                row_i <- row_i + 1
                wedge_year_model_ols_rows[[row_i]] <- tibble(
                    outcome = outcome_var,
                    outcome_label = outcome_label,
                    model_type = model_type,
                    treatment = treatment_var,
                    treatment_label = treatment_label,
                    treatment_effect_unit = treatment_effect_unit,
                    pair_fe = pair_fe,
                    fe_label = fe_label,
                    estimand = "coefficient",
                    year = yr,
                    estimate = estimate,
                    se = estimate_se,
                    ci_l = ci_l,
                    ci_r = ci_r,
                    n_obs = fit$nobs
                )
            }
        }
    }
}

wedge_year_models_ols <- bind_rows(wedge_year_model_ols_rows) %>%
    mutate(
        treatment_label = factor(treatment_label, levels = treatments$treatment_label),
        fe_label = factor(fe_label, levels = fe_specs$fe_label)
    ) %>%
    arrange(treatment, pair_fe, outcome, model_type, year)

write_csv(
    wedge_year_models_ols,
    file.path("output/tables", paste0("wedge_year_by_year_2x2_ols", wedge_data_suffix_part, ".csv"))
)

wedge_year_2x2_ols_plot <- ggplot(
    wedge_year_models_ols,
    aes(x = year, y = estimate, color = outcome_label, group = outcome_label)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.1) +
    facet_grid(treatment_label ~ fe_label, scales = "free_y") +
    scale_x_continuous(breaks = years) +
    labs(
        title = "Wedge Year-by-Year Online Establishment Models (OLS)",
        subtitle = paste0(
            "Rows: treatment variable; columns: pair fixed effects. Sales tax effects are for a 1 percentage point increase.",
            subtitle_extra
        ),
        x = "Year",
        y = "OLS estimate",
        color = NULL
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey90", color = "grey60")
    )

ggsave(
    file.path("output/figures", paste0("wedge_year_by_year_2x2_ols", wedge_data_suffix_part, ".png")),
    wedge_year_2x2_ols_plot,
    width = 11,
    height = 8,
    dpi = 150
)
if (interactive()) print(wedge_year_2x2_ols_plot)


# 10. Tax-gap nonlinear models: all-OLS robustness output ----

# Same tax-gap formulas as block 8, but both outcomes are estimated with OLS:
#
#   outcome ~ tax_diff_bin + high_tax_x_gap_low + high_tax_x_gap_high
#             + population + cit + log_wedge_market_potential
#
#   outcome ~ tax_diff_bin + high_tax_x_gap_low + high_tax_x_gap_high
#             + population + cit + log_wedge_market_potential | wedge_pair_id
#
# The reported coefficients are gamma_L,t and gamma_H,t in the equations above.

wedge_tax_gap_model_ols_rows <- list()
row_i <- 0

for (outcome_i in seq_len(nrow(ols_outcomes))) {
    outcome_var <- ols_outcomes$outcome[outcome_i]
    outcome_label <- ols_outcomes$outcome_label[outcome_i]
    model_type <- ols_outcomes$model_type[outcome_i]

    for (fe_i in seq_len(nrow(fe_specs))) {
        pair_fe <- fe_specs$pair_fe[fe_i]
        fe_label <- fe_specs$fe_label[fe_i]

        for (yr in years) {
            model_data <- wedge_model_data %>%
                filter(
                    year == yr,
                    !is.na(.data[[outcome_var]]),
                    !is.na(tax_diff_bin),
                    !is.na(high_tax_dummy),
                    if_all(all_of(controls), ~ !is.na(.x))
                )

            if (pair_fe) {
                model_data <- model_data %>% filter(!is.na(wedge_pair_id))
            }

            if (nrow(model_data) == 0) next

            if (!pair_fe) {
                model_formula <- as.formula(paste0(
                    outcome_var,
                    " ~ tax_diff_bin + high_tax_x_gap_low + high_tax_x_gap_high",
                    " + population + cit + log_wedge_market_potential"
                ))
            }

            if (pair_fe) {
                model_formula <- as.formula(paste0(
                    outcome_var,
                    " ~ tax_diff_bin + high_tax_x_gap_low + high_tax_x_gap_high",
                    " + population + cit + log_wedge_market_potential | wedge_pair_id"
                ))
            }

            fit <- try(feols(model_formula, data = model_data, cluster = ~statefp + state_pair_id), silent = TRUE)

            if (inherits(fit, "try-error")) next

            available_terms <- tax_gap_terms %>%
                filter(interaction_term %in% names(coef(fit)))

            if (nrow(available_terms) == 0) next

            for (term_i in seq_len(nrow(available_terms))) {
                term <- available_terms$interaction_term[term_i]
                gap_bin <- available_terms$gap_bin[term_i]
                b <- as.numeric(coef(fit)[term])
                estimate <- b
                estimate_se <- as.numeric(se(fit)[term])
                ci_l <- estimate - crit * estimate_se
                ci_r <- estimate + crit * estimate_se

                row_i <- row_i + 1
                wedge_tax_gap_model_ols_rows[[row_i]] <- tibble(
                    outcome = outcome_var,
                    outcome_label = outcome_label,
                    model_type = model_type,
                    gap_bin = gap_bin,
                    interaction_term = term,
                    pair_fe = pair_fe,
                    fe_label = fe_label,
                    estimand = "coefficient",
                    year = yr,
                    estimate = estimate,
                    se = estimate_se,
                    ci_l = ci_l,
                    ci_r = ci_r,
                    n_obs = fit$nobs
                )
            }
        }
    }
}

wedge_tax_gap_models_ols <- bind_rows(wedge_tax_gap_model_ols_rows) %>%
    mutate(
        gap_bin = factor(gap_bin, levels = tax_gap_labels),
        fe_label = factor(fe_label, levels = fe_specs$fe_label)
    ) %>%
    arrange(pair_fe, outcome, model_type, year, gap_bin)

write_csv(
    wedge_tax_gap_models_ols,
    file.path("output/tables", paste0("wedge_tax_gap_nonlinear_by_year_ols", wedge_data_suffix_part, ".csv"))
)

wedge_tax_gap_nonlinear_ols_plot <- ggplot(
    wedge_tax_gap_models_ols,
    aes(x = year, y = estimate, color = gap_bin, group = gap_bin)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.1) +
    facet_grid(outcome_label ~ fe_label, scales = "free_y") +
    scale_x_continuous(breaks = years) +
    labs(
        title = "Wedge High-Tax Effects by Sales Tax Gap (OLS)",
        subtitle = paste0(
            "Tax gap bins use a full-sample median split from complete two-sided wedge pairs.",
            subtitle_extra
        ),
        x = "Year",
        y = "OLS estimate",
        color = NULL
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey90", color = "grey60")
    )

ggsave(
    file.path("output/figures", paste0("wedge_tax_gap_nonlinear_by_year_ols", wedge_data_suffix_part, ".png")),
    wedge_tax_gap_nonlinear_ols_plot,
    width = 11,
    height = 8,
    dpi = 150
)
if (interactive()) print(wedge_tax_gap_nonlinear_ols_plot)
