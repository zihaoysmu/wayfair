source("code/setup.R")

# Robustness pipeline:
# 1. Read the balanced wedge-year panel, with an optional suffix such as "1mile".
# 2. Keep only complete two-sided wedge pairs.
# 3. Build the same variables as wedge-main-yearly.R.
# 4. Write the all-OLS yearly outputs plus quadratic and lagged-treatment
#    robustness outputs.

wedge_data_suffix <- Sys.getenv("WEDGE_DATA_SUFFIX", "")
wedge_data_suffix_part <- if (nzchar(wedge_data_suffix)) paste0("_", wedge_data_suffix) else ""

# All output names use the same suffix convention as the input data.
out_path <- function(dir, stem, ext) file.path(dir, paste0(stem, wedge_data_suffix_part, ".", ext))
subtitle_suffix <- function() if (nzchar(wedge_data_suffix)) paste0(" Data: ", wedge_data_suffix, ".") else ""
save_plot <- function(plot, stem) {
    ggsave(out_path("output/figures", stem, "png"), plot, width = 11, height = 8, dpi = 150)
    if (interactive()) print(plot)
}

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

wedge_data_path <- file.path("data/temp", paste0("wedge_year_balanced_2015_2022", wedge_data_suffix_part, ".csv"))
message("Reading wedge data: ", wedge_data_path)
wedge <- read.csv(wedge_data_path)

for (estab_col in c("warehouse_estab")) {
    if (!estab_col %in% names(wedge)) wedge[[estab_col]] <- 0
}

# A usable wedge pair must have exactly two wedges/two sides in every year.
# This keeps the pair-FE comparisons balanced and prevents one-sided cells
# from entering the tax-gap split.
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

rows_before <- nrow(wedge)
pairs_before <- n_distinct(wedge$wedge_pair_id)
wedge <- semi_join(wedge, select(complete_pairs, wedge_pair_id), by = "wedge_pair_id")
message(
    "Keeping complete wedge pairs: ",
    n_distinct(wedge$wedge_pair_id), " of ", pairs_before,
    " pairs; ", nrow(wedge), " of ", rows_before, " wedge-year rows."
)

# Shared objects used by all model specifications below.
years <- sort(unique(wedge$year))
tax_gap_labels <- c("Low tax gap", "High tax gap")
estab_outcome_vars <- c("warehouse_estab", "online_estab", "local_estab")
controls <- c("population", "cit", "log_wedge_market_potential")

# Attach the neighbor state's sales tax, then create the treatment variables.
# d_* variables are within-wedge annual changes in each establishment outcome.
tax_lookup <- wedge %>% select(state_abbr, year, sales_tax) %>% distinct()
wedge_model_data <- wedge %>%
    left_join(rename(tax_lookup, neighbor_state = state_abbr, tax_other = sales_tax), by = c("neighbor_state", "year")) %>%
    mutate(
        across(c(all_of(estab_outcome_vars), population, cit, sales_tax, tax_other, wedge_market_potential), as.numeric),
        across(all_of(estab_outcome_vars), ~ replace_na(.x, 0)),
        tax_diff = sales_tax - tax_other,
        tax_diff_abs = abs(tax_diff),
        tax_diff_pp = 100 * tax_diff_abs,
        high_tax_dummy = as.integer(sales_tax > tax_other),
        log_wedge_market_potential = log(wedge_market_potential)
    ) %>%
    arrange(wedge_id, year) %>%
    group_by(wedge_id) %>%
    mutate(
        d_warehouse_estab = warehouse_estab - lag(warehouse_estab),
        d_online_estab = online_estab - lag(online_estab),
        d_local_estab = local_estab - lag(local_estab)
    ) %>%
    ungroup()

# Tax-gap bins are computed at the wedge-pair-year level because the absolute
# gap should be identical for both sides of a pair in a given year.
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
if (any(tax_gap$n_tax_gaps != 1)) stop("Expected one absolute tax gap within each wedge_pair_id-year cell.")

tax_gap_two_sided <- filter(tax_gap, n_wedges == 2, n_sides == 2)
if (nrow(tax_gap_two_sided) == 0) stop("No complete two-sided wedge_pair_id-year cells available for tax gap bins.")

tax_gap_cutoff <- quantile(tax_gap_two_sided$tax_diff_abs, probs = 0.5, na.rm = TRUE, names = FALSE)
if (any(!is.finite(tax_gap_cutoff))) stop("Could not create a finite low/high tax gap median cutoff.")

quadratic_gap_values <- as.numeric(quantile(tax_gap_two_sided$tax_diff_pp, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE))
quadratic_gap_labels <- paste0(c("25th pct gap", "Median gap", "75th pct gap"), " (", round(quadratic_gap_values, 2), " pp)")
quadratic_gap_points <- tibble(
    gap_quantile = c(0.25, 0.5, 0.75),
    gap_label = factor(quadratic_gap_labels, levels = quadratic_gap_labels),
    tax_diff_pp = quadratic_gap_values,
    tax_diff_pp_sq = quadratic_gap_values^2
)

tax_gap <- tax_gap %>%
    mutate(tax_diff_bin = cut(tax_diff_abs, c(-Inf, tax_gap_cutoff, Inf), tax_gap_labels, include.lowest = TRUE))

# Diagnostic plot: distribution of pair-year tax gaps and the median split.
tax_gap_histogram <- ggplot(tax_gap_two_sided, aes(x = tax_diff_pp)) +
    geom_histogram(bins = 30, fill = "#2b6cb0", color = "white", linewidth = 0.2) +
    geom_vline(xintercept = 100 * tax_gap_cutoff, linetype = "dashed", color = "grey25", linewidth = 0.6) +
    facet_wrap(~year) +
    labs(
        title = "Wedge Pair Sales Tax Gap Distribution",
        subtitle = paste0(
            "Complete two-sided wedge_pair_id-year cells only. Dashed line marks the full-sample median cutoff.",
            subtitle_suffix()
        ),
        x = "Absolute sales tax rate difference (percentage points)",
        y = "Wedge pair-years"
    ) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey90", color = "grey60"))

# Add low/high tax-gap indicators back to the wedge-year data. The nonlinear
# models estimate separate high-tax-side effects in low-gap and high-gap bins.
wedge_model_data <- wedge_model_data %>%
    left_join(select(tax_gap, wedge_pair_id, year, tax_diff_bin), by = c("wedge_pair_id", "year")) %>%
    mutate(
        tax_diff_bin = factor(tax_diff_bin, levels = tax_gap_labels),
        tax_diff_pp_sq = tax_diff_pp^2,
        high_tax_x_gap_pp = high_tax_dummy * tax_diff_pp,
        high_tax_x_gap_pp_sq = high_tax_dummy * tax_diff_pp_sq,
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
    ) %>%
    arrange(wedge_id, year) %>%
    group_by(wedge_id) %>%
    mutate(
        lag_high_tax_dummy = lag(high_tax_dummy),
        lag_sales_tax = lag(sales_tax),
        lag_tax_diff_bin = lag(tax_diff_bin),
        lag_high_tax_x_gap_low = lag(high_tax_x_gap_low),
        lag_high_tax_x_gap_high = lag(high_tax_x_gap_high)
    ) %>%
    ungroup() %>%
    mutate(
        lag_tax_diff_bin = factor(lag_tax_diff_bin, levels = tax_gap_labels)
    )

# Model specs are stored as small tables so the runners below can loop over
# outcomes, treatments, and fixed-effect choices without copy-pasting models.
outcome_groups <- tibble(
    outcome_group = c("warehouse", "online_retailer", "local_retailer"),
    outcome_group_label = c("Warehouse", "Online retailer", "Local retailer"),
    level_outcome = c("warehouse_estab", "online_estab", "local_estab"),
    change_outcome = c("d_warehouse_estab", "d_online_estab", "d_local_estab")
)

make_outcomes <- function(outcome_group_spec) {
    label <- outcome_group_spec$outcome_group_label
    tibble(
        outcome = c(outcome_group_spec$change_outcome, outcome_group_spec$level_outcome),
        outcome_label = c(
            paste0("Change in ", label, " establishments"),
            paste0(label, " establishments (PPML proportional effect)")
        ),
        model_type = c("OLS", "PPML")
    )
}

make_ols_outcomes <- function(outcome_group_spec) {
    label <- outcome_group_spec$outcome_group_label
    tibble(
        outcome = c(outcome_group_spec$change_outcome, outcome_group_spec$level_outcome),
        outcome_label = c(
            paste0("Change in ", label, " establishments (OLS)"),
            paste0(label, " establishments level (OLS)")
        ),
        model_type = c("OLS", "OLS")
    )
}
treatments <- tibble(
    treatment = c("high_tax_dummy", "sales_tax"),
    treatment_label = c("High-tax dummy", "Sales tax rate (1 pp)"),
    treatment_effect_unit = c(1, 0.01)
)
lag_treatments <- tibble(
    treatment = c("lag_high_tax_dummy", "lag_sales_tax"),
    treatment_label = c("Lagged high-tax dummy", "Lagged sales tax rate (1 pp)"),
    treatment_effect_unit = c(1, 0.01)
)
fe_specs <- tibble(pair_fe = c(FALSE, TRUE), fe_label = c("No pair FE", "Pair FE"))
tax_gap_terms <- tibble(
    interaction_term = c("high_tax_x_gap_low", "high_tax_x_gap_high"),
    gap_bin = factor(tax_gap_labels, levels = tax_gap_labels)
)
lag_tax_gap_terms <- tibble(
    interaction_term = c("lag_high_tax_x_gap_low", "lag_high_tax_x_gap_high"),
    gap_bin = factor(tax_gap_labels, levels = tax_gap_labels)
)

# Build the regression sample for one outcome/year/spec. Pair FE specs keep
# only rows with a valid wedge_pair_id.
model_sample <- function(outcome_var, required_vars, yr, pair_fe) {
    data <- wedge_model_data %>%
        filter(year == yr, !is.na(.data[[outcome_var]]), if_all(all_of(required_vars), ~ !is.na(.x)))
    if (pair_fe) data <- filter(data, !is.na(wedge_pair_id))
    data
}

# Estimate one fixest model. PPML uses fepois; all other specs use feols.
fit_model <- function(data, outcome_var, rhs_vars, model_type, pair_fe) {
    model_formula <- as.formula(paste0(outcome_var, " ~ ", paste(rhs_vars, collapse = " + "), if (pair_fe) " | wedge_pair_id" else ""))
    tryCatch({
        if (model_type == "PPML") fepois(model_formula, data = data, cluster = ~statefp + state_pair_id)
        else feols(model_formula, data = data, cluster = ~statefp + state_pair_id)
    }, error = function(e) NULL)
}

# Convert a raw model coefficient into the reported effect and 95% CI.
# For PPML, estimates are reported as proportional effects: exp(beta) - 1.
# For sales_tax OLS, unit = 0.01 reports a one percentage point effect.
coef_effect <- function(fit, term, model_type, unit = 1) {
    if (is.null(fit) || !term %in% names(coef(fit))) return(NULL)

    crit <- qnorm(0.975)
    b <- as.numeric(coef(fit)[term])
    if (model_type == "PPML") {
        se_b <- as.numeric(sqrt(vcov(fit)[term, term]))
        return(tibble(
            estimand = "proportional_effect",
            estimate = exp(unit * b) - 1,
            se = unit * exp(unit * b) * se_b,
            ci_l = exp(unit * (b - crit * se_b)) - 1,
            ci_r = exp(unit * (b + crit * se_b)) - 1
        ))
    }

    se_b <- as.numeric(se(fit)[term])
    estimate <- unit * b
    estimate_se <- unit * se_b
    tibble(
        estimand = "coefficient",
        estimate = estimate,
        se = estimate_se,
        ci_l = estimate - crit * estimate_se,
        ci_r = estimate + crit * estimate_se
    )
}

# Quadratic tax-gap model:
#   Y = beta_0 H + beta_1 G + beta_2 G^2 + beta_3 H*G + beta_4 H*G^2 + controls + FE + e
# where H is high_tax_dummy and G is the absolute sales-tax gap in percentage
# points. The reported effect at gap G = g is beta_0 + beta_3*g + beta_4*g^2.
quadratic_gap_effect <- function(fit, model_type, gap_pp) {
    if (is.null(fit)) return(NULL)

    terms <- c("high_tax_dummy", "high_tax_x_gap_pp", "high_tax_x_gap_pp_sq")
    if (!all(terms %in% names(coef(fit)))) return(NULL)

    contrast <- c(1, gap_pp, gap_pp^2)
    names(contrast) <- terms

    b <- sum(contrast * coef(fit)[terms])
    v <- vcov(fit)[terms, terms, drop = FALSE]
    se_b <- sqrt(max(0, as.numeric(t(contrast) %*% v %*% contrast)))
    crit <- qnorm(0.975)

    if (model_type == "PPML") {
        return(tibble(
            estimand = "proportional_effect",
            estimate = exp(b) - 1,
            se = exp(b) * se_b,
            ci_l = exp(b - crit * se_b) - 1,
            ci_r = exp(b + crit * se_b) - 1
        ))
    }

    tibble(
        estimand = "coefficient",
        estimate = b,
        se = se_b,
        ci_l = b - crit * se_b,
        ci_r = b + crit * se_b
    )
}

# One year-by-year treatment model: outcome on either high_tax_dummy or
# sales_tax, plus controls, with optional wedge-pair fixed effects.
run_year_model <- function(outcome_spec, treatment_spec, fe_spec, yr) {
    data <- model_sample(outcome_spec$outcome, c(treatment_spec$treatment, controls), yr, fe_spec$pair_fe)
    if (nrow(data) == 0) return(NULL)

    fit <- fit_model(data, outcome_spec$outcome, c(treatment_spec$treatment, controls), outcome_spec$model_type, fe_spec$pair_fe)
    effect <- coef_effect(fit, treatment_spec$treatment, outcome_spec$model_type, treatment_spec$treatment_effect_unit)
    if (is.null(effect)) return(NULL)

    tibble(
        outcome = outcome_spec$outcome,
        outcome_label = outcome_spec$outcome_label,
        model_type = outcome_spec$model_type,
        treatment = treatment_spec$treatment,
        treatment_label = treatment_spec$treatment_label,
        treatment_effect_unit = treatment_spec$treatment_effect_unit,
        pair_fe = fe_spec$pair_fe,
        fe_label = fe_spec$fe_label,
        estimand = effect$estimand,
        year = yr,
        estimate = effect$estimate,
        se = effect$se,
        ci_l = effect$ci_l,
        ci_r = effect$ci_r,
        n_obs = fit$nobs
    )
}

# One nonlinear tax-gap model: estimate the high-tax effect separately in
# low-gap and high-gap pair-years.
run_tax_gap_model <- function(outcome_spec, fe_spec, yr) {
    rhs <- c("tax_diff_bin", tax_gap_terms$interaction_term, controls)
    data <- model_sample(outcome_spec$outcome, c("tax_diff_bin", "high_tax_dummy", controls), yr, fe_spec$pair_fe)
    if (nrow(data) == 0) return(NULL)

    fit <- fit_model(data, outcome_spec$outcome, rhs, outcome_spec$model_type, fe_spec$pair_fe)
    if (is.null(fit)) return(NULL)

    available_terms <- filter(tax_gap_terms, interaction_term %in% names(coef(fit)))
    if (nrow(available_terms) == 0) return(NULL)

    bind_rows(lapply(seq_len(nrow(available_terms)), function(i) {
        effect <- coef_effect(fit, available_terms$interaction_term[i], outcome_spec$model_type)
        if (is.null(effect)) return(NULL)
        tibble(
            outcome = outcome_spec$outcome,
            outcome_label = outcome_spec$outcome_label,
            model_type = outcome_spec$model_type,
            gap_bin = available_terms$gap_bin[i],
            interaction_term = available_terms$interaction_term[i],
            pair_fe = fe_spec$pair_fe,
            fe_label = fe_spec$fe_label,
            estimand = effect$estimand,
            year = yr,
            estimate = effect$estimate,
            se = effect$se,
            ci_l = effect$ci_l,
            ci_r = effect$ci_r,
            n_obs = fit$nobs
        )
    }))
}

run_quadratic_tax_gap_model <- function(outcome_spec, fe_spec, yr) {
    rhs <- c("high_tax_dummy", "tax_diff_pp", "tax_diff_pp_sq", "high_tax_x_gap_pp", "high_tax_x_gap_pp_sq", controls)
    data <- model_sample(outcome_spec$outcome, rhs, yr, fe_spec$pair_fe)
    if (nrow(data) == 0) return(NULL)

    fit <- fit_model(data, outcome_spec$outcome, rhs, outcome_spec$model_type, fe_spec$pair_fe)
    if (is.null(fit)) return(NULL)

    bind_rows(lapply(seq_len(nrow(quadratic_gap_points)), function(i) {
        gap_point <- quadratic_gap_points[i, ]
        effect <- quadratic_gap_effect(fit, outcome_spec$model_type, gap_point$tax_diff_pp)
        if (is.null(effect)) return(NULL)
        tibble(
            outcome = outcome_spec$outcome,
            outcome_label = outcome_spec$outcome_label,
            model_type = outcome_spec$model_type,
            gap_quantile = gap_point$gap_quantile,
            gap_label = gap_point$gap_label,
            tax_diff_pp = gap_point$tax_diff_pp,
            pair_fe = fe_spec$pair_fe,
            fe_label = fe_spec$fe_label,
            estimand = effect$estimand,
            year = yr,
            estimate = effect$estimate,
            se = effect$se,
            ci_l = effect$ci_l,
            ci_r = effect$ci_r,
            n_obs = fit$nobs
        )
    }))
}

run_lag_tax_gap_model <- function(outcome_spec, fe_spec, yr) {
    rhs <- c("lag_tax_diff_bin", lag_tax_gap_terms$interaction_term, controls)
    data <- model_sample(outcome_spec$outcome, c("lag_tax_diff_bin", "lag_high_tax_dummy", controls), yr, fe_spec$pair_fe)
    if (nrow(data) == 0) return(NULL)

    fit <- fit_model(data, outcome_spec$outcome, rhs, outcome_spec$model_type, fe_spec$pair_fe)
    if (is.null(fit)) return(NULL)

    available_terms <- filter(lag_tax_gap_terms, interaction_term %in% names(coef(fit)))
    if (nrow(available_terms) == 0) return(NULL)

    bind_rows(lapply(seq_len(nrow(available_terms)), function(i) {
        effect <- coef_effect(fit, available_terms$interaction_term[i], outcome_spec$model_type)
        if (is.null(effect)) return(NULL)
        tibble(
            outcome = outcome_spec$outcome,
            outcome_label = outcome_spec$outcome_label,
            model_type = outcome_spec$model_type,
            gap_bin = available_terms$gap_bin[i],
            interaction_term = available_terms$interaction_term[i],
            pair_fe = fe_spec$pair_fe,
            fe_label = fe_spec$fe_label,
            estimand = effect$estimand,
            year = yr,
            estimate = effect$estimate,
            se = effect$se,
            ci_l = effect$ci_l,
            ci_r = effect$ci_r,
            n_obs = fit$nobs
        )
    }))
}

# Expand the outcome x treatment x FE x year grid for the main 2x2 figure.
run_year_models <- function(outcome_specs) {
    grid <- expand.grid(
        outcome_i = seq_len(nrow(outcome_specs)),
        treatment_i = seq_len(nrow(treatments)),
        fe_i = seq_len(nrow(fe_specs)),
        year = years,
        KEEP.OUT.ATTRS = FALSE
    )

    bind_rows(lapply(seq_len(nrow(grid)), function(i) {
        g <- grid[i, ]
        run_year_model(outcome_specs[g$outcome_i, ], treatments[g$treatment_i, ], fe_specs[g$fe_i, ], g$year)
    })) %>%
        mutate(
            treatment_label = factor(treatment_label, levels = treatments$treatment_label),
            fe_label = factor(fe_label, levels = fe_specs$fe_label)
        ) %>%
        arrange(treatment, pair_fe, outcome, model_type, year)
}

run_lag_year_models <- function(outcome_specs) {
    grid <- expand.grid(
        outcome_i = seq_len(nrow(outcome_specs)),
        treatment_i = seq_len(nrow(lag_treatments)),
        fe_i = seq_len(nrow(fe_specs)),
        year = years,
        KEEP.OUT.ATTRS = FALSE
    )

    bind_rows(lapply(seq_len(nrow(grid)), function(i) {
        g <- grid[i, ]
        run_year_model(outcome_specs[g$outcome_i, ], lag_treatments[g$treatment_i, ], fe_specs[g$fe_i, ], g$year)
    })) %>%
        mutate(
            treatment_label = factor(treatment_label, levels = lag_treatments$treatment_label),
            fe_label = factor(fe_label, levels = fe_specs$fe_label)
        ) %>%
        arrange(treatment, pair_fe, outcome, model_type, year)
}

# Expand the outcome x FE x year grid for the nonlinear tax-gap figure.
run_tax_gap_models <- function(outcome_specs) {
    grid <- expand.grid(
        outcome_i = seq_len(nrow(outcome_specs)),
        fe_i = seq_len(nrow(fe_specs)),
        year = years,
        KEEP.OUT.ATTRS = FALSE
    )

    bind_rows(lapply(seq_len(nrow(grid)), function(i) {
        g <- grid[i, ]
        run_tax_gap_model(outcome_specs[g$outcome_i, ], fe_specs[g$fe_i, ], g$year)
    })) %>%
        mutate(gap_bin = factor(gap_bin, levels = tax_gap_labels), fe_label = factor(fe_label, levels = fe_specs$fe_label)) %>%
        arrange(pair_fe, outcome, model_type, year, gap_bin)
}

run_quadratic_tax_gap_models <- function(outcome_specs) {
    grid <- expand.grid(
        outcome_i = seq_len(nrow(outcome_specs)),
        fe_i = seq_len(nrow(fe_specs)),
        year = years,
        KEEP.OUT.ATTRS = FALSE
    )

    bind_rows(lapply(seq_len(nrow(grid)), function(i) {
        g <- grid[i, ]
        run_quadratic_tax_gap_model(outcome_specs[g$outcome_i, ], fe_specs[g$fe_i, ], g$year)
    })) %>%
        mutate(gap_label = factor(gap_label, levels = levels(quadratic_gap_points$gap_label)), fe_label = factor(fe_label, levels = fe_specs$fe_label)) %>%
        arrange(pair_fe, outcome, model_type, year, gap_quantile)
}

run_lag_tax_gap_models <- function(outcome_specs) {
    grid <- expand.grid(
        outcome_i = seq_len(nrow(outcome_specs)),
        fe_i = seq_len(nrow(fe_specs)),
        year = years,
        KEEP.OUT.ATTRS = FALSE
    )

    bind_rows(lapply(seq_len(nrow(grid)), function(i) {
        g <- grid[i, ]
        run_lag_tax_gap_model(outcome_specs[g$outcome_i, ], fe_specs[g$fe_i, ], g$year)
    })) %>%
        mutate(gap_bin = factor(gap_bin, levels = tax_gap_labels), fe_label = factor(fe_label, levels = fe_specs$fe_label)) %>%
        arrange(pair_fe, outcome, model_type, year, gap_bin)
}

# Standard plot template for the year-by-year treatment estimates.
plot_year_models <- function(data, title, y_label) {
    ggplot(data, aes(x = year, y = estimate, color = outcome_label, group = outcome_label)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
        geom_line(linewidth = 0.75) +
        geom_point(size = 2.1) +
        facet_grid(treatment_label ~ fe_label, scales = "free_y") +
        scale_x_continuous(breaks = years) +
        labs(
            title = title,
            subtitle = paste0(
                "Rows: treatment variable; columns: pair fixed effects. Sales tax effects are for a 1 percentage point increase.",
                subtitle_suffix()
            ),
            x = "Year",
            y = y_label,
            color = NULL
        ) +
        theme_bw() +
        theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90", color = "grey60"))
}

plot_lag_year_models <- function(data, title, y_label) {
    ggplot(data, aes(x = year, y = estimate, color = outcome_label, group = outcome_label)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
        geom_line(linewidth = 0.75) +
        geom_point(size = 2.1) +
        facet_grid(treatment_label ~ fe_label, scales = "free_y") +
        scale_x_continuous(breaks = years) +
        labs(
            title = title,
            subtitle = paste0(
                "Treatment variables are lagged one year. Lagged sales tax effects are for a 1 percentage point increase.",
                subtitle_suffix()
            ),
            x = "Year",
            y = y_label,
            color = NULL
        ) +
        theme_bw() +
        theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90", color = "grey60"))
}

# Standard plot template for the low/high tax-gap estimates.
plot_tax_gap_models <- function(data, title, y_label) {
    ggplot(data, aes(x = year, y = estimate, color = gap_bin, group = gap_bin)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
        geom_line(linewidth = 0.75) +
        geom_point(size = 2.1) +
        facet_grid(outcome_label ~ fe_label, scales = "free_y") +
        scale_x_continuous(breaks = years) +
        labs(
            title = title,
            subtitle = paste0(
                "Tax gap bins use a full-sample median split from complete two-sided wedge pairs.",
                subtitle_suffix()
            ),
            x = "Year",
            y = y_label,
            color = NULL
        ) +
        theme_bw() +
        theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90", color = "grey60"))
}

plot_quadratic_tax_gap_models <- function(data, title, y_label) {
    ggplot(data, aes(x = year, y = estimate, color = gap_label, group = gap_label)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
        geom_line(linewidth = 0.75) +
        geom_point(size = 2.1) +
        facet_grid(outcome_label ~ fe_label, scales = "free_y") +
        scale_x_continuous(breaks = years) +
        labs(
            title = title,
            subtitle = paste0(
                "Model uses high-tax dummy interacted with absolute sales-tax gap and gap squared. Effects are shown at full-sample gap quantiles.",
                subtitle_suffix()
            ),
            x = "Year",
            y = y_label,
            color = "Tax gap"
        ) +
        theme_bw() +
        theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90", color = "grey60"))
}

plot_lag_tax_gap_models <- function(data, title, y_label) {
    ggplot(data, aes(x = year, y = estimate, color = gap_bin, group = gap_bin)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
        geom_line(linewidth = 0.75) +
        geom_point(size = 2.1) +
        facet_grid(outcome_label ~ fe_label, scales = "free_y") +
        scale_x_continuous(breaks = years) +
        labs(
            title = title,
            subtitle = paste0(
                "Tax-gap treatments are lagged one year. Bins use the prior year's low/high tax-gap split.",
                subtitle_suffix()
            ),
            x = "Year",
            y = y_label,
            color = NULL
        ) +
        theme_bw() +
        theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90", color = "grey60"))
}

# Run models, write the CSV, and save the matching figure.
write_year_output <- function(outcome_specs, stem, title, y_label) {
    models <- run_year_models(outcome_specs)
    write_csv(models, out_path("output/tables", stem, "csv"))
    save_plot(plot_year_models(models, title, y_label), stem)
    models
}

write_lag_year_output <- function(outcome_specs, stem, title, y_label) {
    models <- run_lag_year_models(outcome_specs)
    write_csv(models, out_path("output/tables", stem, "csv"))
    save_plot(plot_lag_year_models(models, title, y_label), stem)
    models
}

# Same output wrapper for the nonlinear tax-gap models.
write_tax_gap_output <- function(outcome_specs, stem, title, y_label) {
    models <- run_tax_gap_models(outcome_specs)
    write_csv(models, out_path("output/tables", stem, "csv"))
    save_plot(plot_tax_gap_models(models, title, y_label), stem)
    models
}

write_quadratic_tax_gap_output <- function(outcome_specs, stem, title, y_label) {
    models <- run_quadratic_tax_gap_models(outcome_specs)
    write_csv(models, out_path("output/tables", stem, "csv"))
    save_plot(plot_quadratic_tax_gap_models(models, title, y_label), stem)
    models
}

write_lag_tax_gap_output <- function(outcome_specs, stem, title, y_label) {
    models <- run_lag_tax_gap_models(outcome_specs)
    write_csv(models, out_path("output/tables", stem, "csv"))
    save_plot(plot_lag_tax_gap_models(models, title, y_label), stem)
    models
}

# Robustness output calls. The all-OLS yearly outputs were moved here from
# wedge-main-yearly.R. Quadratic and lagged-treatment outputs are also written
# separately for each establishment outcome.
wedge_robust_outputs <- lapply(seq_len(nrow(outcome_groups)), function(i) {
    outcome_group_spec <- outcome_groups[i, ]
    outcome_group <- outcome_group_spec$outcome_group
    outcome_label <- outcome_group_spec$outcome_group_label
    outcomes <- make_outcomes(outcome_group_spec)
    ols_outcomes <- make_ols_outcomes(outcome_group_spec)

    list(
        year_ols = write_year_output(
            ols_outcomes,
            paste0("wedge_year_by_year_2x2_ols_", outcome_group),
            paste0("Wedge Year-by-Year ", outcome_label, " Establishment Models (OLS)"),
            "OLS estimate"
        ),
        tax_gap_ols = write_tax_gap_output(
            ols_outcomes,
            paste0("wedge_tax_gap_nonlinear_by_year_ols_", outcome_group),
            paste0("Wedge ", outcome_label, " High-Tax Effects by Sales Tax Gap (OLS)"),
            "OLS estimate"
        ),
        quadratic = write_quadratic_tax_gap_output(
            outcomes,
            paste0("wedge_tax_gap_quadratic_by_year_", outcome_group),
            paste0("Wedge ", outcome_label, " High-Tax Effects by Quadratic Sales Tax Gap"),
            "Estimate"
        ),
        quadratic_ols = write_quadratic_tax_gap_output(
            ols_outcomes,
            paste0("wedge_tax_gap_quadratic_by_year_ols_", outcome_group),
            paste0("Wedge ", outcome_label, " High-Tax Effects by Quadratic Sales Tax Gap (OLS)"),
            "OLS estimate"
        ),
        lag_year = write_lag_year_output(
            outcomes,
            paste0("wedge_year_by_year_lag_2x2_", outcome_group),
            paste0("Wedge Year-by-Year ", outcome_label, " Lagged Treatment Models"),
            "Estimate"
        ),
        lag_tax_gap = write_lag_tax_gap_output(
            outcomes,
            paste0("wedge_tax_gap_lag_by_year_", outcome_group),
            paste0("Wedge ", outcome_label, " Lagged High-Tax Effects by Sales Tax Gap"),
            "Estimate"
        ),
        lag_year_ols = write_lag_year_output(
            ols_outcomes,
            paste0("wedge_year_by_year_lag_2x2_ols_", outcome_group),
            paste0("Wedge Year-by-Year ", outcome_label, " Lagged Treatment Models (OLS)"),
            "OLS estimate"
        ),
        lag_tax_gap_ols = write_lag_tax_gap_output(
            ols_outcomes,
            paste0("wedge_tax_gap_lag_by_year_ols_", outcome_group),
            paste0("Wedge ", outcome_label, " Lagged High-Tax Effects by Sales Tax Gap (OLS)"),
            "OLS estimate"
        )
    )
})
names(wedge_robust_outputs) <- outcome_groups$outcome_group
