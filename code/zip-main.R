source("code/setup.R")

# This script estimates year-by-year ZIP-pair models using the cleaned
# ZIP-year-pair panel created by the ZIP cleaning pipeline.
zip_pair_data_path <- "data/temp/zipcode_year_pair_2015_2022.csv"

message("Reading ZIP pair data: ", zip_pair_data_path)
zip_pair <- read.csv(zip_pair_data_path)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ZIP year-by-year 2x2 models ----
years <- sort(unique(zip_pair$year))

# Prepare numeric variables and compute establishment changes/growth within
# each ZIP-pair history. The lag is grouped by both zipcode and zip_pair_id so
# that a ZIP duplicated across multiple border pairs gets a separate series.
zip_model_data <- zip_pair %>%
    mutate(
        online_estab = as.numeric(online_estab),
        population = as.numeric(population),
        cit = as.numeric(cit),
        sales_tax = as.numeric(sales_tax),
        zipcode_market_potential = as.numeric(zipcode_market_potential),
        log_zipcode_market_potential = log(zipcode_market_potential)
    ) %>%
    arrange(zipcode, zip_pair_id, year) %>%
    group_by(zipcode, zip_pair_id) %>%
    mutate(
        online_estab_lag = lag(online_estab),
        d_online_estab = online_estab - online_estab_lag,
        g_online_estab = if_else(
            !is.na(online_estab_lag) & online_estab_lag != 0,
            d_online_estab / online_estab_lag,
            NA_real_
        )
    ) %>%
    ungroup()

# Outcomes: OLS uses establishment changes and growth rates, while PPML uses
# the establishment level and later reports the treatment effect as exp(beta)-1.
zip_outcomes <- tibble(
    outcome = c("d_online_estab", "g_online_estab", "online_estab"),
    outcome_label = c(
        "Change in online establishments",
        "Growth rate of online establishments",
        "Online establishments (PPML proportional effect)"
    ),
    model_type = c("OLS", "OLS", "PPML")
)

# Treatments for the 2x2 figure rows. The sales tax coefficient is rescaled to
# a 1 percentage point increase, because sales_tax is stored as a rate.
zip_treatments <- tibble(
    treatment = c("high_tax_dummy", "sales_tax"),
    treatment_label = c("High-tax dummy", "Sales tax rate (1 pp)"),
    treatment_effect_unit = c(1, 0.01)
)

# Fixed-effect specifications for the 2x2 figure columns.
zip_fe_specs <- tibble(
    pair_fe = c(FALSE, TRUE),
    fe_label = c("No pair FE", "Pair FE")
)

# Estimate one model for one outcome/treatment/FE/year combination and return
# the treatment estimate with a 95% confidence interval.
run_zip_year_model <- function(outcome_var,
                               outcome_label,
                               model_type,
                               treatment_var,
                               treatment_label,
                               treatment_effect_unit,
                               pair_fe,
                               fe_label,
                               yr) {
    # Unmatched ZIP rows have missing treatment variables, so they are kept in
    # the source panel but naturally exit the regression sample here.
    model_data <- zip_model_data %>%
        filter(year == yr) %>%
        filter(
            !is.na(.data[[outcome_var]]),
            !is.na(.data[[treatment_var]]),
            !is.na(population),
            !is.na(cit),
            !is.na(log_zipcode_market_potential),
            !is.na(state_abbr),
            !is.na(state_pair_id)
        )

    if (pair_fe) {
        model_data <- model_data %>% filter(!is.na(zip_pair_id))
    }

    if (nrow(model_data) == 0) return(NULL)

    # Pair fixed effects are added only in the FE specification. Standard errors
    # follow the wedge/county style: clustered by state and state pair.
    rhs <- paste(
        c(treatment_var, "population", "cit", "log_zipcode_market_potential"),
        collapse = " + "
    )
    model_formula <- as.formula(paste0(
        outcome_var, " ~ ", rhs,
        if (pair_fe) " | zip_pair_id" else ""
    ))

    fit <- tryCatch({
        if (model_type == "PPML") {
            fepois(model_formula, data = model_data, cluster = ~state_abbr + state_pair_id)
        } else {
            feols(model_formula, data = model_data, cluster = ~state_abbr + state_pair_id)
        }
    }, error = function(e) NULL)

    if (is.null(fit)) return(NULL)

    # Some annual specifications can drop the treatment because of collinearity
    # or lack of identifying variation; skip those cleanly.
    coef_names <- names(coef(fit))
    if (!treatment_var %in% coef_names) return(NULL)

    crit <- qnorm(0.975)

    if (model_type == "PPML") {
        # For PPML, report the proportional effect. For sales_tax this is the
        # effect of a 0.01 rate change, i.e. one percentage point.
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

# Run the full model grid: outcome x treatment x pair-FE specification x year.
zip_year_models <- bind_rows(lapply(seq_len(nrow(zip_outcomes)), function(i) {
    bind_rows(lapply(seq_len(nrow(zip_treatments)), function(j) {
        bind_rows(lapply(seq_len(nrow(zip_fe_specs)), function(k) {
            bind_rows(lapply(years, function(y) {
                run_zip_year_model(
                    outcome_var = zip_outcomes$outcome[i],
                    outcome_label = zip_outcomes$outcome_label[i],
                    model_type = zip_outcomes$model_type[i],
                    treatment_var = zip_treatments$treatment[j],
                    treatment_label = zip_treatments$treatment_label[j],
                    treatment_effect_unit = zip_treatments$treatment_effect_unit[j],
                    pair_fe = zip_fe_specs$pair_fe[k],
                    fe_label = zip_fe_specs$fe_label[k],
                    yr = y
                )
            }))
        }))
    }))
})) %>%
    mutate(
        treatment_label = factor(treatment_label, levels = zip_treatments$treatment_label),
        fe_label = factor(fe_label, levels = zip_fe_specs$fe_label)
    ) %>%
    arrange(treatment, pair_fe, outcome, model_type, year)

write_csv(
    zip_year_models,
    "output/tables/zip_year_by_year_2x2.csv"
)

# 2x2 plot: rows compare treatment definitions, columns compare the pair-FE
# specifications, and colors distinguish OLS changes from PPML levels.
zip_year_2x2_plot <- ggplot(
    zip_year_models,
    aes(x = year, y = estimate, color = outcome_label, group = outcome_label)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_r), width = 0.2, linewidth = 0.55) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.1) +
    facet_grid(treatment_label ~ fe_label, scales = "free_y") +
    scale_x_continuous(breaks = years) +
    labs(
        title = "ZIP Year-by-Year Online Establishment Models",
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

ggsave(
    "output/figures/zip_year_by_year_2x2.png",
    zip_year_2x2_plot,
    width = 11,
    height = 8,
    dpi = 150
)
print(zip_year_2x2_plot)
