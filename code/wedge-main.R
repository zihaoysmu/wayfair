source("code/setup.R")

wedge_data_suffix <- Sys.getenv("WEDGE_DATA_SUFFIX", "")
wedge_data_suffix_part <- if (nzchar(wedge_data_suffix)) paste0("_", wedge_data_suffix) else ""
wedge_data_path <- file.path(
    "data/temp",
    paste0("wedge_year_balanced_2015_2022", wedge_data_suffix_part, ".csv")
)

message("Reading wedge data: ", wedge_data_path)
wedge <- read.csv(wedge_data_path)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Wedge year-by-year 2x2 models ----
years <- sort(unique(wedge$year))

wedge_tax_lookup <- wedge %>%
    select(state_abbr, year, sales_tax) %>%
    distinct()

wedge_model_data <- wedge %>%
    left_join(
        wedge_tax_lookup %>%
            rename(
                neighbor_state = state_abbr,
                tax_other = sales_tax
            ),
        by = c("neighbor_state", "year")
    ) %>%
    mutate(
        online_estab = as.numeric(online_estab),
        population = as.numeric(population),
        cit = as.numeric(cit),
        sales_tax = as.numeric(sales_tax),
        tax_other = as.numeric(tax_other),
        wedge_market_potential = as.numeric(wedge_market_potential),
        high_tax_dummy = as.integer(sales_tax > tax_other),
        log_wedge_market_potential = log(wedge_market_potential)
    ) %>%
    arrange(wedge_id, year) %>%
    group_by(wedge_id) %>%
    mutate(d_online_estab = online_estab - lag(online_estab)) %>%
    ungroup()

wedge_outcomes <- tibble(
    outcome = c("d_online_estab", "online_estab"),
    outcome_label = c(
        "Change in online establishments",
        "Online establishments (PPML proportional effect)"
    ),
    model_type = c("OLS", "PPML")
)

wedge_treatments <- tibble(
    treatment = c("high_tax_dummy", "sales_tax"),
    treatment_label = c("High-tax dummy", "Sales tax rate (1 pp)"),
    treatment_effect_unit = c(1, 0.01)
)

wedge_fe_specs <- tibble(
    pair_fe = c(FALSE, TRUE),
    fe_label = c("No pair FE", "Pair FE")
)

run_wedge_year_model <- function(outcome_var,
                                 outcome_label,
                                 model_type,
                                 treatment_var,
                                 treatment_label,
                                 treatment_effect_unit,
                                 pair_fe,
                                 fe_label,
                                 yr) {
    model_data <- wedge_model_data %>%
        filter(year == yr) %>%
        filter(
            !is.na(.data[[outcome_var]]),
            !is.na(.data[[treatment_var]]),
            !is.na(population),
            !is.na(cit),
            !is.na(log_wedge_market_potential)
        )

    if (pair_fe) {
        model_data <- model_data %>% filter(!is.na(wedge_pair_id))
    }

    if (nrow(model_data) == 0) return(NULL)

    rhs <- paste(
        c(treatment_var, "population", "cit", "log_wedge_market_potential"),
        collapse = " + "
    )
    model_formula <- as.formula(paste0(
        outcome_var, " ~ ", rhs,
        if (pair_fe) " | wedge_pair_id" else ""
    ))

    fit <- tryCatch({
        if (model_type == "PPML") {
            fepois(model_formula, data = model_data, cluster = ~statefp + state_pair_id)
        } else {
            feols(model_formula, data = model_data, cluster = ~statefp + state_pair_id)
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

wedge_year_models <- bind_rows(lapply(seq_len(nrow(wedge_outcomes)), function(i) {
    bind_rows(lapply(seq_len(nrow(wedge_treatments)), function(j) {
        bind_rows(lapply(seq_len(nrow(wedge_fe_specs)), function(k) {
            bind_rows(lapply(years, function(y) {
                run_wedge_year_model(
                    outcome_var = wedge_outcomes$outcome[i],
                    outcome_label = wedge_outcomes$outcome_label[i],
                    model_type = wedge_outcomes$model_type[i],
                    treatment_var = wedge_treatments$treatment[j],
                    treatment_label = wedge_treatments$treatment_label[j],
                    treatment_effect_unit = wedge_treatments$treatment_effect_unit[j],
                    pair_fe = wedge_fe_specs$pair_fe[k],
                    fe_label = wedge_fe_specs$fe_label[k],
                    yr = y
                )
            }))
        }))
    }))
})) %>%
    mutate(
        treatment_label = factor(treatment_label, levels = wedge_treatments$treatment_label),
        fe_label = factor(fe_label, levels = wedge_fe_specs$fe_label)
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
            if (nzchar(wedge_data_suffix)) paste0(" Data: ", wedge_data_suffix, ".") else ""
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
print(wedge_year_2x2_plot)
