    # ---- Plot fitted 5th-degree curves by side, by year ----
    # Extract coef names programmatically (fixest may double-wrap I() in interactions)
    all_names <- names(coef(ols_by_year[[1]]))
    poly_low_names <- grep("dist_to_border_edge", all_names, value = TRUE)
    poly_low_names <- poly_low_names[!grepl("^high_tax_dummy:", poly_low_names)]
    poly_int_names <- grep("^high_tax_dummy:.*dist_to_border_edge",
                           all_names, value = TRUE)
    dummy_name     <- "high_tax_dummy"
    stopifnot(length(poly_low_names) == 5, length(poly_int_names) == 5)

    all_d <- main %>%
        filter(type == "online") %>%
        pull(dist_to_border_edge)
    d_max <- quantile(all_d, 0.75, na.rm = TRUE)
    d_grid <- seq(0, d_max, length.out = 200)
    X_poly <- sapply(1:5, function(k) d_grid^k)  # n_grid x 5

    curve_df <- do.call(rbind, lapply(years, function(y) {
        m <- ols_by_year[[as.character(y)]]
        b <- coef(m); V <- vcov(m)

        b_low  <- b[poly_low_names]
        b_int  <- b[poly_int_names]
        b_high <- b_low + b_int

        y_low  <- as.numeric(X_poly %*% b_low)
        y_high <- as.numeric(b[dummy_name] + X_poly %*% b_high)

        V_low <- V[poly_low_names, poly_low_names]
        se_low <- sqrt(diag(X_poly %*% V_low %*% t(X_poly)))

        cmb_names <- c(dummy_name, poly_low_names, poly_int_names)
        L_high <- cbind(1, X_poly, X_poly)
        V_high <- V[cmb_names, cmb_names]
        se_high <- sqrt(diag(L_high %*% V_high %*% t(L_high)))

        rbind(
            data.frame(year = y, side = "Low-tax",  x = -d_grid, y = y_low,  se = se_low),
            data.frame(year = y, side = "High-tax", x =  d_grid, y = y_high, se = se_high)
        )
    }))
    curve_df$ci_lo <- curve_df$y - 1.96 * curve_df$se
    curve_df$ci_hi <- curve_df$y + 1.96 * curve_df$se

    p_curve <- ggplot(curve_df, aes(x = x, y = y, color = side, fill = side)) +
        geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2, color = NA) +
        geom_line(linewidth = 0.8) +
        geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
        geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
        facet_wrap(~ year, ncol = 4) +
        coord_cartesian(ylim = c(-1.5, 1.5)) +
        labs(
            x = "Signed distance to border (km): low-tax side (-), high-tax side (+)",
            y = "Predicted partial effect on lemp",
            title = "Year-by-year 5th-degree polynomial fit, by tax side"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")

    ggsave("output/ols_curve_by_year.png", p_curve,
           width = 14, height = 7, dpi = 150)