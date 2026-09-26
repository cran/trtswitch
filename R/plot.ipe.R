#' @title Plot method for ipe objects
#' @description Generate box plot for AFT model deviance residuals and 
#' Kaplan-Meier (KM) plot for potential outcomes of an ipe object.
#'
#' @param x An object of class \code{ipe}.
#' @param time_unit The time unit used in the input data.
#'   Options are "day" (default), "week", "month", or "year".
#' @param show_hr Logical; whether to show hazard ratio on the KM plot.
#'   Default is TRUE.
#' @param show_risk Logical; whether to show number at risk table
#'   below the KM plot. Default is TRUE.
#' @param ... Ensures that all arguments starting from "..." are named.
#' 
#' @return A list of ggplot2 objects: \code{p_res} for box plot for AFT model 
#' deviance residuals, \code{p_kmstar} for KM plot for counterfactual 
#' untreated survival times (i.e., potential outcomes if all subjects 
#' were untreated), and \code{p_km} for KM plot for counterfactual unswitched 
#' survival times.
#'
#' @keywords internal
#'
#' @author Kaifeng Lu, \email{kaifenglu@@gmail.com}
#'
#' @method plot ipe
#' @export
plot.ipe <- function(x, time_unit = "day", 
                     show_hr = TRUE, show_risk = TRUE, ...) {
  if (!inherits(x, "ipe")) {
    stop("x must be of class 'ipe'")
  }
  
  alpha <- x$settings$alpha
  conflev <- 100*(1 - alpha)
  treat_var <- x$settings$treat

  p_res <- NULL
  p_kmstar <- NULL
  p_km <- NULL

  if (!is.null(x$kmstar) && nrow(x$kmstar) > 0) {
    treatment_labels <- c("Treatment", "Control")
    if (!is.null(x$km_outcome) && nrow(x$km_outcome) > 0) {
      treatment_values <- x$km_outcome[[treat_var]]
      if (is.factor(treatment_values)) {
        treatment_labels <- levels(treatment_values)
      } else if (!(is.numeric(treatment_values) &&
                   all(treatment_values %in% c(0, 1)))) {
        treatment_labels <- levels(factor(treatment_values))
      }
    }

    df_star <- x$kmstar
    if (time_unit == "day") {
      df_star$month <- df_star$time / 30.4375
    } else if (time_unit == "week") {
      df_star$month <- df_star$time / 4.3482
    } else if (time_unit == "month") {
      df_star$month <- df_star$time
    } else if (time_unit == "year") {
      df_star$month <- df_star$time * 12
    } else {
      stop("time_unit must be one of 'day', 'week', 'month', or 'year'")
    }

    df_star$randomized_arm <- factor(
      df_star$treated, levels = c(1, 0), labels = treatment_labels
    )

    p_kmstar <- ggplot2::ggplot(
      df_star,
      ggplot2::aes(
        x = .data$month, y = .data$surv,
        group = .data$randomized_arm, colour = .data$randomized_arm
      )) +
      ggplot2::geom_step() +
      ggplot2::scale_x_continuous(n.breaks = 11) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(
        x = "Months", y = "Survival Probability",
        title = "Kaplan-Meier Curves for Counterfactual Untreated Outcomes"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.title = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(t = 2, r = 5, b = 0, l = 20)
      )

  }

  if (!is.null(x$data_outcome) && nrow(x$data_outcome) > 0) {
    # --- Deviance residuals plot for AFT models ---
    arm <- x$settings$data[[treat_var]]
    df1 <- data.frame(arm = x$data_aft[[treat_var]], res = x$res_aft)
    if (is.factor(arm)) {
      df1$arm <- factor(df1$arm, labels = levels(arm))
    } else if (is.numeric(arm) && all(arm %in% c(0, 1))) {
      df1$arm <- factor(df1$arm, levels = c(1, 0),
                        labels = c("Treatment", "Control"))
    } else {
      df1$arm <- factor(df1$arm)
    }

    p_res <- ggplot2::ggplot(df1, ggplot2::aes(x = .data$arm, y = .data$res)) +
      ggplot2::geom_boxplot(fill = "#77bd89", color = "#1f6e34", alpha = 0.6) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::labs(x = NULL, y = "Deviance Residuals") +
      ggplot2::theme_bw()

    # --- Kaplan-Meier plot for counterfactual unswitched outcomes ---
    df <- x$km_outcome
    if (time_unit == "day") {
      df$month <- df$time / 30.4375
    } else if (time_unit == "week") {
      df$month <- df$time / 4.3482
    } else if (time_unit == "month") {
      df$month <- df$time
    } else if (time_unit == "year") {
      df$month <- df$time * 12
    } else {
      stop("time_unit must be one of 'day', 'week', 'month', or 'year'")
    }

    if (!is.factor(df[[treat_var]])) {
      if (is.numeric(df[[treat_var]]) && all(df[[treat_var]] %in% c(0, 1))) {
        df[[treat_var]] <- factor(df[[treat_var]], levels = c(1, 0),
                                  labels = c("Treatment", "Control"))
      } else {
        df[[treat_var]] <- factor(df[[treat_var]])
      }
    }

    min_surv <- data.table::data.table(df)[, min(get("surv")), 
                                           by = treat_var][, get("V1")]

    p_km <- ggplot2::ggplot(
      df, ggplot2::aes(x = .data$month, y = .data$surv,
                       group = .data[[treat_var]], 
                       colour = .data[[treat_var]])) +
      ggplot2::geom_step() +
      ggplot2::scale_x_continuous(n.breaks = 11) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(
        x = "Months", y = "Survival Probability",
        title = "Kaplan-Meier Curves for Counterfactual Unswitched Outcomes") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.title = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(t = 2, r = 5, b = 0, l = 20))

    legend_position <- if (max(min_surv) < 0.5) {
      c(0.7, 0.85)
    } else {
      c(0.15, 0.25)
    }
    p_km <- p_km + ggplot2::theme(legend.position = legend_position)
    if (!is.null(p_kmstar)) {
      p_kmstar <- p_kmstar +
        ggplot2::theme(legend.position = legend_position)
    }

    if (show_hr) {
      if (max(min_surv) < 0.5) {
        p_km <- p_km +
          ggplot2::annotate(
            "text", x = 0.6 * max(df$month), y = 0.7, hjust = 0,
            label = sprintf("HR = %.3f (%.0f%% CI: %.3f, %.3f)",
                            x$hr, conflev, x$hr_CI[1], x$hr_CI[2]),
            size = 3.5, color = "black"
          )
      } else {
        p_km <- p_km +
          ggplot2::annotate(
            "text", x = 0, y = 0, hjust = 0,
            label = sprintf("HR = %.3f (%.0f%% CI: %.3f, %.3f)",
                            x$hr, conflev, x$hr_CI[1], x$hr_CI[2]),
            size = 3.5, color = "black"
          )
      }
    }

    if (show_risk) {
      xbreaks <- ggplot2::ggplot_build(p_km)$layout$panel_params[[1]]$x$breaks
      xbreaks <- xbreaks[!is.na(xbreaks)]
      limits <- c(min(xbreaks), max(max(xbreaks), max(df$month)))

      tablist <- lapply(0:1, function(h) {
        t <- df$month[df$treated == h]
        n <- df$nrisk[df$treated == h]

        idx <- pmax(findInterval(xbreaks, t), 1L)
        atrisk <- n[idx]
        atrisk[xbreaks > max(t)] <- 0

        df1 <- data.frame(time = xbreaks, atrisk = atrisk)
        df1[[treat_var]] <- df[[treat_var]][df$treated == h][1]
        df1
      })

      df_risk <- do.call(rbind, tablist)
      df_risk[[treat_var]] <- factor(df_risk[[treat_var]],
                                     levels = levels(df[[treat_var]]))

      p_risk <- ggplot2::ggplot(
        df_risk, ggplot2::aes(x = .data$time, y = .data[[treat_var]],
                              label = .data$atrisk, 
                              colour = .data[[treat_var]])) +
        ggplot2::geom_text(size = 3.2, na.rm = TRUE) +
        ggplot2::scale_x_continuous(breaks = xbreaks, limits = range(xbreaks)) +
        ggplot2::scale_y_discrete(limits = rev(levels(df_risk[[treat_var]]))) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = "none",
          panel.grid = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(t = 6, r = 5, b = 0, l = 20)) +
        ggplot2::annotate(
          "text", x = min(xbreaks), y = 3,
          label = "No. of Subjects at Risk", size = 4, hjust = 0.5)

      suppressMessages({
        p_km <- p_km +
          ggplot2::scale_x_continuous(
            breaks = xbreaks, limits = limits, expand = c(0.05, 0))

        p_risk <- p_risk +
          ggplot2::scale_x_continuous(
            breaks = xbreaks, limits = limits, expand = c(0.05, 0))
      })

      aligned <- cowplot::align_plots(p_km, p_risk, align = "v", axis = "lr")
      p_km <- cowplot::plot_grid(aligned[[1]], aligned[[2]], ncol = 1,
                                 rel_heights = c(4, 0.6))
    }
  }

  out <- list()
  if (!is.null(p_res)) out$p_res <- p_res
  if (!is.null(p_kmstar)) out$p_kmstar <- p_kmstar
  if (!is.null(p_km)) out$p_km <- p_km

  if (length(out) == 0) {
    stop("No outcome data available to plot.")
  }

  out
}
