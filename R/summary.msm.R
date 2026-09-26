#' @title Summary method for msm objects
#' @description Summarizes reporting items, switching-model estimates, weight
#' distribution, and final outcome-model estimates from a marginal structural
#' model fit.
#'
#' @param object An object of class \code{msm}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return An object of class \code{summary.msm} containing the analysis
#' population, reporting checklist, covariate summaries, switching-model
#' parameter estimates, weight distribution, and outcome-model estimates.
#'
#' @keywords internal
#'
#' @author Kaifeng Lu, \email{kaifenglu@@gmail.com}
#'
#' @export
summary.msm <- function(object, ...) {
  if (!inherits(object, "msm")) {
    stop("object must be of class 'msm'")
  }
  
  settings <- object$settings
  data <- settings$data
  treat_var <- settings$treat
  event_summary <- object$event_summary
  arm_labels <- as.character(event_summary[[treat_var]])
  population <- cbind(arm = arm_labels, event_summary[, setdiff(
    names(event_summary), c("treated", treat_var)), drop = FALSE])
  names(population)[1] <- treat_var
  
  format_covariates <- function(variables) {
    if (length(variables) == 0 || all(variables == "")) {
      "None"
    } else {
      paste(variables, collapse = ", ")
    }
  }
  denominator_vars <- settings$denominator[settings$denominator != ""]
  time_varying_vars <- detect_time_varying_predictors(
    denominator_vars, data, settings$id
  )
  time_varying_description <- if (length(denominator_vars) == 0) {
    "Not assessable (no denominator covariates)"
  } else if (length(time_varying_vars) > 0) {
    paste0("Detected: ", paste(time_varying_vars, collapse = ", "))
  } else {
    "None detected (all denominator covariates are time-fixed)"
  }
  covariate_summary <- NULL
  positivity_flags <- character(0)
  if (!is.null(data) && treat_var %in% names(data) &&
      settings$swtrt %in% names(data)) {
    variables <- unique(c(settings$denominator, settings$numerator,
                          settings$base_cov))
    variables <- variables[variables != "" & variables %in% names(data)]
    if (length(variables) > 0) {
      arm <- as.character(data[[treat_var]])
      switch_status <- ifelse(data[[settings$swtrt]] == 1,
                              "Switch", "No switch")
      group <- paste(arm, switch_status, sep = " - ")
      arms <- unique(arm)
      arms_with_variation <- arms[vapply(arms, function(arm_value) {
        length(unique(switch_status[arm == arm_value])) > 1
      }, logical(1))]
      groups <- sort(unique(group[arm %in% arms_with_variation]))
      no_switch_groups <- paste(arms_with_variation, "No switch", sep = " - ")
      switch_groups <- paste(arms_with_variation, "Switch", sep = " - ")
      rows <- list()
      lapply(variables, function(variable) {
        value <- data[[variable]]
        if (is.numeric(value)) {
          statistics <- vapply(groups, function(group_value) {
            values <- value[group == group_value]
            values <- values[!is.na(values)]
            if (length(values) == 0) "" else {
              sprintf("%.2f (%.2f)", mean(values), sd(values))
            }
          }, character(1))
          ranges <- lapply(groups, function(group_value) {
            values <- value[group == group_value]
            values <- values[!is.na(values)]
            if (length(values) == 0) c(NA_real_, NA_real_) else range(values)
          })
          names(ranges) <- groups
          range_statistics <- vapply(groups, function(group_value) {
            group_range <- ranges[[group_value]]
            if (anyNA(group_range)) "" else {
              sprintf("[%.2f, %.2f]", group_range[1], group_range[2])
            }
          }, character(1))
          for (arm_index in seq_along(arms_with_variation)) {
            rows[[length(rows) + 1]] <<- c(
              treatment = arms_with_variation[arm_index], variable = variable,
              level = "Mean (SD)", statistics[c(
                no_switch_groups[arm_index], switch_groups[arm_index]
              )]
            )
            rows[[length(rows) + 1]] <<- c(
              treatment = arms_with_variation[arm_index], variable = variable,
              level = "Range", range_statistics[c(
                no_switch_groups[arm_index], switch_groups[arm_index]
              )]
            )
            no_switch_range <- ranges[[no_switch_groups[arm_index]]]
            switch_range <- ranges[[switch_groups[arm_index]]]
            if (!anyNA(no_switch_range) && !anyNA(switch_range) &&
                max(no_switch_range[1], switch_range[1]) >
                min(no_switch_range[2], switch_range[2])) {
              positivity_flags <<- c(positivity_flags, paste0(
                variable, ": no overlap in range between switchers and ",
                "non-switchers within arm ", arms_with_variation[arm_index],
                " (potential positivity violation)"))
            }
          }
          NULL
        } else {
          values <- factor(value)
          for (level in levels(values)) {
            statistics <- vapply(groups, function(group_value) {
              group_values <- values[group == group_value]
              denominator <- sum(!is.na(group_values))
              numerator <- sum(group_values == level, na.rm = TRUE)
              if (denominator == 0) "" else {
                sprintf("%d (%.1f%%)", numerator, 100 * numerator / denominator)
              }
            }, character(1))
            for (arm_index in seq_along(arms_with_variation)) {
              rows[[length(rows) + 1]] <<- c(
                treatment = arms_with_variation[arm_index],
                variable = variable, level = level, statistics[c(
                  no_switch_groups[arm_index], switch_groups[arm_index]
                )]
              )
              no_switch_values <- values[group == no_switch_groups[arm_index]]
              switch_values <- values[group == switch_groups[arm_index]]
              no_switch_count <- sum(no_switch_values == level, na.rm = TRUE)
              switch_count <- sum(switch_values == level, na.rm = TRUE)
              if ((no_switch_count == 0) != (switch_count == 0)) {
                positivity_flags <<- c(positivity_flags, paste0(
                  variable, " = ", level, ": empty cell in ",
                  if (no_switch_count == 0) "non-switchers" else "switchers",
                  " within arm ", arms_with_variation[arm_index],
                  " (potential positivity violation)"))
              }
            }
          }
          NULL
        }
      })
      covariate_summary <- as.data.frame(do.call(rbind, rows),
                                         stringsAsFactors = FALSE)
      names(covariate_summary) <- c(
        treat_var, "variable", "statistic/level", "No switch", "Switch"
      )
      covariate_summary <- covariate_summary[order(
        covariate_summary[[treat_var]], covariate_summary$variable,
        covariate_summary[["statistic/level"]]
      ), , drop = FALSE]
      rownames(covariate_summary) <- NULL
    }
  }
  
  missing_predictors <- object$switch_missing_summary
  if (!is.null(missing_predictors)) {
    missing_predictors$treated <- NULL
    missing_predictors$missing_pct <- 100 * missing_predictors$missing /
      missing_predictors$total
    missing_predictors <- missing_predictors[, c(
      treat_var, "model", "predictor", "missing", "total", "missing_pct"
    )]
  }
  
  extract_model_estimates <- function(fit) {
    if (is.null(fit) || is.null(fit$parest)) {
      return(NULL)
    }
    parest <- fit$parest
    if (!all(c("beta", "expbeta", "sebeta") %in% names(parest))) {
      return(NULL)
    }
    parest[, c("param", "beta", "expbeta", "sebeta", "z", "p"),
           drop = FALSE]
  }
  extract_parest <- function(fit, arm_label, model_label) {
    estimates <- extract_model_estimates(fit)
    if (is.null(estimates)) {
      return(NULL)
    }
    names(estimates)[2:4] <- c("coef", "exp(coef)", "se(coef)")
    estimates <- data.frame(
      model = model_label, estimates,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    estimates <- cbind(arm_label, estimates, stringsAsFactors = FALSE)
    names(estimates)[1] <- treat_var
    estimates
  }
  switching_estimates <- list()
  number_of_arms <- if (isTRUE(settings$swtrt_control_only)) 1L else 2L
  for (arm_index in seq_len(number_of_arms)) {
    fit <- object$fit_switch[[arm_index]]
    switching_estimates <- c(switching_estimates, list(
      extract_parest(fit$fit_den, arm_labels[arm_index], "Denominator")
    ))
    if (isTRUE(settings$stabilized_weights)) {
      switching_estimates <- c(switching_estimates, list(
        extract_parest(fit$fit_num, arm_labels[arm_index], "Numerator")
      ))
    }
  }
  switching_estimates <- Filter(Negate(is.null), switching_estimates)
  switching_estimates <- if (length(switching_estimates) == 0) {
    NULL
  } else {
    do.call(rbind, switching_estimates)
  }
  fit_outcome <- object$fit_outcome
  if (!is.null(fit_outcome$parest) &&
      "sebeta_naive" %in% names(fit_outcome$parest)) {
    outcome_estimates <- fit_outcome$parest[, c(
      "param", "beta", "expbeta", "sebeta_naive", "sebeta", "z", "p"
    ), drop = FALSE]
    names(outcome_estimates)[2:5] <- c(
      "coef", "exp(coef)", "se(coef)", "robust se"
    )
  } else {
    outcome_estimates <- extract_model_estimates(fit_outcome)
    if (!is.null(outcome_estimates)) {
      names(outcome_estimates)[2:4] <- c("coef", "exp(coef)", "se(coef)")
    }
  }
  
  truncation <- if (isTRUE(settings$trunc > 0)) {
    paste0("Weights truncated at the ", 100 * settings$trunc, "/",
           if (isTRUE(settings$trunc_upper_only)) {
             "upper tail only"
           } else {
             paste0(100 * (1 - settings$trunc), " percentiles (both tails)")
           })
  } else {
    "No truncation applied"
  }
  bootstrap <- NULL
  if (isTRUE(settings$boot)) {
    failures <- as.logical(object$fail_boots)
    number_requested <- settings$n_boot
    number_failed <- sum(failures, na.rm = TRUE)
    bootstrap <- data.frame(
      requested = number_requested,
      successful = number_requested - number_failed,
      failed = number_failed,
      failure_pct = 100 * number_failed / number_requested,
      nonfinite_hr = sum(!is.finite(object$hr_boots))
    )
  }
  
  reporting <- data.frame(
    item = paste0("MSM", 1:10),
    information = c(
      paste0("The no-unmeasured-confounders assumption cannot be assessed ",
             "from the fitted object; justify it using clinical knowledge ",
             "and, where useful, a directed acyclic graph."),
      paste0("Review the table below to assess positivity."),
      paste0(if (isTRUE(settings$stabilized_weights)) { 
        "Stabilized" } else { "Unstabilized" },
        " weights were used."),
      paste0("Weights were estimated using pooled logistic regression; ",
             "Firth penalization: ",
             if (isTRUE(settings$firth)) "Yes" else "No",
             "; FLIC: ", if (isTRUE(settings$flic)) "Yes" else "No",
             "; spline df: ", settings$ns_df, "."),
      paste0("Switching-model data exclude person-time after treatment ",
             "switch; time-varying predictors: ",
             time_varying_description, "; ",
             "the final weighted outcome model includes post-switching data."),
      paste0("Switching-model predictor missingness is summarized below by ",
             "treatment arm and model; complete-case analysis was used."),
      paste0("Switching-model parameter estimates and measures of ",
             "precision are shown below."),
      paste0("Weight truncation: ", truncation, ". ",
             "Inspect p_w from plot(object) for weight distribution by ",
             "treatment group."),
      paste0("Final outcome model: weighted Cox PH with robust (sandwich) ",
             "variance clustered by subject id; it includes post-switching ",
             "data; ",
             "baseline covariates: ", format_covariates(settings$base_cov),
             "; stratification variables: ",
             format_covariates(settings$stratum),
             "; ties method: ", settings$ties, "."),
      paste0("Sensitivity to key assumptions cannot be determined from one ",
             "fitted object; compare treatment effects, survival ",
             "extrapolations, AIC/BIC, and minimum/maximum switch weights ",
             "across alternative spline degrees of freedom, functional ",
             "forms, ",
             if (isTRUE(settings$trunc > 0)) {
               paste0("categorical-variable definitions, and truncation ",
                      "percentiles including no truncation.")
             } else {
               "and categorical-variable definitions."
             })
    ),
    stringsAsFactors = FALSE
  )
  
  out <- list(
    call = object$call,
    population = population,
    covariate_summary = covariate_summary,
    positivity_flags = positivity_flags,
    missing_predictors = missing_predictors,
    switching_estimates = switching_estimates,
    weight_summary = object$weight_summary,
    truncation = truncation,
    outcome_estimates = outcome_estimates,
    conf_level = 100 * (1 - settings$alpha),
    hr = object$hr,
    hr_CI = object$hr_CI,
    hr_CI_type = object$hr_CI_type,
    pvalue = object$pvalue,
    pvalue_type = object$pvalue_type,
    bootstrap = bootstrap,
    reporting = reporting
  )
  class(out) <- "summary.msm"
  out
}

#' @title Print method for summary.msm objects
#' @description Prints a detailed summary of an MSM fit.
#'
#' @param x An object of class \code{summary.msm}.
#' @param digits The number of significant digits to print.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return The input object, invisibly.
#'
#' @keywords internal
#'
#' @export
print.summary.msm <- function(x, digits = max(3L, getOption("digits") - 3L),
                              ...) {
  print_key_values <- function(labels, values) {
    label_width <- max(nchar(labels))
    labels <- format(labels, width = label_width, justify = "left")
    cat(paste0(labels, ": ", values, collapse = "\n"), "\n", sep = "")
  }
  format_parameter_estimates <- function(estimates) {
    value_columns <- if ("coef" %in% names(estimates)) {
      c("coef", "exp(coef)", "se(coef)")
    } else {
      c("beta", "expbeta", "sebeta")
    }
    if ("robust se" %in% names(estimates)) {
      value_columns <- c(value_columns, "robust se")
    }
    estimates[value_columns] <- lapply(estimates[value_columns], formatC,
                                       format = "f", digits = 4)
    if ("z" %in% names(estimates)) {
      estimates$z <- formatC(estimates$z, format = "f", digits = 3)
    }
    if ("p" %in% names(estimates)) {
      estimates$p <- ifelse(
        is.na(estimates$p), NA_character_,
        ifelse(estimates$p < 1e-4, "<.0001",
               ifelse(estimates$p > 0.9999, ">.9999",
                      formatC(estimates$p, format = "f", digits = 4)))
      )
    }
    estimates
  }
  
  cat("Marginal Structural Model (MSM)\n\n")
  if (!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl)
    cat("\n")
  }
  cat("Analysis population\n")
  population <- x$population
  pct_columns <- grep("_pct$", names(population), value = TRUE)
  population[pct_columns] <- lapply(
    population[pct_columns], formatC, format = "f", digits = 1)
  print(population, row.names = FALSE, digits = digits, ...)
  
  cat("\nMSM reporting checklist\n")
  for (index in seq_len(nrow(x$reporting))) {
    print_key_values(x$reporting$item[index], x$reporting$information[index])
    if (index == 2L && !is.null(x$covariate_summary)) {
      cat("\nCovariate summary by treatment arm and switch status\n")
      print(x$covariate_summary, row.names = FALSE, ...)
      cat("\nPositivity diagnostics\n")
      if (length(x$positivity_flags) > 0) {
        cat(paste0("* ", x$positivity_flags, collapse = "\n"), "\n", sep = "")
      } else {
        cat("No non-overlapping ranges or empty cells detected.\n")
      }
    } else if (index == 6L && !is.null(x$missing_predictors)) {
      cat("\nMissing switching-model predictors by model\n")
      missing_predictors <- x$missing_predictors
      missing_predictors$missing_pct <- formatC(
        missing_predictors$missing_pct, format = "f", digits = 1
      )
      print(missing_predictors, row.names = FALSE, ...)
    } else if (index == 7L && !is.null(x$switching_estimates)) {
      cat("\nSwitching model parameter estimates\n")
      switching_estimates <- format_parameter_estimates(x$switching_estimates)
      print(switching_estimates, row.names = FALSE, ...)
    } else if (index == 8L) {
      cat("\nWeight distribution\n")
      print(x$weight_summary, row.names = FALSE, digits = digits, ...)
    } else if (index == 9L) {
      cat("\nOutcome model parameter estimates\n")
      if (!is.null(x$outcome_estimates)) {
        outcome_estimates <- format_parameter_estimates(x$outcome_estimates)
        print(outcome_estimates, row.names = FALSE, ...)
      }
      cat("\nTreatment effect estimate (", format(x$conf_level, trim = TRUE),
          "% confidence interval)\n", sep = "")
      cat("Hazard ratio:", formatC(x$hr, format = "f", digits = 3),
          " (", formatC(x$hr_CI[1], format = "f", digits = 3), ", ",
          formatC(x$hr_CI[2], format = "f", digits = 3), "), CI type: ",
          x$hr_CI_type, "\n", sep = "")
      pvalue <- ifelse(
        is.na(x$pvalue), NA_character_,
        ifelse(x$pvalue < 1e-4, "<.0001",
               ifelse(x$pvalue > 0.9999, ">.9999",
                      formatC(x$pvalue, format = "f", digits = 4)))
      )
      cat("P-value (", x$pvalue_type, "): ", pvalue, "\n", sep = "")
      if (!is.null(x$bootstrap)) {
        cat("\nBootstrap performance\n")
        print(x$bootstrap, row.names = FALSE, digits = digits, ...)
      }
    }
    if (index < nrow(x$reporting)) {
      cat("\n")
    }
  }
  invisible(x)
}