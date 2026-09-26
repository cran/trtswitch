#' @title Summary method for ipcw objects
#' @description Summarizes reporting items, switching-model estimates, weight
#' distribution, and final outcome-model estimates from an inverse probability
#' of censoring weights (IPCW) fit.
#'
#' @param object An object of class \code{ipcw}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return An object of class \code{summary.ipcw} containing the analysis
#' population, reporting checklist, covariate balance and positivity
#' diagnostics, switching-model parameter estimates, weight distribution, and
#' final outcome-model estimates.
#'
#' @keywords internal
#'
#' @author Kaifeng Lu, \email{kaifenglu@@gmail.com}
#'
#' @export
summary.ipcw <- function(object, ...) {
  if (!inherits(object, "ipcw")) {
    stop("object must be of class 'ipcw'")
  }
  
  settings <- object$settings
  event_summary <- object$event_summary
  treat_var <- settings$treat
  data <- settings$data
  
  # --- analysis population ---
  arm_labels <- as.character(event_summary[[treat_var]])
  population <- cbind(arm = arm_labels, event_summary[, setdiff(
    names(event_summary), c("treated", treat_var)), drop = FALSE])
  names(population)[1] <- treat_var
  
  format_covariates <- function(x) {
    if (length(x) == 0 || all(x == "")) "None" else paste(x, collapse = ", ")
  }
  
  # --- IPCW2: covariate balance and positivity diagnostics by treatment
  # arm and switch status ---
  covariate_balance <- NULL
  positivity_flags <- character(0)
  if (!is.null(data) && !is.null(treat_var) && !is.null(settings$swtrt) &&
      treat_var %in% names(data) && settings$swtrt %in% names(data)) {
    vars <- unique(c(settings$denominator, settings$numerator,
                     settings$base_cov))
    vars <- vars[vars != "" & vars %in% names(data)]
    if (length(vars) > 0) {
      arm <- as.character(data[[treat_var]])
      swstat <- ifelse(data[[settings$swtrt]] == 1, "Switch", "No switch")
      group <- paste(arm, swstat, sep = " - ")
      
      # arms with both switchers and non-switchers, where positivity of the
      # switch status is assessable
      arms <- unique(arm)
      arms_with_variation <- arms[vapply(arms, function(a) {
        length(unique(swstat[arm == a])) > 1
      }, logical(1))]
      groups <- sort(unique(group[arm %in% arms_with_variation]))
      no_switch_grp <- paste(arms_with_variation, "No switch", sep = " - ")
      switch_grp <- paste(arms_with_variation, "Switch", sep = " - ")
      
      rows <- list()
      for (v in vars) {
        x <- data[[v]]
        if (is.numeric(x)) {
          group_range <- lapply(groups, function(g) {
            xi <- x[group == g]
            xi <- xi[!is.na(xi)]
            if (length(xi) == 0) c(NA_real_, NA_real_) else range(xi)
          })
          names(group_range) <- groups
          
          stats <- vapply(groups, function(g) {
            xi <- x[group == g]
            xi <- xi[!is.na(xi)]
            if (length(xi) == 0) "" else
              sprintf("%.2f (%.2f)", mean(xi), sd(xi))
          }, character(1))
          for (i in seq_along(arms_with_variation)) {
            rows[[length(rows) + 1]] <- c(
              treatment = arms_with_variation[i], variable = v,
              level = "Mean (SD)", stats[c(no_switch_grp[i], switch_grp[i])]
            )
          }
          
          range_str <- vapply(groups, function(g) {
            r <- group_range[[g]]
            if (anyNA(r)) "" else sprintf("[%.2f, %.2f]", r[1], r[2])
          }, character(1))
          for (i in seq_along(arms_with_variation)) {
            rows[[length(rows) + 1]] <- c(
              treatment = arms_with_variation[i], variable = v,
              level = "Range", range_str[c(no_switch_grp[i], switch_grp[i])]
            )
          }
          
          for (i in seq_along(arms_with_variation)) {
            r_no <- group_range[[no_switch_grp[i]]]
            r_sw <- group_range[[switch_grp[i]]]
            if (!anyNA(r_no) && !anyNA(r_sw)) {
              overlap <- max(r_no[1], r_sw[1]) <= min(r_no[2], r_sw[2])
              if (!overlap) {
                positivity_flags <- c(positivity_flags, sprintf(
                  paste0("%s: no overlap in range between switchers and ",
                         "non-switchers within arm %s (potential ",
                         "positivity violation)"),
                  v, arms_with_variation[i]))
              }
            }
          }
        } else {
          xf <- factor(x)
          for (lev in levels(xf)) {
            stats <- vapply(groups, function(g) {
              xi <- xf[group == g]
              n_g <- sum(!is.na(xi))
              n_lev <- sum(xi == lev, na.rm = TRUE)
              if (n_g == 0) "" else
                sprintf("%d (%.1f%%)", n_lev, 100 * n_lev / n_g)
            }, character(1))
            
            for (i in seq_along(arms_with_variation)) {
              rows[[length(rows) + 1]] <- c(
                treatment = arms_with_variation[i], variable = v,
                level = lev, stats[c(no_switch_grp[i], switch_grp[i])]
              )
              n_no <- sum(xf[group == no_switch_grp[i]] == lev, na.rm = TRUE)
              n_no_total <- sum(!is.na(xf[group == no_switch_grp[i]]))
              n_sw <- sum(xf[group == switch_grp[i]] == lev, na.rm = TRUE)
              n_sw_total <- sum(!is.na(xf[group == switch_grp[i]]))
              if (n_no_total > 0 && n_sw_total > 0 &&
                  ((n_no == 0) != (n_sw == 0))) {
                positivity_flags <- c(positivity_flags, paste0(
                  v, " = ", lev, ": empty cell in ",
                  if (n_no == 0) "non-switchers" else "switchers",
                  " within arm ", arms_with_variation[i],
                  " (potential positivity violation)"))
              }
            }
          }
        }
      }
      covariate_balance <- as.data.frame(
        do.call(rbind, rows), stringsAsFactors = FALSE)
      names(covariate_balance) <- c(
        treat_var, "variable", "statistic/level", "No switch", "Switch"
      )
      covariate_balance <- covariate_balance[order(
        covariate_balance[[treat_var]], covariate_balance$variable,
        covariate_balance[["statistic/level"]]
      ), , drop = FALSE]
      rownames(covariate_balance) <- NULL
    }
  }
  
  # --- IPCW5: portion of data and time-varying predictors in switch model ---
  denom_vars <- settings$denominator[settings$denominator != ""]
  time_varying_vars <- detect_time_varying_predictors(
    denom_vars, data, settings$id
  )
  
  missing_predictors <- object$switch_missing_summary
  if (!is.null(missing_predictors)) {
    missing_predictors$treated <- NULL
    missing_predictors$missing_pct <- 100 * missing_predictors$missing /
      missing_predictors$total
    missing_predictors <- missing_predictors[, c(
      treat_var, "model", "predictor", "missing", "total", "missing_pct"
    )]
  }
  
  # --- IPCW7: switch model parameter estimates ---
  extract_model_estimates <- function(fit) {
    if (is.null(fit) || is.null(fit$parest) || nrow(fit$parest) == 0) {
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
  
  switch_estimates <- NULL
  fit_switch <- object$fit_switch
  if (!is.null(fit_switch) && length(fit_switch) > 0) {
    K <- if (isTRUE(settings$swtrt_control_only)) 1 else 2
    rows <- list()
    for (h in seq_len(K)) {
      fh <- fit_switch[[h]]
      rows[[length(rows) + 1]] <- extract_parest(
        fh$fit_den, arm_labels[h], "Denominator")
      if (isTRUE(settings$stabilized_weights)) {
        rows[[length(rows) + 1]] <- extract_parest(
          fh$fit_num, arm_labels[h], "Numerator")
      }
    }
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) > 0) switch_estimates <- do.call(rbind, rows)
  }
  
  # --- IPCW8: weight distribution and truncation ---
  weight_summary <- object$weight_summary
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
  
  # --- IPCW9: final outcome model ---
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
  
  conf_level <- 100 * (1 - settings$alpha)
  
  bootstrap <- NULL
  if (isTRUE(settings$boot)) {
    failures <- as.logical(object$fail_boots)
    n_requested <- settings$n_boot
    n_failed <- sum(failures, na.rm = TRUE)
    bootstrap <- data.frame(
      requested = n_requested,
      successful = n_requested - n_failed,
      failed = n_failed,
      failure_pct = 100 * n_failed / n_requested,
      nonfinite_hr = sum(!is.finite(object$hr_boots))
    )
  }
  
  switching_model <- if (isTRUE(settings$logistic_switching_model)) {
    "Pooled logistic regression"
  } else {
    "Cox model with time-dependent covariates"
  }
  time_varying_description <- if (length(denom_vars) == 0) {
    "Not assessable (no denominator covariates)"
  } else if (length(time_varying_vars) > 0) {
    paste0("Detected: ", paste(time_varying_vars, collapse = ", "))
  } else {
    "None detected (all denominator covariates are time-fixed)"
  }
  ipcw4 <- paste0(
    "Weights were estimated using ", tolower(switching_model),
    if (isTRUE(settings$logistic_switching_model)) {
      paste0("; Firth penalization: ",
             if (isTRUE(settings$firth)) "Yes" else "No",
             "; FLIC: ", if (isTRUE(settings$flic)) "Yes" else "No",
             "; spline df: ", settings$ns_df, ".")
    } else {
      "."
    }
  )
  reporting <- data.frame(
    item = paste0("IPCW", 1:10),
    information = c(
      paste0("The no-unmeasured-confounders assumption cannot be assessed ",
             "from the fitted object; justify it using clinical knowledge ",
             "and, where useful, a directed acyclic graph."),
      if (length(positivity_flags) > 0) {
        paste0("The covariate summary identifies possible positivity ",
               "violations; review the table and listed diagnostics below.")
      } else {
        paste0("Review the table below to assess positivity.")
      },
      paste0(if (isTRUE(settings$stabilized_weights)) {
        "Stabilized"
      } else {
        "Unstabilized"
      },
      " weights were used."),
      ipcw4,
      paste0("Switching-model data exclude person-time after treatment ",
             "switch; intervals run from ", settings$tstart, " to ", 
             settings$tstop,
             " up to switch, death, or censoring; time-varying predictors: ",
             time_varying_description, "."),
      paste0("Switching-model predictor missingness is summarized below by ",
             "treatment arm and model; complete-case analysis was used.",
             if (grepl("cox model with time-dependent covariates", ipcw4,
                       fixed = TRUE)) {
               paste0(" This summary is based on records before intervals ",
                      "are split at distinct event times.")
             } else {
               ""
             }),
      paste0("Switching-model parameter estimates and measures of precision ",
             "are shown below."),
      paste0("Weight truncation: ", truncation, ". ",
             "Inspect p_w from plot(object) for weight distribution by ",
             "treatment group."),
      paste0("Final outcome model: weighted Cox PH with robust (sandwich) ",
             "variance clustered by subject id; baseline covariates: ",
             format_covariates(settings$base_cov),
             "; stratification variables: ",
             format_covariates(settings$stratum), "; ties method: ",
             settings$ties, "."),
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
    covariate_balance = covariate_balance,
    positivity_flags = positivity_flags,
    missing_predictors = missing_predictors,
    switch_estimates = switch_estimates,
    weight_summary = weight_summary,
    truncation = truncation,
    outcome_estimates = outcome_estimates,
    conf_level = conf_level,
    hr = object$hr,
    hr_CI = object$hr_CI,
    hr_CI_type = object$hr_CI_type,
    pvalue = object$pvalue,
    pvalue_type = object$pvalue_type,
    bootstrap = bootstrap,
    reporting = reporting
  )
  class(out) <- "summary.ipcw"
  out
}

#' @title Print method for summary.ipcw objects
#' @description Prints a detailed summary of an IPCW fit organized to
#' align with the reporting recommendations of NICE DSU TSD 24.
#'
#' @param x An object of class \code{summary.ipcw}.
#' @param digits The number of significant digits to print.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return The input object, invisibly.
#'
#' @keywords internal
#'
#' @export
print.summary.ipcw <- function(
    x, digits = max(3L, getOption("digits") - 3L), ...) {
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
  
  cat("Inverse Probability of Censoring Weights (IPCW)\n\n")
  
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
  
  cat("\nIPCW reporting checklist\n")
  for (index in seq_len(nrow(x$reporting))) {
    print_key_values(x$reporting$item[index], x$reporting$information[index])
    
    if (index == 2L && !is.null(x$covariate_balance)) {
      cat("\nCovariate summary by treatment arm and switch status\n")
      print(x$covariate_balance, row.names = FALSE, ...)
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
    } else if (index == 7L && !is.null(x$switch_estimates)) {
      cat("\nSwitching model parameter estimates\n")
      switch_estimates <- format_parameter_estimates(x$switch_estimates)
      print(switch_estimates, row.names = FALSE, ...)
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
