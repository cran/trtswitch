#' @title Summary method for tsegest objects
#' @description Summarizes reporting items and treatment effect estimates from
#' a two-stage g-estimation fit.
#'
#' @param object An object of class \code{tsegest}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return An object of class \code{summary.tsegest} containing the analysis
#' population, reporting checklist, parameter estimates, treatment effect
#' estimates, and bootstrap summary.
#'
#' @keywords internal
#'
#' @author Kaifeng Lu, \email{kaifenglu@@gmail.com}
#'
#' @export
summary.tsegest <- function(object, ...) {
  if (!inherits(object, "tsegest")) {
    stop("object must be of class 'tsegest'")
  }
  
  settings <- object$settings
  event_summary <- object$event_summary
  treat_var <- settings$treat
  
  population <- event_summary
  arm <- if (treat_var %in% names(population)) {
    as.character(population[[treat_var]])
  } else if ("treated" %in% names(population)) {
    ifelse(population$treated == 0, "Control", "Treatment")
  } else {
    as.character(seq_len(nrow(population)))
  }
  population <- cbind(arm = arm, population[, setdiff(
    names(population), c("treated", treat_var)), drop = FALSE])
  names(population)[1] <- treat_var
  
  format_covariates <- function(x) {
    if (length(x) == 0 || all(x == "")) "None" else paste(x, collapse = ", ")
  }
  search_method <- if (isTRUE(settings$gridsearch)) {
    "Grid search with linear interpolation"
  } else {
    paste("Root finding (", settings$root_finding, ")", sep = "")
  }
  switching_covariates <- c(
    "counterfactual",
    if (length(settings$stratum) == 0 || all(settings$stratum == "")) {
      NULL
    } else {
      paste0("stratum: ", paste(settings$stratum, collapse = ", "))
    },
    if (length(settings$conf_cov) == 0 || all(settings$conf_cov == "")) {
      NULL
    } else {
      paste0("conf_cov: ", paste(settings$conf_cov, collapse = ", "))
    },
    if (settings$ns_df > 0) {
      paste0("natural spline terms: ns1-ns", settings$ns_df)
    }
  )
  confounders <- settings$conf_cov[settings$conf_cov != ""]
  time_varying_confounders <- detect_time_varying_predictors(
    confounders, settings$data, settings$id
  )
  time_varying_description <- if (length(confounders) == 0) {
    "Not assessable (no conf_cov predictors)"
  } else if (length(time_varying_confounders) > 0) {
    paste0("Detected: ", paste(time_varying_confounders, collapse = ", "))
  } else {
    "None detected (all conf_cov predictors are time-fixed)"
  }
  
  estimation <- data.frame(
    item = c(
      "Search method", "Search interval", "Number of Z evaluations",
      "Root-finding tolerance", "Control-arm zero-crossings",
      "Treatment-arm zero-crossings", "Selected control-arm psi",
      "Selected treatment-arm psi"
    ),
    value = c(
      search_method,
      paste0("[", settings$low_psi, ", ", settings$hi_psi, "]"),
      as.character(settings$n_eval),
      as.character(settings$tol),
      as.character(sum(is.finite(object$psi_roots))),
      if (isTRUE(settings$swtrt_control_only)) "Not applicable" else
        as.character(sum(is.finite(object$psi_trt_roots))),
      formatC(object$psi, format = "f", digits = 3),
      if (isTRUE(settings$swtrt_control_only)) "Not applicable" else
        formatC(object$psi_trt, format = "f", digits = 3)
    ),
    stringsAsFactors = FALSE
  )
  
  conf_level <- 100 * (1 - settings$alpha)
  estimates <- data.frame(
    estimand = c(
      "Causal parameter psi for control arm",
      "Causal survival time ratio for control arm",
      if (!isTRUE(settings$swtrt_control_only)) {
        c("Causal parameter psi for treatment arm",
          "Causal survival time ratio for treatment arm")
      }
    ),
    estimate = c(
      object$psi, exp(-object$psi),
      if (!isTRUE(settings$swtrt_control_only)) {
        c(object$psi_trt, exp(-object$psi_trt))
      }
    ),
    lower = c(
      object$psi_CI[1], exp(-object$psi_CI[2]),
      if (!isTRUE(settings$swtrt_control_only)) {
        c(object$psi_trt_CI[1], exp(-object$psi_trt_CI[2]))
      }
    ),
    upper = c(
      object$psi_CI[2], exp(-object$psi_CI[1]),
      if (!isTRUE(settings$swtrt_control_only)) {
        c(object$psi_trt_CI[2], exp(-object$psi_trt_CI[1]))
      }
    ),
    ci_method = c(
      object$psi_CI_type, object$psi_CI_type,
      if (!isTRUE(settings$swtrt_control_only)) {
        c(object$psi_CI_type, object$psi_CI_type)
      }
    ),
    stringsAsFactors = FALSE
  )
  
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
  
  treatment_labels <- if (is.factor(settings$data[[treat_var]])) {
    levels(settings$data[[treat_var]])
  } else {
    NULL
  }
  switching_estimates <- lapply(object$fit_logis, function(fit) {
    estimates <- extract_model_estimates(fit$fit)
    if (is.null(estimates)) {
      return(NULL)
    }
    treatment_value <- fit[[treat_var]]
    if (!is.null(treatment_labels) && is.numeric(treatment_value) &&
        treatment_value %in% seq_along(treatment_labels)) {
      treatment_value <- treatment_labels[as.integer(treatment_value)]
    }
    estimates <- cbind(as.character(treatment_value), estimates,
                       stringsAsFactors = FALSE)
    names(estimates)[1] <- treat_var
    names(estimates)[3:5] <- c("coef", "exp(coef)", "se(coef)")
    estimates
  })
  switching_estimates <- Filter(Negate(is.null), switching_estimates)
  switching_estimates <- if (length(switching_estimates) == 0) {
    NULL
  } else {
    do.call(rbind, switching_estimates)
  }
  missing_switch_predictors <- object$switch_missing_summary
  if (!is.null(missing_switch_predictors)) {
    missing_switch_predictors$treated <- NULL
    missing_switch_predictors$missing_pct <- 100 *
      missing_switch_predictors$missing / missing_switch_predictors$total
    missing_switch_predictors <- missing_switch_predictors[, c(
      treat_var, "predictor", "missing", "total", "missing_pct"
    )]
  }
  outcome_estimates <- extract_model_estimates(object$fit_outcome)
  if (!is.null(outcome_estimates)) {
    names(outcome_estimates)[2:4] <- c("coef", "exp(coef)", "se(coef)")
  }
  
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
      nonfinite_psi = sum(!is.finite(object$psi_boots)),
      nonfinite_hr = sum(!is.finite(object$hr_boots))
    )
    if (!isTRUE(settings$swtrt_control_only)) {
      bootstrap$nonfinite_psi_trt <- sum(!is.finite(object$psi_trt_boots))
    }
  }
  
  reporting <- data.frame(
    item = paste0("TSEg", 1:14),
    information = c(
      paste0("Disease-related secondary baseline: ", settings$pd,
             " at ", settings$pd_time, "."),
      paste0("The no-unmeasured-confounding assumption cannot be assessed ",
             "from the fitted object; justify it using clinical knowledge ", 
             "and, where useful, a directed acyclic graph."),
      paste0("Inspect p_km_switch from plot(object) for time from the ",
             "secondary baseline to switching among all patients with a ",
             "secondary baseline, regardless of switching status; switching ",
             "is the event and patients who do not switch are censored."),
      paste0("Switching model: pooled logistic regression with ",
             "counterfactual survival; counterfactual survival model: Cox PH ",
             "model with no covariates; switching covariates: ",
             paste(switching_covariates, collapse = "; "), "."),
      paste0("The switching model uses post-progression intervals ", 
             "through switching, death, or censoring; time-varying ", 
             "predictors: ", time_varying_description, "."),
      paste0("Switching-model conf_cov missingness is summarized below by ",
             "treatment arm among pre-filter eligible records; complete-case ",
             "analysis was used."),
      paste0("G-test: Wald test from the pooled logistic switching model ", 
             "with sandwich variance clustered by subject."),
      paste0("Potential survival is entered as counterfactual ", 
             "post-progression survival through martingale residuals from ", 
             "the no-covariate Cox model."),
      paste0("G-estimation used ", search_method,
             if (!isTRUE(settings$gridsearch)) {
               paste0(" with root-finding method ", settings$root_finding)
             } else {
               ""
             }, " over [",
             settings$low_psi, ", ", settings$hi_psi, "]."),
      paste0("G-estimation diagnostics: ",
             "Inspect p_z from plot(object) to assess whether the ", 
             "g-estimation process has worked well."),
      paste0("Causal parameter estimates, including causal survival time ", 
             "ratios and confidence intervals, are shown below."),
      paste0("Outcome model: Cox PH for counterfactual unswitched survival; ",
             "baseline covariates: ", format_covariates(settings$base_cov),
             "; stratification variables: ", 
             format_covariates(settings$stratum), "; ties method: ",
             settings$ties, "."),
      paste0("This analysis was fitted ",
             if (isTRUE(settings$recensor)) "with" else "without",
             " re-censoring. ",
             if (isTRUE(settings$recensor))
               paste0("A companion analysis without re-censoring must be ", 
                      "fitted separately.")
             else
               paste0("A companion analysis with re-censoring must be ", 
                      "fitted separately.")),
      paste0("Sensitivity to key assumptions cannot be determined from one ", 
             "fitted object; fit and compare models with alternative ", 
             "functional forms and covariate sets in terms of treatment ", 
             "effects and survival extrapolations.")
    ),
    stringsAsFactors = FALSE
  )
  
  out <- list(
    call = object$call,
    population = population,
    estimation = estimation,
    estimates = estimates,
    switching_estimates = switching_estimates,
    missing_switch_predictors = missing_switch_predictors,
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
  class(out) <- "summary.tsegest"
  out
}

#' @title Print method for summary.tsegest objects
#' @description Prints a detailed summary of a two-stage g-estimation fit.
#'
#' @param x An object of class \code{summary.tsegest}.
#' @param digits The number of significant digits to print.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return The input object, invisibly.
#'
#' @keywords internal
#'
#' @export
print.summary.tsegest <- function(
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
  
  cat("Two-Stage Estimation with G-Estimation (TSEgest)\n\n")
  
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
  
  cat("\nTSEgest reporting checklist\n")
  for (index in seq_len(nrow(x$reporting))) {
    print_key_values(x$reporting$item[index], x$reporting$information[index])
    
    if (index == 4L && !is.null(x$switching_estimates)) {
      cat("\nSwitching model parameter estimates\n")
      switching_estimates <- format_parameter_estimates(x$switching_estimates)
      print(switching_estimates, row.names = FALSE, ...)
    } else if (index == 6L && !is.null(x$missing_switch_predictors)) {
      cat(paste0("\nMissing switching-model predictors before complete-case ", 
                 "filtering\n"))
      missing_predictors <- x$missing_switch_predictors
      missing_predictors$missing_pct <- formatC(
        missing_predictors$missing_pct, format = "f", digits = 1
      )
      print(missing_predictors, row.names = FALSE, ...)
    } else if (index == 9L) {
      cat("\nEstimation procedure\n")
      print_key_values(x$estimation$item, x$estimation$value)
    } else if (index == 11L) {
      cat("\nCausal parameter estimates (", format(x$conf_level, trim = TRUE),
          "% confidence intervals)\n", sep = "")
      estimates <- x$estimates
      estimates[c("estimate", "lower", "upper")] <- lapply(
        estimates[c("estimate", "lower", "upper")], formatC,
        format = "f", digits = 3)
      print(estimates, row.names = FALSE, ...)
    } else if (index == 12L) {
      cat("\nOutcome model parameter estimates\n")
      if (!is.null(x$outcome_estimates)) {
        outcome_estimates <- format_parameter_estimates(x$outcome_estimates)
        print(outcome_estimates, row.names = FALSE, ...)
      }
      cat("\nAdjusted hazard ratio (", format(x$conf_level, trim = TRUE),
          "% confidence interval)\n", sep = "")
      cat("Hazard ratio: ", formatC(x$hr, format = "f", digits = 3),
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
