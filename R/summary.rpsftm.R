#' @title Summary method for rpsftm objects
#' @description Summarizes reporting items and treatment effect estimates from
#' a rank-preserving structural failure time model.
#'
#' @param object An object of class \code{rpsftm}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return An object of class \code{summary.rpsftm} containing the analysis
#' population, reporting checklist, estimation procedure, treatment effect
#' estimates, outcome-model parameter estimates, and bootstrap summary.
#'
#' @keywords internal
#'
#' @author Kaifeng Lu, \email{kaifenglu@@gmail.com}
#'
#' @export
summary.rpsftm <- function(object, ...) {
  if (!inherits(object, "rpsftm")) {
    stop("object must be of class 'rpsftm'")
  }
  
  settings <- object$settings
  event_summary <- object$event_summary
  treat_var <- settings$treat
  
  population <- event_summary
  arm <- if ("treated" %in% names(population)) population$treated else 0:1
  if (treat_var %in% names(population)) {
    arm <- as.character(population[[treat_var]])
  } else {
    arm <- ifelse(arm == 0, "Control", "Treatment")
  }
  population <- cbind(arm = arm, population[, setdiff(
    names(population), c("treated", treat_var)), drop = FALSE])
  names(population)[1] <- treat_var
  
  g_test <- switch(
    settings$psi_test,
    logrank = "Log-rank test",
    phreg = "Cox PH model",
    lifereg = paste0("Parametric AFT (", settings$aft_dist, ")"),
    settings$psi_test
  )
  search_method <- if (isTRUE(settings$gridsearch)) {
    "Grid search with linear interpolation"
  } else {
    paste("Root finding (", settings$root_finding, ")", sep = "")
  }
  
  estimation <- data.frame(
    item = c(
      "Search method", "Search interval", "Number of Z evaluations",
      "Root-finding tolerance", "Number of zero-crossings", "Zero-crossings",
      "Selected root"
    ),
    value = c(
      search_method,
      paste0("[", settings$low_psi, ", ", settings$hi_psi, "]"),
      as.character(settings$n_eval_z),
      as.character(settings$tol),
      as.character(sum(is.finite(object$psi_roots))),
      if (any(is.finite(object$psi_roots))) {
        paste(formatC(object$psi_roots[is.finite(object$psi_roots)],
                      format = "f", digits = 3), collapse = ", ")
      } else {
        "None"
      },
      formatC(object$psi, format = "f", digits = 3)
    ),
    stringsAsFactors = FALSE
  )
  
  conf_level <- 100 * (1 - settings$alpha)
  estimates <- data.frame(
    estimand = c("Causal parameter psi", "Causal survival time ratio"),
    estimate = c(object$psi, exp(-object$psi)),
    lower = c(object$psi_CI[1], exp(-object$psi_CI[2])),
    upper = c(object$psi_CI[2], exp(-object$psi_CI[1])),
    ci_method = c(object$psi_CI_type, object$psi_CI_type),
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
    estimates <- parest[, c("param", "beta", "expbeta", "sebeta", "z", "p"),
                        drop = FALSE]
    names(estimates)[2:4] <- c("coef", "exp(coef)", "se(coef)")
    estimates
  }
  
  outcome_estimates <- extract_model_estimates(object$fit_outcome)
  
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
  }
  
  base_covariates <- if (length(settings$base_cov) == 0 ||
                         all(settings$base_cov == "")) {
    "None"
  } else {
    paste(settings$base_cov, collapse = ", ")
  }
  stratification_variables <- if (length(settings$stratum) == 0 ||
                                  all(settings$stratum == "")) {
    "None"
  } else {
    paste(settings$stratum, collapse = ", ")
  }
  reporting <- data.frame(
    item = paste0("RPSFTM", 1:10),
    information = c(
      paste0("The common treatment-effect assumption cannot be assessed ", 
             "from the fitted object; justify it using clinical and external ",
             "evidence."),
      paste0("Structural model: U_{i,\\psi} = T_{C_i} + e^{\\psi} T_{E_i}, ",
              "where U_{i,\\psi} is the counterfactual survival time for ",
              "participant i had they remained untreated, and T_{C_i} and ",
              "T_{E_i} are the observed times spent receiving control and ",
              "experimental treatment, respectively. The parameter \\psi ",
              "quantifies the causal treatment effect, with e^{-\\psi} ",
              "representing the causal survival time ratio. Treatment exposure ",
              "is represented by ", settings$rx, ", the proportion of observed ",
              "time spent receiving experimental treatment. The interpretation ",
              "of this exposure measure and the plausibility of the structural ",
              "model should be justified using clinical and scientific ",
              "evidence."),
      paste0("G-estimation metric: ", g_test,
             if (!identical(settings$psi_test, "logrank")) {
               paste0("; baseline covariates: ", base_covariates)
             } else {
               ""
             },
             "; stratification variables: ", stratification_variables, "."),
      paste0("G-estimation used ", tolower(search_method), " over [",
             settings$low_psi, ", ", settings$hi_psi, "]."),
      paste0("G-estimation diagnostics: ",
             "Inspect p_z and p_kmstar from plot(object) to assess ",
             "whether the g-estimation process has worked well."),
      paste0("Causal parameter estimates, including the causal survival time ",
             "ratio and confidence interval, are shown below."),
      paste0("Inspect p_km from plot(object) for the Kaplan-Meier plot of ",
             "counterfactual unswitched survival by randomized group."),
      paste0("Outcome model: Cox PH for counterfactual unswitched survival; ",
             "baseline covariates: ",
             base_covariates, "; stratification variables: ",
             stratification_variables, "; ties method: ", settings$ties, "."),
      paste0("This analysis was fitted ",
             if (isTRUE(settings$recensor)) "with" else "without",
             " re-censoring. ",
             if (isTRUE(settings$recensor))
               paste0("A companion analysis without re-censoring must be ", 
                      "fitted separately.")
             else
               paste0("A companion analysis with re-censoring must be ", 
                      "fitted separately.")),
      paste0("Sensitivity to treatment effect estimates and survival ",
             "extrapolations to alternative treatment-effect modifiers ",
             "and adjustment methods cannot be ",
             "determined from one fitted object; fit and report the relevant ",
             "analyses separately.")
    ),
    stringsAsFactors = FALSE
  )
  
  out <- list(
    call = object$call,
    population = population,
    estimation = estimation,
    estimates = estimates,
    outcome_estimates = outcome_estimates,
    conf_level = conf_level,
    hr = object$hr,
    hr_CI = object$hr_CI,
    hr_CI_type = object$hr_CI_type,
    pvalue = object$pvalue,
    pvalue_type = if (identical(object$pvalue_type, "log-rank")) {
      "ITT log-rank"
    } else {
      object$pvalue_type
    },
    bootstrap = bootstrap,
    reporting = reporting
  )
  class(out) <- "summary.rpsftm"
  out
}

#' @title Print method for summary.rpsftm objects
#' @description Prints a detailed summary of a rpsftm fit.
#'
#' @param x An object of class \code{summary.rpsftm}.
#' @param digits The number of significant digits to print.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return The input object, invisibly.
#'
#' @keywords internal
#'
#' @export
print.summary.rpsftm <- function(x, digits = max(3L, getOption("digits") - 3L),
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
  
  cat("Rank Preserving Structural Failure Time Model (RPSFTM)\n\n")
  
  if(!is.null(cl <- x$call)) {
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
  
  cat("\nRPSFTM reporting checklist\n")
  for (index in seq_len(nrow(x$reporting))) {
    print_key_values(x$reporting$item[index], x$reporting$information[index])
    
    if (index == 4L) {
      cat("\nEstimation procedure\n")
      print_key_values(x$estimation$item, x$estimation$value)
    } else if (index == 6L) {
      cat("\nCausal parameter estimates (", format(x$conf_level, trim = TRUE),
          "% confidence intervals)\n", sep = "")
      estimates <- x$estimates
      estimates[c("estimate", "lower", "upper")] <- lapply(
        estimates[c("estimate", "lower", "upper")], formatC,
        format = "f", digits = 3)
      print(estimates, row.names = FALSE, ...)
    } else if (index == 8L) {
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