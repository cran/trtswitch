#' @title Summary method for ipe objects
#' @description Summarizes reporting items and treatment effect estimates from
#' an iterative parameter estimation fit.
#'
#' @param object An object of class \code{ipe}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return An object of class \code{summary.ipe} containing the analysis
#' population, reporting checklist, treatment effect estimates, outcome-model
#' parameter estimates, and bootstrap summary.
#'
#' @keywords internal
#'
#' @author Kaifeng Lu, \email{kaifenglu@@gmail.com}
#'
#' @export
summary.ipe <- function(object, ...) {
  if (!inherits(object, "ipe")) {
    stop("object must be of class 'ipe'")
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
    parest[, c("param", "beta", "expbeta", "sebeta", "z", "p"),
           drop = FALSE]
  }
  
  outcome_estimates <- extract_model_estimates(object$fit_outcome)
  aft_estimates <- extract_model_estimates(object$fit_aft)
  if (!is.null(outcome_estimates)) {
    names(outcome_estimates)[2:4] <- c("coef", "exp(coef)", "se(coef)")
  }
  if (!is.null(aft_estimates)) {
    names(aft_estimates)[2:4] <- c("coef", "exp(coef)", "se(coef)")
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
    item = paste0("IPE", 1:8),
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
      paste0("Parametric model: accelerated failure time with ",
             settings$aft_dist,
             " distribution; baseline covariates: ", base_covariates,
             ". Its appropriateness must be assessed ",
             "outside the fitted object."),
      paste0("Causal parameter estimates, including the causal survival time ",
             "ratio and confidence interval, are shown below."),
      paste0("Inspect p_kmstar from plot(object) for the Kaplan-Meier plot ",
             "of counterfactual untreated survival by randomized group. ",
             "Inspect p_km from plot(object) for the Kaplan-Meier plot of ",
             "counterfactual unswitched survival by randomized group."),
      paste0("Outcome model: Cox PH for counterfactual unswitched survival; ",
             "baseline covariates: ", base_covariates,
             "; stratification variables: ", stratification_variables,
             "; ties method: ", settings$ties, "."),
      paste0("This analysis was fitted ",
             if (isTRUE(settings$recensor)) "with" else "without",
             " re-censoring. ",
             if (isTRUE(settings$recensor))
               paste0("A companion analysis without re-censoring must be ", 
                      "fitted separately.")
             else
               paste0("A companion analysis with re-censoring must be ", 
                      "fitted separately.")),
      paste0("Sensitivity of treatment effect estimates and survival ",
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
    estimates = estimates,
    aft_estimates = aft_estimates,
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
  class(out) <- "summary.ipe"
  out
}

#' @title Print method for summary.ipe objects
#' @description Prints a detailed summary of an ipe fit.
#'
#' @param x An object of class \code{summary.ipe}.
#' @param digits The number of significant digits to print.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#'
#' @return The input object, invisibly.
#'
#' @keywords internal
#'
#' @export
print.summary.ipe <- function(x, digits = max(3L, getOption("digits") - 3L),
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
  
  cat("Iterative Parameter Estimation (IPE)\n\n")
  
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
  
  cat("\nIPE reporting checklist\n")
  for (index in seq_len(nrow(x$reporting))) {
    print_key_values(x$reporting$item[index], x$reporting$information[index])
    
    if (index == 4L) {
      cat("\nCausal parameter estimates (", format(x$conf_level, trim = TRUE),
          "% confidence intervals)\n", sep = "")
      estimates <- x$estimates
      estimates[c("estimate", "lower", "upper")] <- lapply(
        estimates[c("estimate", "lower", "upper")], formatC,
        format = "f", digits = 3)
      print(estimates, row.names = FALSE, ...)
    } else if (index == 3L && !is.null(x$aft_estimates)) {
      cat("\nAFT model parameter estimates\n")
      aft_estimates <- format_parameter_estimates(x$aft_estimates)
      print(aft_estimates, row.names = FALSE, ...)
    } else if (index == 6L) {
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
