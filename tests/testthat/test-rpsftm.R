library(dplyr, warn.conflicts = FALSE)
library(survival)

testthat::test_that("rpsftm: control to active switch", {
  data1 <- immdef %>% mutate(rx = 1-xoyrs/progyrs)
  
  fit1 <- rpsftm(
    data1, id = "id", time = "progyrs", event = "prog", treat = "imm", 
    rx = "rx", censor_time = "censyrs", gridsearch = FALSE, boot = FALSE)
  
  # log-rank for ITT
  fit_lr <- survdiff(Surv(progyrs, prog) ~ imm, data = data1)
  z_lr <- sqrt(fit_lr$chisq)
  if (fit_lr$obs[2] < fit_lr$exp[2]) z_lr <- -z_lr
  
  f <- function(psi) {
    data2 <- data1 %>%
      mutate(u_star = xoyrs + (progyrs - xoyrs)*exp(psi),
             c_star = ifelse(imm == 0, pmin(censyrs, censyrs*exp(psi)), 1e10),
             t_star = pmin(u_star, c_star),
             d_star = ifelse(c_star < u_star, 0, prog))
    fit_lr <- survdiff(Surv(t_star, d_star) ~ imm, data = data2)
    z_lr <- sqrt(fit_lr$chisq)
    if (fit_lr$obs[2] < fit_lr$exp[2]) z_lr <- -z_lr
    z_lr
  }
  
  # psi based on log-rank test
  psi <- uniroot(f, c(-2,2), tol = 1e-6)$root
  
  data2 <- data1 %>%
    filter(imm == 0) %>%
    mutate(u_star = xoyrs + (progyrs - xoyrs)*exp(psi),
           c_star = pmin(censyrs, censyrs*exp(psi)),
           t_star = pmin(u_star, c_star),
           d_star = ifelse(c_star < u_star, 0, prog)) %>%
    select(-c("u_star", "c_star")) %>%
    bind_rows(data1 %>%
                filter(imm == 1) %>%
                mutate(t_star = progyrs, 
                       d_star = prog))
  
  fit <- coxph(Surv(t_star, d_star) ~ imm, data = data2)
  beta <- as.numeric(fit$coefficients[1])
  se <- beta/z_lr
  
  zcrit <- qnorm(0.975)
  hr1 <- exp(c(beta, beta - zcrit*se, beta + zcrit*se))
  testthat::expect_equal(hr1, c(fit1$hr, fit1$hr_CI))

  s <- summary(fit1)
  testthat::expect_s3_class(s, "summary.rpsftm")
  testthat::expect_true(is.call(fit1$call))
  testthat::expect_identical(s$call, fit1$call)
  testthat::expect_equal(
    s$estimates$estimate,
    c(fit1$psi, exp(-fit1$psi))
  )
  testthat::expect_identical(
    s$estimation$value[s$estimation$item == "Zero-crossings"],
    paste(formatC(fit1$psi_roots[is.finite(fit1$psi_roots)],
                  format = "f", digits = 3), collapse = ", ")
  )
  testthat::expect_identical(
    s$estimation$value[s$estimation$item == "Selected root"],
    formatC(fit1$psi, format = "f", digits = 3)
  )
  testthat::expect_equal(nrow(s$population), 2L)
  testthat::expect_true(all(c(
    "estimation", "estimates", "outcome_estimates", "reporting"
  ) %in% names(s)))
  testthat::expect_identical(
    names(s$outcome_estimates),
    c("param", "coef", "exp(coef)", "se(coef)", "z", "p")
  )
  testthat::expect_identical(s$reporting$item, paste0("RPSFTM", 1:10))
  testthat::expect_match(s$reporting$information[10], "^Sensitivity to")
  testthat::expect_match(
    s$reporting$information[3], "stratification variables: None\\."
  )
  testthat::expect_false(grepl(
    "baseline covariates", s$reporting$information[3], fixed = TRUE
  ))
  testthat::expect_match(
    s$reporting$information[8], "stratification variables: None"
  )
  testthat::expect_match(s$reporting$information[8], "ties method: efron\\.")
  testthat::expect_match(
    s$reporting$information[5],
    "Inspect p_z and p_kmstar from plot\\(object\\)"
  )
  testthat::expect_match(
    s$reporting$information[7],
    "Inspect p_km from plot\\(object\\) for the Kaplan-Meier plot of "
  )
  testthat::expect_identical(s$pvalue_type, "ITT log-rank")
  testthat::expect_null(s$bootstrap)

  printed <- capture.output(print(s))
  population_output <- printed[
    seq_len(match(TRUE, grepl("RPSFTM reporting checklist", printed)) - 1L)
  ]
  population_pct <- unlist(lapply(
    s$population[grep("_pct$", names(s$population))],
    formatC, format = "f", digits = 1
  ))
  testthat::expect_true(all(vapply(
    population_pct,
    function(value) any(grepl(value, population_output, fixed = TRUE)),
    logical(1)
  )))
  fixed_precision <- capture.output(print(s, digits = 1))
  testthat::expect_true(all(vapply(
    unlist(s$estimates[c("estimate", "lower", "upper")]),
    function(value) any(grepl(
      formatC(value, format = "f", digits = 3), fixed_precision, fixed = TRUE
    )), logical(1)
  )))
  formatted_pvalue <- ifelse(
    s$pvalue < 1e-4, "<.0001",
    ifelse(s$pvalue > 0.9999, ">.9999",
           formatC(s$pvalue, format = "f", digits = 4))
  )
  testthat::expect_true(any(grepl(
    paste0("P-value (", s$pvalue_type, "): ", formatted_pvalue),
    fixed_precision, fixed = TRUE
  )))
  boundary_summary <- s
  boundary_summary$pvalue <- 0
  testthat::expect_true(any(grepl(
    "P-value (ITT log-rank): <.0001",
    capture.output(print(boundary_summary)), fixed = TRUE
  )))
  boundary_summary$pvalue <- 1
  testthat::expect_true(any(grepl(
    "P-value (ITT log-rank): >.9999",
    capture.output(print(boundary_summary)), fixed = TRUE
  )))
  hr_ci <- paste0(
    formatC(fit1$hr, format = "f", digits = 3), " (",
    formatC(fit1$hr_CI[1], format = "f", digits = 3), ", ",
    formatC(fit1$hr_CI[2], format = "f", digits = 3), ")"
  )
  testthat::expect_true(any(grepl(hr_ci, printed, fixed = TRUE)))
  testthat::expect_true(any(grepl("RPSFTM reporting checklist", printed)))
  testthat::expect_match(
    s$reporting$information[2],
    "e^{-\\psi} representing the causal survival time ratio",
    fixed = TRUE
  )
  testthat::expect_match(
    s$reporting$information[2],
    paste0("plausibility of the structural model should be justified using ",
           "clinical and scientific evidence"),
    fixed = TRUE
  )
  testthat::expect_match(s$reporting$information[2], fit1$settings$rx)
  headings <- c("Analysis population", "RPSFTM1:", "RPSFTM4:",
                "Estimation procedure", "RPSFTM6:",
                "Causal parameter estimates (", "RPSFTM8:",
                "Outcome model parameter estimates")
  heading_positions <- vapply(headings, function(heading) {
    match(TRUE, grepl(heading, printed, fixed = TRUE))
  }, integer(1))
  testthat::expect_true(all(diff(heading_positions) > 0L))
  testthat::expect_true(
    match(TRUE, grepl("Adjusted hazard ratio", printed, fixed = TRUE)) >
      match(TRUE, grepl("RPSFTM8:", printed, fixed = TRUE))
  )
  testthat::expect_equal(
    match(TRUE, grepl("RPSFTM2:", printed, fixed = TRUE)) -
      match(TRUE, grepl("RPSFTM1:", printed, fixed = TRUE)),
    2L
  )
  testthat::expect_true(all(vapply(
    unlist(s$outcome_estimates[c("coef", "exp(coef)", "se(coef)")]),
    function(value) any(grepl(formatC(value, format = "f", digits = 4), printed,
                               fixed = TRUE)), logical(1)
  )))
  testthat::expect_true(all(vapply(s$outcome_estimates$z, function(value) {
    any(grepl(formatC(value, format = "f", digits = 3), printed, fixed = TRUE))
  }, logical(1))))
  format_pvalue <- function(value) {
      ifelse(value < 1e-4, "<.0001",
        ifelse(value > 0.9999, ">.9999",
          formatC(value, format = "f", digits = 4)))
  }
  testthat::expect_true(all(vapply(s$outcome_estimates$p, function(value) {
    any(grepl(format_pvalue(value), printed, fixed = TRUE))
  }, logical(1))))
  extreme_summary <- s
  extreme_summary$outcome_estimates <- extreme_summary$outcome_estimates[
    rep(1L, 2L), , drop = FALSE
  ]
  extreme_summary$outcome_estimates$p <- c(0, 1)
  extreme_printed <- capture.output(print(extreme_summary))
  testthat::expect_true(any(grepl("<.0001", extreme_printed, fixed = TRUE)))
  testthat::expect_true(any(grepl(">.9999", extreme_printed, fixed = TRUE)))

  plots <- plot(fit1, show_hr = FALSE, show_risk = FALSE)
  testthat::expect_named(plots, c("p_z", "p_kmstar", "p_km"))
  testthat::expect_s3_class(plots$p_kmstar, "ggplot")
  testthat::expect_identical(
    plots$p_kmstar$labels$title,
    "Kaplan-Meier Curves for Counterfactual Untreated Outcomes"
  )
  testthat::expect_equal(nrow(plots$p_kmstar$data), nrow(fit1$kmstar))
  testthat::expect_equal(
    plots$p_kmstar$data$month,
    plots$p_kmstar$data$time / 30.4375
  )
  testthat::expect_identical(plots$p_kmstar$labels$x, "Months")
  built_kmstar <- ggplot2::ggplot_build(plots$p_kmstar)
  testthat::expect_length(unique(built_kmstar$data[[1]]$group), 2L)
  testthat::expect_length(unique(built_kmstar$data[[1]]$colour), 2L)
  testthat::expect_identical(unique(built_kmstar$data[[1]]$linetype), 1L)

  min_surv_star <- data.table::data.table(plots$p_kmstar$data)[
    , min(get("surv")), by = "randomized_arm"][, get("V1")]
  expected_legend_position <- if (max(min_surv_star) < 0.5) {
    c(0.7, 0.85)
  } else {
    c(0.15, 0.25)
  }
  testthat::expect_equal(
    plots$p_kmstar$theme$legend.position.inside,
    expected_legend_position
  )
})
