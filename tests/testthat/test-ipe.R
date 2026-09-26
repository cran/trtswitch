library(dplyr, warn.conflicts = FALSE)
library(survival)

testthat::test_that("ipe: control to active switch", {
  data1 <- immdef %>% mutate(rx = 1-xoyrs/progyrs)
  
  fit1 <- ipe(
    data1, id = "id", time = "progyrs", event = "prog", treat = "imm", 
    rx = "rx", censor_time = "censyrs", aft_dist = "weibull",
    boot = FALSE)
  
  # log-rank for ITT
  fit_lr <- survdiff(Surv(progyrs, prog) ~ imm, data = data1)
  z_lr <- sqrt(fit_lr$chisq)
  if (fit_lr$obs[2] < fit_lr$exp[2]) z_lr <- -z_lr
  
  f <- function(psi) {
    data1 %>%
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
  }
  
  g <- function(psi) {
    data2 <- f(psi)
    fit_aft <- survreg(Surv(t_star, d_star) ~ imm, 
                       data = data2, dist = "weibull")
    -fit_aft$coefficients[2] - psi
  }
  
  # psi based on AFT model
  psi <- uniroot(g, c(-2,2), tol = 1e-6)$root
  
  data2 <- f(psi)
  fit <- coxph(Surv(t_star, d_star) ~ imm, data = data2)
  beta <- as.numeric(fit$coefficients[1])
  se <- beta/z_lr
  
  zcrit <- qnorm(0.975)
  hr1 <- exp(c(beta, beta - zcrit*se, beta + zcrit*se))
  testthat::expect_equal(hr1, c(fit1$hr, fit1$hr_CI))
  
  plots <- plot(fit1, show_hr = FALSE, show_risk = FALSE)
  testthat::expect_named(plots, c("p_res", "p_kmstar", "p_km"))
  testthat::expect_s3_class(plots$p_kmstar, "ggplot")
  testthat::expect_equal(nrow(plots$p_kmstar$data), nrow(fit1$kmstar))
  
  s <- summary(fit1)
  testthat::expect_s3_class(s, "summary.ipe")
  testthat::expect_true(is.call(fit1$call))
  testthat::expect_identical(s$call, fit1$call)
  testthat::expect_equal(
    s$estimates$estimate,
    c(fit1$psi, exp(-fit1$psi))
  )
  testthat::expect_equal(nrow(s$population), 2L)
  testthat::expect_true(all(c(
    "estimates", "aft_estimates", "outcome_estimates", "reporting"
  ) %in% names(s)))
  testthat::expect_identical(s$reporting$item, paste0("IPE", 1:8))
  testthat::expect_identical(
    names(s$outcome_estimates),
    c("param", "coef", "exp(coef)", "se(coef)", "z", "p")
  )
  testthat::expect_identical(
    names(s$aft_estimates),
    c("param", "coef", "exp(coef)", "se(coef)", "z", "p")
  )
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
  testthat::expect_match(
    s$reporting$information[3], "baseline covariates: None\\."
  )
  testthat::expect_match(s$reporting$information[5], "p_kmstar")
  testthat::expect_match(
    s$reporting$information[6], "stratification variables: None"
  )
  testthat::expect_match(s$reporting$information[6], "ties method: efron\\.")
  testthat::expect_identical(s$pvalue_type, "ITT log-rank")
  testthat::expect_null(s$bootstrap)
  
  printed <- capture.output(print(s))
  population_output <- printed[
    seq_len(match(TRUE, grepl("IPE reporting checklist", printed)) - 1L)
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
  hr_ci <- paste0(
    formatC(fit1$hr, format = "f", digits = 3), " (",
    formatC(fit1$hr_CI[1], format = "f", digits = 3), ", ",
    formatC(fit1$hr_CI[2], format = "f", digits = 3), ")"
  )
  testthat::expect_true(any(grepl(hr_ci, printed, fixed = TRUE)))
  testthat::expect_true(any(grepl("IPE reporting checklist", printed)))
  headings <- c("Analysis population", "IPE1:", "IPE3:",
                "AFT model parameter estimates", "IPE4:",
                "Causal parameter estimates (", "IPE6:",
                "Outcome model parameter estimates")
  heading_positions <- vapply(headings, function(heading) {
    match(TRUE, grepl(heading, printed, fixed = TRUE))
  }, integer(1))
  testthat::expect_true(all(diff(heading_positions) > 0L))
  testthat::expect_true(
    match(TRUE, grepl("Adjusted hazard ratio", printed, fixed = TRUE)) >
      match(TRUE, grepl("IPE6:", printed, fixed = TRUE))
  )
  format_pvalue <- function(value) {
    ifelse(value < 1e-4, "<.0001",
           ifelse(value > 0.9999, ">.9999",
                  formatC(value, format = "f", digits = 4)))
  }
  testthat::expect_true(all(vapply(
    unlist(s$aft_estimates[c("coef", "exp(coef)", "se(coef)")]),
    function(value) any(grepl(formatC(value, format = "f", digits = 4), printed,
                              fixed = TRUE)), logical(1)
  )))
  testthat::expect_true(all(vapply(
    unlist(s$outcome_estimates[c("coef", "exp(coef)", "se(coef)")]),
    function(value) any(grepl(formatC(value, format = "f", digits = 4), printed,
                              fixed = TRUE)), logical(1)
  )))
  for (estimates in list(s$aft_estimates, s$outcome_estimates)) {
    testthat::expect_true(all(vapply(estimates$z, function(value) {
      any(grepl(formatC(value, format = "f", digits = 3), printed, 
                fixed = TRUE))
    }, logical(1))))
    testthat::expect_true(all(vapply(estimates$p, function(value) {
      any(grepl(format_pvalue(value), printed, fixed = TRUE))
    }, logical(1))))
  }
  extreme_summary <- s
  extreme_summary$aft_estimates$p <- rep(
    c(0, 1), length.out = nrow(extreme_summary$aft_estimates)
  )
  extreme_summary$outcome_estimates$p <- rep(
    c(0, 1), length.out = nrow(extreme_summary$outcome_estimates)
  )
  extreme_printed <- capture.output(print(extreme_summary))
  testthat::expect_true(any(grepl("<.0001", extreme_printed, fixed = TRUE)))
  testthat::expect_true(any(grepl(">.9999", extreme_printed, fixed = TRUE)))
})
