library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(splines)
library(survival)

testthat::test_that("msm: pooled logistic regression switching model", {
  sim1 <- tssim(
    tdxo = 1, coxo = 1, allocation1 = 1, allocation2 = 1,
    p_X_1 = 0.3, p_X_0 = 0.3, 
    rate_T = 0.002, beta1 = -0.5, beta2 = 0.3, 
    gamma0 = 0.3, gamma1 = -0.9, gamma2 = 0.7, gamma3 = 1.1, gamma4 = -0.8,
    zeta0 = -3.5, zeta1 = 0.5, zeta2 = 0.2, zeta3 = -0.4, 
    alpha0 = 0.5, alpha1 = 0.5, alpha2 = 0.4, 
    theta1_1 = -0.4, theta1_0 = -0.4, theta2 = 0.2,
    rate_C = 0.0000855,  accrualIntensity = 20/30, 
    fixedFollowup = FALSE, plannedTime = 1350, days = 30,
    n = 500, NSim = 100, seed = 314159)
  
  data1 <- sim1[[1]] %>% 
    mutate(os = died, ostime = timeOS)
  
  fit1 <- msm(
    data1, id = "id", tstart = "tstart", 
    tstop = "tstop", event = "event", treat = "trtrand", 
    swtrt = "xo", swtrt_time = "xotime", 
    base_cov = "bprog", numerator = "bprog", 
    denominator = c("bprog", "L"), 
    ns_df = 3, swtrt_control_only = TRUE, boot = FALSE)
  
  # exclude observations after treatment switch
  data2 <- data1 %>%
    filter(ifelse(xo == 1, tstart < xotime, tstop < ostime)) %>%
    group_by(id) %>%
    mutate(condition = row_number() == n() & xo == 1 & tstop >= xotime,
           cross = ifelse(condition, 1, 0)) %>%
    ungroup()
  
  # fit pooled logistic regression switching models
  data3 <- data2 %>% filter(trtrand == 0)
  
  s1 <- ns(data3$tstop[data3$cross == 1], df = 3)
  s2 <- ns(data3$tstop, knots = attr(s1, "knots"), 
           Boundary.knots = attr(s1, "Boundary.knots"))
  switch1 <- glm(cross ~ bprog + L + s2, 
                 family = binomial, data = data3)
  phat1 <- as.numeric(predict(switch1, newdata = data3, type = "response"))
  
  switch2 <- glm(cross ~ bprog + s2, family = binomial, data = data3)
  phat2 <- as.numeric(predict(switch2, newdata = data3, type = "response"))
  
  # stabilized weights and time-dependent covariates
  data4 <- data1 %>% 
    filter(trtrand == 0) %>% 
    select(id, trtrand, bprog, tstart, tstop, event, xo, xotime) %>%
    left_join(data3 %>% 
                mutate(tstart = tstop,
                       o_den = ifelse(cross, phat1, 1-phat1), 
                       o_num = ifelse(cross, phat2, 1-phat2)) %>%
                select(id, tstart, o_den, o_num), 
              by = c("id", "tstart")) %>%
    mutate(o_den = ifelse(is.na(o_den), 1, o_den),
           o_num = ifelse(is.na(o_num), 1, o_num)) %>%
    group_by(id) %>%
    mutate(p_den = cumprod(o_den), p_num = cumprod(o_num),
           stabilized_weight = p_num/p_den) %>%
    select(id, tstart, tstop, event, stabilized_weight, bprog, 
           trtrand, xo, xotime) %>%
    ungroup() %>%
    bind_rows(data1 %>% 
                filter(trtrand == 1) %>%
                mutate(stabilized_weight = 1) %>%
                select(id, tstart, tstop, event, stabilized_weight, bprog, 
                       trtrand, xo, xotime)) %>%
    mutate(cross = ifelse(xo == 1, tstart >= xotime, 0))
  
  fit <- coxph(Surv(tstart, tstop, event) ~ trtrand + bprog + cross, 
               data = data4, weight = stabilized_weight,
               id = id, ties = "efron", robust = TRUE)
  
  hr1 <- as.numeric(exp(cbind(fit$coefficients, confint(fit)))["trtrand",])
  
  testthat::expect_equal(data4$stabilized_weight, 
                         fit1$data_outcome$stabilized_weight)
  
  testthat::expect_equal(hr1, c(fit1$hr, fit1$hr_CI))
  
  testthat::expect_true(is.call(fit1$call))
  testthat::expect_true("switch_missing_summary" %in% names(fit1))
  printed_fit <- capture.output(print(fit1))
  testthat::expect_true(any(grepl("Call:", printed_fit, fixed = TRUE)))
  
  summary_fit <- summary(fit1)
  testthat::expect_s3_class(summary_fit, "summary.msm")
  testthat::expect_identical(summary_fit$call, fit1$call)
  testthat::expect_true(all(c(
    "covariate_summary", "switching_estimates", "weight_summary",
    "missing_predictors", "outcome_estimates", "reporting"
  ) %in% names(summary_fit)))
  testthat::expect_identical(
    names(summary_fit$missing_predictors),
    c("trtrand", "model", "predictor", "missing", "total", "missing_pct")
  )
  testthat::expect_identical(summary_fit$reporting$item, paste0("MSM", 1:10))
  testthat::expect_match(
    summary_fit$reporting$information[10],
    "and categorical-variable definitions\\.$"
  )
  truncated_fit <- fit1
  truncated_fit$settings$trunc <- 0.01
  testthat::expect_match(
    summary(truncated_fit)$reporting$information[10],
    paste0("categorical-variable definitions, and truncation percentiles ",
           "including no truncation\\.$")
  )
  testthat::expect_match(
    summary_fit$reporting$information[8],
    paste0("Inspect p_w from plot\\(object\\) for weight distribution by ", 
           "treatment group\\.")
  )
  testthat::expect_identical(
    names(summary_fit$switching_estimates),
    c("trtrand", "model", "param", "coef", "exp(coef)", "se(coef)", "z", "p")
  )
  testthat::expect_identical(
    names(summary_fit$covariate_summary),
    c("trtrand", "variable", "statistic/level", "No switch", "Switch")
  )
  testthat::expect_identical(unique(summary_fit$covariate_summary$trtrand), "0")
  testthat::expect_identical(
    order(
      summary_fit$covariate_summary$trtrand,
      summary_fit$covariate_summary$variable,
      summary_fit$covariate_summary[["statistic/level"]]
    ),
    seq_len(nrow(summary_fit$covariate_summary))
  )
  testthat::expect_true(
    "Range" %in% summary_fit$covariate_summary[["statistic/level"]]
  )
  testthat::expect_identical(
    names(summary_fit$outcome_estimates),
    c("param", "coef", "exp(coef)", "se(coef)", "robust se", "z", "p")
  )
  testthat::expect_equal(
    summary_fit$outcome_estimates[["se(coef)"]],
    fit1$fit_outcome$parest$sebeta_naive
  )
  testthat::expect_equal(
    summary_fit$outcome_estimates[["robust se"]],
    fit1$fit_outcome$parest$sebeta
  )
  testthat::expect_match(summary_fit$reporting$information[5],
                         "includes post-switching data")
  testthat::expect_match(
    summary_fit$reporting$information[5],
    "time-varying predictors: Detected: L;"
  )
  testthat::expect_match(summary_fit$reporting$information[9],
                         "includes post-switching data")
  
  printed_summary <- capture.output(print(summary_fit))
  population_output <- printed_summary[
    seq_len(match(TRUE, grepl("MSM reporting checklist", printed_summary)) - 1L)
  ]
  population_pct <- unlist(lapply(
    summary_fit$population[grep("_pct$", names(summary_fit$population))],
    formatC, format = "f", digits = 1
  ))
  testthat::expect_true(all(vapply(
    population_pct,
    function(value) any(grepl(value, population_output, fixed = TRUE)),
    logical(1)
  )))
  fixed_precision <- capture.output(print(summary_fit, digits = 1))
  formatted_pvalue <- ifelse(
    summary_fit$pvalue < 1e-4, "<.0001",
    ifelse(summary_fit$pvalue > 0.9999, ">.9999",
           formatC(summary_fit$pvalue, format = "f", digits = 4))
  )
  testthat::expect_true(any(grepl(
    paste0("P-value (", summary_fit$pvalue_type, "): ", formatted_pvalue),
    fixed_precision, fixed = TRUE
  )))
  hr_ci <- paste0(
    formatC(fit1$hr, format = "f", digits = 3), " (",
    formatC(fit1$hr_CI[1], format = "f", digits = 3), ", ",
    formatC(fit1$hr_CI[2], format = "f", digits = 3), ")"
  )
  testthat::expect_true(any(grepl(hr_ci, printed_summary, fixed = TRUE)))
  for (estimates in list(summary_fit$switching_estimates,
                         summary_fit$outcome_estimates)) {
    testthat::expect_true(all(vapply(
      unlist(estimates[c("coef", "exp(coef)", "se(coef)")]),
      function(value) any(grepl(
        formatC(value, format = "f", digits = 4), printed_summary,
        fixed = TRUE
      )), logical(1)
    )))
  }
  testthat::expect_true(any(grepl("MSM reporting checklist", printed_summary)))
  testthat::expect_true(any(grepl("Positivity diagnostics", printed_summary)))
  testthat::expect_true(any(grepl(
    "Missing switching-model predictors by model", printed_summary,
    fixed = TRUE
  )))
  heading_positions <- c(
    match("Analysis population", printed_summary),
    match(TRUE, grepl("^MSM1:", printed_summary)),
    match(TRUE, grepl("^MSM2:", printed_summary)),
    match("Covariate summary by treatment arm and switch status", 
          printed_summary),
    match(TRUE, grepl("^MSM7:", printed_summary)),
    match("Switching model parameter estimates", printed_summary),
    match(TRUE, grepl("^MSM8:", printed_summary)),
    match("Weight distribution", printed_summary),
    match(TRUE, grepl("^MSM9:", printed_summary)),
    match("Outcome model parameter estimates", printed_summary)
  )
  testthat::expect_true(all(diff(heading_positions) > 0L))
})

