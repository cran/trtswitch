library(dplyr, warn.conflicts = FALSE)
library(survival)

testthat::test_that("tsesimp: weibull aft", {
  # modify pd and dpd based on co and dco
  shilong <- shilong %>%
    mutate(dpd = ifelse(co & !pd, dco, dpd),
           pd = ifelse(co & !pd, 1, pd)) %>%
    mutate(dpd = ifelse(pd & co & dco < dpd, dco, dpd))
  
  # the eventual survival time
  shilong1 <- shilong %>%
    arrange(bras.f, id, tstop) %>%
    group_by(bras.f, id) %>%
    slice(n()) %>%
    select(-c("ps", "ttc", "tran"))
  
  # the last value of time-dependent covariates before pd
  shilong2 <- shilong %>%
    filter(pd == 0 | tstart <= dpd) %>%
    arrange(bras.f, id, tstop) %>%
    group_by(bras.f, id) %>%
    slice(n()) %>%
    select(bras.f, id, ps, ttc, tran)
  
  # combine baseline and time-dependent covariates
  shilong3 <- shilong1 %>%
    left_join(shilong2, by = c("bras.f", "id"))
  
  # apply the two-stage method
  fit1 <- tsesimp(
    data = shilong3, id = "id", time = "tstop", event = "event",
    treat = "bras.f", censor_time = "dcut", pd = "pd",
    pd_time = "dpd", swtrt = "co", swtrt_time = "dco",
    base_cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c",
                 "pathway.f"),
    base2_cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c",
                  "pathway.f", "ps", "ttc", "tran"),
    aft_dist = "weibull", alpha = 0.05,
    recensor = TRUE, swtrt_control_only = FALSE, offset = 1,
    boot = FALSE)

  # numeric code of treatment and apply administrative censoring
  data1 <- shilong3 %>% 
    mutate(treated = 1 * (bras.f == "MTA"), swtrt = 1 * co)
  
  tablist <- lapply(0:1, function(h) {
    df1 <- data1 %>% 
      filter(treated == h & pd == 1) %>%
      mutate(time = tstop - dpd + 1)
    
    fit_aft <- survreg(Surv(time, event) ~ swtrt + agerand + sex.f + 
                         tt_Lnum + rmh_alea.c + pathway.f + 
                         ps + ttc + tran, data = df1)
    
    psi <- -fit_aft$coefficients[2]

    data1 %>% 
      filter(treated == h) %>%
      mutate(u_star = ifelse(swtrt == 1, 
                             dpd - 1 + (tstop - dpd + 1) * exp(psi), tstop),
             c_star = pmin(dcut, dcut*exp(psi)),
             t_star = pmin(u_star, c_star),
             d_star = ifelse(c_star < u_star, 0, event))
  })
  
  data2 <- do.call(rbind, tablist)
  
  fit <- coxph(Surv(t_star, d_star) ~ treated + agerand + sex.f + 
                 tt_Lnum + rmh_alea.c + pathway.f, data = data2)
    
  hr1 <- as.numeric(exp(cbind(fit$coefficients, confint(fit)))["treated",])
  testthat::expect_equal(hr1, c(fit1$hr, fit1$hr_CI))

  testthat::expect_length(fit1$data_switch, 2L)
  testthat::expect_length(fit1$km_switch, 2L)
  testthat::expect_true(all(c("data", "bras.f") %in% 
                              names(fit1$data_switch[[1]])))
  testthat::expect_true(all(c("swtrt", "swtrt_time") %in%
                            names(fit1$data_switch[[1]]$data)))
  testthat::expect_gt(nrow(fit1$km_switch[[1]]$data), 0L)

  plots <- plot(fit1, show_hr = FALSE, show_risk = FALSE)
  testthat::expect_named(plots, c("p_res", "p_km_switch", "p_km"))
  testthat::expect_s3_class(plots$p_km_switch, "ggplot")
  testthat::expect_equal(
    nrow(plots$p_km_switch$data),
    sum(vapply(fit1$km_switch, function(x) nrow(x$data), integer(1)))
  )
  testthat::expect_identical(
    plots$p_km_switch$labels$title,
    "Kaplan-Meier Curves for Time from Disease Progression to Switching"
  )
  testthat::expect_equal(
    fit1$data_switch[[1]]$data$swtrt_time,
    ifelse(fit1$data_switch[[1]]$data$swtrt == 1,
           shilong3$dco[match(fit1$data_switch[[1]]$data$id, shilong3$id)] -
             shilong3$dpd[match(fit1$data_switch[[1]]$data$id, shilong3$id)] +1,
           shilong3$tstop[match(fit1$data_switch[[1]]$data$id, shilong3$id)] -
             shilong3$dpd[match(fit1$data_switch[[1]]$data$id, shilong3$id)] +1)
  )

  summary_fit <- summary(fit1)
  testthat::expect_s3_class(summary_fit, "summary.tsesimp")
  testthat::expect_identical(summary_fit$call, fit1$call)
  testthat::expect_equal(
    summary_fit$estimates$estimate,
    c(fit1$psi, exp(-fit1$psi), fit1$psi_trt, exp(-fit1$psi_trt))
  )
  testthat::expect_true(all(c(
    "estimates", "aft_estimates", "aft_missing_predictors",
    "outcome_estimates", "reporting"
  ) %in% names(summary_fit)))
  testthat::expect_true("aft_missing_summary" %in% names(fit1))
  testthat::expect_identical(
    names(summary_fit$aft_missing_predictors),
    c("bras.f", "predictor", "missing", "total", "missing_pct")
  )
  testthat::expect_identical(summary_fit$reporting$item, paste0("TSEs", 1:9))
  testthat::expect_identical(
    names(summary_fit$aft_estimates),
    c("bras.f", "param", "coef", "exp(coef)", "se(coef)", "z", "p")
  )
  testthat::expect_setequal(summary_fit$aft_estimates$bras.f, c("MTA", "CT"))
  testthat::expect_identical(
    names(summary_fit$outcome_estimates),
    c("param", "coef", "exp(coef)", "se(coef)", "z", "p")
  )
  testthat::expect_match(summary_fit$reporting$information[4], "p_km_switch")
  testthat::expect_match(
    summary_fit$reporting$information[7], "ties method: efron\\."
  )
  testthat::expect_null(summary_fit$bootstrap)

  printed <- capture.output(print(summary_fit))
  population_output <- printed[
    seq_len(match(TRUE, grepl("TSEsimp reporting checklist", printed)) - 1L)
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
  testthat::expect_true(all(vapply(
    unlist(summary_fit$estimates[c("estimate", "lower", "upper")]),
    function(value) any(grepl(
      formatC(value, format = "f", digits = 3), fixed_precision, fixed = TRUE
    )), logical(1)
  )))
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
  testthat::expect_true(any(grepl(hr_ci, printed, fixed = TRUE)))
  testthat::expect_true(any(grepl("TSEsimp reporting checklist", printed)))
  testthat::expect_true(any(grepl(
    "Missing AFT-model predictors before complete-case filtering", printed,
    fixed = TRUE
  )))
  headings <- c("Analysis population", "TSEs1:", "TSEs2:",
                "AFT model parameter estimates", "TSEs6:",
                "Causal parameter estimates (", "TSEs7:",
                "Outcome model parameter estimates")
  heading_positions <- vapply(headings, function(heading) {
    match(TRUE, grepl(heading, printed, fixed = TRUE))
  }, integer(1))
  testthat::expect_true(all(diff(heading_positions) > 0L))
  testthat::expect_true(
    match(TRUE, grepl("Adjusted hazard ratio", printed, fixed = TRUE)) >
      match(TRUE, grepl("TSEs7:", printed, fixed = TRUE))
  )
  for (estimates in list(summary_fit$aft_estimates,
                         summary_fit$outcome_estimates)) {
    testthat::expect_true(all(vapply(
      unlist(estimates[c("coef", "exp(coef)", "se(coef)")]), function(value) {
        any(grepl(formatC(value, format = "f", digits = 4), printed,
                  fixed = TRUE))
      }, logical(1)
    )))
    testthat::expect_true(all(vapply(estimates$z, function(value) {
      any(grepl(formatC(value, format = "f", digits = 3), printed,
                fixed = TRUE))
    }, logical(1))))
  }
  extreme_summary <- summary_fit
  extreme_summary$aft_estimates <- extreme_summary$aft_estimates[
    rep(1L, 2L), , drop = FALSE
  ]
  extreme_summary$aft_estimates$p <- c(0, 1)
  extreme_summary$outcome_estimates <- extreme_summary$outcome_estimates[
    rep(1L, 2L), , drop = FALSE
  ]
  extreme_summary$outcome_estimates$p <- c(0, 1)
  extreme_printed <- capture.output(print(extreme_summary))
  testthat::expect_true(any(grepl("<.0001", extreme_printed, fixed = TRUE)))
  testthat::expect_true(any(grepl(">.9999", extreme_printed, fixed = TRUE)))
})


testthat::test_that("tsesimp: boot", {
  # modify pd and dpd based on co and dco
  shilong <- shilong %>%
    mutate(dpd = ifelse(co & !pd, dco, dpd),
           pd = ifelse(co & !pd, 1, pd)) %>%
    mutate(dpd = ifelse(pd & co & dco < dpd, dco, dpd))
  
  # the eventual survival time
  shilong1 <- shilong %>%
    arrange(bras.f, id, tstop) %>%
    group_by(bras.f, id) %>%
    slice(n()) %>%
    select(-c("ps", "ttc", "tran"))
  
  # the last value of time-dependent covariates before pd
  shilong2 <- shilong %>%
    filter(pd == 0 | tstart <= dpd) %>%
    arrange(bras.f, id, tstop) %>%
    group_by(bras.f, id) %>%
    slice(n()) %>%
    select(bras.f, id, ps, ttc, tran)
  
  # combine baseline and time-dependent covariates
  shilong3 <- shilong1 %>%
    left_join(shilong2, by = c("bras.f", "id"))
  
  fit2 <- tsesimp(
    data = shilong3, id = "id", time = "tstop", event = "event",
    treat = "bras.f", censor_time = "dcut", pd = "pd",
    pd_time = "dpd", swtrt = "co", swtrt_time = "dco",
    base_cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c",
                 "pathway.f"),
    base2_cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c",
                  "pathway.f", "ps", "ttc", "tran"),
    aft_dist = "weibull", alpha = 0.05,
    recensor = TRUE, swtrt_control_only = FALSE, offset = 1,
    boot = TRUE, n_boot = 1000, seed = 0)
  
  hr2 <- c(0.9125816, 0.5540115, 1.5032271)
  testthat::expect_equal(hr2, round(c(fit2$hr, fit2$hr_CI), 7))
})
