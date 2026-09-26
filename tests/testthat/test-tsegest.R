library(dplyr, warn.conflicts = FALSE)
library(survival)
library(splines)

testthat::test_that("tsegest: logistic g-estimation", {
  sim1 <- tsegestsim(
    n = 500, allocation1 = 2, allocation2 = 1, pbprog = 0.5, 
    trtlghr = -0.5, bprogsl = 0.3, shape1 = 1.8, 
    scale1 = 360, shape2 = 1.7, scale2 = 688, 
    pmix = 0.5, admin = 5000, pcatnotrtbprog = 0.5, 
    pcattrtbprog = 0.25, pcatnotrt = 0.2, pcattrt = 0.1, 
    catmult = 0.5, tdxo = 1, ppoor = 0.1, pgood = 0.04, 
    ppoormet = 0.4, pgoodmet = 0.2, xomult = 1.4188308, 
    milestone = 546, seed = 2000)
  
  data1 <- sim1$paneldata %>%
    mutate(visit7on = ifelse(progressed == 1, tstop > timePFSobs + 105, 0))
  
  fit1 <- tsegest(
    data = data1, id = "id", 
    tstart = "tstart", tstop = "tstop", event = "event", 
    treat = "trtrand", censor_time = "censor_time", 
    pd = "progressed", pd_time = "timePFSobs", 
    swtrt = "xo", swtrt_time = "xotime", 
    base_cov = "bprog", 
    conf_cov = c("bprog*cattdc", "timePFSobs", "visit7on"), 
    ns_df = 3, recensor = TRUE, admin_recensor_only = TRUE, 
    swtrt_control_only = TRUE, gridsearch = FALSE,
    alpha = 0.05, ties = "efron", 
    tol = 1.0e-6, offset = 0, boot = FALSE)
  
  # last observation within each subject
  data2 <- data1 %>%
    group_by(id) %>%
    slice(n()) %>%
    mutate(ostime = tstop, os = event) %>% 
    select(id, trtrand, progressed, timePFSobs, os, ostime, 
           censor_time, xo, xotime, bprog)
  
  data1 <- data1 %>% 
    left_join(data2 %>% select(id, os, ostime), by = "id")
  
  # post progression data up to switching 
  data3 <- data1 %>%
    filter(trtrand == 0 & progressed == 1 & tstop >= timePFSobs & 
             ifelse(xo == 1, tstart < xotime, tstop < ostime)) %>%
    mutate(y = ifelse(xo == 1 & tstop >= xotime, 1, 0))
  
  s1 <- ns(data3$tstop[data3$y == 1], df = 3)
  s2 <- ns(data3$tstop, knots = attr(s1, "knots"), 
           Boundary.knots = attr(s1, "Boundary.knots"))
  data3$ns1 <- s2[,1]
  data3$ns2 <- s2[,2]
  data3$ns3 <- s2[,3]
  
  # re-baseline
  data4 <- data3 %>% 
    group_by(id) %>%
    slice(1) %>%
    mutate(ostime = ostime - timePFSobs,
           censor_time = censor_time - timePFSobs,
           xotime = xotime - timePFSobs) %>%
    ungroup()
  
  f <- function(psi) {
    data5 <- data4 %>%
      mutate(u_star = ifelse(xo == 1, xotime + (ostime - xotime)*exp(psi), 
                             ostime), 
             c_star = pmin(censor_time, censor_time*exp(psi)),
             t_star = pmin(u_star, c_star),
             d_star = ifelse(c_star < u_star, 0, os))
    
    fit_cox <- coxph(Surv(t_star, d_star) ~ 1, data = data5)
    resid <- fit_cox$residuals
    
    data6 <- data3 %>%
      left_join(data5 %>% 
                  mutate(resid = resid) %>%
                  select(id, resid), by = "id")
    
    fit_lgs <- logisregr(data6, event = "y", 
                         covariates = c("resid", "bprog*cattdc", 
                                        "timePFSobs", "visit7on", 
                                        "ns1", "ns2", "ns3"),
                         id = "id", robust = 1)
    
    z_lgs <- fit_lgs$parest$z
    
    as.numeric(z_lgs[2])
  }
  
  psi <- uniroot(f, c(-2,2), tol = 1.0e-6)$root
  
  data7 <- data2 %>%
    filter(trtrand == 0) %>%
    mutate(u_star = ifelse(xo == 1, xotime + (ostime - xotime)*exp(psi), 
                           ostime), 
           c_star = pmin(censor_time, censor_time*exp(psi)),
           t_star = pmin(u_star, c_star),
           d_star = ifelse(c_star < u_star, 0, os)) %>%
    select(id, t_star, d_star, trtrand, bprog) %>%
    bind_rows(data2 %>% 
                filter(trtrand == 1) %>%
                mutate(t_star = ostime, d_star = os) %>%
                select(id, t_star, d_star, trtrand, bprog))
  
  fit <- coxph(Surv(t_star, d_star) ~ trtrand + bprog, 
               data = data7, ties = "efron")
  
  hr1 <- as.numeric(exp(cbind(fit$coefficients, confint(fit)))["trtrand",])
  testthat::expect_equal(hr1, c(fit1$hr, fit1$hr_CI))
})

testthat::test_that(
  "tsegest: summary reports record-based analysis population", {
  sim1 <- tsegestsim(
    n = 50, allocation1 = 2, allocation2 = 1, pbprog = 0.5,
    trtlghr = -0.5, bprogsl = 0.3, shape1 = 1.8, scale1 = 360,
    shape2 = 1.7, scale2 = 688, pmix = 0.5, admin = 5000,
    pcatnotrtbprog = 0.5, pcattrtbprog = 0.25, pcatnotrt = 0.2,
    pcattrt = 0.1, catmult = 0.5, tdxo = 1, ppoor = 0.1,
    pgood = 0.04, ppoormet = 0.4, pgoodmet = 0.2,
    xomult = 1.4188308, milestone = 546, seed = 1001)

  data1 <- sim1$paneldata %>%
    mutate(visit7on = ifelse(progressed == 1, tstop > timePFSobs + 105, 0))

  fit1 <- tsegest(
    data = data1, id = "id",
    tstart = "tstart", tstop = "tstop", event = "event",
    treat = "trtrand", censor_time = "censor_time",
    pd = "progressed", pd_time = "timePFSobs",
    swtrt = "xo", swtrt_time = "xotime",
    base_cov = "bprog",
    conf_cov = c("bprog*cattdc", "timePFSobs", "visit7on"),
    ns_df = 3, recensor = TRUE, admin_recensor_only = TRUE,
    swtrt_control_only = TRUE, gridsearch = FALSE,
    alpha = 0.05, ties = "efron",
    tol = 1.0e-6, offset = 0, boot = FALSE)

  summary_fit <- summary(fit1)
  out <- capture.output(print(summary_fit))
  population_output <- out[
    seq_len(match(TRUE, grepl("TSEgest reporting checklist", out)) - 1L)
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
  testthat::expect_true(any(grepl(hr_ci, out, fixed = TRUE)))
  testthat::expect_identical(
    summary_fit$estimation$value[
      summary_fit$estimation$item == "Selected control-arm psi"
    ],
    formatC(fit1$psi, format = "f", digits = 3)
  )
  testthat::expect_identical(
    summary_fit$estimation$value[
      summary_fit$estimation$item == "Selected treatment-arm psi"
    ],
    "Not applicable"
  )
  testthat::expect_true(all(c(
    "estimation", "estimates", "switching_estimates", "outcome_estimates",
    "missing_switch_predictors", "reporting"
  ) %in% names(summary_fit)))
  testthat::expect_true("switch_missing_summary" %in% names(fit1))
  testthat::expect_identical(
    names(summary_fit$missing_switch_predictors),
    c("trtrand", "predictor", "missing", "total", "missing_pct")
  )
  testthat::expect_identical(summary_fit$reporting$item, paste0("TSEg", 1:14))
  testthat::expect_identical(
    names(summary_fit$switching_estimates),
    c("trtrand", "param", "coef", "exp(coef)", "se(coef)", "z", "p")
  )
  testthat::expect_identical(
    names(summary_fit$outcome_estimates),
    c("param", "coef", "exp(coef)", "se(coef)", "z", "p")
  )
  testthat::expect_match(summary_fit$reporting$information[3], "p_km_switch")
  testthat::expect_match(
    summary_fit$reporting$information[3],
    "all patients with a secondary baseline, regardless of switching status"
  )
  testthat::expect_match(
    summary_fit$reporting$information[3],
    "switching is the event and patients who do not switch are censored"
  )
  testthat::expect_match(summary_fit$reporting$information[10], "p_z")
  testthat::expect_match(
    summary_fit$reporting$information[5],
    paste0("time-varying predictors: Detected: cattdc, ",
           "bprog\\.cattdc, visit7on\\.")
  )
  testthat::expect_match(
    summary_fit$reporting$information[6], "conf_cov missingness"
  )
  testthat::expect_match(
    summary_fit$reporting$information[9], "root-finding method brent"
  )
  testthat::expect_match(
    summary_fit$reporting$information[12], "ties method: efron\\."
  )
  grid_summary <- summary_fit
  grid_summary$reporting$information[9] <- sub(
    "Root finding \\(brent\\)", "Grid search with linear interpolation",
    grid_summary$reporting$information[9]
  )
  grid_summary$reporting$information[9] <- sub(
    " with root-finding method brent", "", 
    grid_summary$reporting$information[9]
  )
  testthat::expect_match(
    grid_summary$reporting$information[9], 
    "Grid search with linear interpolation"
  )
  testthat::expect_false(grepl("root-finding method", 
                               grid_summary$reporting$information[9]))
  testthat::expect_true(any(grepl("TSEgest reporting checklist", out)))
  testthat::expect_true(any(grepl(
    "Missing switching-model predictors before complete-case filtering", out,
    fixed = TRUE
  )))
  headings <- c("Analysis population", "TSEg1:", "TSEg4:",
                "Switching model parameter estimates", "TSEg9:",
                "Estimation procedure", "TSEg11:",
                "Causal parameter estimates (", "TSEg12:",
                "Outcome model parameter estimates")
  heading_positions <- vapply(headings, function(heading) {
    match(TRUE, grepl(heading, out, fixed = TRUE))
  }, integer(1))
  testthat::expect_true(all(diff(heading_positions) > 0L))
})


