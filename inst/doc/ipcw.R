## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(trtswitch)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

## ----data example 1-----------------------------------------------------------
sim1 <- tssim(
  tdxo = 1, coxo = 1, allocation1 = 1, allocation2 = 1,
  p_X_1 = 0.3, p_X_0 = 0.3, 
  rate_T = 0.002, beta1 = -0.5, beta2 = 0.3, 
  gamma0 = 0.3, gamma1 = -0.9, gamma2 = 0.7, gamma3 = 1.1, gamma4 = -0.8,
  zeta0 = -3.5, zeta1 = 0.5, zeta2 = 0.2, zeta3 = -0.4, 
  alpha0 = 0.5, alpha1 = 0.5, alpha2 = 0.4, 
  theta1_1 = -0.4, theta1_0 = -0.4, theta2 = 0.2,
  rate_C = 0.0000855, accrualIntensity = 20/30, 
  followupTime = 600, fixedFollowup = 0, days = 30,
  n = 500, NSim = 100, seed = 314159)

## ----analysis example 1-------------------------------------------------------
fit1 <- ipcw(
  sim1[[1]], id = "id", tstart = "tstart", 
  tstop = "tstop", event = "Y", treat = "trtrand", 
  swtrt = "xo", swtrt_time = "xotime", 
  base_cov = "bprog", numerator = "bprog", 
  denominator = c("bprog", "L"),
  logistic_switching_model = TRUE, ns_df = 3,
  swtrt_control_only = TRUE, boot = FALSE)

## ----switching models example 1-----------------------------------------------
# denominator switching model fit
fit1$fit_switch[[1]]$fit_den$parest[, c("param", "beta", "sebeta", "z")]

# numerator switching model fit
fit1$fit_switch[[1]]$fit_num$parest[, c("param", "beta", "sebeta", "z")]

## ----weights example 1--------------------------------------------------------
# unstabilized weights
ggplot(fit1$data_outcome %>% filter(trtrand == 0), 
       aes(x = unstabilized_weight)) + 
  geom_density(fill="#77bd89", color="#1f6e34", alpha=0.8) +
  scale_x_continuous("unstabilized weights")

# stabilized weights
ggplot(fit1$data_outcome %>% filter(trtrand == 0), 
       aes(x = stabilized_weight)) + 
  geom_density(fill="#77bd89", color="#1f6e34", alpha=0.8) +
  scale_x_continuous("stabilized weights")

## ----cox example 1------------------------------------------------------------
fit1$fit_outcome$parest[, c("param", "beta", "sebeta", "z")]
    
exp(fit1$fit_outcome$parest[1, c("beta", "lower", "upper")])

## ----example 2----------------------------------------------------------------
fit2 <- ipcw(
  shilong, id = "id", tstart = "tstart", tstop = "tstop", 
  event = "event", treat = "bras.f", swtrt = "co", 
  swtrt_time = "dco", 
  base_cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c", 
               "pathway.f"),
  numerator = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c", 
                "pathway.f"),
  denominator = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c",
                  "pathway.f", "ps", "ttc", "tran"),
  swtrt_control_only = FALSE, boot = FALSE)

## ----switching models for control example 2-----------------------------------
# denominator switching model for the control group
fit2$fit_switch[[1]]$fit_den$parest[, c("param", "beta", "sebeta", "z")]

# numerator switching model for the control group
fit2$fit_switch[[1]]$fit_num$parest[, c("param", "beta", "sebeta", "z")]

## ----switching models for experimental example 2------------------------------
# denominator switching model for the experimental group
fit2$fit_switch[[2]]$fit_den$parest[, c("param", "beta", "sebeta", "z")]

# numerator switching model for the experimental group
fit2$fit_switch[[2]]$fit_num$parest[, c("param", "beta", "sebeta", "z")]

## ----weights example 2--------------------------------------------------------
# unstabilized weights
ggplot(fit2$data_outcome, aes(x = unstabilized_weight)) + 
  geom_density(fill="#77bd89", color="#1f6e34", alpha=0.8) + 
  scale_x_continuous("unstabilized weights") + 
  facet_wrap(~treated) 

# stabilized weights
ggplot(fit2$data_outcome, aes(x = stabilized_weight)) + 
  geom_density(fill="#77bd89", color="#1f6e34", alpha=0.8) + 
  scale_x_continuous("stabilized weights") + 
  facet_wrap(~treated)

## ----cox example 2------------------------------------------------------------
fit2$fit_outcome$parest[, c("param", "beta", "sebeta", "z")]

c(fit2$hr, fit2$hr_CI)

