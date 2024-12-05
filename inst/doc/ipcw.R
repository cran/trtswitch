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
sim1 <- tsegestsim(
  n = 500, allocation1 = 2, allocation2 = 1, pbprog = 0.5, 
  trtlghr = -0.5, bprogsl = 0.3, shape1 = 1.8, 
  scale1 = 0.000025, shape2 = 1.7, scale2 = 0.000015, 
  pmix = 0.5, admin = 5000, pcatnotrtbprog = 0.5, 
  pcattrtbprog = 0.25, pcatnotrt = 0.2, pcattrt = 0.1, 
  catmult = 0.5, tdxo = 1, ppoor = 0.1, pgood = 0.04, 
  ppoormet = 0.4, pgoodmet = 0.2, xomult = 1.4188308, 
  milestone = 546, swtrt_control_only = TRUE,
  outputRawDataset = 1, seed = 2000)

## ----analysis example 1-------------------------------------------------------
fit1 <- ipcw(
  sim1$paneldata, id = "id", tstart = "tstart", 
  tstop = "tstop", event = "died", treat = "trtrand", 
  swtrt = "xo", swtrt_time = "xotime", 
  swtrt_time_lower = "timePFSobs",
  swtrt_time_upper = "xotime_upper", base_cov = "bprog", 
  numerator = "bprog", denominator = "bprog*catlag", 
  logistic_switching_model = TRUE, ns_df = 3,
  relative_time = TRUE, swtrt_control_only = TRUE, 
  boot = FALSE)

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

