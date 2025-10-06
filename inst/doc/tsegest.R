## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(trtswitch)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

## ----data---------------------------------------------------------------------
sim1 <- tsegestsim(
  n = 500, allocation1 = 2, allocation2 = 1, pbprog = 0.5, 
  trtlghr = -0.5, bprogsl = 0.3, shape1 = 1.8, 
  scale1 = 360, shape2 = 1.7, scale2 = 688, 
  pmix = 0.5, admin = 5000, pcatnotrtbprog = 0.5, 
  pcattrtbprog = 0.25, pcatnotrt = 0.2, pcattrt = 0.1, 
  catmult = 0.5, tdxo = 1, ppoor = 0.1, pgood = 0.04, 
  ppoormet = 0.4, pgoodmet = 0.2, xomult = 1.4188308, 
  milestone = 546, outputRawDataset = 1, seed = 2000)

## ----analysis-----------------------------------------------------------------
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
  ns_df = 3, strata_main_effect_only = TRUE,
  recensor = TRUE, admin_recensor_only = TRUE, 
  swtrt_control_only = TRUE, alpha = 0.05, ties = "efron", 
  tol = 1.0e-6, offset = 0, boot = FALSE)

## ----switch time points-------------------------------------------------------
switched <- fit1$analysis_switch$data_switch[[1]]$data %>% 
  filter(swtrt == 1)
table(switched$swtrt_time)

## ----km-----------------------------------------------------------------------
ggplot(fit1$analysis_switch$km_switch[[1]]$data %>% 
         filter(nevent > 0), 
       aes(x=time, y=survival)) + 
  geom_step() + 
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_continuous(breaks = seq(0,105,21)) + 
  xlab("time from progression to switch") + 
  ggtitle(paste("trtrand: ", 
                fit1$analysis_switch$km_switch[[1]]$trtrand)) + 
  theme(panel.grid.minor.x = element_blank())

## ----logis--------------------------------------------------------------------
parest <- fit1$analysis_switch$fit_logis[[1]]$fit$parest
parest[, c("param", "beta", "sebeta", "z")]

## ----psi estimates------------------------------------------------------------
c(fit1$psi, fit1$psi_CI)

## ----Z(psi)-------------------------------------------------------------------
psi_CI_width <- fit1$psi_CI[2] - fit1$psi_CI[1]

ggplot(fit1$analysis_switch$eval_z[[1]]$data %>% 
         filter(psi > fit1$psi_CI[1] - psi_CI_width*0.25 & 
                  psi < fit1$psi_CI[2] + psi_CI_width*0.25), 
       aes(x=psi, y=Z)) + 
  geom_line() + 
  geom_hline(yintercept = c(0, -1.96, 1.96), linetype = 2) + 
  scale_y_continuous(breaks = c(0, -1.96, 1.96)) + 
  geom_vline(xintercept = c(fit1$psi, fit1$psi_CI), linetype = 2) + 
  scale_x_continuous(breaks = round(c(fit1$psi, fit1$psi_CI), 3)) + 
  ylab("Wald Z for counterfactual") + 
  theme(panel.grid.minor = element_blank())

## ----cox----------------------------------------------------------------------
fit1$fit_outcome$parest[, c("param", "beta", "sebeta", "z")]
c(fit1$hr, fit1$hr_CI)

