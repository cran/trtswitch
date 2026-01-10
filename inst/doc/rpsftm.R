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
head(immdef, 10)

## ----analysis-----------------------------------------------------------------
data <- immdef %>% mutate(rx = 1-xoyrs/progyrs)

fit1 <- rpsftm(
  data, id = "id", time = "progyrs", event = "prog", treat = "imm",
  rx = "rx", censor_time = "censyrs", gridsearch = FALSE, boot = FALSE)

## ----logrank------------------------------------------------------------------
paste0("P-value", " (", fit1$pvalue_type, "): ", formatC(fit1$pvalue, format = "f", digits = 4))

## ----psi----------------------------------------------------------------------
c(fit1$psi, fit1$psi_CI)

## ----Z(psi)-------------------------------------------------------------------
psi_CI_width <- fit1$psi_CI[2] - fit1$psi_CI[1]

ggplot(fit1$eval_z %>% 
         filter(psi > fit1$psi_CI[1] - psi_CI_width*0.25 & 
                  psi < fit1$psi_CI[2] + psi_CI_width*0.25), 
       aes(x=psi, y=Z)) + 
  geom_line() + 
  geom_hline(yintercept = c(0, -1.96, 1.96), linetype = 2) + 
  scale_y_continuous(breaks = c(0, -1.96, 1.96)) + 
  geom_vline(xintercept = c(fit1$psi, fit1$psi_CI), linetype = 2) + 
  scale_x_continuous(breaks = round(c(fit1$psi, fit1$psi_CI), 3)) + 
  ylab("log-rank Z") + 
  theme(panel.grid.minor = element_blank())

## ----km-----------------------------------------------------------------------
ggplot(fit1$kmstar, aes(x=time, y=surv, group=treated,
                        linetype=as.factor(treated))) + 
  geom_step() + 
  scale_linetype_discrete(name = "treated") + 
  scale_y_continuous(limits = c(0,1))

## ----hr-----------------------------------------------------------------------
c(fit1$hr, fit1$hr_CI)

