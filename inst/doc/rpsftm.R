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
  data, time = "progyrs", event = "prog", treat = "imm",
  rx = "rx", censor_time = "censyrs", boot = FALSE)

## ----logrank------------------------------------------------------------------
fit1$logrank_pvalue

## ----psi----------------------------------------------------------------------
c(fit1$psi, fit1$psi_CI)

## ----hr-----------------------------------------------------------------------
c(fit1$hr, fit1$hr_CI)

