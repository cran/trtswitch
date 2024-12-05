## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(trtswitch)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

## ----analysis-----------------------------------------------------------------
data <- immdef %>% mutate(rx = 1-xoyrs/progyrs)

fit1 <- ipe(
  data, time = "progyrs", event = "prog", treat = "imm", 
  rx = "rx", censor_time = "censyrs", aft_dist = "weibull",
  boot = FALSE)

## ----logrank------------------------------------------------------------------
fit1$logrank_pvalue

## ----psi----------------------------------------------------------------------
fit1$psi

## ----km-----------------------------------------------------------------------
ggplot(fit1$kmstar, aes(x=time, y=survival, group=treated,
                        linetype=as.factor(treated))) + 
  geom_step() + 
  scale_linetype_discrete(name = "treated") + 
  scale_y_continuous(limits = c(0,1))

## ----hr-----------------------------------------------------------------------
c(fit1$hr, fit1$hr_CI)

## ----SHIVA data---------------------------------------------------------------
shilong1 <- shilong %>%
  arrange(bras.f, id, tstop) %>%
  group_by(bras.f, id) %>%
  slice(n()) %>%
  select(-c("ps", "ttc", "tran"))

shilong2 <- shilong1 %>%
  mutate(rx = ifelse(co, ifelse(bras.f == "MTA", dco/ady, 
                                1 - dco/ady),
                     ifelse(bras.f == "MTA", 1, 0)),
         treated = 1*(bras.f == "MTA"))

## ----SHIVA analysis-----------------------------------------------------------
fit2 <- ipe(
  shilong2, time = "tstop", event = "event",
  treat = "bras.f", rx = "rx", censor_time = "dcut",
  base_cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c",
               "pathway.f"),
  aft_dist = "weibull", boot = FALSE)

## ----AFT update---------------------------------------------------------------
fit2$fit_aft$parest[, c("param", "beta", "sebeta", "z")]

## ----convergence issue--------------------------------------------------------

f <- function(psi) {
  data1 <- shilong2 %>%
    filter(treated == 0) %>%
    mutate(u_star = tstop*(1 - rx + rx*exp(psi)),
           c_star = pmin(dcut, dcut*exp(psi)),
           t_star = pmin(u_star, c_star),
           d_star = event*(u_star <= c_star)) %>%
    select(-c("u_star", "c_star")) %>%
    bind_rows(shilong2 %>%
                filter(treated == 1) %>%
                mutate(u_star = tstop*(rx + (1-rx)*exp(-psi)),
                       c_star = pmin(dcut, dcut*exp(-psi)),
                       t_star = pmin(u_star, c_star),
                       d_star = event*(u_star <= c_star)))
  
  fit_aft <- liferegr(data1, time = "t_star", event = "d_star", 
                      covariates = c("treated", "agerand", "sex.f", 
                                     "tt_Lnum", "rmh_alea.c", 
                                     "pathway.f"), 
                      dist = "weibull")
  
  -fit_aft$beta[2]
}

B <- 30
psihats <- rep(0, B)
psihats[1] <- fit2$psi
for (i in 2:B) {
  psihats[i] <- f(psihats[i-1])
}

data2 <- data.frame(index = 1:B, psi = psihats)
tail(data2)

## ----psi plot-----------------------------------------------------------------
ggplot(data2, aes(x = index, y = psi)) + 
  geom_point() + 
  geom_line()

