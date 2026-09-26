library(survival)

testthat::test_that("kmest: estimate and standard error", {
  df1 <- kmest(aml, stratum = "x", time = "time", event = "status",
               conftype = "none")

  df2 <- summary(survfit(Surv(time, status) ~ x, data = aml,
                         conf.type = "none"))

  testthat::expect_equal(df1$surv[df1$time > 0], df2$surv)
  testthat::expect_equal(df1$sesurv[df1$time > 0], df2$std.err)
})


testthat::test_that("kmest: confidence interval", {
  for (conftype in c("plain", "log", "log-log", "arcsin")) {
    df1 <- kmest(aml, stratum = "x", time = "time", event = "status",
                 conftype = conftype)

    df2 <- summary(survfit(Surv(time, status) ~ x, data = aml,
                           conf.type = conftype))

    testthat::expect_equal(df1$lower[df1$time > 0], df2$lower)
    testthat::expect_equal(df1$upper[df1$time > 0], df2$upper)
  }
})


testthat::test_that("kmest: subjects remain at risk at time zero", {
  df <- data.frame(time = c(0, 1, 2), event = c(1, 0, 0))

  result <- kmest(df, time = "time", event = "event", conftype = "none")

  testthat::expect_equal(result$nrisk[1], 3)
  testthat::expect_equal(result$nevent[1], 1)
  testthat::expect_equal(result$surv[1], 2 / 3)
  testthat::expect_true(all(is.finite(result$surv)))
})


