library(testthat)
library(pmpackage)

# ---------------------------------------------------------------------------
# logit / inv_logit
# ---------------------------------------------------------------------------

test_that("logit and inv_logit are inverses of each other", {
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  expect_equal(inv_logit(logit(probs)), probs, tolerance = 1e-9)
})

test_that("logit(0.5) == 0", {
  expect_equal(logit(0.5), 0)
})

test_that("logit applies ceiling and floor for edge cases", {
  expect_true(is.finite(logit(0)))
  expect_true(is.finite(logit(1)))
})

test_that("logit skips ceiling/floor when use_ceiling_and_floor = FALSE", {
  expect_equal(logit(0.5, use_ceiling_and_floor = FALSE), 0)
  expect_true(is.infinite(logit(0, use_ceiling_and_floor = FALSE)))
})

test_that("inv_logit(0) == 0.5", {
  expect_equal(inv_logit(0), 0.5)
})

test_that("inv_logit output is always in (0, 1)", {
  log_odds <- c(-10, -2, 0, 2, 10)
  results  <- inv_logit(log_odds)
  expect_true(all(results > 0 & results < 1))
})

# ---------------------------------------------------------------------------
# pm_modelling_iv
# ---------------------------------------------------------------------------

make_binary_data <- function(n = 200, seed = 42) {
  set.seed(seed)
  dv <- rbinom(n, 1, 0.5)
  iv <- rnorm(n)
  list(dv = dv, iv = iv)
}

test_that("pm_modelling_iv returns the expected list elements", {
  d   <- make_binary_data()
  res <- pm_modelling_iv(dv = d$dv, iv = d$iv)

  expect_named(res, c("independent_varname", "dependent_varname", "infoval_df", "info_val"))
  expect_true(is.numeric(res$info_val))
  expect_true(is.data.frame(res$infoval_df))
})

test_that("pm_modelling_iv info_val is non-negative for a continuous predictor", {
  d   <- make_binary_data()
  res <- pm_modelling_iv(dv = d$dv, iv = d$iv)
  expect_gte(res$info_val, 0)
})

test_that("pm_modelling_iv infoval_df contains required columns", {
  d   <- make_binary_data()
  res <- pm_modelling_iv(dv = d$dv, iv = d$iv)
  expected_cols <- c("grouped_var", "num", "event", "nonevent",
                     "pct_event", "pct_nonevent", "woe", "info_val")
  for (col in expected_cols) {
    expect_true(col %in% names(res$infoval_df),
                info = paste("Missing column:", col))
  }
})

test_that("pm_modelling_iv handles factor (categorical) input", {
  set.seed(1)
  dv <- rbinom(100, 1, 0.5)
  iv <- sample(c("A", "B", "C"), 100, replace = TRUE)
  res <- pm_modelling_iv(dv = dv, iv = iv)
  expect_true(is.numeric(res$info_val))
})

# ---------------------------------------------------------------------------
# pm_modelling_report_all_info_vals
# ---------------------------------------------------------------------------

test_that("pm_modelling_report_all_info_vals returns one row per column and is sorted", {
  set.seed(7)
  df <- data.frame(
    dv = rbinom(200, 1, 0.5),
    x1 = rnorm(200),
    x2 = runif(200)
  )
  res <- pm_modelling_report_all_info_vals(df, dependent_var = "dv")

  expect_true(is.data.frame(res))
  # Should have a row for every column in df
  expect_equal(nrow(res), ncol(df))
  # Should be sorted descending by info_val
  expect_true(all(diff(res$info_val) <= 0))
})

# ---------------------------------------------------------------------------
# pm_modelling_calc_gini
# ---------------------------------------------------------------------------

test_that("pm_modelling_calc_gini returns list with gini_df, auc, gini", {
  set.seed(99)
  df <- data.frame(pred = runif(100), true = rbinom(100, 1, 0.5))
  res <- pm_modelling_calc_gini(df, predvar = "pred", truevar = "true")

  expect_named(res, c("gini_df", "auc", "gini"))
  expect_true(is.numeric(res$auc))
  expect_true(is.numeric(res$gini))
})

test_that("pm_modelling_calc_gini returns AUC near 1 for perfect predictor", {
  df <- data.frame(
    true = c(rep(1, 50), rep(0, 50)),
    pred = c(seq(0.51, 1, length.out = 50), seq(0, 0.49, length.out = 50))
  )
  res <- pm_modelling_calc_gini(df, predvar = "pred", truevar = "true")
  expect_gt(res$auc, 0.9)
})

test_that("pm_modelling_calc_gini AUC is between 0 and 1", {
  set.seed(5)
  df <- data.frame(pred = runif(200), true = rbinom(200, 1, 0.5))
  res <- pm_modelling_calc_gini(df, predvar = "pred", truevar = "true")
  expect_gte(res$auc, 0)
  expect_lte(res$auc, 1)
})

test_that("pm_modelling_calc_gini auto-corrects negatively correlated predictions", {
  # Perfect negative predictor — score should still be high after flip
  df <- data.frame(
    true = c(rep(1, 50), rep(0, 50)),
    pred = c(seq(0, 0.49, length.out = 50), seq(0.51, 1, length.out = 50))
  )
  res <- pm_modelling_calc_gini(df, predvar = "pred", truevar = "true")
  expect_gt(res$auc, 0.9)
})

# ---------------------------------------------------------------------------
# pm_modelling_compare_player_opponent
# ---------------------------------------------------------------------------

test_that("pm_modelling_compare_player_opponent adds ratio, difference, log-ratio", {
  df <- data.frame(player_elo = c(1600, 1400), opponent_elo = c(1400, 1600))
  res <- pm_modelling_compare_player_opponent(df, varname = "elo")

  expect_true("elo_ratio"      %in% names(res))
  expect_true("elo_difference" %in% names(res))
  expect_true("elo_ratio_ln"   %in% names(res))
})

test_that("pm_modelling_compare_player_opponent computes values correctly", {
  df <- data.frame(player_elo = 200, opponent_elo = 100)
  res <- pm_modelling_compare_player_opponent(df, varname = "elo")

  expect_equal(res$elo_ratio[1],      2,        tolerance = 1e-9)
  expect_equal(res$elo_difference[1], 100,      tolerance = 1e-9)
  expect_equal(res$elo_ratio_ln[1],   log(2),   tolerance = 1e-9)
})
