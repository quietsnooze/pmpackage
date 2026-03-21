library(testthat)
library(pmpackage)

# ---------------------------------------------------------------------------
# pm_dq_checkColumn
# ---------------------------------------------------------------------------

test_that("pm_dq_checkColumn returns expected names", {
  df  <- data.frame(x = c(1, 2, 3, NA, 5))
  res <- pm_dq_checkColumn(df, "x")

  expect_named(res, c("col", "class", "num", "numMissing",
                      "numInfinite", "avgVal", "minVal", "maxVal"))
})

test_that("pm_dq_checkColumn counts rows correctly", {
  df  <- data.frame(x = 1:10)
  res <- pm_dq_checkColumn(df, "x")
  expect_equal(res$num, 10)
})

test_that("pm_dq_checkColumn detects NAs in numeric column", {
  df  <- data.frame(x = c(1, NA, 3, NA))
  res <- pm_dq_checkColumn(df, "x")
  expect_equal(res$numMissing, 2)
})

test_that("pm_dq_checkColumn detects Inf values", {
  df  <- data.frame(x = c(1, Inf, 3, -Inf))
  res <- pm_dq_checkColumn(df, "x")
  expect_equal(res$numInfinite, 2)
  # Inf also counts in numMissing (treated as non-finite)
  expect_equal(res$numMissing, 2)
})

test_that("pm_dq_checkColumn computes correct mean, min, max", {
  df  <- data.frame(x = c(1, 2, 3, 4, 5))
  res <- pm_dq_checkColumn(df, "x")
  expect_equal(res$avgVal, 3)
  expect_equal(res$minVal, 1)
  expect_equal(res$maxVal, 5)
})

test_that("pm_dq_checkColumn handles character column — no numeric stats", {
  df  <- data.frame(name = c("alice", "bob", "charlie"), stringsAsFactors = FALSE)
  res <- pm_dq_checkColumn(df, "name")

  expect_equal(res$num, 3)
  expect_true(is.na(res$avgVal))
  expect_true(is.na(res$minVal))
  expect_true(is.na(res$maxVal))
})

test_that("pm_dq_checkColumn handles Date column as numeric", {
  df  <- data.frame(d = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01")))
  res <- pm_dq_checkColumn(df, "d")

  expect_equal(res$num, 3)
  expect_equal(res$numMissing, 0)
})

# ---------------------------------------------------------------------------
# pm_dq_check_all_columns
# ---------------------------------------------------------------------------

test_that("pm_dq_check_all_columns returns one row per column", {
  df  <- data.frame(a = 1:5, b = letters[1:5], c = as.numeric(1:5),
                    stringsAsFactors = FALSE)
  res <- pm_dq_check_all_columns(df)

  expect_equal(nrow(res), ncol(df))
})

test_that("pm_dq_check_all_columns column names match df column names", {
  df  <- data.frame(score = c(10, 20, 30), label = c("x", "y", "z"),
                    stringsAsFactors = FALSE)
  res <- pm_dq_check_all_columns(df)

  expect_equal(as.character(res$col), names(df))
})

test_that("pm_dq_check_all_columns works on a single-column dataframe", {
  df  <- data.frame(solo = c(1.5, 2.5, NA))
  res <- pm_dq_check_all_columns(df)

  expect_equal(nrow(res), 1)
  expect_equal(res$numMissing, 1)
})
