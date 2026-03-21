library(testthat)
library(pmpackage)

# ---------------------------------------------------------------------------
# pm_eloPrepDatabase
# ---------------------------------------------------------------------------

test_that("pm_eloPrepDatabase creates new dicts with correct defaults", {
  res <- pm_eloPrepDatabase(unseenDataPlayers = c("alice", "bob"))
  expect_true(inherits(res$eloDB, "Dict"))
  expect_true(inherits(res$matchDB, "Dict"))
  expect_equal(res$eloDB$get("alice"), 1500)
  expect_equal(res$eloDB$get("bob"),   1500)
  expect_equal(res$matchDB$get("alice"), 0)
  expect_equal(res$matchDB$get("bob"),   0)
})

test_that("pm_eloPrepDatabase respects custom defaultScore and defaultMatches", {
  res <- pm_eloPrepDatabase(
    unseenDataPlayers = c("charlie"),
    defaultScore = 1600,
    defaultMatches = 10
  )
  expect_equal(res$eloDB$get("charlie"),   1600)
  expect_equal(res$matchDB$get("charlie"), 10)
})

test_that("pm_eloPrepDatabase adds only new players when passed existing dicts", {
  res1 <- pm_eloPrepDatabase(unseenDataPlayers = c("alice"))
  res2 <- pm_eloPrepDatabase(
    eloDB    = res1$eloDB,
    matchDB  = res1$matchDB,
    unseenDataPlayers = c("alice", "bob")  # alice already exists
  )
  # alice should keep her original score
  expect_equal(res2$eloDB$get("alice"), 1500)
  # bob is new — should be initialised at default
  expect_equal(res2$eloDB$get("bob"),  1500)
})

# ---------------------------------------------------------------------------
# pm_eloShowWinProbForScoreDiff
# ---------------------------------------------------------------------------

test_that("pm_eloShowWinProbForScoreDiff returns 0.5 for equal ratings", {
  expect_equal(pm_eloShowWinProbForScoreDiff(0), 0.5)
})

test_that("pm_eloShowWinProbForScoreDiff returns >0.5 for positive diff", {
  expect_gt(pm_eloShowWinProbForScoreDiff(200), 0.5)
})

test_that("pm_eloShowWinProbForScoreDiff returns <0.5 for negative diff", {
  expect_lt(pm_eloShowWinProbForScoreDiff(-200), 0.5)
})

test_that("pm_eloShowWinProbForScoreDiff is symmetric", {
  p_pos <- pm_eloShowWinProbForScoreDiff(400)
  p_neg <- pm_eloShowWinProbForScoreDiff(-400)
  expect_equal(p_pos + p_neg, 1, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# pm_eloRunTimeSlice
# ---------------------------------------------------------------------------

make_simple_db <- function(players) {
  pm_eloPrepDatabase(unseenDataPlayers = players)
}

test_that("pm_eloRunTimeSlice returns the expected output structure", {
  res_db  <- make_simple_db(c("alice", "bob"))
  match_df <- tibble::tribble(
    ~player_name, ~opponent_name, ~match_date,            ~actualResult,
    "alice",      "bob",          as.Date("2020-01-01"),  1L
  )

  res <- pm_eloRunTimeSlice(res_db$eloDB, res_db$matchDB, match_df)

  expect_named(res, c("eloDB", "matchDB", "simDF"))
  expected_cols <- c(
    "elo_player_prior_elo", "elo_opponent_prior_elo",
    "elo_player_prior_matches", "elo_opponent_prior_matches",
    "elo_player_pred_score", "elo_Kfactor_player", "elo_Kfactor_opponent",
    "elo_pchange", "elo_player_post_elo", "elo_opponent_post_elo",
    "elo_player_post_matches", "elo_opponent_post_matches",
    "elo_predictedResult", "elo_accuratePred"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(res$simDF),
                info = paste("Missing column:", col))
  }
})

test_that("pm_eloRunTimeSlice starts both players at 1500 when equal", {
  res_db   <- make_simple_db(c("alice", "bob"))
  match_df <- tibble::tribble(
    ~player_name, ~opponent_name, ~match_date,            ~actualResult,
    "alice",      "bob",          as.Date("2020-01-01"),  1L
  )

  res <- pm_eloRunTimeSlice(res_db$eloDB, res_db$matchDB, match_df)

  expect_equal(res$simDF$elo_player_prior_elo[1],   1500)
  expect_equal(res$simDF$elo_opponent_prior_elo[1], 1500)
})

test_that("pm_eloRunTimeSlice updates ELO — winner gains, loser loses", {
  res_db   <- make_simple_db(c("alice", "bob"))
  match_df <- tibble::tribble(
    ~player_name, ~opponent_name, ~match_date,            ~actualResult,
    "alice",      "bob",          as.Date("2020-01-01"),  1L
  )

  res <- pm_eloRunTimeSlice(res_db$eloDB, res_db$matchDB, match_df)

  expect_gt(res$simDF$elo_player_post_elo[1],   1500)  # alice won → higher
  expect_lt(res$simDF$elo_opponent_post_elo[1], 1500)  # bob lost  → lower
})

test_that("pm_eloRunTimeSlice increments match counts by 1", {
  res_db   <- make_simple_db(c("alice", "bob"))
  match_df <- tibble::tribble(
    ~player_name, ~opponent_name, ~match_date,            ~actualResult,
    "alice",      "bob",          as.Date("2020-01-01"),  1L
  )

  res <- pm_eloRunTimeSlice(res_db$eloDB, res_db$matchDB, match_df)

  expect_equal(res$simDF$elo_player_post_matches[1],   1)
  expect_equal(res$simDF$elo_opponent_post_matches[1], 1)
})

test_that("pm_eloRunTimeSlice errors when actualResult has NAs", {
  res_db   <- make_simple_db(c("alice", "bob"))
  match_df <- tibble::tribble(
    ~player_name, ~opponent_name, ~match_date,            ~actualResult,
    "alice",      "bob",          as.Date("2020-01-01"),  NA_integer_
  )

  expect_error(pm_eloRunTimeSlice(res_db$eloDB, res_db$matchDB, match_df))
})

# ---------------------------------------------------------------------------
# pm_eloExtractDatabaseFromDataframe  (tests the typo-fix: plalyer_name)
# ---------------------------------------------------------------------------

test_that("pm_eloExtractDatabaseFromDataframe recovers correct ELO scores", {
  res_db   <- make_simple_db(c("alice", "bob"))
  match_df <- tibble::tribble(
    ~player_name, ~opponent_name, ~match_date,            ~actualResult,
    "alice",      "bob",          as.Date("2020-01-01"),  1L
  )

  run1 <- pm_eloRunTimeSlice(res_db$eloDB, res_db$matchDB, match_df)

  # Extract DB from the resulting dataframe
  recovered <- pm_eloExtractDatabaseFromDataframe(run1$simDF)
  expect_true(inherits(recovered$eloDB, "Dict"))

  alice_from_db   <- run1$eloDB$get("alice")
  alice_recovered <- recovered$eloDB$get("alice")
  expect_equal(alice_from_db, alice_recovered, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# pm_eloRunMultipleTimeSlices
# ---------------------------------------------------------------------------

test_that("pm_eloRunMultipleTimeSlices processes multiple dates in order", {
  players <- c("alice", "bob", "charlie")
  res_db  <- pm_eloPrepDatabase(unseenDataPlayers = players)

  match_df <- tibble::tribble(
    ~player_name, ~opponent_name, ~match_date,            ~actualResult,
    "alice",      "bob",          as.Date("2020-01-01"),  1L,
    "alice",      "charlie",      as.Date("2020-01-02"),  0L
  )

  res <- pm_eloRunMultipleTimeSlices(res_db$eloDB, res_db$matchDB, match_df)

  expect_named(res, c("eloDB", "matchDB", "simAllRes"))
  expect_equal(nrow(res$simAllRes), 2)
  # Dates should be processed in ascending order
  expect_true(res$simAllRes$match_date[1] <= res$simAllRes$match_date[2])
})
