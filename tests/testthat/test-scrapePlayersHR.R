#' testthat
#' updatePlayerInfo (given old player) gets new player

context("Testing scraping player data from HR")
source("helper-scrapePlayersHR.R")

test_that("getPlayerList from HR returns ok", {
  player_list <- getPlayerList.HR(sleep = 0, letters = "a")

  expect_named(player_list, pl_names)
  expect_true(player_list[1, ]$Name == "Antti Aalto")
})

test_that("Players are properly scraped from HR", {
  pstats <- getPlayerStats.HR(player_list[c(1:6),], sleep = 10, progress=FALSE)

  expect_type(pstats, "list")
  expect_true(length(pstats) == 3)
  expect_true(nrow(pstats$PlayerMeta) == 3)
})

test_that("Scraping Player from HR by Alphabet works", {
  player_ab <- scrapeByAlphabet.HR(pl_ab, long_sleep = 0, sleep = 10, directory = "./", combine = FALSE, progress=FALSE)
  expect_true(player_ab)
  expect_true(file.exists("./players_a.RDS"))
  expect_true(file.exists("./players_b.RDS"))
  player_ab <- combinePlayerDataFrames.HR(directory = "./")

  expect_true(length(player_ab$PlayerMeta$Name) == 2)
  f1 <- paste0("HR_allPlayers-", Sys.Date(), ".RDS")
  expect_true(file.exists(f1))
  expect_false(file.exists("./players_a.RDS"))
  expect_false(file.exists("./players_b.RDS"))
  try_delete(f1)
})

test_that("HR Player Processing Works", {
  p_data <- processPlayerData.HR(player_stats)

  expect_type(p_data, "list")

  players <- p_data[[1]]
  goalies <- p_data[[2]]
  meta <- p_data[[3]]
  expect_named(p_data, proc_names[["lnames"]])
  expect_true(ncol(players) == 31)
  expect_true(ncol(goalies) == 28)
  expect_true(ncol(meta) == 19)
  expect_false("Active" %in% names(meta))
  expect_type(object = players$ATOI, "double")
})

test_that("HR Updates Work", {
  p_new <- updatePlayers.HR(pl_data_oldnew, data_dir = "./", player_list = pl_list_oldnew, sleep = 5,
    long_sleep = 0, progress=FALSE)

  p <- p_new[[1]]

  expect_equal(sum(p$Season == "2017-18"), 1)

  f1 <- paste0("./HR_allPlayers-", Sys.Date(), ".RDS")
  expect_true(file.exists(f1))
  try_delete(f1)
})
