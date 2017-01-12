#' testthat
#' updatePlayerInfo (given old player) gets new player

context("Testing scraping player data")
source("helper-scrapePlayers.R")

test_that("getPlayerList returns ok", {
  player_list <- getPlayerList(sleep = 0, letters = "a")

  expect_named(player_list, pl_names)
  expect_equivalent(player_list[1, ], pl_aa)
})

test_that("Players are properly scraped", {
  pstats <- getPlayerStats(player_list[], sleep = 10)

  expect_type(pstats, "list")
  expect_equivalent(pstats, player_stats)
})

test_that("Scraping Player by Alphabet works", {
  player_ab <- scrapeByAlphabet(pl_ab, long_sleep = 0, sleep = 10, directory = "./", combine = FALSE)
  expect_true(player_ab)
  expect_true(file.exists("./players_a.RDS"))
  expect_true(file.exists("./players_b.RDS"))
  player_ab <- combinePlayerDataFrames(directory="./")

  expect_equivalent(player_ab, player_ab_data)
  expect_true(file.exists("./allPlayers.RDS"))
  expect_false(file.exists("./players_a.RDS"))
  expect_false(file.exists("./players_b.RDS"))
  try(file.remove("./allPlayers.RDS"))
})

test_that("Player Processing Works", {
  p_data <- processPlayerData(player_stats)

  expect_type(p_data, "list")
  #expect_equivalent(p_data, processed_player_data)
})
