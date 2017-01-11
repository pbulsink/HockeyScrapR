#' testthat
#' scrapeplinfo (1 of 4 players)
#' returns
#' 1 of four players' tables
#'
#' testthat
#' updatePlayerInfo (given old player) gets new player

context("Testing scraping player data")
source("./tests/testthat/helper-scrapePlayers.R")

test_that("getPlayerList", {
    player_list<-getPlayerList(sleep=0, letters='a')
    
    expect_named(player_list, pl_names)
    expect_equivalent(player_list[1,], pl_aa)
})

test_that("Players are properly scraped", {
    #Use this stable player list
    pchoice<-sample(1:10, 2)
    
    #ideally just sample 2 players, extract from player_stats
    pstats<-getPlayerStats(player_list[], sleep=10)
    
    
    expect_equivalent(pstats, player_stats)
})

test_that("", {
    
})