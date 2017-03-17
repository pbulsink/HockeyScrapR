#' testthat

context("Testing scraping player data for QuantHockey")
source("helper-scrapePlayersQH.R")

test_that("getPlayerList returns ok", {
    pl <- getPlayerList.QH()
    expect_equal(dim(pl)[1], 50000)
    expect_equal(dim(pl)[2], 3)
    expect_named(pl, pl_names.QH)
    expect_equal(sum(pl$Exists), 50000)

    pl2 <- getPlayerList.QH(prebuilt  = TRUE)
    expect_equal(dim(pl2)[2], 3)
    expect_named(pl2, pl_names.QH)
    expect_lt(sum(pl2$Exists), 50000)
})

test_that("Players are properly scraped", {
    pstats <- scrapePlayers.QH(player_list=player_list.QH, sleep = 10, long_sleep=0, directory = '.', save_player_list=FALSE)
    expect_type(pstats, "list")

    expect_equivalent(pstats, player_mini_data)
    f1<-paste0("./QH_allPlayers-",Sys.Date(),".RDS")
    expect_true(file.exists(f1))
    try_delete(f1)
})
