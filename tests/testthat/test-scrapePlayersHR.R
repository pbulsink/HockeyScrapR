#' testthat
#' updatePlayerInfo (given old player) gets new player

context("Testing scraping player data for HockeyReference")
source("helper-scrapePlayersHR.R")

test_that("getPlayerList returns ok", {
    plist <- getPlayerList.HR(sleep = 0, letters = "a")
    expect_named(plist, pl_names.HR)
    expect_equivalent(plist[1, ], pl_aa)
})

test_that("Players are properly scraped", {
    pstats <- getPlayerStats.HR(player_list.HR, sleep = 10)
    expect_type(pstats, "list")
    expect_equivalent(pstats, player_stats.HR)
})

test_that("Scraping Player by Alphabet works", {
    player_ab <- scrapeByAlphabet.HR(pl_ab, long_sleep = 0, sleep = 10, directory = "./",
        combine = FALSE)
    expect_true(player_ab)
    expect_true(file.exists("./HR_players_a.RDS"))
    expect_true(file.exists("./HR_players_b.RDS"))
    player_ab <- combinePlayerDataFrames.HR(directory = "./")

    expect_equivalent(player_ab, player_ab_data)
    f1<-paste0("./HR_allPlayers-", Sys.Date(), ".RDS")
    expect_true(file.exists(f1))
    expect_false(file.exists("./HR_players_a.RDS"))
    expect_false(file.exists("./HR_players_b.RDS"))
    if (file.exists(f1)) {
        tryCatch({
            file.remove(f1, showWarnings = FALSE)
        }, error = function(e) message(paste0("Error deleting file ", f1, ", Continuing...")))
    }
})

test_that("Player Processing Works", {
    p_data <- processPlayerData.HR(player_stats.HR)

    expect_type(p_data, "list")

    players <- p_data[[1]]
    goalies <- p_data[[2]]
    meta <- p_data[[3]]
    expect_named(p_data, proc_names[["lnames"]])
    expect_named(players, proc_names[["pnames"]])
    expect_named(goalies, proc_names[["gnames"]])
    expect_named(meta, proc_names[["mnames"]])
    expect_false("Active" %in% names(meta))
    expect_type(object = players$ATOI, "double")
    expect_type(object = meta$Birthdate, "character")
})

test_that("Updates Work", {
    p_new <- updatePlayers.HR(pl_data_oldnew, data_dir = "./", player_list = pl_list_oldnew,
        sleep = 5, long_sleep = 0)

    p <- p_new[[1]]

    expect_equal(sum(p$Season == "2016-17"), 1)

    f1 <- paste0("./HR_allPlayers-", Sys.Date(), ".RDS")
    expect_true(file.exists(f1))
    if (file.exists(f1)) {
        tryCatch({
            file.remove(f1, showWarnings = FALSE)
        }, error = function(e) message(paste0("Error deleting file ", f1, ", Continuing...")))
    }

})
