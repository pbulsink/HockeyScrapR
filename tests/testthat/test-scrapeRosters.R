context("Test URL scraping works.")
source("helper-scrapeRosters.R")

test_that("Getting and parsing rosters URL page works", {
    teamURLList <- data.frame(URL = c("http://www.dailyfaceoff.com/teams/anaheim-ducks/line-combinations/"),
        Team = c("Anaheim Ducks"), stringsAsFactors = FALSE)
    roster <- getCurrentRosters(sleep = 5, teamUrlList = teamURLList)

    expect_named(roster, c("Team", "Players", "updateDate"))
    # Teams can dress up to 20 active players in a game
    expect_equal(nrow(roster), 20)
})

test_that("Scraping Rosters Works", {
    teamURLList <- data.frame(URL = c("http://www.dailyfaceoff.com/teams/anaheim-ducks/line-combinations/"),
        Team = c("Anaheim Ducks"), stringsAsFactors = FALSE)
    roster <- scrapeRosters(data_dir = "./", sleep = 5, teamUrlList = teamURLList)

    expect_named(roster, c("Team", "Players", "updateDate"))
    # Teams can dress up to 20 active players in a game.
    expect_true(nrow(roster) <= 20)
    expect_true(nchar(roster$Players[1]) > 3)
    f1 <- paste0("./rosters-", Sys.Date(), ".RDS")
    expect_true(file.exists(f1))
    try_delete(f1)
})
