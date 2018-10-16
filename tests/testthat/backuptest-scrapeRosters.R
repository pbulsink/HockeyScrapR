context("Test URL scraping works.")
source("helper-scrapeRosters.R")

test_that("Getting and parsing rosters URL page works", {

    roster <- getCurrentRosters(sleep = 0, team = 'ANA', progress = FALSE)


    expect_named(roster, "ANA")
    expect_named(roster$ANA, c("Forwards", "Defence", "Goalies", "PP1", "PP2", "Injuries", "UpdateDate"))
    # Teams can dress up to 23 active players in a game

    expect_lte(sum(length(roster$ANA$Forwards),length(roster$ANA$Defence), length(roster$ANA$Goalies)), 23)

    # Teams have 2 power play lines of 5 each
    expect_length(roster$ANA$PP1, 5)
    expect_length(roster$ANA$PP2, 5)
})

test_that("Scraping Rosters Works", {
    roster <- scrapeRosters(data_dir = "./", sleep = 0, team = 'ANA', progress=FALSE)

    #team name should be list key
    expect_named(roster, "ANA")
    #All sublists should exist even if no data (ie no one on IR)
    expect_named(roster$ANA, c("Forwards", "Defence", "Goalies", "PP1", "PP2", "Injuries", "UpdateDate"))
    # Teams can dress up to 23 active players in a game
    expect_lte(sum(length(roster$ANA$Forwards),length(roster$ANA$Defence), length(roster$ANA$Goalies)), 23)

    # Teams have 2 power play lines of 5 each
    expect_length(roster$ANA$PP1, 5)
    expect_length(roster$ANA$PP2, 5)
    #File should be written
    f1 <- paste0("./rosters-", Sys.Date(), ".RDS")
    expect_true(file.exists(f1))
    try_delete(f1)
})

test_that("Bad Teams get caught", {
  expect_error(getCurrentRosters(sleep=0, team="Bob", progress=FALSE), NULL)
})

