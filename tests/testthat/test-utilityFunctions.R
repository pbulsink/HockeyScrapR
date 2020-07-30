context("Testing getURL handles ok")

test_that("Test getURLInternal Returns HTML Page", {
    urlscraped <- getURLInternal("https://data-miner.io/sandbox/table")
    expect_type(object = urlscraped, type = "character")
    expect_false(class(urlscraped) == "try-error")
    expect_true(grepl('<title>Sandbox Table - Data Miner</title>', urlscraped))
    expect_true(grepl('<h1 class="center"> Company Data - Historical</h1>', urlscraped))
})

test_that("Test bad getURLInternal Throws Messages", {
    expect_error(getURLInternal("http://notarealwebpage.thisisntone.com/nope"))
})

test_that("Get Current Season works", {
    actual_season <- as.numeric(format(Sys.Date(), "%Y")) + 1
    if (as.numeric(format(Sys.Date(), "%m")) < 9) {
        actual_season <- actual_season - 1
    }
    expect_equal(getCurrentSeason(), actual_season)
})
