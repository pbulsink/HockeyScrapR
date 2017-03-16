context("Testing getURL handles ok")

test_that("Test getURLInternal Returns HTML Page", {
    urlscraped <- getURLInternal("http://webscraper.io/test-sites/tables")
    expect_type(object = urlscraped, type = "character")
    expect_false(class(urlscraped) == "try-error")
    expect_true(grepl("<title>Web Scraper</title>", urlscraped))
    expect_true(grepl("<h1>Table playground</h1>", urlscraped))
})

test_that("Test bad getURLInternal Throws Messages", {
    urlscraped <- getURLInternal("http://notarealwebpage.thisisntone/nope")
    expect_type(object = urlscraped, type = "character")
    expect_true(class(urlscraped) == "try-error")
    expect_message(getURLInternal("http://notarealwebpage.thisisntone/nope"), "HTML Try Error on:")
})

test_that("Get Current Season works", {
    actual_season <- as.numeric(format(Sys.Date(), "%Y")) + 1
    if (as.numeric(format(Sys.Date(), "%m")) < 8) {
        actual_season <- actual_season - 1
    }
    expect_equal(getCurrentSeason(), actual_season)
})
