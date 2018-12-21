context("Test score data scraping")
source("helper-scrapeScores.R")

test_that("Testing scraping NHL scores", {
    expect_false(getAndSaveNHLGames(start = 2005, end = 2005, progress=FALSE))
    expect_message(getAndSaveNHLGames(start = 2005, end = 2005, progress=FALSE), "Can't collect 2004-2005. No season due to lockout. Not collecting any data.")
    expect_true(getAndSaveNHLGames(start = 2013, end = 2013, data_dir = "./", sleep = 10, progress=FALSE))
    f1 <- "./20122013.csv"
    f2 <- "./20122013Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)

    expect_message(getAndSaveNHLGames(start = 2013, end = 2012, data_dir = "./", sleep = 10, progress=FALSE),
        "Start must be less than end. Reversing values.")
    f1 <- "./20112012.csv"
    f2 <- "./20112012Playoffs.csv"
    f3 <- "./20122013.csv"
    f4 <- "./20122013Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    expect_true(file.exists(f3))
    expect_true(file.exists(f4))
    try_delete(f1)
    try_delete(f2)
    try_delete(f3)
    try_delete(f4)

    expect_message(getAndSaveNHLGames(start = 2005, end = 2006, data_dir = "./", sleep = 10, progress=FALSE),
        "Can't collect 2004-2005. No season due to lockout. Collecting from 2005-2006 to there.")
    f1 <- "./20052006.csv"
    f2 <- "./20052006Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)

    expect_message(getAndSaveNHLGames(start = 2004, end = 2005, data_dir = "./", sleep = 10, progress=FALSE),
        "Can't collect 2004-2005. No season due to lockout. Collecting to 2003-2004.")
    f1 <- "./20032004.csv"
    f2 <- "./20032004Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)

    expect_message(getAndSaveNHLGames(start = 1917, end = 1918, data_dir = "./", sleep = 10, progress=FALSE),
        "NHL started in 1917-1918. Collecting data from there forwards.")
    f1 <- "./19171918.csv"
    f2 <- "./19171918Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)

    expect_message(getAndSaveNHLGames(start = getCurrentSeason()+1, end = getCurrentSeason()+2, data_dir = "./", sleep = 10, progress=FALSE),
        "Can't start collecting data past next season. Collecting final season.")
    f1 <- paste("./",getCurrentSeason()-1, getCurrentSeason(), '.csv', sep="")
    expect_true(file.exists(f1))
    try_delete(f1)

    expect_message(getAndSaveNHLGames(start = getCurrentSeason(), end = getCurrentSeason()+2, data_dir = "./", sleep = 10, progress=FALSE),
        "Can't collect past this season. Collecting up to there.")
    f1 <- paste("./",getCurrentSeason()-1, getCurrentSeason(), '.csv', sep="")
    expect_true(file.exists(f1))
    try_delete(f1)

})

test_that("Testing scraping WHA scores", {
    expect_true(getAndSaveWHAGames(start = 1975, end = 1975, data_dir = "./", sleep = 10, progress=FALSE))
    f1 <- "./wha19741975.csv"
    f2 <- "./wha19741975Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)

    expect_message(getAndSaveWHAGames(start = 1975, end = 1974, data_dir = "./", sleep = 10, progress=FALSE),
        "Start must be less than end. Reversing values.")
    f1 <- "./wha19731974.csv"
    f2 <- "./wha19731974Playoffs.csv"
    f3 <- "./wha19741975.csv"
    f4 <- "./wha19741975Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    expect_true(file.exists(f3))
    expect_true(file.exists(f4))
    try_delete(f1)
    try_delete(f2)
    try_delete(f3)
    try_delete(f4)

    expect_message(getAndSaveWHAGames(start = 1972, end = 1973, data_dir = "./", sleep = 10, progress=FALSE),
        "WHA started in 1972-1973. Collecting data from there forwards.")
    f1 <- "./wha19721973.csv"
    f2 <- "./wha19721973Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)

    expect_message(getAndSaveWHAGames(start = 1980, end = 1981, data_dir = "./", sleep = 10, progress=FALSE),
        "WHA ended in 1978-1979. Collecting final season.")
    f1 <- "./wha19781979.csv"
    f2 <- "./wha19781979Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)

    expect_message(getAndSaveWHAGames(start = 1979, end = 1981, data_dir = "./", sleep = 10, progress=FALSE),
        "WHA ended in 1978-1979. Collecting up to there.")
    f1 <- "./wha19781979.csv"
    f2 <- "./wha19781979Playoffs.csv"
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)
})

test_that("Reading Hockey Data Works", {
    f1 <- "./20122013.csv"
    f2 <- "./20122013Playoffs.csv"
    f3 <- "./wha19721973.csv"
    f4 <- "./wha19721973Playoffs.csv"
    write.csv(sample_csv1, f1)
    write.csv(sample_csv2, f2)
    write.csv(sample_csv3, f3)
    write.csv(sample_csv4, f4)
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    expect_true(file.exists(f3))
    expect_true(file.exists(f4))

    scores_data <- readHockeyData(data_dir = "./", nhl_year_list = c(2013), wha_year_list = c(1973),
        playoffs = TRUE, lastPlayoffs = TRUE)

    expect_equivalent(scores_data, cleaned_scores)

    try_delete(f1)
    try_delete(f2)
    try_delete(f3)
    try_delete(f4)
})

test_that("Scores Update", {
    up_to_date_scores <- updateScores(out_of_date_scores, "./", sleep = 10, progress=FALSE)
    expect_true(nrow(up_to_date_scores) > nrow(out_of_date_scores))
    f1 <- "./scores.RDS"
    f2 <- paste("./",getCurrentSeason()-1, getCurrentSeason(), '.csv', sep="")
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    try_delete(f1)
    try_delete(f2)
})

test_that("Scraper works", {
    scraped_scores <- scrapeScores(data_dir = "./", start = 1976, end = 1976, nhl_year_list = 1976,
        wha_year_list = 1976, playoffs = TRUE, last_playoffs = TRUE, sleep = 10, progress=FALSE)
    f1 <- "./19751976.csv"
    f2 <- "./19751976Playoffs.csv"
    f3 <- "./wha19751976.csv"
    f4 <- "./wha19751976Playoffs.csv"
    f5 <- "./scores.RDS"

    expect_equivalent(scraped_scores, sscores)

    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    expect_true(file.exists(f3))
    expect_true(file.exists(f4))
    expect_true(file.exists(f5))
    try_delete(f1)
    try_delete(f2)
    try_delete(f3)
    try_delete(f4)
    try_delete(f5)
})

test_that("Schedule returns full cleaned season", {
    fromdate <- paste0(getCurrentSeason() - 1, "-09-01")

    scraped_schedule <- getSchedule(fromdate, data_dir = "./", include_playoffs = FALSE)

    f1 <- "./schedule.RDS"
    expect_true(file.exists(f1))
    try_delete(f1)

    expect_equal(nrow(scraped_schedule), 1271)
    expect_equal(unique(scraped_schedule$G), "")
    expect_equal(unique(scraped_schedule$G.1), "")
    expect_equal(unique(scraped_schedule$X), "")

    fromdate <- paste0(getCurrentSeason() + 2, "-09-01")
    expect_false(getSchedule(from_date = fromdate))
})
