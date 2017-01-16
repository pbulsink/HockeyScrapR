context("Test score data scraping")
source("helper-scrapeScores.R")

test_that("Testing scraping NHL scores", {
  expect_false(getAndSaveNHLGames(start = 2005, end = 2005))
  expect_message(getAndSaveNHLGames(start = 2005, end = 2005), "Can't collect 2004-2005. No season due to lockout. Not collecting any data.")

  expect_true(getAndSaveNHLGames(start = 2013, end = 2013, data_dir = "./", sleep = 0))
  f1 <- "./20122013.csv"
  f2 <- "./20122013Playoffs.csv"
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))
  try_delete(f1)
  try_delete(f2)

  expect_message(getAndSaveNHLGames(start = 2013, end = 2012, data_dir = "./",
    sleep = 10), "Start must be less than end. Reversing values.")
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
})

test_that("Testing scraping WHA scores", {

  expect_true(getAndSaveWHAGames(start = 1975, end = 1975, data_dir = "./", sleep = 0))
  f1 <- "./wha19741975.csv"
  f2 <- "./wha19741975Playoffs.csv"
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))
  try_delete(f1)
  try_delete(f2)

  expect_message(getAndSaveWHAGames(start = 1975, end = 1974, data_dir = "./",
    sleep = 10), "Start must be less than end. Reversing values.")
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
})

test_that("Reading Hockey Data Works", {
  f1 <- "./20122013.csv"
  write.csv(sample_csv, f1)
  expect_true(file.exists(f1))

  scores_data <- readHockeyData(data_dir = "./", nhl_year_list = c(2013), wha_year_list = c(),
    playoffs = FALSE, lastPlayoffs = FALSE)

  expect_equivalent(scores_data, cleaned_scores)

  try_delete(f1)
})

test_that("Scores Update", {
  up_to_date_scores<-updateScores(out_of_date_scores, "./", sleep=10)
  expect_true(nrow(up_to_date_scores) > nrow(out_of_date_scores))
  f1<-paste0("./scores-", Sys.Date(), ".RDS")
  f2<-paste0("./20162017.csv")
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))
  try_delete(f1)
  try_delete(f2)

})
