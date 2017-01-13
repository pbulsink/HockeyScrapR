context("Test score data scraping")
source("helper-scrapeScores.R")

test_that("Testing scraping NHL scores",{
  expect_false(getAndSaveNHLGames(start=2005, end=2005))
  expect_message(getAndSaveNHLGames(start=2005, end=2005), "Can't collect 2004-2005. No season due to lockout. Not collecting any data.")

  expect_true(getAndSaveNHLGames(start=2013, end=2013, data_dir = "./", sleep=0))
  f<-"./20122013.csv"
  expect_true(file.exists(f))
  try_delete(f)

  expect_message(getAndSaveNHLGames(start=2013, end=2012, data_dir = "./", sleep=10),
                 "Start must be less than end. Reversing values.")
  f<-"./20112012.csv"
  f2<-"./20122013.csv"
  expect_true(file.exists(f))
  expect_true(file.exists(f2))
  try_delete(f)
  try_delete(f2)
})

test_that("Testing scraping WHA scores",{

  expect_true(getAndSaveWHAGames(start=1975, end=1975, data_dir = "./", sleep=0))
  f<-"./wha19741975.csv"
  expect_true(file.exists(f))
  try_delete(f)

  expect_message(getAndSaveWHAGames(start=1975, end=1974, data_dir = "./", sleep=10),
                 "Start must be less than end. Reversing values.")
  f<-"./wha19731974.csv"
  f2<-"./wha19741975.csv"
  expect_true(file.exists(f))
  expect_true(file.exists(f2))
  try_delete(f)
  try_delete(f2)
})

test_that("Reading Hockey Data Works",{
  f<-"./20122013.csv"
  write.csv(sample_csv, f)
  expect_true(file.exists(f))

  scores_data<-readHockeyData(data_dir = "./", nhl_year_list = c(2013), wha_year_list = c(),playoffs = FALSE, lastPlayoffs = FALSE)

  expect_equivalent(scores_data, cleaned_scores)
})
