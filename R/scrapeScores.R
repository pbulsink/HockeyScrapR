#' Download and save WHA Games
#'
#' Download WHA games scores from Hockey-Reference.com
#'
#' @param start Starting season to download. Season is \code{value-1 to value} (eg. start=1973 --> 1972-1973 season)
#' @param end Last season to download
#' @param sleep Time to sleep between scrapes
#' @param data_dir Location to save the csv data
#' @param ... Additional parameters to pass
#'
#' @export
#' @keywords internal
getAndSaveWHAGames <- function(start = 1973, end = 1979, sleep = 30, data_dir = "./data/scores/",
  ...) {
  if (start > end) {
    message("Start must be less than end. Reversing values.")
    tmp <- start
    start <- end
    end <- tmp
    rm(tmp)
  }
  if (start < 1973) {
    message("WHA started in 1972-1973. Collecting data from there forwards.")
    start <- 1973
  }
  if (start > 1979) {
    message("WHA ended in 1978-1979. Collecting final season.")
    start <- 1979
    end <- 1979
  }
  if (end > 1979) {
    message("WHA ended in 1978-1979. Collecting up to there.")
    end <- 1979
  }

  message("Scraping WHA games")
  if (start != end) {
    pb <- utils::txtProgressBar(min = start, max = end, initial = start)
  }

  for (i in c(start:end)) {
    url <- paste0("http://www.hockey-reference.com/leagues/WHA_", i, "_games.html")
    htmlpage <- getURLInternal(url, referer = "http://www.hockey-reference.com/")
    if (class(htmlpage) == "try-error") {
      tables <- NULL
    } else {
      tables <- XML::readHTMLTable(htmlpage)
    }
    if (!is.null(tables)) {
      ## In case of download error, don't process
      regular <- tables$games
      playoff <- tables$games_playoffs
      utils::write.csv(regular, file = paste0(data_dir, "wha", i - 1, i, ".csv"))
      utils::write.csv(playoff, file = paste0(data_dir, "wha", i - 1, i, "Playoffs.csv"))
    }
    Sys.sleep(sleep)
    if (start != end) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  return(TRUE)
}

#' Download and save NHL Games
#'
#' Download NHL games scores from Hockey-Reference.com
#'
#' @param start Starting season to download. Season is \code{value-1 to value} (eg. start=1918 --> 1917-1918 season)
#' @param end Last season to download
#' @param sleep Time to sleep between scrapes
#' @param data_dir Location to save the csv data
#' @param ... Additional parameters to pass
#'
#' @export
#' @keywords internal
getAndSaveNHLGames <- function(start = 1918, end = 2017, sleep = 30, data_dir = "./data/scores/",
  ...) {
  if (start == 2005 && end == 2005) {
    message("Can't collect 2004-2005. No season due to lockout. Not collecting any data.")
    return(FALSE)
  }
  if (start == 2005) {
    message("Can't collect 2004-2005. No season due to lockout. Collecting from 2005-2006 to there.")
    start <- 2006
  }
  if (end == 2005) {
    message("Can't collect 2004-2005. No season due to lockout. Collecting to 2003-2004.")
    end <- 2004
  }
  if (start > end) {
    message("Start must be less than end. Reversing values.")
    tmp <- start
    start <- end
    end <- tmp
    rm(tmp)
  }
  if (start < 1918) {
    message("NHL started in 1917-1918. Collecting data from there forwards.")
    start <- 1918
  }
  if (start > 2017) {
    message("Can't start collecting data past next season. Collecting final season.")
    start <- 2017
    end <- 2017
  }
  if (end > 2017) {
    message("Can't collect past this season. Collecting up to there.")
    end <- 2017
  }

  message("Scraping NHL games")
  if (start != end)
    pb <- utils::txtProgressBar(min = start, max = end, initial = start)

  for (i in c(start:end)) {
    # No season in 2004-2005. Don't try process that year.
    if (i == 2005) {
      next
    }
    url <- paste0("http://www.hockey-reference.com/leagues/NHL_", i, "_games.html")
    htmlpage <- getURLInternal(url, referer = "http://www.hockey-reference.com/")
    if (class(htmlpage) == "try-error") {
      tables <- NULL
    } else {
      tables <- XML::readHTMLTable(htmlpage)
    }

    if (!is.null(tables)) {
      ## In case of download error, don't process
      regular <- tables$games
      playoff <- tables$games_playoffs
      utils::write.csv(regular, file = paste0(data_dir, "", i - 1, i, ".csv"))
      if (!is.null(playoff)) {
        utils::write.csv(playoff, file = paste0(data_dir, "", i - 1, i, "Playoffs.csv"))
      }
    }
    Sys.sleep(sleep)
    if (start != end)
      utils::setTxtProgressBar(pb, i)
  }
  return(TRUE)
}

#' Sorting out ties
#'
#' This function sorts scores (including ties) into [0..1] win ratio for elo
#'
#' @param game The game to sort ties in
#'
#' @return elo score value
#' @keywords internal
tieSort <- function(game) {
  if (game[3] > game[5]) {
    if (game[6] %in% c("SO")) {
      return(0.4)
    } else if (game[6] %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT")) {
      return(0.25)
    } else {
      return(0)
    }
  } else if (game[3] < game[5]) {
    if (game[6] %in% c("SO")) {
      return(0.6)
    } else if (game[6] %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "OT")) {
      return(0.75)
    } else {
      return(1)
    }
  } else if (game[3] == game[5]) {
    return(0.5)
  }
}


#' Read in Hockey Data csv files.
#'
#' @param data_dir The location of the files
#' @param nhl_year_list The NHL years to read in
#' @param wha_year_list The WHA years to read in
#' @param playoffs Whether to read in playoffs (Boolean, default = \code{TRUE})
#' @param lastPlayoffs Whether to read the last listed playoffs. Default = FALSE
#' @param ... Further parameters to pass to \code{\link{cleanHockeyData}}
#'
#' @return returns a un-cleaned data frame from NHL and WHA score csv files
#' @keywords internal
#' @export
readHockeyData <- function(data_dir = "./data/scores/", nhl_year_list = c(1918:2017),
  wha_year_list = c(1973:1979), playoffs = TRUE, lastPlayoffs = FALSE, ...) {

  df_nhl <- data.frame(Date = NULL, Visitor = NULL, G = NULL, Home = NULL, G.1 = NULL,
    X.1 = NULL, stringsAsFactors = FALSE)
  df_wha <- df_nhl
  nhl_year_list <- nhl_year_list[nhl_year_list != 2005]
  message("reading NHL data")
  for (year in 1:length(nhl_year_list)) {
    df_nhl <- rbind(df_nhl, utils::read.csv(paste(data_dir, nhl_year_list[year] -
      1, nhl_year_list[year], ".csv", sep = ""), stringsAsFactors = FALSE)[2:7])
  }
  if (playoffs) {
    if (length(nhl_year_list) > 1) {
      for (year in 1:(length(nhl_year_list) - 1)) {
        if (nhl_year_list[year] != 1920) {
          df_nhl <- rbind(df_nhl, utils::read.csv(paste(data_dir, nhl_year_list[year] -
          1, nhl_year_list[year], "Playoffs.csv", sep = ""), stringsAsFactors = FALSE)[2:7])
        }
      }
    }
    if (lastPlayoffs) {
      df_nhl <- rbind(df_nhl, utils::read.csv(paste(data_dir, nhl_year_list[length(nhl_year_list)] -
        1, nhl_year_list[length(nhl_year_list)], "Playoffs.csv", sep = ""),
        stringsAsFactors = FALSE)[2:7])
    }
  }

  df_nhl$League <- "NHL"

  if (length(wha_year_list) > 0) {
    message("reading WHA data")
    for (year in 1:length(wha_year_list)) {
      df_wha <- rbind(df_wha, utils::read.csv(paste(data_dir, "wha", wha_year_list[year] -
        1, wha_year_list[year], ".csv", sep = ""), stringsAsFactors = FALSE)[2:7])
    }
    if (playoffs) {
      for (year in 1:(length(wha_year_list))) {
        df_wha <- rbind(df_wha, utils::read.csv(paste(data_dir, "wha", wha_year_list[year] -
          1, wha_year_list[year], "Playoffs.csv", sep = ""), stringsAsFactors = FALSE)[2:7])
      }
    }

    df_wha$League <- "WHA"
  }

  df <- rbind(df_nhl, df_wha)

  df <- cleanHockeyData(hockey_data = df, ...)
  return(df)
}

#' Clean the CSV imported hockey data
#'
#' @param hockey_data the combined CSV data
#' @param cleanTeams Whether to clean historical team names & track moving teams (Boolean, default = \code{TRUE})
#' @param identifyTies Whether to identify ties & adjust results (Boolean, default = \code{TRUE})
#' @param listWinnersLosers Whether to add column for Winners & Losers (Boolean, default = \code{TRUE})
#' @param removeInternational Whether to add games between NHL teams and international teams (Boolean, default = \code{TRUE})
#' @param eloResults Whether to add a column for results in a [0..1] set (Boolean, default = \code{TRUE})
#' @param ... Additional parameters to pass
#'
#' @return Returns a cleaned data frame of hockey scores, with options as selected
#' @keywords internal
cleanHockeyData <- function(hockey_data, cleanTeams = TRUE, identifyTies = TRUE,
  listWinnersLosers = TRUE, removeInternational = TRUE, eloResults = TRUE, ...) {
  teamReplace <- list(list("Alberta Oilers", "Edmonton Oilers"), list("Mighty Ducks of Anaheim",
    "Anaheim Ducks"), list("Winnipeg Jets (historical)", "Arizona Coyotes"),
    list("Phoenix Coyotes", "Arizona Coyotes"), list("Atlanta Flames", "Calgary Flames"),
    list("Atlanta Thrashers", "Winnipeg Jets"), list("Toronto Toros", "Birmingham Bulls"),
    list("Ottawa Nationals", "Birmingham Bulls"), list("Quebec Athletic Club/Bulldogs",
      "Brooklyn Americans"), list("Hamilton Tigers", "Brooklyn Americans"),
    list("New York Americans", "Brooklyn Americans"), list("Philadelphia Blazers",
      "Calgary Cowboys"), list("Vancouver Blazers", "Calgary Cowboys"), list("Oakland Seals",
      "Cleveland Barons"), list("California Golden Seals", "Cleveland Barons"),
    list("New England Whalers", "Carolina Hurricanes"), list("Hartford Whalers",
      "Carolina Hurricanes"), list("Chicago Black Hawks", "Chicago Blackhawks"),
    list("Quebec Nordiques", "Colorado Avalanche"), list("Kansas City Scouts",
      "New Jersey Devils"), list("Colorado Rockies", "New Jersey Devils"),
    list("Minnesota North Stars", "Dallas Stars"), list("Detroit Cougars", "Detroit Red Wings"),
    list("Detroit Falcons", "Detroit Red Wings"), list("Los Angeles Sharks",
      "Michigan Stags/Baltimore Blades"), list("New York Raiders", "San Diego Mariners"),
    list("New York Golden Blades/New Jersey Knights", "San Diego Mariners"),
    list("Pittsburgh Pirates", "Philadelphia Quakers"), list("Toronto Arenas",
      "Toronto Maple Leafs"), list("Toronto St. Patricks", "Toronto Maple Leafs"),
    list("Ottawa Senators (historical)", "St. Louis Eagles"))

  # ReType frame
  hockey_data <- unique(hockey_data)
  hockey_data <- hockey_data[, !names(hockey_data) %in% c("LOG", "X")]
  hockey_data$Date <- as.Date(hockey_data$Date)
  if ("Att." %in% names(hockey_data)) {
    hockey_data$Att. <- as.integer(hockey_data$Att.)
  }
  names(hockey_data)[names(hockey_data) == "G"] <- "VisitorGoals"
  names(hockey_data)[names(hockey_data) == "G.1"] <- "HomeGoals"
  names(hockey_data)[names(hockey_data) == "X.1"] <- "OTStatus"
  # hockey_data$OTStatus <- factor(hockey_data$OTStatus) hockey_data$League <-
  # factor(hockey_data$League)

  if (identifyTies) {
    hockey_data$Tie <- FALSE
    try(hockey_data[hockey_data$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT",
      "OT", "SO"), ]$Tie <- TRUE, silent = TRUE)
  }

  # Remove games against international teams
  if (removeInternational) {
    hockey_data <- hockey_data[!(hockey_data$Visitor %in% c("Soviet All-Stars",
      "Czechoslovakia", "Finland")), ]
  }

  hockey_data <- hockey_data[!is.na(hockey_data$VisitorGoals), ]

  if (cleanTeams) {
    # Special Casing out the various teams with repeat existances

    hockey_data$Home <- unlist(hockey_data$Home)
    hockey_data$Visitor <- unlist(hockey_data$Visitor)

    try(hockey_data[hockey_data$Visitor == "Winnipeg Jets" & hockey_data$Date <
      as.Date("1997-01-01", format = "%Y-%m-%d"), ]$Visitor <- "Winnipeg Jets (historical)",
      silent = TRUE)
    try(hockey_data[hockey_data$Home == "Winnipeg Jets" & hockey_data$Date <
      as.Date("1997-01-01", format = "%Y-%m-%d"), ]$Home <- "Winnipeg Jets (historical)",
      silent = TRUE)
    try(hockey_data[hockey_data$Visitor == "Ottawa Senators" & hockey_data$Date <
      as.Date("1935-01-01", format = "%Y-%m-%d"), ]$Visitor <- "Ottawa Senators (historical)",
      silent = TRUE)
    try(hockey_data[hockey_data$Home == "Ottawa Senators" & hockey_data$Date <
      as.Date("1935-01-01", format = "%Y-%m-%d"), ]$Home <- "Ottawa Senators (historical)",
      silent = TRUE)

    for (t in teamReplace) {
      try(hockey_data[hockey_data$Visitor == t[[1]], ]$Visitor <- t[[2]], silent = TRUE)
      try(hockey_data[hockey_data$Home == t[[1]], ]$Home <- t[[2]], silent = TRUE)
    }

    try(hockey_data[hockey_data$Date > as.Date("1976-09-01", format = "%Y-%m-%d") &
      hockey_data$Visitor == "Minnesota Fighting Saints", ]$Visitor <- "Cleveland Crusaders",
      silent = TRUE)
    try(hockey_data[hockey_data$Date > as.Date("1976-09-01", format = "%Y-%m-%d") &
      hockey_data$Home == "Minnesota Fighting Saints", ]$Home <- "Cleveland Crusaders",
      silent = TRUE)

    hockey_data$Home <- factor(hockey_data$Home)
    hockey_data$Visitor <- factor(hockey_data$Visitor, levels = levels(hockey_data$Home))
  }

  if (listWinnersLosers) {
    hockey_data$Winner <- factor(apply(hockey_data, 1, function(x) ifelse(x[3] >
      x[5], x[2], x[4])), levels = levels(hockey_data$Home))
    hockey_data$Loser <- factor(apply(hockey_data, 1, function(x) ifelse(x[3] <=
      x[5], x[2], x[4])), levels = levels(hockey_data$Home))
  }

  if (eloResults) {
    hockey_data$Result <- apply(hockey_data, 1, function(x) tieSort(x))
  }

  hockey_data <- hockey_data[order(hockey_data$Date, hockey_data$League), ]

  return(hockey_data)
}

#' Scrape and clean all score data
#'
#' This is a one-command function to scrape and clean all score data available from Hockey-Reference.com.
#' Also saves results to a dated .RDS file.
#'
#' @param data_dir Directory to store data in.
#' @param ... Additional parameters to pass
#'
#' @return scores information data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' scrapeScores()
#' }
scrapeScores <- function(data_dir = "./data/scores/", ...) {
  if (!dir.exists(data_dir))
    dir.create(data_dir, recursive = TRUE)
  getAndSaveWHAGames(data_dir = data_dir, ...)
  getAndSaveNHLGames(data_dir = data_dir, ...)
  hockey_data <- readHockeyData(data_dir = data_dir, ...)
  saveRDS(hockey_data, paste0(data_dir, "scores-", Sys.Date(), ".RDS"))
  return(hockey_data)
}

#' Update scores information (don't rescrape old scores).
#'
#' @param score_data The player_data data.frame to update
#' @param data_dir The data dir of stored player information
#' @param ... Additional parameters to pass
#'
#' @return updated scores information data.frame
#' @export
#'
#' @examples
#' \dontrun{updateScores()}
#' \dontrun{updatePlayers(score_data, data_dir = './data/'}
updateScores <- function(score_data, data_dir = "./data/scores/", ...) {
  if (!dir.exists(data_dir)) {
    message("Data directory '", data_dir, "' does not exist. Scraping all scores.")
    hockey_data <- scrapeScores(data_dir = data_dir, ...)
  } else {
    last_score_date <- max(score_data$Date)
    getAndSaveNHLGames(data_dir = data_dir, start = getCurrentSeason(), ...)
    new_hockey_data <- readHockeyData(data_dir = data_dir, nhl_year_list = c(getCurrentSeason()),
      wha_year_list = c(), playoffs = FALSE, lastPlayoffs = FALSE)
    hockey_data <- unique(rbind(score_data, new_hockey_data))
    saveRDS(hockey_data, paste0(data_dir, "scores-", Sys.Date(), ".RDS"))
  }
  return(hockey_data)
}

#' Get season schedule
#'
#' @param from_date The date from which to return an empty schedule
#' @param data_dir The location to save the schedule
#' @param include_playoffs Whether or not to include this years' plaoyffs, if available
#' @param ... Additional parameters to pass
#'
#' @return A data.frame of schedule, with scores removed
#'
#' @export
getSchedule <- function(from_date = Sys.Date(), data_dir = "./data/scores", include_playoffs = TRUE,
  ...) {
  if (as.numeric(format(as.Date(from_date), "%Y")) > getCurrentSeason())
    return(FALSE)
  if (!dir.exists(data_dir))
    dir.create(data_dir, recursive = TRUE)
  current_season <- getCurrentSeason()
  schedule_url <- paste0("http://www.hockey-reference.com/leagues/NHL_", current_season,
    "_games.html")
  htmlpage <- getURLInternal(schedule_url, referer = "http://www.hockey-reference.com")

  if (class(htmlpage) == "try-error")
    return(FALSE)

  tables <- XML::readHTMLTable(htmlpage, stringsAsFactors = FALSE)

  schedule <- tables$games
  playoff <- tables$games_playoffs

  if (!is.null(playoff) && include_playoffs == TRUE)
    schedule <- rbind(schedule, playoff)

  colnames(schedule) <- c("Date", "Visitor", "G", "Home", "G.1", "X", "Att.", "LOG",
    "Notes")

  schedule <- schedule[as.Date(schedule$Date) >= as.Date(from_date), ]

  schedule$G <- ""
  schedule$G.1 <- ""
  schedule$X <- ""

  saveRDS(schedule, paste0(data_dir, "schedule.RDS"))
  return(schedule)

}

