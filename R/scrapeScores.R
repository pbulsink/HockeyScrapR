#' Download and save WHA Games
#'
#' Download WHA games scores from Hockey-Reference.com
#'
#' @param start Starting season to download. Season is \code{value-1 to value} (eg. start=1973 --> 1972-1973 season)
#' @param end Last season to download
#' @param sleep Time to sleep between scrapes
#' @param data_dir Location to save the csv data
#' @param progress Whether to show a progress bar. Default = TRUE.
#' @param ... Additional parameters to pass
#'
#' @export
#' @keywords internal
getAndSaveWHAGames <- function(start = 1973, end = 1979, sleep = 30, data_dir = "./data/scores/", progress = TRUE,
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

  message("Scraping WHA games...")
  if (progress && start != end) {
    pb <- progress::progress_bar$new(
      format = "  downloading seasons [:bar] :percent eta: :eta",
      clear = FALSE,
      width = 80,
      total=end-start+1
    )
    pb$tick(0)
  }

  for (i in c(start:end)) {
    url <- paste0("https://www.hockey-reference.com/leagues/WHA_", i, "_games.html")
    htmlpage <- getURLInternal(url, referer = "https://www.hockey-reference.com/")
    if (class(htmlpage) == "try-error") {
      tables <- NULL
    } else {
      tables <- XML::readHTMLTable(htmlpage)
    }
    if (!is.null(tables)) {
      ## In case of download error, don't process
      regular <- tables$games
      playoff <- tables$games_playoffs
      utils::write.csv(regular, file = file.path(data_dir, paste0("wha", i - 1, i, ".csv")))
      utils::write.csv(playoff, file = file.path(data_dir, paste0("wha", i - 1, i, "Playoffs.csv")))
    }
    Sys.sleep(sleep)
    if (progress && start != end) {
      pb$tick()
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
#' @param progress Whether to show a progress bar. Default = TRUE
#' @param ... Additional parameters to pass
#'
#' @export
#' @keywords internal
getAndSaveNHLGames <- function(start = 1918, end = getCurrentSeason(), sleep = 30, data_dir = "./data/scores/", progress = TRUE,
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
  if (start > getCurrentSeason()) {
    message("Can't start collecting data past next season. Collecting final season.")
    start <- getCurrentSeason()
    end <- getCurrentSeason()
  }
  if (end > getCurrentSeason()) {
    message("Can't collect past this season. Collecting up to there.")
    end <- getCurrentSeason()
  }

  message("Scraping NHL games...")
  if (progress && start != end){
    pb <- progress::progress_bar$new(
      format = "  downloading players [:bar] :percent eta: :eta",
      clear = FALSE,
      width = 80,
      total=end-start+1
    )
    pb$tick(0)
  }

  for (i in c(start:end)) {
    # No season in 2004-2005. Don't try process that year.
    if (i == 2005) {
      next
    }
    url <- paste0("https://www.hockey-reference.com/leagues/NHL_", i, "_games.html")
    htmlpage <- getURLInternal(url, referer = "https://www.hockey-reference.com/")
    if (class(htmlpage) == "try-error") {
      tables <- NULL
    } else {
      tables <- XML::readHTMLTable(htmlpage)
    }

    if (!is.null(tables)) {
      ## In case of download error, don't process
      regular <- tables$games
      playoff <- tables$games_playoffs
      utils::write.csv(regular, file = file.path(data_dir, paste0(i - 1, i, ".csv")))
      if (!is.null(playoff)) {
        utils::write.csv(playoff, file = file.path(data_dir, paste0(i - 1, i, "Playoffs.csv")))
      }
    }
    Sys.sleep(sleep)
    if (progress && start != end){
      utils::setTxtProgressBar(pb, i)
    }
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
readHockeyData <- function(data_dir = "./data/scores/", nhl_year_list = c(1918:getCurrentSeason()),
  wha_year_list = c(1973:1979), playoffs = TRUE, lastPlayoffs = FALSE, ...) {
  df_nhl <- data.frame(Date = NULL, Visitor = NULL, G = NULL, Home = NULL, G.1 = NULL,
    X.1 = NULL, stringsAsFactors = FALSE)
  df_wha <- df_nhl
  nhl_year_list <- nhl_year_list[nhl_year_list != 2005]
  message("Processing NHL score data...")
  for (year in 1:length(nhl_year_list)) {
    df_nhl <- rbind(df_nhl, utils::read.csv(file.path(data_dir, paste0(nhl_year_list[year] -
      1, nhl_year_list[year], ".csv")), stringsAsFactors = FALSE)[2:7])
  }
  if (playoffs) {
    if (length(nhl_year_list) > 1) {
      for (year in 1:(length(nhl_year_list) - 1)) {
        if (nhl_year_list[year] != 1920) {
          df_nhl <- rbind(df_nhl, utils::read.csv(file.path(data_dir, paste0(nhl_year_list[year] -
          1, nhl_year_list[year], "Playoffs.csv")), stringsAsFactors = FALSE)[2:7])
        }
      }
    }
    if (lastPlayoffs) {
      df_nhl <- rbind(df_nhl, utils::read.csv(file.path(data_dir, paste0(nhl_year_list[length(nhl_year_list)] -
        1, nhl_year_list[length(nhl_year_list)], "Playoffs.csv")),
        stringsAsFactors = FALSE)[2:7])
    }
  }

  df_nhl$League <- "NHL"

  if (length(wha_year_list) > 0) {
    message("Processing WHA score data...")
    for (year in 1:length(wha_year_list)) {
      df_wha <- rbind(df_wha, utils::read.csv(file.path(data_dir, paste0("wha", wha_year_list[year] -
        1, wha_year_list[year], ".csv")), stringsAsFactors = FALSE)[2:7])
    }
    if (playoffs) {
      for (year in 1:(length(wha_year_list))) {
        df_wha <- rbind(df_wha, utils::read.csv(file.path(data_dir, paste0("wha", wha_year_list[year] -
          1, wha_year_list[year], "Playoffs.csv")), stringsAsFactors = FALSE)[2:7])
      }
    }

    df_wha$League <- "WHA"
  }

  df <- rbind(df_nhl, df_wha)

  df <- cleanHockeyData(hockey_data = df, ...)

  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)

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

    #hockey_data$Home <- factor(hockey_data$Home)
    #hockey_data$Visitor <- factor(hockey_data$Visitor, levels = levels(hockey_data$Home))
  }

  if (listWinnersLosers) {
    hockey_data$Winner <- apply(hockey_data, 1, function(x) ifelse(x[3] >
      x[5], x[2], x[4]))
    hockey_data$Loser <- apply(hockey_data, 1, function(x) ifelse(x[3] <=
      x[5], x[2], x[4]))
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
#' Also saves results to a .RDS file.
#'
#' @param data_dir Directory to store data in.
#' @param save_rds Whether to save rds file
#' @param ... Additional parameters to pass
#'
#' @return scores information data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' scrapeScores()
#' }
scrapeScores <- function(data_dir = "./data/scores/", save_rds = FALSE, ...) {
  if (!dir.exists(data_dir))
    dir.create(data_dir, recursive = TRUE)
  getAndSaveWHAGames(data_dir = data_dir, ...)
  getAndSaveNHLGames(data_dir = data_dir, ...)
  hockey_data <- readHockeyData(data_dir = data_dir, ...)
  if(save_rds){
    saveRDS(hockey_data, file.path(data_dir, "scores.RDS"))
  }

  return(hockey_data)
}

#' Update scores information (don't rescrape old scores).
#'
#' @param score_data The player_data data.frame to update
#' @param data_dir The data dir of stored player information
#' @param save_rds
#' @param ... Additional parameters to pass
#'
#' @return updated scores information data.frame
#' @export
#'
#' @examples
#' \dontrun{updateScores()}
updateScores <- function(score_data, data_dir = "./data/scores/", save_rds = FALSE, playoffs = FALSE, last_playoffs = FALSE, ...) {
  if (!dir.exists(data_dir)) {
    message("Data directory '", data_dir, "' does not exist. Scraping all scores.")
    hockey_data <- scrapeScores(data_dir = data_dir, ...)
  } else {
    last_score_date <- max(score_data$Date)
    getAndSaveNHLGames(data_dir = data_dir, start = getCurrentSeason(), ...)
    new_hockey_data <- readHockeyData(data_dir = data_dir, nhl_year_list = c(getCurrentSeason()),
      wha_year_list = c(), playoffs = playoffs, lastPlayoffs = last_playoffs)
    if('HomeTeam' %in% colnames(score_data)){
      new_hockey_data$HomeTeam<-new_hockey_data$Home
      new_hockey_data$Home<-NULL
    }
    if('AwayTeam' %in% colnames(score_data)){
      new_hockey_data$AwayTeam<-new_hockey_data$Visitor
      new_hockey_data$Visitor<-NULL
    }
    if('AwayGoals' %in% colnames(score_data)){
      new_hockey_data$AwayGoals<-new_hockey_data$VisitorGoals
      new_hockey_data$VisitorGoals<-NULL
    }
    hockey_data <- unique(dplyr::bind_rows(score_data, new_hockey_data))

    if(save_rds){
      saveRDS(hockey_data, file.path(data_dir, "scores.RDS"))
    }
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
  message('Downloading schedule...')
  if (!dir.exists(data_dir))
    dir.create(data_dir, recursive = TRUE)
  current_season <- getCurrentSeason()
  schedule_url <- paste0("https://www.hockey-reference.com/leagues/NHL_", current_season,
    "_games.html")
  htmlpage <- getURLInternal(schedule_url, referer = "https://www.hockey-reference.com")

  if (class(htmlpage) == "try-error")
    return(FALSE)

  tables <- XML::readHTMLTable(htmlpage, stringsAsFactors = FALSE)

  message('Compiling Schedule...')
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

  saveRDS(schedule, file.path(data_dir, "schedule.RDS"))
  return(schedule)

}

#' Scrape Advanced Stats
#'
#' @param scores historical scores to pair to advanced stats
#' @param data_dir where to store checkpoint data
#' @param sleep number of seconds between scrapes
#'
#' @return a larger data frame.
#' @export
scrapeAdvancedStats <- function(scores=NULL, data_dir = "./data/", sleep=10) {
  if(is.null(scores)){
    scores<-scrapeScores(data_dir = data_dir)
  }
  if (!dir.exists(data_dir)){
    dir.create(data_dir, recursive = TRUE)
  }
  scores<-scores[scores$Date > as.Date('2008-09-01'),]
  scores<-scores[scores$Date < Sys.Date(),]

  scores$HomeAllCF <- NA
  scores$HomeCloseCF <- NA
  scores$HomeEvenCF <- NA
  scores$HomeHits <- NA
  scores$HomeBlocks <- NA
  scores$HomeShots <- NA
  scores$HomePIM <- NA
  scores$HomeEV <- NA
  scores$HomePP <- NA
  scores$HomeSH <- NA
  scores$HomeSaves <- NA
  scores$HomeShifts <- NA
  scores$HomeTOI <- NA

  scores$AwayAllCF <- NA
  scores$AwayCloseCF <- NA
  scores$AwayEvenCF <- NA
  scores$AwayHits <- NA
  scores$AwayBlocks <- NA
  scores$AwayShots <- NA
  scores$AwayPIM <- NA
  scores$AwayEV <- NA
  scores$AwayPP <- NA
  scores$AwaySH <- NA
  scores$AwaySaves <- NA
  scores$AwayShifts <- NA
  scores$AwayTOI <- NA

  pb <- utils::txtProgressBar(0, nrow(scores), style = 3)
  errorscount<-0
  for(i in 1:nrow(scores)){
    home <- as.character(scores$HomeTeam[[i]])
    home <- getHome(home, scores$Date[[i]])
    away <- as.character(scores$AwayTeam[[i]])
    away <- getHome(away, scores$Date[[i]])
    dated <- format(scores$Date[[i]], "%Y%m%d")
    url<-paste0("https://www.hockey-reference.com/boxscores/",dated,"0",home,".html")
    htmlpage <- getURLInternal(url, referer = "https://www.hockey-reference.com/")
    htmlpage <- uncommentHTML(htmlpage)
    if (class(htmlpage) == "try-error") {
      tables <- NULL
    } else {
      htmlpage <- uncommentHTML(htmlpage)
      tables <- XML::readHTMLTable(htmlpage, stringsAsFactors = FALSE)
    }
    if (!is.null(tables)) {
      ## In case of download error, don't process
      tryCatch(home_adv <- tables[names(tables) %in% paste0(home, "_adv")][[1]],
               error = function(e) {message('Error: ',e,' Home_adv ', home); errorscount<-errorscount+1; home_adv<-NULL})
      tryCatch(home_skaters <- tables[names(tables) %in% paste0(home, "_skaters")][[1]],
               error = function(e) {message('Error: ',e,' Home_skaters ', home); errorscount<-errorscount+1; home_skaters<-NULL})
      tryCatch(home_goalies <- tables[names(tables) %in% paste0(home, "_goalies")][[1]],
               error = function(e) {message('Error: ',e,' Home_goalies ', home); errorscount<-errorscount+1; home_goalies<-NULL})
      tryCatch(away_adv <- tables[names(tables) %in% paste0(away, "_adv")][[1]],
               error = function(e) {message('Error: ',e,' Away_adv ', away); errorscount<-errorscount+1; away_adv<-NULL})
      tryCatch(away_skaters <- tables[names(tables) %in% paste0(away, "_skaters")][[1]],
               error = function(e) {message('Error: ',e,' Away_skaters ', away); errorscount<-errorscount+1; away_skaters<-NULL})
      tryCatch(away_goalies <- tables[names(tables) %in% paste0(away, "_goalies")][[1]],
               error = function(e) {message('Error: ',e,' Away_goalies ', away); errorscount<-errorscount+1; away_goalies<-NULL})
      # ALL5v5, Cl5v5, ALLAll, CLAll, ALLEV, CLEV, ALLPP, CLPP, ALLSH, CLSH

      if(!is.null(home_adv)){
        scores$HomeAllCF[[i]] <- sum(as.numeric(home_adv[seq(from=3, to=nrow(home_adv), by=10), 'iCF']), na.rm = TRUE)
        scores$HomeCloseCF[[i]] <- sum(as.numeric(home_adv[seq(from=4, to=nrow(home_adv), by=10), 'iCF']), na.rm = TRUE)
        scores$HomeEvenCF[[i]] <- sum(as.numeric(home_adv[seq(from=5, to=nrow(home_adv), by=10), 'iCF']), na.rm = TRUE)
        scores$HomeHits[[i]] <- sum(as.numeric(home_adv[seq(from=3, to=nrow(home_adv), by=10), 'HIT']), na.rm = TRUE)
        scores$HomeBlocks[[i]] <- sum(as.numeric(home_adv[seq(from=3, to=nrow(home_adv), by=10), 'BLK']), na.rm = TRUE)
      }
      if(!is.null(home_skaters)){
        scores$HomeShots[[i]] <- sum(as.numeric(home_skaters$S), na.rm = TRUE)
        scores$HomePIM[[i]] <- sum(as.numeric(home_skaters$PIM), na.rm = TRUE)
        scores$HomeEV[[i]] <- sum(as.numeric(home_skaters$EV), na.rm = TRUE)
        scores$HomePP[[i]] <- sum(as.numeric(home_skaters$PP), na.rm = TRUE)
        scores$HomeSH[[i]] <- sum(as.numeric(home_skaters$SH), na.rm = TRUE)
        scores$HomeShifts[[i]] <- sum(as.numeric(home_skaters$SHFT), na.rm = TRUE)
        scores$HomeTOI[[i]] <- sum(lubridate::seconds(lubridate::ms(home_skaters$TOI)))
      }
      if(!is.null(home_goalies)){
        scores$HomeSaves[[i]] <- sum(as.numeric(home_goalies$SV), na.rm = TRUE)
        scores$HomePIM[[i]] <- sum(c(scores$HomePIM[[i]], sum(as.numeric(home_goalies$PIM), na.rm = TRUE)), na.rm = TRUE)
      }
      if(!is.null(away_adv)){
        scores$AwayAllCF[[i]] <- sum(as.numeric(away_adv[seq(from=3, to=nrow(away_adv), by=10), 'iCF']), na.rm = TRUE)
        scores$AwayCloseCF[[i]] <- sum(as.numeric(away_adv[seq(from=4, to=nrow(away_adv), by=10), 'iCF']), na.rm = TRUE)
        scores$AwayEvenCF[[i]] <- sum(as.numeric(away_adv[seq(from=5, to=nrow(away_adv), by=10), 'iCF']), na.rm = TRUE)
        scores$AwayHits[[i]] <- sum(as.numeric(away_adv[seq(from=3, to=nrow(away_adv), by=10), 'HIT']), na.rm = TRUE)
        scores$AwayBlocks[[i]] <- sum(as.numeric(away_adv[seq(from=3, to=nrow(away_adv), by=10), 'BLK']), na.rm = TRUE)
      }
      if(!is.null(away_skaters)){
        scores$AwayShots[[i]] <- sum(as.numeric(away_skaters$S), na.rm = TRUE)
        scores$AwayPIM[[i]] <- sum(as.numeric(away_skaters$PIM), na.rm = TRUE)
        scores$AwayEV[[i]] <- sum(as.numeric(away_skaters$EV), na.rm = TRUE)
        scores$AwayPP[[i]] <- sum(as.numeric(away_skaters$PP), na.rm = TRUE)
        scores$AwaySH[[i]] <- sum(as.numeric(away_skaters$SH), na.rm = TRUE)
        scores$AwayShifts[[i]] <- sum(as.numeric(away_skaters$SHFT), na.rm = TRUE)
        scores$AwayTOI[[i]] <- sum(lubridate::seconds(lubridate::ms(away_skaters$TOI)))
      }
      if(!is.null(away_goalies)){
        scores$AwaySaves[[i]] <- sum(as.numeric(away_goalies$SV), na.rm = TRUE)
        scores$AwayPIM[[i]] <- sum(c(scores$AwayPIM[[i]], sum(as.numeric(away_goalies$PIM), na.rm = TRUE)), na.rm = TRUE)
      }

      scores[i,][is.na(scores[i,])]<-0  #replaces NA values with 0. For most count values, this is fine.
    } else {
      message(url)
      message(home, away, dated)
    }

    if(i%%1000 == 0){
      saveRDS(scores, file = file.path(data_dir, paste0('scores',i,'2.RDS')))
      errorscount<-0
    }

    if(errorscount == 30) break

    Sys.sleep(sleep)
    utils::setTxtProgressBar(pb, i)
  }

  saveRDS(scores, file = file.path(data_dir, 'scoresComplete2.RDS'))
  return(scores)

}

getHome <- function(home, d) {
  lookup <- list('Toronto Maple Leafs' = 'TOR',
                 'Montreal Canadiens' = 'MTL',
                 'Boston Bruins' = 'BOS',
                 'New York Rangers' = 'NYR',
                 'Chicago Blackhawks' = 'CHI',
                 'Detroit Red Wings' = 'DET',
                 'Pittsburgh Penguins' = 'PIT',
                 'St. Louis Blues' = 'STL',
                 'Los Angeles Kings' = 'LAK',
                 'Philadelphia Flyers' = 'PHI',
                 'Dallas Stars' = 'DAL',
                 'Vancouver Canucks' = 'VAN',
                 'Buffalo Sabres' = 'BUF',
                 'New York Islanders' = 'NYI',
                 'Carolina Hurricanes' = 'CAR',
                 'Colorado Avalanche' = 'COL',
                 'Calgary Flames' = 'CGY',
                 'Arizona Coyotes' = 'ARI',
                 'Edmonton Oilers' = 'EDM',
                 'Washington Capitals' = 'WSH',
                 'New Jersey Devils' = 'NJD',
                 'San Jose Sharks' = 'SJS',
                 'Tampa Bay Lightning' = 'TBL',
                 'Ottawa Senators' = 'OTT',
                 'Anaheim Ducks' = 'ANA',
                 'Florida Panthers' = 'FLA',
                 'Nashville Predators' = 'NSH',
                 'Winnipeg Jets' = 'WPG',
                 'Columbus Blue Jackets' = 'CBJ',
                 'Minnesota Wild' = 'MIN',
                 'Vegas Golden Knights' = 'VEG')
  h <- lookup[home]
  if (d < as.Date('2014-08-01') & h == 'ARI'){
    h <- 'PHX'
  } else if (d < as.Date('2011-08-01') & h == 'WPG'){
    h <- 'ATL'
  }
  return(h)
}
