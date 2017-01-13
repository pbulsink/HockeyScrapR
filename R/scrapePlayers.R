
#' Get a list of Players
#' Download the full list of active and past players from Hockey-Reference.com
#'
#' @param sleep The sleep time between scrape requests
#' @param letters The letters to scrape player names from. Default: \code{letters}
#'
#' @return a data.frame containing:
#' \item{Complete}{The complete line that data was scraped from}
#' \item{BlnNHL}{Whether the player played any time in the NHL}
#' \item{URL}{Player page URL}
#' \item{Name}{Player Name}
#' \item{Active}{Whether the player is currently active}
#' @export
getPlayerList <- function(sleep = 30, letters=letters) {
  pattern <- "<p class=\"([a-z\\_]+)\">(?:<strong>)*<a href=\"(\\/players\\/[a-z]+\\/[a-zA-Z0-9]+\\.html)\">([a-zA-Z ]+)<\\/a>(?:<\\/strong>)*\\s*\\(([0-9-]+)*"
  player_list <- data.frame(Complete = character(), BlnNHL = character(),
                            URL = character(), Name = character(),
                            Active = character())
  for (letter in letters) {
    message(letter)
    url <- paste0("http://www.hockey-reference.com/players/", letter, "/")
    raw_player_list <- getURLInternal(url, referer = "http://www.hockey-reference.com/")
    pl <- stringr::str_match_all(raw_player_list, pattern)
    pl <- as.data.frame(pl[1], stringsAsFactors = FALSE)
    colnames(pl) <- c("Complete", "BlnNHL", "URL", "Name", "Active")
    player_list <- rbind(player_list, pl)
    Sys.sleep(sleep)
  }
  player_list[player_list$BlnNHL == "nhl", "BlnNHL"] <- TRUE
  player_list[player_list$BlnNHL == "non_nhl", "BlnNHL"] <- FALSE
  player_list$BlnNHL <- as.factor(player_list$BlnNHL)
  return(player_list)
}

#' Get player info from a page
#' A function to manage scraping all of the info from Hockey-Reference.com player pages.
#' Also perform collection player meta info (name, birth, etc)
#'
#' @param url The player URL
#'
#' @return a list of data.frames containing
#' \item{Tables}{Player Statistics Tables from HTML Page}
#' \item{Metas}{Player Meta information (Name, Birth, Height, Weight, Shot, etc.)}
#'
#' @keywords internal
scrapePlInfo <- function(url) {
  htmlpage <- getURLInternal(url, referer = "http://www.hockey-reference.com/")
  if (class(htmlpage) == "try-error")
    return(NULL)
  htmlpage <- gsub(htmlpage, pattern = "<!--", replacement = "")
  htmlpage <- gsub(htmlpage, pattern = "-->", replacement = "")

  # Read in Tables
  tables <- XML::readHTMLTable(htmlpage)

  m1 <- "<p><strong>Position<\\/strong>:\\s*([A-Z\\/]+)\\&*"
  meta_pos <- stringr::str_match(htmlpage, m1)[, 2]
  names(meta_pos) <- "Position"

  m1b <- "<strong>(?:Shoots|Catches)<\\/strong>:\\s*([A-Za-z\\/]+)\\s*"
  meta_hand <- stringr::str_match(htmlpage, m1b)[, 2]
  names(meta_hand) <- "Handed"

  m2 <- "<p><span itemprop=\"height\">([0-9-]+)<\\/span>.+itemprop=\"weight\">([0-9]+)lb.+\\(([0-9]+)cm,.+;([0-9]+)kg\\).<\\/p>"
  meta_h_w <- stringr::str_match(htmlpage, m2)[, c(2:5)]
  names(meta_h_w) <- c("HeightImp", "WeightImp", "HeightMetric", "WeightMetric")

  m3 <- "data-birth=\"([0-9-]*)\"+>.+\"birthPlace\">\\s*in\\&nbsp;([A-Za-z\\.(?:\\&nbsp;)]*),.+country=([A-Za-z\\.(?:\\&nbsp;)]*).+province=([A-Za-z\\.(?:\\&nbsp;)]*).+state=([A-Za-z\\.(?:\\&nbsp;)]*)\""
  meta_birth <- stringr::str_match(htmlpage, m3)[2:6]
  names(meta_birth) <- c("Birthdate", "BirthPlace", "Country", "Province", "State")

  m4 <- "data-death=\"([0-9-]*)\""
  meta_death <- stringr::str_match(htmlpage, m4)[[1]][2]
  names(meta_death) <- c("Deathdate")

  m5 <- "draft.html\">([A-Za-z]+)<\\/a>,\\s*([0-9A-Za-z]+)\\s*round\\s*\\(([0-9]+)[a-z]{2}\\&nbsp;overall\\), <[a-zA-Z\\s\\/=\"+_0-9]+\\.html\">([0-9]{4})"
  meta_draft <- stringr::str_match_all(htmlpage, m5)[[1]][, c(2:5)]
  if (is.null(nrow(meta_draft)) || nrow(meta_draft) > 0) {
    # handles redrafted players (about 30 players)
    if (class(meta_draft) == "matrix") {
      m <- character()
      for (i in 1:nrow(meta_draft)) m <- c(m, meta_draft[i, ])
      meta_draft <- m
    }
    names(meta_draft) <- c("DraftTeam", "DraftRound", "DraftOverall", "DraftYear",
      rep(c("ReDraftTeam", "ReDraftRound", "ReDraftOverall", "ReDraftYear"),
        times = ((length(meta_draft) - 4) / 4)))

    if (length(meta_draft) > 8) {
      for (i in 9:length(meta_draft)) {
        names(meta_draft)[i] <- paste0(names(meta_draft)[i], (i - 4) %/% 4)
      }
    }
  }

  # Stitch it together
  metas <- unlist(list(meta_pos, meta_hand, meta_h_w, meta_birth,
                       meta_death, meta_draft))
  return(list(Tables = tables, Metas = metas))
}

#' Scrape & compile a player's info
#' Read each table on hockey-reference.com and flatten to single useful tables.
#' Avoids tables such as last5, awards, etc.
#' Melts all NHL (regular season and playoff) and non-NHL, recording league and playoff status
#'
#' @param tables Tables output from \code{XML::readHTMLTables} of the player's page
#'
#' @return single combined stats Table as data.frame
flattenTables <- function(tables) {
  stats_nhl <- data.frame()
  if ("stats_basic_plus_nhl" %in% names(tables)) {
    stats_nhl <- tables$stats_basic_plus_nhl
    stats_nhl$Playoffs <- FALSE
    # Sometimes there's '' named columns. Messes up plyr::rbind.fill
    if ("" %in% colnames(stats_nhl))
      stats_nhl <- stats_nhl[, -which(names(stats_nhl) %in% "")]
    colnames(stats_nhl)[colnames(stats_nhl) == "Tm"] <- "Team"
  } else if ("stats_basic_nhl" %in% names(tables)) {
    stats_nhl <- tables$stats_basic_nhl
    stats_nhl$Playoffs <- FALSE
    # Sometimes there's '' named columns. Messes up plyr::rbind.fill
    if ("" %in% colnames(stats_nhl))
      stats_nhl <- stats_nhl[, -which(names(stats_nhl) %in% "")]
    colnames(stats_nhl)[colnames(stats_nhl) == "Tm"] <- "Team"
  }
  colnames(stats_nhl) <- make.unique(colnames(stats_nhl))
  colnames(stats_nhl)[colnames(stats_nhl) == "EV.1"] <- "EV.Assists"
  colnames(stats_nhl)[colnames(stats_nhl) == "PP.1"] <- "PP.Assists"
  colnames(stats_nhl)[colnames(stats_nhl) == "SH.1"] <- "SH.Assists"

  if ("skaters_advanced" %in% names(tables)) {
    advnhl <- tables$skaters_advanced
    stats_nhl <- merge(stats_nhl, advnhl, by = c("Season", "Team"), all = TRUE)
    if ("Age.y" %in% colnames(stats_nhl))
      stats_nhl$Age.y <- NULL
    if ("Lg.y" %in% colnames(stats_nhl))
      stats_nhl$Lg.y <- NULL
    if ("GP.y" %in% colnames(stats_nhl))
      stats_nhl$GP.y <- NULL
    if ("TOI.y" %in% colnames(stats_nhl))
      stats_nhl$TOI.y <- NULL

    if ("" %in% colnames(stats_nhl))
      stats_nhl <- stats_nhl[, -which(names(stats_nhl) %in% "")]
    colnames(stats_nhl)[colnames(stats_nhl) == "Age.x"] <- "Age"
    colnames(stats_nhl)[colnames(stats_nhl) == "Lg.x"] <- "Lg"
    colnames(stats_nhl)[colnames(stats_nhl) == "GP.x"] <- "GP"
    colnames(stats_nhl)[colnames(stats_nhl) == "TOI.x"] <- "TOI"
    colnames(stats_nhl)[colnames(stats_nhl) == "Tm"] <- "Team"
  }

  if ("stats_misc_nhl" %in% names(tables)) {
    stmisc <- tables$stats_misc_nhl
    colnames(stmisc)[colnames(stmisc) == "Tm"] <- "Team"
    stmisc[, 7:12] <- NULL
    stmisc <- stmisc[, names(stmisc) %in% c("Season", "Age", "Team",
                                            "Lg", "GC", "G", "A", "PTS",
                                            "GC.1", "OPS", "DPS", "PS")]
    colnames(stmisc)[colnames(stmisc) == "G"] <- "Adj.G"
    colnames(stmisc)[colnames(stmisc) == "A"] <- "Adj.A"
    colnames(stmisc)[colnames(stmisc) == "PTS"] <- "Adj.PTS"
    colnames(stmisc)[colnames(stmisc) == "GC.1"] <- "Adj.GC"

    stats_nhl <- merge(stats_nhl, stmisc, by = c("Season", "Team"),
                       all = TRUE)
    colnames(stats_nhl)[colnames(stats_nhl) == "Age.x"] <- "Age"
    colnames(stats_nhl)[colnames(stats_nhl) == "Lg.x"] <- "Lg"
    colnames(stats_nhl)[colnames(stats_nhl) == "GP.x"] <- "GP"
    colnames(stats_nhl)[colnames(stats_nhl) == "TOI.x"] <- "TOI"
    colnames(stats_nhl)[colnames(stats_nhl) == "Tm"] <- "Team"
    if ("" %in% colnames(stats_nhl))
      stats_nhl <- stats_nhl[, -which(names(stats_nhl) %in% "")]
    if ("Age.y" %in% colnames(stats_nhl))
      stats_nhl$Age.y <- NULL
    if ("Lg.y" %in% colnames(stats_nhl))
      stats_nhl$Lg.y <- NULL
  }

  playoffs_nhl <- data.frame()
  if ("stats_playoffs_nhl" %in% names(tables)) {
    playoffs_nhl <- tables$stats_playoffs_nhl
    playoffs_nhl$Playoffs <- TRUE
    colnames(playoffs_nhl)[colnames(playoffs_nhl) == "Tm"] <- "Team"
    # Sometimes there's '' named columns. Messes up plyr::rbind.fill
    if ("" %in% colnames(playoffs_nhl))
      playoffs_nhl <- playoffs_nhl[, -which(names(playoffs_nhl) %in% "")]
  }

  stats_other <- data.frame()
  if ("stats_basic_other" %in% names(tables)) {
    stats_other <- tables$stats_basic_other
    stats_other$Playoffs <- FALSE
    # Sometimes there's '' named columns. Messes up plyr::rbind.fill
    if ("" %in% colnames(stats_other))
      stats_other <- stats_other[, -which(names(stats_other) %in% "")]
    colnames(stats_other)[colnames(stats_other) == "Tm"] <- "Team"
  }

  playoffs_other <- data.frame()
  if ("stats_playoffs_other" %in% names(tables)) {
    playoffs_other <- tables$stats_playoffs_other
    playoffs_other$Playoffs <- TRUE
    # Sometimes there's '' named columns. Messes up plyr::rbind.fill
    if ("" %in% colnames(playoffs_other))
      playoffs_other <- playoffs_other[, -which(names(playoffs_other) %in%
        c(""))]
    colnames(playoffs_other)[colnames(playoffs_other) == "Tm"] <- "Team"
  }

  stats <- plyr::rbind.fill(stats_nhl, playoffs_nhl, stats_other, playoffs_other)
  colnames(stats)[colnames(stats) == "CF% rel"] <- "CF%rel"
  colnames(stats)[colnames(stats) == "FF% rel"] <- "FF%rel"
  colnames(stats)[colnames(stats) == "EV"] <- "EV.Goals"
  colnames(stats)[colnames(stats) == "PP"] <- "PP.Goals"
  colnames(stats)[colnames(stats) == "SH"] <- "SH.Goals"

  return(stats)
}

#' Get stats for a list of players
#' A function to get all player stats from a list of players (in the form provided by \code{\link{getPlayerList}})
#'
#' @param player_list A player list (data.frame) from \code{\link{getPlayerList}}
#' @param sleep Time to sleep between player scrapings
#'
#' @return a list of three data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}

getPlayerStats <- function(player_list, sleep = 30) {
  player_stats_tables <- data.frame()
  goalie_stats_tables <- data.frame()
  player_meta_tables <- data.frame()
  plist <- player_list[player_list$BlnNHL == TRUE, ]
  pretry <- NULL
  pdrop <- NULL
  if (nrow(plist) == 0)
    return(NULL)
  pb <- utils::txtProgressBar(min = 0, max = nrow(plist), initial = 0)
  player <- 0
  while (player < nrow(plist)) {
    player <- player + 1
    # prep HTML
    url <- paste0("http://www.hockey-reference.com", plist[player, "URL"])

    pname <- plist[player, "Name"]
    if (grepl("02.html", url, fixed = TRUE)) {
      pname <- paste(pname, "02")
    } else if (grepl("03.html", url, fixed = TRUE)) {
      pname <- paste(pname, "03")
    }

    scrape <- scrapePlInfo(url)

    if (is.null(scrape)) {
      if (pname %in% pretry) {
        pdrop <- c(pdrop, pname)
        message(paste0("Error getting ", pname, "'s record. Retry failed"))
      } else {
        plist <- rbind(plist, plist[player])
        pretry <- c(pretry, pname)
        message(paste0("Error getting ", pname, "'s record. Will retry."))
      }
    } else {
      # Add to record

      tables <- flattenTables(scrape[[1]])
      tables$Name <- pname

      if ("G" %in% scrape[[2]]["Position"]) {
        goalie_stats_tables <- plyr::rbind.fill(goalie_stats_tables, tables)
      } else {
        player_stats_tables <- plyr::rbind.fill(player_stats_tables, tables)
      }
      player_meta_tables <- plyr::rbind.fill(player_meta_tables,
                                             data.frame(Name = pname,
                                                        Active = plist[player, "Active"],
                                                        t(unlist(scrape[[2]]))))
    }
    utils::setTxtProgressBar(pb, player, )
    Sys.sleep(sleep)
  }
  return(list(PlayerStats = player_stats_tables, GoalieStats = goalie_stats_tables,
    PlayerMeta = player_meta_tables))
}

#' Scrape players by Alphabet
#' A function to scrape and save player tables by last name, breaking up the scraping
#' into each chunk to prevent progress loss by scraping error (HTML error)
#'
#' @param player_list a player list (data.frame) of the type created by \code{\link{getPlayerList}}
#' @param letters_to_scrape the letters of last names to scrape (default all letters)
#' @param long_sleep The length of time to sleep between letters
#' @param combine Whether to combine all player data.frames (a-z) after downloading
#' @param directory Where to store data files
#' @param ... Additional params for getPlayerStats
#'
#' @return True, if successful
#' @export
#' @keywords internal
scrapeByAlphabet <- function(player_list, letters_to_scrape = letters, long_sleep = 120,
  combine = TRUE, directory = "./data/players/", ...) {
  for (letter in letters_to_scrape) {
    message(paste0("Getting Players with last name of ", toupper(letter), "."))
    ps <- getPlayerStats(player_list[startsWith(player_list$URL, paste0("/players/",
      letter)), ], ...)
    if(!is.null(ps)){
        saveRDS(ps, paste0(directory, "players_", letter, ".RDS"))
    }
    Sys.sleep(long_sleep)
  }
  gc(verbose = FALSE)
  message("All player data downloaded")
  if (combine == TRUE) {
    return(combinePlayerDataFrames(directory, ...))
  }
  return(TRUE)
}

#' Combine single letter player data frames
#' Easily combine player data.frames into one single RDS file.
#'
#' @param directory The directory where data files are stored. Default './data/players/'
#' @param return_data_frame Boolean, whether to return data or 'TRUE' when complete
#' @param ... Additional parameters to pass
#'
#' @return TRUE, or the player data.frame, if successful
#' @export
#' @keywords internal
combinePlayerDataFrames <- function(directory = "./data/players/",
                                    return_data_frame = TRUE, ...) {
  message("Combining all player data to one object")
  ldf <- list()
  meta <- list()
  players <- list()
  goalies <- list()
  for (letter in letters) {
      f<-paste0(directory, "players_", letter, ".RDS")
      if (file.exists(f)){
        tryCatch({
              ldf[[letter]] <- readRDS(paste0(directory, "players_", letter, ".RDS"))
            },
            error = function(e) message(paste0("Error opening file players_", letter, ".RDS, Skipping...")))
          if(!is.null(ldf[[letter]])){
              meta[[letter]] <- ldf[[letter]][[3]]
              goalies[[letter]] <- ldf[[letter]][[2]]
              players[[letter]] <- ldf[[letter]][[1]]
          }
      }
      else{
          message(paste0("File '",f,"' does not exist."))
      }
  }
  all_players <- plyr::rbind.fill(players)
  all_goalies <- plyr::rbind.fill(goalies)
  all_meta <- plyr::rbind.fill(meta)
  all_df <- list(PlayerStats = all_players, GoalieStats = all_goalies, PlayerMeta = all_meta)
  saveRDS(all_df, paste0(directory, "allPlayers.RDS"))
  for (letter in letters){
      f<-paste0(directory, "players_", letter, ".RDS")
      if(file.exists(f)){
          tryCatch({
              file.remove(f)
          },
          error = function(e) print(paste0("Error deleting file ",f,", Continuing...")))
      }
  }
  if (return_data_frame)
    return(all_df)
  return(TRUE)
}

#' Clean Player Data
#' This function will process player data, returning clean data frames as a list
#'
#' @param player_data The player_data to clean up
#' @param drop_awards Whether to drop awards column.
#' @param ... Additional parameters to pass
#'
#' @return a list of three cleaned data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}
#' @export

processPlayerData <- function(player_data, drop_awards = TRUE, ...) {
  players <- player_data[[1]]
  goalies <- player_data[[2]]
  meta <- player_data[[3]]

  # Undo factors
  numeric_columns <- c("Age", "GP", "G", "A", "PTS", "+/-", "PIM", "EV.Goals",
    "PP.Goals", "SH.Goals", "GW", "EV.Assists", "PP.Assists", "SH.Assists", "S",
    "S%", "TOI", "GC", "Adj.G", "Adj.A", "Adj.PTS", "Adj.GC", "TSA", "OPS", "DPS",
    "PS", "FOW", "FOL", "FO%", "HIT", "BLK", "TK", "GV", "CF", "CA", "CF%", "CF%rel",
    "FF", "FA", "FF%", "FF%rel", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "GS",
    "W", "L", "T/O", "GA", "SA", "SV", "SV%", "GAA", "SO", "MIN", "QS", "QS%",
    "RBS", "GA%-", "GSAA", "GPS")
  pnames <- colnames(players)
  players <- data.frame(lapply(players, as.character), stringsAsFactors = FALSE)
  colnames(players) <- pnames

  gnames <- colnames(goalies)
  goalies <- data.frame(lapply(goalies, as.character), stringsAsFactors = FALSE)
  colnames(goalies) <- gnames

  mnames <- colnames(meta)
  meta <- data.frame(lapply(meta, as.character), stringsAsFactors = FALSE)
  colnames(meta) <- mnames

  players[, colnames(players) %in% numeric_columns] <- as.numeric(unlist(players[,
    colnames(players) %in% numeric_columns]))
  goalies[, colnames(goalies) %in% numeric_columns] <- as.numeric(unlist(goalies[,
    colnames(goalies) %in% numeric_columns]))

  # Fix Team vs. Tm
  if ("Tm" %in% colnames(players)) {
    players[is.na(players$Team), ]$Team <- players[is.na(players$Team), ]$Tm
    players$Tm <- NULL
  }

  if ("Tm" %in% colnames(goalies)) {
    goalies[is.na(goalies$Team), ]$Team <- goalies[is.na(goalies$Team), ]$Tm
    goalies$Tm <- NULL
  }

  # Remove double or more teams sums
  for (i in c(2:5)) {
    players <- players[players$Team != paste0(i, " Teams"), ]
    goalies <- goalies[goalies$Team != paste0(i, " Teams"), ]
  }

  # Average Time On Ice
  toi <- players$ATOI
  toi[toi == ""] <- "0:0"
  toi[is.na(toi)] <- "0:0"
  players$ATOI <- unlist(lapply(toi, function(x) as.numeric(unlist(strsplit(x,
    ":")))[1] + as.numeric(unlist(strsplit(x, ":"))[2]) / 60))
  toi <- goalies$ATOI
  toi[toi == ""] <- "0:0"
  toi[is.na(toi)] <- "0:0"
  goalies$ATOI <- unlist(lapply(toi, function(x) as.numeric(unlist(strsplit(x,
    ":")))[1] + as.numeric(unlist(strsplit(x, ":"))[2]) / 60))

  # Drop Awards
  if (drop_awards) {
    players$Awards <- NULL
    goalies$Awards <- NULL
  }

  # meta Cleanup
  meta[!is.na(meta$Birthdate), "Birthdate"] <- as.Date(meta[!is.na(meta$Birthdate),
    "Birthdate"])
  meta[!is.na(meta$Deathdate), "Deathdate"] <- as.Date(meta[!is.na(meta$Deathdate),
    "Deathdate"])

  imp <- meta$HeightImp
  imp[imp == ""] <- "0-0"
  imp[is.na(imp)] <- "0-0"
  meta$HeightImp <- unlist(lapply(imp, function(x) as.numeric(unlist(strsplit(x,
    "-")))[1] * 12 + as.numeric(unlist(strsplit(x, "-"))[2])))

  active <- meta$Active
  active[active == ""] <- "0-0"
  active[is.na(active)] <- "0-0"
  meta$ActiveStart <- unlist(lapply(active, function(x) as.numeric(unlist(strsplit(x,
    "-")))[1]))
  meta$ActiveEnd <- unlist(lapply(active, function(x) as.numeric(unlist(strsplit(x,
    "-")))[2]))
  meta$Active <- NULL

  mnumeric <- c("HeightImp", "WeightImp", "HeightMetric", "WeightMetric", "DraftRound",
    "DraftOverall", "DraftYear", "ReDraftRound", "ReDraftOverall", "ReDraftYear",
    "ActiveStart", "ActiveEnd")
  meta[, colnames(meta) %in% mnumeric] <- as.numeric(unlist(meta[, colnames(meta) %in%
    mnumeric]))

  # Order data.frame
  players <- players[with(players, order(Name, Age, Lg, Team, Playoffs)), ]
  goalies <- goalies[with(goalies, order(Name, Age, Lg, Team, Playoffs)), ]
  meta <- meta[with(meta, order(Name, Birthdate)), ]

  # Refactor Select Columns
  meta$Name <- factor(meta$Name)
  meta$Country <- gsub("&amp", "", meta$Country)
  meta$Country <- factor(meta$Country)
  meta$Province <- gsub("&amp", "", meta$Province)
  meta$Province <- factor(meta$Province)
  meta$State <- gsub("&amp", "", meta$State)
  meta$State <- factor(meta$State)
  meta$BirthPlace <- gsub("&amp", "", meta$BirthPlace)
  meta$BirthPlace <- gsub("&nbsp;", " ", meta$BirthPlace)
  meta$BirthPlace <- factor(meta$BirthPlace)
  meta$DraftTeam <- factor(meta$DraftTeam)
  if('ReDraftTeam' %in% colnames(meta))
    meta$ReDraftTeam <- factor(meta$ReDraftTeam, levels = levels(meta$DraftTeam))

  players$Season <- factor(players$Season, ordered = TRUE)
  players$Team <- factor(players$Team)
  players$Lg <- factor(players$Lg)
  players$Name <- factor(players$Name, levels = levels(meta$Name))

  goalies$Season <- factor(goalies$Season, ordered = TRUE)
  goalies$Team <- factor(goalies$Team, levels = levels(players$Team))
  goalies$Lg <- factor(goalies$Lg, levels = levels(players$Lg))
  goalies$Name <- factor(goalies$Name, levels = levels(meta$Name))

  return(list(PlayerStats = players, GoalieStats = goalies, PlayerMeta = meta))
}

#' Scrape and clean all player data
#'
#' This is a one-command function to scrape and clean all player data available from Hockey-Reference.com. This takes many hours.
#' Also saves results to a dated .RDS file.
#'
#' @param data_dir Directory to store data
#' @param ... Additional parameters to pass
#'
#' @return a list of three cleaned data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}
#' @export
#'
#' @examples
#' \dontrun{scrapePlayers()}
#' \dontrun{scrapePlayers(sleep=15, long_sleep=180)}
scrapePlayers <- function(data_dir = "./data/players/", ...) {
  if (!dir.exists(data_dir))
    dir.create(data_dir, recursive = TRUE)
  player_list <- getPlayerList(...)
  player_data <- scrapeByAlphabet(player_list = player_list, directory = data_dir,
    ...)
  player_data <- processPlayerData(player_data, ...)
  saveRDS(player_data, paste0(data_dir, "allPlayers-", Sys.Date(), ".RDS"))
  return(player_data)
}

#' Update player information (don't rescrape old players).
#'
#' @param player_data The player_data data.frame to update
#' @param data_dir The data dir of stored player information
#' @param years_back The number of years to go back (will scrape all players currently active + past \code{years_back} years)
#' @param player_list Optional player list to scrape from (default NULL --> get updated player list)
#' @param ... Additional parameters to pass
#'
#' @return a list of three updated cleaned data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}
#' @export
#'
#' @examples
#' \dontrun{updatePlayers()}
#' \dontrun{updatePlayers(data_dir = "./data/", years_back=2}
updatePlayers <- function(player_data, data_dir = "./data/players/", years_back=1, player_list = NULL, ...) {

    if(is.null(player_list)){
        player_list <- getPlayerList(...)
    }
    active <- player_list$Active
    active[active == ""] <- "0-0"
    active[is.na(active)] <- "0-0"
    player_list$ActiveEnd <- unlist(lapply(active,
                                           function(x) as.numeric(unlist(strsplit(x,"-")))[2]))
    update_players<-player_list[player_list$ActiveEnd >= as.numeric(format(Sys.Date(), "%Y"))-years_back,]
    update_players$ActiveEnd <- NULL

    new_player_data<-player_data[[1]]
    new_goalie_data<-player_data[[2]]
    new_meta<-player_data[[3]]

    new_player_data<-new_player_data[!new_player_data$Name %in% unique(update_players$Name), ]
    new_goalie_data<-new_goalie_data[!new_goalie_data$Name %in% unique(update_players$Name), ]
    new_meta<-new_player_data[!new_meta$Name %in% unique(update_players$Name), ]

    p<-scrapeByAlphabet(player_list = update_players, directory = data_dir, ...)
    p_player<-p[[1]]
    p_goalie<-p[[2]]
    p_meta<-p[[3]]
    p_player<-plyr::rbind.fill(p_player, new_player_data)
    p_goalie<-plyr::rbind.fill(p_goalie, new_goalie_data)
    p_meta<-plyr::rbind.fill(p_meta, new_meta)
    p<-list(PlayerStats = p_player, GoalieStats = p_goalie, PlayerMeta = p_meta)

    new_player_data <- processPlayerData(p, ...)
    saveRDS(new_player_data, paste0(data_dir, "allPlayers-", Sys.Date(), ".RDS"))
    return(new_player_data)
}
