#' Get player info from a page
#' A function to manage scraping all of the info from Quanthockey.com player pages.
#' Also perform collection player meta info (name, birth, etc)
#'
#' @param url The player URL
#'
#' @return a list of data.frames containing
#' \item{Tables}{Player Statistics Tables from HTML Page}
#' \item{Metas}{Player Meta information (Name, Birth, Height, Weight, Shot, etc.)}
#'
#' @keywords internal
scrapePlInfo.QH <- function(url) {
  htmlpage <- getURLInternal(url, referer = "http://www.quanthockey.com/")
  if (class(htmlpage) == "try-error")
    return(NULL)
  if (grepl("<h1>Could not find the player you were looking for.<\\/h1>", htmlpage))
    return(NULL)
  htmlpage <- gsub(htmlpage, pattern = "<!--", replacement = "")
  htmlpage <- gsub(htmlpage, pattern = "-->", replacement = "")

  # Read in Tables
  tables <- XML::readHTMLTable(htmlpage, stringsAsFactors=FALSE)

  #This fixes weird XML processing of the non-breaking hyphen in years
  for (t in 1:length(tables)){
    if('Season' %in% names(tables[[t]])){
      tables[[t]]$Season<-as.character(tables[[t]]$Season)
      for(s in 1:length(tables[[t]]$Season)){
        if(tables[[t]]$Season[s] != ""){
          yrs<-stringr::str_match(tables[[t]]$Season[s],'([0-9]{4}).+([0-9]{2})')
          tables[[t]]$Season[s]<-paste(c(yrs[,2],yrs[,3]), collapse = '-')
        }
      }
    }
  }

  m1 <- "<h1 itemprop=\"name\" id=\"pp_title\">([\\p{L}\\s\\'\\-]+)<\\/h1>"
  meta_name <- stringr::str_match(htmlpage, m1)[, 2]
  meta_name <- gsub("\\'", "", meta_name)
  names(meta_name) <- "Name"

  m2 <- "<time itemprop=\"birthDate\" datetime=\"([0-9-]+)\">[0-9a-zA-Z\\s,]+<\\/time>"
  meta_birthdate <- stringr::str_match(htmlpage, m2)[, 2]
  names(meta_birthdate) <- "Birthdate"

  m3 <- "<span itemprop=\"birthPlace\">([\\p{L}\\s\\.]+),?([\\p{L}\\s\\.]+)?,([\\p{L}\\s\\.]+)<\\/span>"
  meta_birthplace <- stringr::str_trim(stringr::str_match(htmlpage, m3)[,c(2:4)])
  names(meta_birthplace) <- c("Birthplace", "Province", "Country")

  m4 <- "<br>(Forward|Defenseman|Goalie),?\\s?(?:shoots|catches)?\\s?(left|right)?<br>"
  meta_pos <- stringr::str_match(htmlpage, m4)[,c(2:3)]
  names(meta_pos) <- c("Position", "Handed")

  m5 <- "<br>([0-9])\\s?\\'\\s?([0-9]+)\\s?\"\\s?\\|\\s?([0-9]+)\\s?lb<br>"
  meta_size <- stringr::str_match(htmlpage, m5)[,c(2:4)]
  names(meta_size) <- c("Feet", "Inches", "Weight")

  m6 <- "<span class=\"hl draft\">Drafted by: <a href=\"\\/nhl-draft\\/en\\/teams\\/(?:[a-zA-Z\\-]+)\\.html\">([A-Za-z\\s]+)<\\/a>, ([0-9]+)\\. round \\(\\#([0-9]+) overall\\), in <a href=\"[\\/a-z0-9\\-\\.]+\">([0-9]+) NHL (?:Entry|Amateur) Draft<\\/a><\\/span>"
  meta_draft <- stringr::str_match(htmlpage, m6)[, c(2:5)]
  names(meta_draft)<-c("DraftTeam", "DraftRound","DraftOverall","DraftYear")

  # Stitch it together
  metas <- unlist(list(meta_name, meta_birthdate, meta_birthplace, meta_pos, meta_size, meta_draft))
  return(list(Tables = tables, Metas = metas))
}

#' Scrape & compile a player's info
#' Read each table on Quanthockey.com and flatten to single useful tables.
#' Avoids tables such as last5, awards, etc.
#' Melts all NHL (regular season and playoff) and non-NHL, recording league and playoff status
#'
#' @param tables Tables output from \code{XML::readHTMLTables} of the player's page
#'
#' @return single combined stats Table as data.frame
flattenPlayerTables.QH <- function(tables) {
  stats_reg <- data.frame()
  if ("r_stats" %in% names(tables)) {
    stats_reg <- tables$r_stats
    stats_reg$Playoffs <- FALSE
    colnames(stats_reg)[colnames(stats_reg) == "LGE"] <- "League"
  }
  stats_reg<-stats_reg[stats_reg$Age != '', ]

  stats_play <- data.frame()
  if ("p_stats" %in% names(tables)) {
    stats_play <- tables$p_stats
    stats_play$Playoffs <- TRUE
    colnames(stats_play)[colnames(stats_play) == "LGE"] <- "League"
  }
  stats_play<-stats_play[stats_play$Age != '', ]

  stats <- plyr::rbind.fill(stats_reg, stats_play)
  return(stats)
}

#' Get stats for a list of players
#' A function to get all player stats from a list of players (in the form provided by \code{\link{getPlayerList.QH}})
#'
#' @param player_list A player list (data.frame) from \code{\link{getPlayerList.QH}}
#' @param sleep Time to sleep between player scrapings
#' @param progress Whether to show a progress bar. Default = TRUE.
#'
#' @return a list of three data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}

getPlayerStats.QH <- function(player_list, sleep = 30, progress=TRUE) {
  player_stats_tables <- data.frame()
  goalie_stats_tables <- data.frame()
  player_meta_tables <- data.frame()
  plist <- player_list[player_list$Exists == TRUE, ]
  plist <- plist[sample(nrow(plist)), ]
  pdrop <- NULL
  if (nrow(plist) == 0)
    return(NULL)
  if(progress && nrow(plist) > 1){
    pb <- progress::progress_bar$new(
      format = "  downloading players [:bar] :percent eta: :eta",
      clear = FALSE,
      width = 80,
      total=nrow(plist)
      )
    pb$tick(0)
  }
  player <- 0
  while (player < nrow(plist)) {
    player <- player + 1
    # prep HTML
    url <- paste0("http://www.quanthockey.com/hockey-stats/en/profile.php?player=", plist[player, "PlayerNum"])

    scrape <- scrapePlInfo.QH(url)

    if (is.null(scrape)) {
      pdrop <- c(pdrop, plist[player, "PlayerNum"])
    } else {
      # Add to record
      metas<-scrape$Metas
      pname<-metas['Name']
      metas['PlayerNum']<-plist[player, "PlayerNum"]
      tables <- flattenPlayerTables.QH(scrape$Tables)
      if(nrow(tables)>0){
        tables$Name <- pname
        tables$PlayerNum <- plist[player, "PlayerNum"]

        if ("Goalie" %in% scrape$Metas["Position"]) {
          goalie_stats_tables <- plyr::rbind.fill(goalie_stats_tables, tables)
        } else {
          player_stats_tables <- plyr::rbind.fill(player_stats_tables, tables)
        }
      }
      player_meta_tables <- plyr::rbind.fill(player_meta_tables, data.frame(t(metas)))
    }

    Sys.sleep(sleep)
    if(progress){
      pb$tick()
    }
  }
  if (length(pdrop)>0){
    m1<-paste0("Error getting ",length(pdrop), ' player record(s):\n')
    m2<-paste(pdrop[order(pdrop)], collapse=', ')
    m<-paste0(m1, m2)
    message(m)
  }
  return(list(PlayerStats = player_stats_tables, GoalieStats = goalie_stats_tables, PlayerMeta = player_meta_tables, pdrop=pdrop))
}


#' Get a list of Players
#' Download the full list of active and past players from Quanthockey.com
#'
#' @param prebuilt Whether t0 use a prebuilt player list or try a new one.
#'
#' @return a data.frame containing:
#' \item{Complete}{The complete line that data was scraped from}
#' \item{BlnNHL}{Whether the player played any time in the NHL}
#' \item{URL}{Player page URL}
#' \item{Name}{Player Name}
#' \item{Active}{Whether the player is currently active}
#' @export
getPlayerList.QH <- function(prebuilt=FALSE) {
  if (prebuilt){
    player_list<-player_list_internal
  }
  else{
    player_list<-data.frame("PlayerNum"=c(1:50000), "Exists"=rep(TRUE, 50000), "Name"=rep("", 50000))
  }
  return(player_list)
}

#' Scrape players by Alphabet
#' A function to scrape and save player tables by last name, breaking up the scraping
#' into each chunk to prevent progress loss by scraping error (HTML error)
#'
#' @param player_list a player list (data.frame) of the type created by \code{\link{getPlayerList.QH}}
#' @param group_by The group size to scrape. Default 1000
#' @param long_sleep The length of time to sleep between groups
#' @param start_at The playernumber to start at. Default = 1
#' @param save_player_list Whether to save the player list
#' @param combine Whether to combine all player data.frames (groups) after downloading
#' @param directory Where to store data files
#' @param ... Additional params for getPlayerStats
#'
#' @return True, if successful
#' @export
#' @keywords internal
scrapeByNumber <- function(player_list, group_by = 1000, long_sleep = 120, combine = TRUE,
  directory = "./data/players/", start_at = 1, save_player_list = TRUE, ...) {
  if (substring(directory, nchar(directory)) != '/')
    directory<-paste0(directory, '/')
  if (!dir.exists(directory))
    dir.create(directory, recursive = TRUE)
  if (start_at > nrow(player_list))
    return(FALSE)
  for (num in c(start_at:(player_list[nrow(player_list), 'PlayerNum']/group_by))) {
    start<-((num-1)*group_by)+1
    end<-num*group_by
    if (end > nrow(player_list))
      end<-nrow(player_list)
    if (end < start)
      break
    message(paste0("Getting players from ", start, " to ", end,"."))
    ps <- getPlayerStats.QH(player_list[c(start:end),], ...)
    player_list[player_list$PlayerNum %in% ps$pdrop, 'Exists']<-FALSE
    player_list<-merge(player_list[,c("PlayerNum", "Exists")], ps$PlayerMeta[,c("PlayerNum", "Name")], by="PlayerNum", all=TRUE)
    ps$pdrop<-NULL
    if (!is.null(ps)) {
      saveRDS(ps, paste0(directory, "QH_players_", start, "-", end, ".RDS"))
    }
    if (save_player_list){
      player_list_internal<-player_list
      devtools::use_data(player_list_internal, internal = TRUE, overwrite = TRUE)
    }
    Sys.sleep(long_sleep)
  }
  gc(verbose = FALSE)
  message("All player data downloaded")
  if (combine == TRUE) {
    return(combinePlayerDataFrames.QH(directory, ...))
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
combinePlayerDataFrames.QH <- function(directory = "./data/players/", return_data_frame = TRUE, ...) {
  message("Combining all player data to one object")
  if (substring(directory, nchar(directory)) != '/')
    directory<-paste0(directory, '/')
  if (!dir.exists(directory))
    dir.create(directory, recursive = TRUE)
  ldf <- list()
  meta <- list()
  players <- list()
  goalies <- list()
  files<-list.files(path=directory, pattern='QH_players_[0-9\\-]+\\.RDS', full.names = TRUE)
  if (length(files) > 0){
    for (f in 1:length(files)) {
      fl<-files[f]
      if (file.exists(fl)) {
        tryCatch({
          ldf[[f]] <- readRDS(fl)
        }, error = function(e) message(paste0("Error opening file ",fl, ", Skipping...")))
        if (!is.null(ldf[[f]])) {
          meta[[f]] <- ldf[[f]][[3]]
          goalies[[f]] <- ldf[[f]][[2]]
          players[[f]] <- ldf[[f]][[1]]
        }
      } else {
        message(paste0("File '", fl, "' does not exist."))
      }
    }
  }
  all_players <- plyr::rbind.fill(players)
  all_goalies <- plyr::rbind.fill(goalies)
  all_meta <- plyr::rbind.fill(meta)
  all_df <- list(PlayerStats = all_players, GoalieStats = all_goalies, PlayerMeta = all_meta)
  saveRDS(all_df, paste0(directory, "QH_allPlayers-",Sys.Date(),".RDS"))
  for (f in 1:length(files)) {
    fl <- files[f]
    if (file.exists(fl)) {
      tryCatch({
        file.remove(fl)
      }, error = function(e) print(paste0("Error deleting file ", fl, ", Continuing...")))
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
#' @param ... Additional parameters to pass
#'
#' @return a list of three cleaned data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}
#' @export
processPlayerData.QH <- function(player_data, ...) {
  players <- player_data[[1]]
  goalies <- player_data[[2]]
  meta <- player_data[[3]]

  # Undo factors
  numeric_columns <- c("Age", "GP", "G", "A", "P", "+/-", "PIM", "PPG", "SHG", "GWG", "SH%",
                       "GAA", "SV%", "W", "L", "GA", "SV", "SO", "PlayerNum", "DraftYear", "DraftOverall",
                       "DraftRound","Weight","Inches","Feet")


  pnames <- colnames(players)
  players <- data.frame(lapply(players, as.character), stringsAsFactors = FALSE)
  colnames(players) <- pnames

  gnames <- colnames(goalies)
  goalies <- data.frame(lapply(goalies, as.character), stringsAsFactors = FALSE)
  colnames(goalies) <- gnames

  mnames <- colnames(meta)
  meta <- data.frame(lapply(meta, as.character), stringsAsFactors = FALSE)
  colnames(meta) <- mnames

  players[, colnames(players) %in% numeric_columns] <- as.numeric(unlist(players[, colnames(players) %in%
    numeric_columns]))
  goalies[, colnames(goalies) %in% numeric_columns] <- as.numeric(unlist(goalies[, colnames(goalies) %in%
    numeric_columns]))
  meta[, colnames(meta) %in% numeric_columns] <- as.numeric(unlist(meta[, colnames(meta) %in% numeric_columns]))

  # meta Cleanup
  meta$Birthdate <- as.Date(meta$Birthdate)

  meta$HeightImp <- meta$Feet * 12 + meta$Inches
  meta$HeightMet <- meta$HeightImp * 2.54
  meta$Feet<-NULL
  meta$Inches<-NULL

  # Order data.frame
  players <- players[with(players, order(Name, Age, League, Team, Playoffs)), ]
  goalies <- goalies[with(goalies, order(Name, Age, League, Team, Playoffs)), ]
  meta <- meta[with(meta, order(Name, Birthdate)), ]

  # Refactor Select Columns
  meta$Name <- factor(meta$Name)
  meta$Country <- factor(meta$Country)
  meta$Province <- factor(meta$Province)
  meta$BirthPlace <- factor(meta$Birthplace)
  meta$DraftTeam <- factor(meta$DraftTeam)
  meta$Position <- factor(meta$Position)

  players$Season <- factor(players$Season, ordered = TRUE)
  players$Team <- factor(players$Team)
  players$League <- factor(players$League)
  players$Name <- factor(players$Name, levels = levels(meta$Name))

  goalies$Season <- factor(goalies$Season, ordered = TRUE)
  goalies$Team <- factor(goalies$Team, levels = levels(players$Team))
  goalies$League <- factor(goalies$League, levels = levels(players$League))
  goalies$Name <- factor(goalies$Name, levels = levels(meta$Name))

  return(list(PlayerStats = players, GoalieStats = goalies, PlayerMeta = meta))
}

#' Scrape and clean all player data
#'
#' This is a one-command function to scrape and clean all player data available from Quanthockey.com. This takes many hours.
#' Also saves results to a dated .RDS file.
#'
#' @param directory Directory to store data
#' @param player_list A player list of the type type created by \code{\link{getPlayerList.QH}}
#' @param ... Additional parameters to pass
#'
#' @return a list of three cleaned data.frames, containing
#' \item{PlayerStats}{Combined player statistics}
#' \item{GoalieStats}{Combined goalie statistics}
#' \item{PlayerMeta}{Meta statistics for all (goalies and players)}
#' @export
#'
#' @examples
#' \dontrun{scrapePlayers.QH()}
#' \dontrun{scrapePlayers.QH(sleep=15, long_sleep=180, group_by=500)}
scrapePlayers.QH <- function(player_list, directory = "./data/players/", ...) {
  if (substring(directory, nchar(directory)) != '/')
    directory<-paste0(directory, '/')
  if (!dir.exists(directory))
    dir.create(directory, recursive = TRUE)
  player_data <- scrapeByNumber(player_list = player_list, directory = directory, ...)
  if (class(player_data)=='logical') {
    if (!player_data)
      message("Error in getting player data. Check your parameters.")
  }
  else {
    player_data <- processPlayerData.QH(player_data, ...)
    saveRDS(player_data, paste0(directory, "QH_allPlayers-", Sys.Date(), ".RDS"))
  }
  return(player_data)
}
