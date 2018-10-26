teamURLList <- data.frame(URL = c("http://www.dailyfaceoff.com/teams/anaheim-ducks/line-combinations/",
  "http://www.dailyfaceoff.com/teams/arizona-coyotes/line-combinations/", "http://www.dailyfaceoff.com/teams/boston-bruins/line-combinations/",
  "http://www.dailyfaceoff.com/teams/buffalo-sabres/line-combinations/", "http://www.dailyfaceoff.com/teams/calgary-flames/line-combinations/",
  "http://www.dailyfaceoff.com/teams/carolina-hurricanes/line-combinations/", "http://www.dailyfaceoff.com/teams/chicago-blackhawks/line-combinations/",
  "http://www.dailyfaceoff.com/teams/colorado-avalanche/line-combinations/", "http://www.dailyfaceoff.com/teams/columbus-blue-jackets/line-combinations/",
  "http://www.dailyfaceoff.com/teams/dallas-stars/line-combinations/", "http://www.dailyfaceoff.com/teams/detroit-red-wings/line-combinations/",
  "http://www.dailyfaceoff.com/teams/edmonton-oilers/line-combinations/", "http://www.dailyfaceoff.com/teams/florida-panthers/line-combinations/",
  "http://www.dailyfaceoff.com/teams/los-angeles-kings/line-combinations/", "http://www.dailyfaceoff.com/teams/minnesota-wild/line-combinations/",
  "http://www.dailyfaceoff.com/teams/montreal-canadiens/line-combinations/", "http://www.dailyfaceoff.com/teams/nashville-predators/line-combinations/",
  "http://www.dailyfaceoff.com/teams/new-jersey-devils/line-combinations/", "http://www.dailyfaceoff.com/teams/new-york-islanders/line-combinations/",
  "http://www.dailyfaceoff.com/teams/new-york-rangers/line-combinations/", "http://www.dailyfaceoff.com/teams/ottawa-senators/line-combinations/",
  "http://www.dailyfaceoff.com/teams/philadelphia-flyers/line-combinations/", "http://www.dailyfaceoff.com/teams/pittsburgh-penguins/line-combinations/",
  "http://www.dailyfaceoff.com/teams/san-jose-sharks/line-combinations/", "http://www.dailyfaceoff.com/teams/st-louis-blues/line-combinations/",
  "http://www.dailyfaceoff.com/teams/tampa-bay-lightning/line-combinations/", "http://www.dailyfaceoff.com/teams/toronto-maple-leafs/line-combinations/",
  "http://www.dailyfaceoff.com/teams/vancouver-canucks/line-combinations/", "http://www.dailyfaceoff.com/teams/washington-capitals/line-combinations/",
  "http://www.dailyfaceoff.com/teams/winnipeg-jets/line-combinations/", "http://www.dailyfaceoff.com/teams/vegas-golden-knights/line-combinations/"),
                          Team = c("ANA", "ARI", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL", "DET", "EDM",
  "FLA", "LAK", "MIN", "MTL", "NSH","NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "WSH", "WPG", "VGK"), stringsAsFactors = FALSE)


#' Get Current estimated rosters
#'
#' @param sleep The amount of time to sleep between requests
#' @param teams A list of teams to scrape. Defaults to all (as NULL)
#' @param progress Whether to show a progress bar. Default = TRUE.
#'
#' @return a list of vectors by team, with each team's list named:
#' \item{Forwards}{The names of the Forwards playing}
#' \item{Defence}{The names of the Defence playing}
#' \item{Goalies}{The names of the Goalies playing}
#' \item{PP1}{The first powerplay unit}
#' \item{PP2}{The second powerplay unit}
#' \item{Injuries}{Any players on the injury reserve}
#' \item{updateDate}{The date the Team's page was updated}
#' @export
getCurrentRosters <- function(sleep = 30, teams = NULL, progress=TRUE) {
  if(!is.null(teams)){
    if(sum(!teams %in% teamURLList$Team) != 0){
      stop(paste("Only select teams that are in the following list:", teamURLList$Team, collapse = ' '))
    }
    teamURLs <- teamURLList[teamURLList$Team %in% teams, ]
  } else {
    teamURLs <- teamURLList
  }

  if(progress){
    pb <- progress::progress_bar$new(
      format = "  downloading rosters [:bar] :percent eta: :eta",
      clear = FALSE,
      width = 80,
      total=nrow(teamURLs)
    )
    pb$tick(0)
  }

  rosters <- data.frame(Team = character(), Players = character(), updateDate = character())

  nteams<-nrow(teamURLs)
  rosters<-vector(mode = 'list', length=nteams)
  names(rosters)<-teamURLs[,2]
  date_pattern <- "Last update: ([A-Za-z0-9\\., ]+)<\\/div>"
  #Note: includes special characters
  player_pattern<-"\\$[0-9]+([A-z\u00c0-\u00ff\\s.\\-]+)$"

  for (i in 1:nrow(teamURLs)) {
    htmlpage <- getURLInternal(teamURLs[i, 1], referer = "http://www.dailyfaceoff.com/")
    tabs <- XML::readHTMLTable(htmlpage, header = FALSE, stringsAsFactors=FALSE)

    dt <- as.character(as.Date(gsub("\\.", "", stringr::str_match(htmlpage, date_pattern)[1, 2]),
      format = "%b%t%e,%t%Y"))
    f <- stringr::str_match(unlist(tabs$forwards), player_pattern)[,2]
    d <- stringr::str_match(unlist(tabs$defense), player_pattern)[,2]
    g <- stringr::str_match(unlist(tabs$goalie_list), player_pattern)[,2]
    p1 <- c(stringr::str_match(unlist(tabs[3]), player_pattern)[,2], stringr::str_match(unlist(tabs[4]), player_pattern)[,2])
    p2 <- c(stringr::str_match(unlist(tabs[5]), player_pattern)[,2], stringr::str_match(unlist(tabs[6]), player_pattern)[,2])
    ij <- stringr::str_match(unlist(tabs[8]), player_pattern)[,2]
    #Making sure no NA make it to the rosters.
    f <- f[!is.na(f)]
    d <- d[!is.na(d)]
    g <- g[!is.na(g)]
    p1 <- p1[!is.na(p1)]
    p2 <- p2[!is.na(p2)]
    ij <- ij[!is.na(ij)]
    if(length(ij) == 0){
      ij<-""
    }

    rosters[[i]] <- list(Forwards = f, Defence = d, Goalies = g, PP1 = p1,
                       PP2 = p2, Injuries = ij, UpdateDate = as.Date(dt))

    Sys.sleep(sleep)
    if(progress){
      pb$tick()
    }
  }
  if(progress){
    pb$update(1)
  }
  return(rosters)
}


#' Scrape all roster data
#'
#' This is a one-command function to scrape all roster data available from DailyFaceoff.com.
#' Also saves results to a dated .RDS file.
#'
#' @param data_dir Directory to store data in
#' @param ... Additional parameters to pass to getCurrentRosters
#'
#' @return roster information list
#' @export
#'
#' @examples
#' \dontrun{
#' scrapeRosters()
#' }
scrapeRosters <- function(data_dir = "./data/rosters/", ...) {
  rosters <- getCurrentRosters(...)
  if(!dir.exists(data_dir)){
    dir.create(data_dir, recursive = TRUE)
  }
  saveRDS(rosters, paste0(data_dir, "rosters-", Sys.Date(), ".RDS"))
  return(rosters)
}
