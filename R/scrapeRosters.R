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
#' @param teamUrlList A list of team roster page urls on dailyfaceoff.com. Defaults to correct team URLs.
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

getCurrentRosters <- function(sleep = 30, teamUrlList = teamURLList, progress=TRUE) {
  if(progress){
    pb <- progress::progress_bar$new(
      format = "  downloading rosters [:bar] :percent eta: :eta",
      clear = FALSE,
      width = 80,
      total=nrow(teamUrlList)
    )
    pb$tick(0)
  }

  nteams<-nrow(teamUrlList)
  rosters<-vector(mode = 'list', length=nteams)
  names(rosters)<-teamURLList[,2]

  for (i in 1:nrow(teamUrlList)) {
    htmlpage <- getURLInternal(teamUrlList[i, 1], referer = "http://www.dailyfaceoff.com/")
    tabs <- XML::readHTMLTable(htmlpage, header = FALSE, stringsAsFactors=FALSE)
    date_pattern <- "Last update: ([A-Za-z0-9\\., ]+)<\\/div>"
    dt <- as.character(as.Date(gsub("\\.", "", stringr::str_match(htmlpage, date_pattern)[1, 2]),
      format = "%b%t%e,%t%Y"))
    player_pattern<-"<span class=\\\"player-name\\\">([A-Za-z\\-\\.\\s]+)</span>"
    rosters[i] <- list(Forwards = stringr::str_match(unlist(tabs$forwards), player_pattern)[,2],
                       Defence = stringr::str_match(unlist(tabs$defense), player_pattern)[,2],
                       Goalies = stringr::str_match(unlist(tabs$goalie_list), player_pattern)[,2],
                       PP1 = c(stringr::str_match(unlist(tabs[3]), player_pattern)[,2],
                               stringr::str_match(unlist(tabs[4]), player_pattern)[,2]),
                       PP2 = c(stringr::str_match(unlist(tabs[5]), player_pattern)[,2],
                               stringr::str_match(unlist(tabs[6]), player_pattern)[,2]),
                       Injuries = stringr::str_match(unlist(tabs[8]), player_pattern)[,2],
                       UpdateDate = as.Date(dt))

    Sys.sleep(sleep)
    if(progress){
      pb$tick()
    }
  }
  return(rosters)
}


#' Scrape all roster data
#'
#' This is a one-command function to scrape all roster data available from DailyFaceoff.com.
#' Also saves results to a dated .RDS file.
#'
#' @param data_dir Directory to store data in
#' @param ... Additional parameters to pass
#'
#' @return roster information data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' scrapeRosters()
#' }
scrapeRosters <- function(data_dir = "./data/rosters/", ...) {
  rosters <- getCurrentRosters(...)
  saveRDS(rosters, paste0(data_dir, "rosters-", Sys.Date(), ".RDS"))
  return(rosters)
}
