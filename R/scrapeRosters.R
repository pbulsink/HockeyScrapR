teamURLList <- data.frame(URL = c("http://www2.dailyfaceoff.com/teams/lines/13/anaheim-ducks/",
  "http://www2.dailyfaceoff.com/teams/lines/35/arizona-coyotes", "http://www2.dailyfaceoff.com/teams/lines/15/boston-bruins/",
  "http://www2.dailyfaceoff.com/teams/lines/16/buffalo-sabres/", "http://www2.dailyfaceoff.com/teams/lines/17/calgary-flames",
  "http://www2.dailyfaceoff.com/teams/lines/18/carolina-hurricanes", "http://www2.dailyfaceoff.com/teams/lines/19/chicago-blackhawks",
  "http://www2.dailyfaceoff.com/teams/lines/20/colorado-avalanche", "http://www2.dailyfaceoff.com/teams/lines/21/columbus-blue-jackets",
  "http://www2.dailyfaceoff.com/teams/lines/22/dallas-stars", "http://www2.dailyfaceoff.com/teams/lines/23/detroit-red-wings",
  "http://www2.dailyfaceoff.com/teams/lines/24/edmonton-oilers", "http://www2.dailyfaceoff.com/teams/lines/25/florida-panthers",
  "http://www2.dailyfaceoff.com/teams/lines/26/los-angeles-kings", "http://www2.dailyfaceoff.com/teams/lines/27/minnesota-wild",
  "http://www2.dailyfaceoff.com/teams/lines/28/montreal-canadiens", "http://www2.dailyfaceoff.com/teams/lines/29/nashville-predators",
  "http://www2.dailyfaceoff.com/teams/lines/30/new-jersey-devils", "http://www2.dailyfaceoff.com/teams/lines/31/new-york-islanders",
  "http://www2.dailyfaceoff.com/teams/lines/32/new-york-rangers", "http://www2.dailyfaceoff.com/teams/lines/33/ottawa-senators",
  "http://www2.dailyfaceoff.com/teams/lines/34/philadelphia-flyers", "http://www2.dailyfaceoff.com/teams/lines/36/pittsburgh-penguins",
  "http://www2.dailyfaceoff.com/teams/lines/37/san-jose-sharks", "http://www2.dailyfaceoff.com/teams/lines/38/st-louis-blues",
  "http://www2.dailyfaceoff.com/teams/lines/39/tampa-bay-lightning", "http://www2.dailyfaceoff.com/teams/lines/40/toronto-maple-leafs",
  "http://www2.dailyfaceoff.com/teams/lines/41/vancouver-canucks", "http://www2.dailyfaceoff.com/teams/lines/42/washington-capitals",
  "http://www2.dailyfaceoff.com/teams/lines/14/winnipeg-jets"), Team = c("Anaheim Ducks", "Arizona Coyotes",
  "Boston Bruins", "Buffalo Sabres", "Calgary Flames", "Carolina Hurricanes", "Chicago Blackhawks",
  "Colorado Avalanche", "Columbus Blue Jackets", "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers",
  "Florida Panthers", "Los Angeles Kings", "Minnesota Wild", "Montreal Canadiens", "Nashville Predators",
  "New Jersey Devils", "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers",
  "Pittsburgh Penguins", "San Jose Sharks", "St. Louis Blues", "Tampa Bay Lightning", "Toronto Maple Leafs",
  "Vancouver Canucks", "Washington Capitals", "Winnipeg Jets"), stringsAsFactors = FALSE)

#' Get Current estimated rosters
#'
#' @param sleep The amount of time to sleep between requests
#' @param teamUrlList A list of team roster page urls on dailyfaceoff.com. Defaults to correct team URLs.
#' @param progress Whether to show a progress bar. Default = TRUE.
#'
#' @return a long data frame with three columns:
#' \item{Team}{The team}
#' \item{Player}{The name of the Forward, Defence or Goalie playing}
#' \item{updateDate}{The date the Team's page was updated}
getCurrentRosters <- function(sleep = 30, teamUrlList = teamURLList, progress=TRUE) {
  rosters <- data.frame(Team = character(), Players = character(), updateDate = character())

  if(progress){
  pb <- utils::txtProgressBar(min = 0, max = nrow(teamUrlList), initial = 0)
  }

  for (i in 1:nrow(teamUrlList)) {
    htmlpage <- getURLInternal(teamUrlList[i, 1], referer = "http://www2.dailyfaceoff.com/")
    tabs <- XML::readHTMLTable(htmlpage, header = FALSE)
    pattern <- "Last update: ([A-Za-z0-9\\., ]+)<\\/div>"
    dt <- as.character(as.Date(gsub("\\.", "", stringr::str_match(htmlpage, pattern)[1, 2]),
      format = "%b%t%e,%t%Y"))
    r <- data.frame(Team = rep(teamUrlList[i, 2]), Players = c(levels(unlist(tabs$forwards)),
      levels(unlist(tabs$defense)), levels(unlist(tabs$goalie_list))), updateDate = rep(dt),
      stringsAsFactors = FALSE)
    rosters <- rbind(rosters, r)
    if(progress){
      utils::setTxtProgressBar(pb, i)
    }
    Sys.sleep(sleep)
  }

  rosters$Team <- factor(rosters$Team)
  rosters$updateDate <- as.Date(rosters$updateDate)
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
