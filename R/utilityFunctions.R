#' Extra wrapped getURL
#' \code{\link[RCurl]{getURL}} wrapped in useragent sampling and try/retry, with referer.
#'
#' @param url The URL to try download
#' @param referer A referer source
#'
#' @return raw url contents output from getURL as the HTTP reply from the server, or try-error if an error has occurred and cannot be recovered from.
#'
#' @export
#' @keywords internal
getURLInternal <- function(url, referer = "http://www.google.ca") {
  agents <- "HockeyScrapR http://github.com/pbulsink/HockeyScrapR"
  # htmlpage <- try(RCurl::getURL(url, header = FALSE, .opts = RCurl::curlOptions(referer = referer,
  #   header = TRUE, followLocation = TRUE, useragent = agents)),
  #   silent = TRUE)

  htmlpage <- try(httr::GET(url=url, httr::user_agent(agents), httr::config("referer" = referer)))

  if (httr::http_error(htmlpage)) {
    message(paste0("HTML Try Error on: ", url, "/nStatus Code: ", htmlpage$status_code))
    htmlpage <- try(httr::GET(url=url))
  }
  if(htmlpage$status_code == "200"){
    return(httr::content(htmlpage, as="text"))
  } else {
    stop("HTML error on ", url, "/nStatus Code: ",htmlpage$status_code)
  }
}

#' Get the current Season 'year code'. From 01 August 2016 to 31 July 2017 will return 2017
#'
#' @return Numeric, 4 digit year
#'
#' @export
getCurrentSeason <- function() {
  year <- as.numeric(format(Sys.Date(), "%Y"))
  if (as.numeric(format(Sys.Date(), "%m")) >= 10) {
    year <- year + 1
  }
  return(year)
}

uncommentHTML<-function(htmlpage){
  htmlpage<-gsub(pattern = '<!--', replacement = '', x = htmlpage)
  htmlpage<-gsub(pattern = '-->', replacement = '', x = htmlpage)
  return(htmlpage)
}
