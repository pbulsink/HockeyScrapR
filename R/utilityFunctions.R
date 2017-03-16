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
getURLInternal <- function(url, referer = "http://www.google.com") {
  agents <- c("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36")
  htmlpage <- try(RCurl::getURL(url, header = FALSE, .opts = RCurl::curlOptions(referer = referer,
    header = TRUE, followLocation = TRUE, useragent = agents[sample(1:8, 1)])),
    silent = TRUE)

  if (class(htmlpage) == "try-error") {
    message(paste0("HTML Try Error on: ", url))
    htmlpage <- try(RCurl::getURL(url, header = FALSE, .opts = RCurl::curlOptions(referer = referer,
      header = TRUE, followLocation = TRUE, useragent = agents[sample(1:8,
        1)])), silent = TRUE)
  }
  return(htmlpage)
}

#' Get the current Season 'year code'. From 01 August 2016 to 31 July 2017 will return 2017
#'
#' @return Numeric, 4 digit year
#'
#' @export
getCurrentSeason <- function() {
  year <- as.numeric(format(Sys.Date(), "%Y"))
  if (as.numeric(format(Sys.Date(), "%m")) >= 8) {
    year <- year + 1
  }
  return(year)
}
