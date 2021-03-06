% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrapeRosters.R
\name{getCurrentRosters}
\alias{getCurrentRosters}
\title{Get Current estimated rosters}
\usage{
getCurrentRosters(sleep = 30, teams = NULL, progress = TRUE)
}
\arguments{
\item{sleep}{The amount of time to sleep between requests}

\item{teams}{A list of teams to scrape. Defaults to all (as NULL)}

\item{progress}{Whether to show a progress bar. Default = TRUE.}
}
\value{
a list of vectors by team, with each team's list named:
\item{Forwards}{The names of the Forwards playing}
\item{Defence}{The names of the Defence playing}
\item{Goalies}{The names of the Goalies playing}
\item{PP1}{The first powerplay unit}
\item{PP2}{The second powerplay unit}
\item{Injuries}{Any players on the injury reserve}
\item{updateDate}{The date the Team's page was updated}
}
\description{
Get Current estimated rosters
}
