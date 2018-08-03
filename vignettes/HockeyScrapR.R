## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval=FALSE-------------------------------------------------
#  install.packages('devtools')
#  
#  devtools::install_github(pbulsink/HockeyScrapR)

## ----library-------------------------------------------------------------
library(HockeyScrapR)

## ----getrosters----------------------------------------------------------
roster<-getCurrentRosters(teams = 'TOR', sleep = 0, progress = FALSE)

## ----seeRosterForwards---------------------------------------------------
roster$TOR$Forwards

## ----seeRosterGoalies----------------------------------------------------
roster$TOR$Goalies

## ----seeRostersInjury----------------------------------------------------
roster$TOR$Injury

## ----seeRosterUpdate-----------------------------------------------------
roster$TOR$UpdateDate

## ----seeRostersList------------------------------------------------------
names(roster$TOR)

## ----saveRoster, eval=FALSE----------------------------------------------
#  roster<-scrapeRosters(data_dir = "./data/rosters/", teamUrlList = teamTOR, sleep = 0, progress = FALSE)

