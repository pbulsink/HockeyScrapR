---
title: "HockeyScrapR Usage"
author: "Philip Bulisnk"
date: "2019-10-18"
output:
  rmarkdown::html_vignette:
    toc: true
    keep_md: true
vignette: >
  %\VignetteIndexEntry{HockeyScrapR Usage}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




#Introduction
HockeyScrapR is a collection of tools for scraping information from various sites to use in hockey analytics. 

#Installation
To use, start by installing the package from GitHub. If you haven't installed the `devtools` package, you'll need to do so, else you can just run the second line below.

```r
install.packages('devtools')

devtools::install_github(pbulsink/HockeyScrapR)
```

Once the package is installed, load it with the library function:

```r
library(HockeyScrapR)
```

#General Usage
Many of the functions have similar usage notes. There's a few parameters that are optional, but you may wish to use in your own programming:
- `sleep=n` is used to pause the scraping for `n` seconds. Use a sleep of 15-30 seconds to respect the websites from which you're collecting data. A sleep of 0 is fine if you're collecting only one object, but may appear as abusive to the website if you're scraping a whole collection of data. You could be temporarally blocked or banned if you scrape too hard.
- `progress=TRUE` progress bars can provide good information if you're using an interactive session, but can clutter up automated data collection. Progress bars are by default on, but turn them off by calling `progress=FALSE`. 
- `data_dir` is the directory you wish to save any data collected in. Not all functions save data, but those that do have a default. You can change this if you want to store your data other than in a `data` subfolder of the working directory. 

#Daily Rosters
The rosters for teams change almost daily, depending on coaching decisions, injuries, trades, suspensions, and how well players are performing. You may wish to have a whole list of who is playing on each team, including their power play units, and injury list.

I'm interested just in just the rosters from one team, Toronto, but you would normally collect the whole league at once. 

```r
roster<-getCurrentRosters(teams = 'TOR', sleep = 0, progress = FALSE)
```

Normally, just calling `getCurrentRosters()` would be the typical usage. 

This returns a roster list, which we can explore. Here's Toronto's forwards:

```r
roster$TOR$Forwards
#> character(0)
```
Note that this returns a flat list, not a matrix of pairings. One could generate them by taking the first, fifth, and ninth player as the 'first liners', etc., but the ordering is subject to change depending on total skater numbers. 

Similarly, here are their active goalies:

```r
roster$TOR$Goalies
#> character(0)
```

And here's the Injury Reserve:

```r
roster$TOR$Injury
#> NULL
```

These were all current as of the updated date:

```r
roster$TOR$UpdateDate
#> [1] "2019-10-18"
```

There are a few other categories scraped, here's the full list:

```r
names(roster$TOR)
#> [1] "Forwards"   "Defence"    "Goalies"    "PP1"        "PP2"       
#> [6] "Injuries"   "UpdateDate"
```

The same information can be collected and returned, and a file saved for future reference, by calling:

```r
roster<-scrapeRosters(data_dir = "./data/rosters/", teamUrlList = teamTOR, sleep = 0, progress = FALSE)
```

#Past Scores and Upcoming Schedule

#Player Stats

