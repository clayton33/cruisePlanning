## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  library(cruisePlanning)
#  data("halifaxLineMission")
#  halifaxLineMission

## ----eval=FALSE---------------------------------------------------------------
#  library(cruisePlanning)
#  # provide path to the file
#  file <- 'path/to/file.csv'
#  # provide startTime, two suggestions
#  startTime <- ISOdatetime(year = 2024,
#                           month = 4,
#                           day = 18,
#                           hour = 6,
#                           min = 0,
#                           sec = 0,
#                           tz = "" # leaving it as "" denotes the current time zone, can set to 'UTC'
#                           )
#  startTime <- as.POSIXct(x = "2024-04-18 06:00:00") # again, can specify another timezone other than your current timezone by the 'tz' parameter
#  d <- runCruisePlanning(file = file,
#                         startTime = startTime)

## ----eval=FALSE---------------------------------------------------------------
#  library(cruisePlanning)
#  data("halifaxLineMission")
#  # provide path to the file
#  #file <- 'path/to/file.csv'
#  # provide startTime
#  startTime <- ISOdatetime(year = 2024,
#                           month = 4,
#                           day = 18,
#                           hour = 6,
#                           min = 0,
#                           sec = 0,
#                           tz = "" # leaving it as "" denotes the current time zone, can set to 'UTC'
#                           )
#  d <- runCruisePlanning(file = halifaxLineMission, # replace `halifaxLineMission` with user defined `file`
#                         startTime = startTime,
#                         writeCsv = FALSE,
#                         writeSubsetCsv = FALSE
#                         )
#  # create map
#  map <- makeCruisePlanningMap(data = d,
#                               markercolumn = 'type',
#                               labelColumn = 'station',
#                               markerLabelFactor = 'Operations')
#  map # to view in 'Viewer' in RStudio.

