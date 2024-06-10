rm(list=ls())
library(usethis)
library(cruisePlanning)
file <- 'data-raw/halifaxLineMission/halifaxLine.csv'
halifaxLineMission <- cruisePlanning::readCruiseInput(file)
usethis::use_data(halifaxLineMission, compress = "xz", overwrite = T)
