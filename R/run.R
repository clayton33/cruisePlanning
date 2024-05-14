#' @title Run cruise planning.
#'
#' @description Run cruise planning. The distance to next set of coordinate, `dist_to_next`, transit time to next set of coordinates,
#' `transit_time`, arrival time on the coordinates, `arrival_time`, departure time at the coordinates, `departure_time`,
#' and the depth at the set of coordinates is added to the provided
#'
#' @param file a connection or a character string giving the name of the file to load.
#' @param startTime a `POSIXct` format of the start date time. Suggested to use `ISOdatetime()` for simplicity.
#' @param writeCsv a logical value, if `TRUE` a `.csv` file will be written in the working directory using the provided
#' filename, with the current local date and time appended to the end of the filename.
#' @param writeSubsetCsv a logical value, if `TRUE` a `.csv` file will be written in the working directory using the provided
#' filename, with the current local date and time appended to the end of the filename using provided names in `abbreviatedNames`
#' @param subsetNames a vector of names in provided filename to be output into a `.csv`. Ignored if `writeAbbreviatedCsv = FALSE`.
#' Defaults are `c('lon_dd', 'lat_dd','station','operation','depth','dist_to_next', 'arrival_time','departure_time')`. The
#' output file will provide variable names in order provided. During a cruise, this is what is readily printed off and circulated to staff.
#'
#' @author Chantelle Layton
#'
#' @return a data.frame with added calculated variables, which includes `dist_to_next`, `transit_time`, `arrival_time`, and `departure_time`
#' @export
#'
runCruisePlanning <- function(file,
                              startTime,
                              writeCsv = TRUE,
                              writeSubsetCsv = TRUE,
                              subsetNames = c('lon_dd',
                                              'lat_dd',
                                              'operation',
                                              'station',
                                              'depth',
                                              'dist_to_next',
                                              'arrival_time',
                                              'departure_time')) {
  # Read in cruise input file
  d <- readCruiseInput(file)
  # Calculations
  ## Distance to next station
  distToNextStation <- calculateDistanceToNextStation(longitude = d[['lon_dd']],
                                                      latitude = d[['lat_dd']])
  ## Transit time to next station
  transitTime <- calculateTransitTime(distanceToNextStation = distToNextStation,
                                      transitSpeed = d[['transit_speed']])
  ## Arrival and departure time
  ### xoptime might not be in the file, check, if not, vector of 0's
  if('xoptime' %in% names(d)){
    xoptime <- d[['xoptime']]
  } else {
    xoptime <- rep(0, dim(d)[1])
  }
  arrivalAndDepartureTime <- calculateArrivalAndDepartureTime(startTime = startTime,
                                                              transitTime = transitTime,
                                                              operationTime = d[['optime']],
                                                              extraOperationTime = xoptime)
  ## Infer bottom depth
  bottomDepth <- calculateBottomDepth(longitude = d[['lon_dd']],
                                      latitude = d[['lat_dd']])
  # Add variables to d
  # reduce number of decimal points for output using sprintf()
  dnew <- data.frame(d,
                     dist_to_next = sprintf('%.2f', distToNextStation),
                     transit_time = sprintf('%.1f', transitTime),
                     arrival_time = arrivalAndDepartureTime[['arrivalTime']],
                     departure_time = arrivalAndDepartureTime[['departureTime']],
                     depth = sprintf('%.0f', bottomDepth))
  # write data if specified
  if(writeCsv){
    # Construct output filename
    ## get directoryname
    directoryname <- dirname(file)
    ## get filename
    filename  <- basename(file)
    ## remove extension from filename
    filename <- strsplit(filename, split = '\\.')[[1]][1]
    ## get date and time to append at end of file
    currentDate <- Sys.time()
    fileDate <- format(currentDate, '%Y%m%d_%H%M')
    # combine
    outputFile <- paste(directoryname,
                        paste0(paste(filename,
                                     fileDate,
                                     sep = '_'),
                               '.csv'),
                        sep = '/')
    utils::write.csv(x = dnew, file = outputFile)
  }
  if(writeSubsetCsv){
    # subset 'dnew' to the order of supplied subsetNames
    ok <- unlist(lapply(subsetNames, function(k) which(names(dnew) == k)))
    dnewsub <- dnew[, ok]
    # similar to writeCsv code, construct output filename
    ## get directoryname
    directoryname <- dirname(file)
    ## get filename
    filename  <- basename(file)
    ## remove extension from filename
    filename <- strsplit(filename, split = '\\.')[[1]][1]
    ## get date and time to append at end of file
    currentDate <- Sys.time()
    fileDate <- format(currentDate, '%Y%m%d_%H%M')
    # combine
    outputFile <- paste(directoryname,
                        paste0(paste(filename,
                                     fileDate,
                                     'subset',
                                     sep = '_'),
                               '.csv'),
                        sep = '/')
    utils::write.csv(x = dnewsub, file = outputFile)
  }
}
