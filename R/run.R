#' @title Run cruise planning.
#'
#' @description Run cruise planning. The distance to next set of coordinate, `dist_to_next`, transit time to next set of coordinates,
#' `transit_time`, arrival time on the coordinates, `arrival_time`, departure time at the coordinates, `departure_time`,
#' and the depth at the set of coordinates is added to the provided variables in the supplied `file`.
#'
#' @param file a connection or a character string giving the name of the `.csv` file to load or a `data.frame` that contains the minimum
#' set of variables. See details on the minimum and suggested variables.
#' @param startTime a `POSIXct` format of the start date time. Suggested to use `ISOdatetime()` for simplicity.
#' @param writeCsv a logical value, if `TRUE` a `.csv` file will be written in the working directory using the provided
#' filename, with the current local date and time appended to the end of the filename.
#' @param writeSubsetCsv a logical value, if `TRUE` a `.csv` file will be written in the working directory using the provided
#' filename, with the current local date and time appended to the end of the filename using provided names in `subsetNames`.The
#' output file will provide variable names in order provided. During a cruise, this is what is readily printed off and circulated to staff.
#' @param subsetNames a vector of names in provided `file` to be output into a `.csv`. Ignored if `writeSubsetCsv = FALSE`.
#' Defaults are `c('lon_dd', 'lat_dd','station','operation','depth','dist_to_next', 'arrival_time','departure_time')`.
#'
#' @details The following provides additional details for what to include in the provided `.csv` file.
#' @section file:
#' At a minimum, the supplied `.csv` file should include the following variables
#' \tabular{ll}{
#' \strong{variable name} \tab \strong{variable description} \cr
#' lon_dd \tab A numeric value indicating longitude in decimal degrees. \cr
#' lat_dd \tab A numeric value indicating latitude in decimal degrees. \cr
#' optime \tab A numeric value indicating the total expected operational time in hours at given lon_dd, lat_dd. \cr
#' transit_speed \tab A numeric value indicating the transit speed of the vessel.
#' }
#' Additional variables that are suggested (but not limited to):
#' \tabular{ll}{
#' \strong{variable name} \tab \strong{variable description} \cr
#' xoptime \tab A numeric value indicating any extra operational time in hours at given lon_dd, lat_dd. \cr
#' station \tab A character string indicating the name of the station. \cr
#' operation \tab An character string indicating the specific operation order for the given operation type (e.g. Net_CTD, Net_CTD_Argo, Net_2_CTD). \cr
#' type \tab A character string indicating the operation type (e.g. `operation`, `transit`).\cr
#' }
#'
#' @section Sample of usage :
#' \preformatted{
#' library(cruisePlanning)
#' data(halifaxLineMission)
#' startTime <- ISOdatetime(year = 2024,
#'                          month = 4,
#'                          day = 18,
#'                          hour = 6,
#'                          min = 0,
#'                          sec = 0,
#'                          tz = "" # leaving it as "" denotes the current time zone, can set to 'UTC'
#'                          )
#' d <- runCruisePlanning(file = halifaxLineMission,
#'                        startTime = startTime,
#'                        writeCsv = FALSE, # for example purposes
#'                        writeSubsetCsv = FALSE # for example purposes
#'                        )
#' }
#'
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
  if(inherits(file, "data.frame")){
    d <- file
    # check to see that it has minimum set of variables
    # this is copied over from 'read.R (readCruiseInput), there is a note there to update this fn if necessary
    # define minimum required columns
    requiredColumns <- c("lon_dd",
                         "lat_dd",
                         "optime",
                         "transit_speed")
    # define required columns description (for error messages)
    requiredColumnsDescrip <- c("A numeric value indicating longitude in decimal degrees.",
                                "A numeric value indicating latitude in decimal degrees.",
                                "A numeric value indicating the total expected operational time in hours at given lon_dd, lat_dd.",
                                "A numeric value indicating the transit speed of the vessel.")
    # check for required columns
    dnames <- names(d)
    ind <- requiredColumns %in% dnames
    if(!all(ind)){
      missingColumns <- which(!ind)
      missingText <- paste("Missing the following required columns in provided file: ",
                           paste(
                             paste(requiredColumns[missingColumns],
                                   requiredColumnsDescrip[missingColumns], sep = ' : '),
                             collapse = '\n'),
                           sep = '\n')
      stop(missingText)
    } else {
      d
    }
  } else if(inherits(file, "character")){
    # Read in cruise input file
    d <- readCruiseInput(file)
  } else {
    stop("first argument 'file' must be a 'data.frame' or 'character' string")
  }

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
  # calculate the total time of the planned cruise
  duration <- paste("The estimated total time to complete the proposed cruise is",
                          sprintf('%.2f', difftime(strptime(dnew[['arrival_time']][dim(dnew)[1]],"%Y-%m-%d %H:%M:%S"),
                                          strptime(dnew[['departure_time']][1],"%Y-%m-%d %H:%M:%S"),
                                  units = 'days')),
                     "days.", sep=" ")
  cat(duration, sep = '\n')
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
  return(dnew)
}
