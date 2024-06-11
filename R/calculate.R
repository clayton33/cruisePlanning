#' @title Calculate distance to next station
#' @description
#' Calculate the distance to next station.
#'
#' @param longitude A numeric value indicating longitude in decimal degrees.
#' @param latitude A numeric value indicating latitude in decimal degrees.
#' @param unit A character string indicating the output unit for distance. Options include `nm` and `km`, `nm` is the default.
#'
#' @importFrom oce geodDist
#' @export

calculateDistanceToNextStation <- function(longitude,
                                           latitude,
                                           unit = 'nm') {
  # check unit
  if(!unit %in% c('nm', 'km')){
    stop("Unit must be either 'nm' or 'km'")
  }
  # set up index for calculation
  idx1 <- 1:(length(longitude)-1)
  idx2 <- 2:length(longitude)
  # calculate distance
  distkm <- oce::geodDist(longitude1 = longitude[idx1],
                          latitude1 = latitude[idx1],
                          longitude2 = longitude[idx2],
                          latitude2 = latitude[idx2])
  # add 0 for length agreements
  distkm <- c(distkm, 0)
  # convert to nautical miles (nm)
  distnm <- distkm * 0.539957
  if(unit == 'nm') {return(distnm)}
  if(unit == 'km') {return(distkm)}
}

#' @title Calculate transit time to next station.
#' @description Calculate the transit time to next station.
#'
#' @param distanceToNextStation a numeric vector of distances to next station in either
#' nautical miles (`nm`) or kilometers (`km`), as denoted by `distanceUnit`,
#' @param distanceUnit a character vector indicating the unit of `distanceToNextStation`.
#' @param transitSpeed a numeric vector of transit speed of the vessel in either knots (`kts`)
#' or kilometers per hour (`kph`), as indicated by `speedUnit`.
#' @param speedUnit a character vector indicating the unit of `transitSpeed`.
#'
#' @author Chantelle Layton
#' @export

calculateTransitTime <- function(distanceToNextStation,
                                 distanceUnit = 'nm',
                                 transitSpeed,
                                 speedUnit = 'kts'){
  # check that units make sense
  # OK combinations :
  #   distanceUnit = 'nm' and speedUnit = 'kts'
  #   distanceUnit = 'km' and speedUnit = 'kph'
  transitTime <- distanceToNextStation / transitSpeed
  return(transitTime)
}

#' @title Calculate station arrival and departure time
#' @description Calculate station arrival and departure time
#'
#' @param startTime a `POSIXct` value indicating the start time of the cruise.
#' @param transitTime a numeric vector of calculated transit times in hours.
#' @param operationTime a numeric vector of total operation time in hours.
#' @param extraOperationTime a numeric vector of extra operation time in hours.
#'
#' @author Chantelle Layton
#' @export
#'

calculateArrivalAndDepartureTime <- function(startTime,
                                             transitTime,
                                             operationTime,
                                             extraOperationTime){
  # initialize vectors for [arrival, departure]Time
  arrivalTime <- departureTime <- vector(mode = 'logical',
                                         length = length(transitTime))
  # initialize arrivalTime[1] and departureTime[1]
  arrivalTime[1] <- "start"
  departureTime[1] <- as.character(format(startTime, '%Y-%m-%d %H:%M:%S'))
  # initialize 's' with startTime
  s <- startTime
  for (i in 2:(length(transitTime)-1)){
    # arrivalTime = departureTime[i-1] + transitTime[i-1]
    s <- s + (transitTime[i-1]*3600)
    arrivalTime[i] <- as.character(format(s, '%Y-%m-%d %H:%M:%S'))
    # departureTime = arrivalTime[i] + operationTime[i] + extraOperationTime[i]
    s <- s + ((operationTime[i] + extraOperationTime[i]) * 3600)
    departureTime[i] <- as.character(format(s, '%Y-%m-%d %H:%M:%S'))
  }
  # fill last index
  arrivalTime[length(transitTime)] <- as.character(format(s + (transitTime[(length(transitTime)-1)]*3600),'%Y-%m-%d %H:%M:%S'))
  departureTime[length(transitTime)] <- "end"
  outdf <- data.frame(arrivalTime = arrivalTime,
                      departureTime = departureTime)
  outdf
}

#' @title Infer bottom depth at stations.
#' @description Infer bottom depth at stations by downloading ETOPO1 dataset using the `oce::download.topo()` function.
#' The topographic data is saved as a netCDF file in the local working directory, and on subsequent calls to
#' `oce::download.topo()` with identical parameters, it will simply return the cached file. See the help page for `oce:download.topo()`
#' for additional details.
#'
#' @param longitude A numeric value indicating longitude in decimal degrees.
#' @param latitude A numeric value indicating latitude in decimal degrees.
#' @param lonrange A numeric vector of length 2 indicating the range of longitude to use to download topographic data. Default is `NULL`, meaning
#' it will be inferred from the provided `longitude`.
#' @param latrange A numeric vector of length 2 indicating the range of latitude to use to download topographic data. Default is `NULL`, meaning
#' it will be inferred from the provided `latitude`.
#'
#' @importFrom oce download.topo
#' @importFrom oce read.topo
#' @importFrom fields interp.surface
#'
#' @author Chantelle Layton
#' @export
#'
#'

calculateBottomDepth <- function(longitude,
                                 latitude,
                                 lonrange = NULL,
                                 latrange = NULL){
  # Need to download topo data
  ## define little function for rounding
  mround <- function(x, base, method = 'round'){
    base*do.call(method, list(x = x/base))
  }
  ## round up to nearest 10 degrees to avoid needing to download
  ## while at sea. rounding to nearest 5 was considered, but 10
  ## will help with boundary cases.
  if(is.null(lonrange)){
    lonrange <- c(mround(min(longitude), base = 10, method = 'floor'),
                  mround(max(longitude), base = 10, method = 'ceiling'))
    ## do a check for near boundary cases
    ## if min/max longitude/latitude is within 2.5 degrees of lon/latrange, +/- 10
    lonrange <- c(ifelse(abs(min(longitude) - lonrange[1]) < 2.5, lonrange[1] - 10, lonrange[1]),
                  ifelse(abs(max(longitude) - lonrange[2]) < 2.5, lonrange[2] + 10, lonrange[2]))
    ## return lon range for the user to see
    cat(paste('For topography file : '), sep = '\n')
    cat(paste0('    Using a longitude range of (', paste(rev(lonrange), collapse = ', '), ')'), sep = '\n')
  } else {
    ## return lon range for the user to see
    cat(paste('For topography file : '), sep = '\n')
    cat(paste0('    Using a longitude range of (', paste(lonrange, collapse = ', '), ')'), sep = '\n')
  }
  if(is.null(latrange)){
    latrange <- c(mround(min(latitude), base = 10, method = 'floor'),
                  mround(max(latitude), base = 10, method = 'ceiling'))
    ## do a check for near boundary cases
    ## if min/max longitude/latitude is within 2.5 degrees of lon/latrange, +/- 10
    latrange <- c(ifelse(abs(min(latitude) - latrange[1]) < 2.5, latrange[1] - 10, latrange[1]),
                  ifelse(abs(max(latitude) - latrange[2]) < 2.5, latrange[2] + 10, latrange[2]))
    ## return lat range for the user to see
    cat(paste('For topography file : '), sep = '\n')
    cat(paste0('    Using a latitude range of (', paste(latrange, collapse = ', '), ')'), sep = '\n')
  } else {
    ## return lat range for the user to see
    cat(paste('For topography file : '), sep = '\n')
    cat(paste0('    Using a latitude range of (', paste(latrange, collapse = ', '), ')'), sep = '\n')
  }

  ## download topo file
  cat('Downloading topo data in local directory', sep = '\n')
  topoFile <- oce::download.topo(west = lonrange[1], east = lonrange[2],
                            south = latrange[1], north = latrange[2],
                            resolution = 1)
  topo <- oce::read.topo(topoFile)
  #
  # Infer depth at provided longitude and latitude using topo
  zs <- fields::interp.surface(obj = list(x = topo[['longitude']],
                                          y = topo[['latitude']],
                                          z = topo[['z']]),
                               loc = cbind(longitude,
                                           latitude))
  zs
}
