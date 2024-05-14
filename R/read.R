#' @title Read cruise input file
#'
#' @description Read in mission input file required for `runCruisePlanning`. It will
#' check for required columns.
#'
#' @param file a connection or a character string giving the name of the file to load
#'
#' @author Chantelle Layton
#'
#' @importFrom utils read.csv
#'
#' @return a data.frame
#' @export

readCruiseInput <- function(file) {
  d <- utils::read.csv(file = file)
  # define required columns
  requiredColumns <- c("lon_dd",
                       "lat_dd",
                       "optime",
                       "xoptime",
                       "transit_speed")
  # define required columns description (for error messages)
  requiredColumnsDescrip <- c("A numeric value indicating longitude in decimal degrees.",
                              "A numeric value indicating latitude in decimal degrees.",
                              "A numeric value indicating the total expected operational time in hours at given lon_dd, lat_dd.",
                              "A numeric value indicating any extra operational time in hours at given lon_dd, lat_dd",
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
}
