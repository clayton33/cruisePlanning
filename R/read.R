#' @title Read cruise input file
#'
#' @description Read in mission input file required for `runCruisePlanning`. It will
#' check for required columns. See details for required columns.
#'
#' @param file a connection or a character string giving the name of the file to load.
#'
#' @details
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
#' @author Chantelle Layton
#'
#' @importFrom utils read.csv
#'
#' @return a data.frame
#' @export

readCruiseInput <- function(file) {
  d <- utils::read.csv(file = file)
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
}
