#' @title Create cruise map
#'
#' @param data a data.frame containing cruise planning variables, either from running `runCruisePlanning`, or reading
#' in the data file using `readCruiseInput`.
#' @param markerColumn a character vector of length one indicating which column in `data` to color code the markers. Default is `NULL`,
#' meaning all coordinates will be the same colour.
#' @param markerLabelFactor a character vector indicating which factors in `markerColumn` to label on the map.
#' Default is `NULL`, meaning put labels for all factors.
#' @param labelColumn a character vector of length one indicating which column in `data` to use as labels on the map
#' for the given indicated factor in `markerLabelFactor` for the `markerColumn`.
#' @param markerPopupNames a character vector of desired columns in `data` to display in popup for each station.
#' Default is `NULL`, meaning include information from all columns in `data`. If `FALSE`, no popup will show.
#' Default is `NULL`, meaning label each station with a sequential number. If `FALSE`, no labels will be placed at each station.
#' @param NAFOShpFile a connection or a character string giving the name of the `.shp` file associated with the
#' NAFO zones. See details on where to download this file.
#' @param MPAShpFile a connection or a character string giving the name of the `.shp` file associated
#' with marine protected areas. See details on where to download this file.
#' @param ClosureShpFile a connection or a character string giving the name of the `.shp` file associated
#' with Other Effective Area-Based Conservation Measures (OECM). See details on where to download this file.
#'
#' @details
#' * NAFO `.shp` file can be downloaded from \url{https://www.nafo.int/Data/GIS} (accessed on 2024-05-15), click on `Divisions`.
#' * MPA `.shp` file can be downloaded from \url{https://open.canada.ca/data/en/dataset/a1e18963-25dd-4219-a33f-1a38c4971250}
#' (accessed on 2024-05-15). Under `Data and Resources`, navigate to the `Oceans Act Marine Protected Areas` with `dataset` and `SHP`
#' listed as keywords. Click on `Explore`, then click `Go to resource` and it will download.
#' * Closure `.shp` file can be downloaded from \url{https://open.canada.ca/data/en/dataset/44769543-7a23-4991-a53f-c2cf7c7a946f}
#' (accessed on 2024-05-15). Under `Data and Resources`, navigate to the `Other Effective Area-Based Conservation Measures` with `dataset` and `SHP`
#' listed as keywords. Click on `Explore`, then click `Go to resource` and it will download.
#'
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet fitBounds
#' @importFrom leaflet addPolygons
#' @importFrom leaflet addPolylines
#' @importFrom leaflet addCircles
#' @importFrom leaflet addCircleMarkers
#' @importFrom leaflet addLabelOnlyMarkers
#' @importFrom leaflet addLegend
#' @importFrom leaflet addLayersControl
#' @importFrom leaflet highlightOptions
#' @importFrom leaflet markerClusterOptions
#' @importFrom leaflet labelOptions
#' @importFrom leaflet layersControlOptions
#' @importFrom leaflet colorFactor
#' @importFrom magrittr %>%
#'
#' @author Chantelle Layton
#' @export

makeCruisePlanningMap <- function(data,
                                  markerColumn = NULL,
                                  labelColumn = NULL,
                                  markerLabelFactor = NULL,
                                  markerPopupNames = NULL,
                                  NAFOShpFile,
                                  MPAShpFile,
                                  ClosureShpFile
                                  ){
  ##
  # check markerColumn input
  ##
  if(is.null(markerColumn)){ # add placeholder to get markerCol if NULL
    data <- data.frame(data,
                       markerColumn = 'All')
    markerColumn <- 'markerColumn'
  } else { # not null, but check that the column in is 'data'
    markerColumnCheck <- markerColumn %in% names(data)
    if(!markerColumnCheck) stop(paste("Provided 'markerColumn', ", markerColumn, "is not a column in provided 'data'.",
                                      "Please provide one of the following :", paste(names(data), collapse = ', '), ".",
                                      "Or set parameter to NULL."))
  }
  ## set up colors for coordinate markers
  markerCol <- leaflet::colorFactor(palette = 'Dark2', levels = unique(data[[which(names(data) == markerColumn)]]))
  ##
  # check markerLabelFactor input
  ##
  if(is.null(markerLabelFactor)){ # labeling all points
    markerLabelIdx <- 1:dim(data)[1]
  } else { # not NULL, subset based on provided factors
    if(markerColumn == 'markerColumn'){ # markerColumn = NULL, this is it's placeholder
      # therefore label at every station
      markerLabelIdx <- 1:dim(data)[1]
    } else { # markerColumn provided, it was checked earlier that it exists, so no need to again
      # check that the factors provided are in 'markerColumn'
      inMarkerColumn <- markerLabelFactor %in% data[[markerColumn]]
      if(all(inMarkerColumn)){ # all provided factors are in defined markerColumn
        markerLabelIdx <- which(data[[markerColumn]] %in% markerLabelFactor)
      } else { # at least one provided factor not in markerColumn, proceed depending on number
        if(any(inMarkerColumn)){
          message(paste("only", length(which(inMarkerColumn)), "provided 'markerLabelFactor' in 'markerColumn'.",
                        "Factor(s) not in 'markerColumn' are:", paste(markerLabelFactor[!inMarkerColumn], collapse = ', '), ".",
                        "Potential factors to provide include : ",  paste(unique(data[[markerColumn]]), collapse = ', '), "."))
          markerLabelIdx <- which(data[[markerColumn]] %in% markerLabelFactor)
        } else { # none of provided factors in 'markerColumn'
          stop(paste("Provided 'markerLabelFactor', ", paste(markerLabelFactor, collapse = ', '), "are not in 'markerColumn'",
                     "Please provide one or more of the following :", paste(unique(data[[markerColumn]]), collapse = ', '), ".",
                     "OR supply 'NULL' to label all points."))
        } # closes else of 'any(inMarkerColumn)'
      } # closese else of 'all(inMarkerColumn)'
    } # closes else of if 'markerColumn' input is not NULL
  } # closes else of if 'is.null(markerLabelFactor)', meaning factors have been provided
  ##
  # check labelColumn
  ##
  if(is.null(labelColumn)){ #sequentially label points
    # same index as markerLabelFactor
    markerLabel <- 1:length(markerLabelIdx)
  } else if(labelColumn == FALSE){
    markerLabel <- NA # not sure about this
    markerLabelIdx <- NULL # maybe this to not throw error ?
  } else { # labelColumn provided
    # check that labelColumn in data
    labelColumnCheck <- labelColumn %in% names(data)
    if(!labelColumnCheck) {
      stop(paste("Provided 'labelColumn', ", labelColumn, "is not a column in provided 'data'.",
                 "Please provide one of the following :", paste(names(data), collapse = ', '), ".",
                 "Or set parameter to NULL or FALSE."))
    } else { # labelColumn in data
      markerLabel <- data[[labelColumn]][markerLabelIdx]
    }
  }
  ##
  # markerPopupNames, construct popup text
  ##
  createPopup <- function(data, names){
    dsub <- data[, names(data) %in% names]
    pu <- apply(X = dsub,
                MARGIN = 1, # rows
                FUN = function(k) paste("<br>","<b>", names(k), ":</b>", k, collapse = '') # not sure if 'collapse' is correct
                )
    pu
  }
  if(is.null(markerPopupNames)){ # include all information
    markerPopup <- createPopup(data = data, names = names(data))
  } else if(markerPopupNames == FALSE){
    markerPopup <- NA # not sure
  } else { # markerPopupNames provided
    # check that the names provided are in data
    inData <- markerPopupNames %in% names(data)
    if(any(inData)){
      message(paste("only", length(which(inData)), "provided 'markerPopupNames' in 'data'.",
                    "'markerPopupNames' not in data are:", paste(markerPopupNames[!inData], collapse = ', '), ".",
                    "Potential names to provide include : ",  paste(names(data), collapse = ', '), "."))
      markerPopup <- createPopup(data = data, names = markerPopupNames)
    } else { # none of provided factors in 'markerColumn'
      stop(paste("Provided 'markerPopupNames', ", paste(markerPopupNames, collapse = ', '), "are not in 'data'",
                 "Please provide one or more of the following :", paste(names(data), collapse = ', '), ".",
                 "OR supply 'NULL' to include all information in data or FALSE for no popup."))
    } # closes else of 'any(inData)'
  }
  ##
  # read in shp files
  ##
  ## NAFO
  nafo <- sf::st_read(NAFOShpFile) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
  ## MPA
  mpa <- sf::st_read(MPAShpFile) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
  ## Closures
  closures <- sf::st_read(ClosureShpFile) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
  # Create map
  route <- leaflet::leaflet(data) %>%
    leaflet::fitBounds(min(data$lon_dd),min(data$lat_dd),max(data$lon_dd),max(data$lat_dd)) %>%
    leaflet::addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}',
                      attribution = 'Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC')%>%

    #NAFO Divisions
    leaflet::addPolygons(data=nafo, group = "NAFO Zones", color = "#444444", weight = 0.5,
                         smoothFactor = 0.5, popup=paste(nafo$Division),
                         opacity = 0.5, fillOpacity = 0,
                         highlightOptions = leaflet::highlightOptions(color = "white", weight = 2))%>%
    # #EEZ
    # leaflet::addPolylines(data = EEZ,
    #                       group = "EEZ",
    #                       color = "magenta", weight = 2,
    #                       smoothFactor = 0.5, opacity = 0.5)%>%
    #MPA
    leaflet::addPolygons(data = mpa, group = "MPA", color = "#444444", weight = 1,
                         smoothFactor = 0.5, popup=paste(mpa$NAME_E),
                         opacity = 1.0, fillOpacity = 0.2,
                         highlightOptions = leaflet::highlightOptions(color = "white", weight = 2))%>%
    #Closures
    leaflet::addPolygons(data = closures, group = "Closure Areas", color = "#444444", weight = 1,
                         smoothFactor = 0.5, popup=paste(closures$NAME_E),
                         opacity = 1.0, fillOpacity = 0.2,
                         highlightOptions = leaflet::highlightOptions(color = "white", weight = 2))%>%

    #Mission Route
    leaflet::addPolylines(data = data, lng = ~lon_dd, lat = ~lat_dd, group="Route", color = "blue", weight=1,
                          popup=paste(#file,
                                      "Route",
                                      "|",
                                      #dur,
                                      sep=" "),
                          highlightOptions = leaflet::highlightOptions(color = "white", weight = 5, bringToFront = FALSE))%>%
    leaflet::addCircleMarkers(data = data, lng = ~lon_dd, lat = ~lat_dd,
                              popup = markerPopup,
                              group = "Route",
                              color = markerCol(data[[markerColumn]]),
                              weight = 2, radius = 6, stroke = TRUE,
                              opacity = 1, fillOpacity = 1)%>%

    # markerLabels
    leaflet::addLabelOnlyMarkers(lng=data[['lon_dd']][markerLabelIdx], lat=data[['lat_dd']][markerLabelIdx],
                                 label =  as.character(markerLabel),
                                 group="Station Labels",
                                 labelOptions = leaflet::labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
    # #Transit Points
    # leaflet::addCircles(lng = tpts$lon_dd,lat = tpts$lat_dd, group="Transit Locations",
    #                     color="red", weight = 5, radius=10, stroke = TRUE,
    #                     opacity=0.5, fillOpacity = 1,
    #                     popup=paste ("ID:",tpts$ID,"|",
    #                                  "Station:", tpts$type,"|",
    #                                  "Lon:", round(tpts$lon_dd,3), "|",
    #                                  "Lat:", round(tpts$lat_dd,3),"|",
    #                                  "Arrival:",tpts$arrival,16,"|",
    #                                  "Departure:", tpts$departure,16,
    #                                  "Next Stn:",round(tpts$dist_nm,1),"nm","&",round(tpts$trans_hr,1), "hr(s)",
    #                                  sep=" "),
    #                     highlightOptions = leaflet::highlightOptions(color = "white", weight = 20,bringToFront = TRUE))%>%
    #
    # #Operations Points
    # leaflet::addCircles(lng = opts$lon_dd, lat = opts$lat_dd, group="Operations Locations",
    #                     weight = 5, radius = 10, color="yellow", stroke = TRUE, opacity=.5,
    #                     fillOpacity = 1,
    #                     popup=paste ("ID:",opts$ID,"|",
    #                                  "Station:", opts$station,"|",
    #                                  "Lon:", round(opts$lon_dd,3), "|",
    #                                  "Lat:", round(opts$lat_dd,3), "|",
    #                                  "Depth:",round(opts$depth_m,1),"m","|",
    #                                  "Arrival:", substrLeft(opts$arrival,16),"|",
    #                                  "Departure:", substrLeft(opts$departure,16), "|",
    #                                  "Op Time:", (opts$optime+opts$xoptime),"hr(s)","|",
    #                                  "Operation(s):",opts$operation, "|",
    #                                  "Next Stn:", round(opts$dist_nm,1),"nm","&",round(opts$trans_hr,1),"hr(s)",
    #                                  sep=" "),
    #                     highlightOptions = leaflet::highlightOptions(color = "white", weight = 20,bringToFront = TRUE))%>%
    #
    # # Mooring Operations
    # leaflet::addCircleMarkers(lng = mpts$lon_dd, lat = mpts$lat_dd, group="Mooring Locations",
    #                           weight = 2, radius=20, color="green",stroke = TRUE, opacity=0.5,
    #                           fillOpacity = 0.5, clusterOptions = markerClusterOptions(radius=5),
    #                           popup=paste("ID:",mpts$ID,"|",
    #                                       "Station:", mpts$station,"|",
    #                                       "Lon:", round(mpts$lon_dd,3), "|",
    #                                       "Lat:",round(mpts$lat_dd,3), "|",
    #                                       "Depth:",round(mpts$depth_m,1),"m","|",
    #                                       "Arrival:", substrLeft(mpts$arrival,16),"|",
    #                                       "Departure:",substrLeft(mpts$departure,16), "|",
    #                                       "Op Time:", (mpts$optime+mpts$xoptime),"hr(s)","|",
    #                                       "Operation(s):",mpts$operation, "|",
    #                                       "Next Stn:", round(mpts$dist_nm,1),"nm","&",round(mpts$trans_hr,1),"hr(s)",
    #                                       sep=" ")) %>%
    # #Add Mouse Coordinates
    # leafem::addMouseCoordinates(epsg = NULL,proj4string = NULL, native.crs = FALSE)%>%

    # #Legend
    leaflet::addLegend("bottomright",
                       pal = markerCol,
                       values=data[[markerColumn]],
                       #title=paste("Map created on ",Sys.Date(),": ",file),
                       opacity=1) %>%

    # #Scale Bar
    # leaflet::addScaleBar("bottomleft",options=scaleBarOptions(maxWidth=100,imperial=T,metric=T,updateWhenIdle=T))

    #Layer Control
    leaflet::addLayersControl(
      overlayGroups = c("Operations Locations","Transit Locations","Mooring Locations","Route","Station Labels", "Closure Areas", "NAFO Zones", "EEZ"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
  route
}
