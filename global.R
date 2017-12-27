##global R

## version 1.1 ## Updated 6Dec17. 
## version 1.0 ## Added modal page with JS to replace welcome page 
##version 0.53  ## changes package build and loading to work with mapview
## added tile version of nautical chart the changes basemaps on zoom
##version 0.52 ## Improve CSS thanks to CER!
  ## Coordinate display center
  ## Zoom button moved down
  ## Sidebar Colors Changed
  ## Transparency removed on leaflet tools for consistency
  ## Background color added to floating panel
  ## Images rounded
  ## Spacing improved on legend
  ## Fonts and colors modified
##version 0.50 ## Update 'recommended' data layers
  ## Add mapview dependency
  ## Add MouseCoordinates2() : 
  ## Remove click for coordinates
##version 0.44 ## Not deployed
##version 0.43 ## Replace 'proposed' with 'recommended' throughout
##version 0.42 ## Replace coral data source with cleaned spatial data
##version 0.40 ## Replace coral data source with cleaned spatial data
##version 0.34 ## Replace coral data source with cleaned spatial data
##version 0.33 ## see change log
##version 0.32 ## Added about page
##version 0.31 ## Added cleaned coral points
##version 0.1.1
#setwd("X:/Claire/shiny/coralhapcv100")
#setwd("X:/Claire/shiny/coralhapc-masterv051")
#setwd("X:/Data_John/shiny/coralhapcv052")
#setwd("X:/Data_John/shiny/coralhapcv042")
#setwd("X:/Data_John/shiny/coralhapcv040")
#setwd("C:/Users/Froeschke/John/GMFMC/shiny/coralhapcv034")
#setwd("X:/Data_John/shiny/coralhapcv050")
##use development version of leaflet to leverage new features 11.3.2015
if (!require('devtools')) install.packages('devtools')
if (!require('sp')) install.packages('sp')
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('knitr')) install.packages('knitr')
if (!require('rmarkdown')) install.packages('rmarkdown')
if (!require('assertthat')) install.packages('assertthat')
if (!require('backports')) install.packages('backports')
if (!require('caTools')) install.packages('caTools')
if (!require('dichromat')) install.packages('dichromat')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('gridExtra')) install.packages('gridExtra')
if (!require('gtable')) install.packages('gtable')
if (!require('labeling')) install.packages('labeling')
if (!require('rprojroot')) install.packages('rprojroot')
if (!require('lazyeval')) install.packages('lazyeval')
if (!require('munsell')) install.packages('munsell')
if (!require('plyr')) install.packages('plyr')
if (!require('DBI')) install.packages('DBI')
if (!require('units')) install.packages('units')
if (!require('units')) install.packages('rgeos')
if (!require('units')) install.packages('units')
if (!require('DT')) install.packages('DT')
if (!require('yaml')) install.packages('yaml')
if (!require('rgdal')) install.packages('rgdal')
if (!require('leaflet')) devtools::install_github('rstudio/leaflet')
if (!require('leaflet.extras')) devtools::install_github('bhaskarvk/leaflet.extras')
if (!require('leaflet.esri')) devtools::install_github('bhaskarvk/leaflet.esri')
#if (!require('mapview')) devtools::install_github("environmentalinformatics-marburg/mapview", ref = "develop")



library(devtools)
library(leaflet)
library(shinydashboard)

#library(rgdal)
library(DT)
library(rmarkdown)
#library(mapview)
library(sp)
library(leaflet.extras)
library(leaflet.esri)



load("HAPCs.RData")
load("HAPCwRegsv010.RData")
load("cpa.RData")
load("depth50m.RData")
#head(HAPCwRegs@data)

#load("coralpoints.RData")
#load("coralpoints5.RData")

load("coralpointsv023.RData")

### split layers for layer control
LayerNames <- unique(coralpoints2@data$Name)
StonyCoral <- subset(coralpoints2, coralpoints2$Name=="Stony coral")
BlackCoral <- subset(coralpoints2, coralpoints2$Name=="Black coral")
SoftCoral <- subset(coralpoints2, coralpoints2$Name=="Soft coral")
Seapen <- subset(coralpoints2, coralpoints2$Name=="Sea pen")
Octocoral <- subset(coralpoints2, coralpoints2$Name=="Octocoral")
Sponge <- subset(coralpoints2, coralpoints2$Name=="Sponge")

##clean-up pHAPC
pHAPC$AREA_GEO <- round(pHAPC$AREA_GEO,1) ##better table display v0.33

## get units in nm^2 for HAPCwRegs, Thanks Bryan Schoonard for these numbers

x <- HAPCwRegs@data
x[1,2] <- 38.10  ##FGB East 38.10
x[2,2] <- 47.36  ##FGB West 47.36
x[3,2] <- 18.67  ##McGrail Bank 18.67
x[4,2] <- 449.30  ##Middle Grounds 449.30  Sq Miles
x[5,2] <- 133.27  ##Pulley Ridge 133.27
x[6,2] <- 2.33  ##Stetson Bank 2.33
x[7,2] <- 72.28  ##Tortugas South 72.28
x[8,2] <- 15.98  ##Tortugas North 15.98
x[9,2] <- 516.49  ##Edges
x[10,2] <- 141.27  ##Steamboat Lumps
x[11,2] <- 152.61  ##Madison Swanson
#x[11,4] <- "Madison Swanson"
rownames(x) <- NULL
HAPCwRegs@data <- x
rm(x)


##extract dataframe and get centroids of polygonsh
cpadf <- cpa@data
coords <- coordinates(cpa)
colnames(coords) <- c("x","y")
cpadfcoords <- cbind(cpadf, coords)

FishingRegulations <- data.frame(id=0:22,Name=cpadfcoords$Name,
                                 regs=c("Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "No",
                                    "No",
                                    "No",
                                    "No",
                                    "No",
                                    "Yes",
                                    "No",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes"))
                                    


## extract dataframe and get centroids of polygons
pHAPC@data$AREA_GEO <- pHAPC@data$AREA_GEO * 0.386102 ##new version 0.40
pHAPCdf <- pHAPC@data
coords <- coordinates(pHAPC)
colnames(coords) <- c("x","y")
pHAPCdfcoords <- cbind(pHAPCdf, coords)



HAPCwregsdf <- HAPCwRegs@data
coords2 <- coordinates(HAPCwRegs)
colnames(coords2) <- c("x","y")
HAPCwregsdfcoords <- cbind(HAPCwregsdf, coords2)

HAPCnoregsdf <- HAPCnoregs@data
coords3 <- coordinates(HAPCnoregs)
colnames(coords3) <- c("x","y")
HAPCnoregsdfcoords <- cbind(HAPCnoregsdf, coords3)

HAPCneedrevisiondf <- HAPCneedrevision@data
coords4 <- coordinates(HAPCneedrevision)
colnames(coords4) <- c("x","y")
HAPCneedrevisioncoords <- cbind(HAPCneedrevisiondf, coords4)


##Calculate total area
#Area <- sum(HAPCwregsdfcoords$st_area_sh) + (sum(pHAPCdfcoords$AREA_GEO) * 0.386102) ##removed conversion v0.40
Area <- sum(HAPCwregsdfcoords$st_area_sh) + (sum(cpadf$Area))
#Area2 <- data.frame(Total=c(sum(HAPCwregsdfcoords$st_area_sh), (sum(pHAPCdfcoords$AREA_GEO) * 0.386102)))

##legend for coral locations
# pal <- colorFactor(c("#D0FA58", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
#                    domain = c("Black coral", "Hydrozoan", "Octocoral",
#                               "Sea anemones", "Stony Coral"))

#depth50m <- readOGR("50MeterBathy.shp", layer="50MeterBathy")


##Data Request
#writeOGR(HAPCwRegs, "K:/hanson/HAPCwRegs.shp", layer="HAPCwRegs", driver="ESRI Shapefile")
#writeOGR(pHAPC, "K:/hanson/pHAPC.shp", layer="pHAPC", driver="ESRI Shapefile")
#writeOGR(HAPCnoregs, "K:/hanson/HAPCnoregs.shp", layer="HAPCnoregs", driver="ESRI Shapefile")

####========================== Added version 0.50
# addMouseCoordinates2 <- function (map)
# {
#   if (inherits(map, "mapview"))
#     map <- mapview2leaflet(map)
#   stopifnot(inherits(map, "leaflet"))
#   map <- htmlwidgets::onRender(map, "\nfunction(el, x, data) {\n  // we need a new div element because we have to handle\n  // the mouseover output seperately\n  debugger;\n  function addElement () {\n    // generate new div Element\n    var newDiv = $(document.createElement('div'));\n    // append at end of leaflet htmlwidget container\n    $(el).append(newDiv);\n    //provide ID and style\n    newDiv.addClass('lnlt');\n    newDiv.css({\n      'position': 'relative',\n      'bottomleft':  '0px',\n      'background-color': 'rgba(255, 255, 255, 1)',\n      'box-shadow': '0 0 2px #bbb',\n      'background-clip': 'padding-box',\n      'margin': '0',\n      'color': '#333',\n      'font': '12px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',\n    });\n    return newDiv;\n  }\n\n  // check for already existing lnlt class to not duplicate\n  var lnlt = $(el).find('.lnlt');\n  if(!lnlt.length) {\n    lnlt = addElement();\n    // get the leaflet map\n    var map = HTMLWidgets.find('#' + el.id);\n\n    // grab the special div we generated in the beginning\n    // and put the mousmove output there\n    map.on('mousemove', function (e) {\n      lnlt.text('Latitude: ' + (e.latlng.lat).toFixed(5) +\n      ' | Longitude: ' + (e.latlng.lng).toFixed(5) +\n      ' | Zoom: ' + map.getZoom() + ' '\n      );\n    })\n  };\n}\n")
#   map
# }
#  
cpaVK906 <- subset(cpa, cpa$Name=="Viosca Knoll 862/906")
#plot(cpaVK906)
cpaVK826 <- subset(cpa, cpa$Name=="Viosca Knoll 826")
#plot(cpaVK826, axes=TRUE)
## Experimental
 # bathy <- raster("bathy.tif")

# ###Add nautical chart
# nautical <- "https://api.mapbox.com/styles/v1/auburngrad2015/ciyept29t00202rmzwegvwum7/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYXVidXJuZ3JhZDIwMTUiLCJhIjoiY2l5ZWhvaHplMDB3MjJxbXA1MHF5czR0biJ9.rEHf8qpxUi9Uv0rjMO0fEg"
# 
# ## MapBox Attribution ##
# mb_att <- "<a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> "

####========================== Added version 0.53




addMouseCoordinates2 <- function (map, style = c("detailed", "basic"), epsg = NULL, proj4string = NULL) 
{
  style <- style[1]
  if (inherits(map, "mapview")) 
    map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))
  if (style == "detailed" && is.null(epsg) && is.null(proj4string)) {
    txt_detailed <- paste0("\n      ' x: ' + L.CRS.EPSG3857.project(e.latlng).x.toFixed(0) +\n      ' | y: ' + L.CRS.EPSG3857.project(e.latlng).y.toFixed(0) +\n      ' | epsg: 3857 ' +\n      ' | proj4: +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs ' +\n      ' | lon: ' + (e.latlng.lng).toFixed(5) +\n      ' | lat: ' + (e.latlng.lat).toFixed(5) +\n      ' | zoom: ' + map.getZoom() + ' '")
  }
  else {
    txt_detailed <- paste0("\n      ' x: ' + (e.latlng.lng).toFixed(5) +\n      ' | y: ' + (e.latlng.lat).toFixed(5) +\n      ' | epsg: ", 
                           epsg, " ' +\n      ' | proj4: ", proj4string, " ' +\n      ' | zoom: ' + map.getZoom() + ' '")
  }
  txt_basic <- paste0("\n    ' lon: ' + (e.latlng.lng).toFixed(5) +\n    ' | lat: ' + (e.latlng.lat).toFixed(5) +\n    ' | zoom: ' + map.getZoom() + ' '")
  txt <- switch(style, detailed = txt_detailed, basic = txt_basic)
  map <- htmlwidgets::onRender(map, paste0("\nfunction(el, x, data) {\n\n  // get the leaflet map\n  var map = this; //HTMLWidgets.find('#' + el.id);\n\n  // we need a new div element because we have to handle\n  // the mouseover output separately\n  // debugger;\n  function addElement () {\n    // generate new div Element\n    var newDiv = $(document.createElement('div'));\n    // append at end of leaflet htmlwidget container\n    $(el).append(newDiv);\n    //provide ID and style\n    newDiv.addClass('lnlt');\n    newDiv.css({\n      'position': 'relative',\n      'bottomleft':  '0px',\n      'background-color': 'rgba(255, 255, 255, 0.7)',\n      'box-shadow': '0 0 2px #bbb',\n      'background-clip': 'padding-box',\n      'margin': '0',\n      'padding-left': '5px',\n      'color': '#333',\n      'font': '12px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',\n    });\n    return newDiv;\n  }\n\n  // check for already existing lnlt class to not duplicate\n  var lnlt = $(el).find('.lnlt');\n  if(!lnlt.length) {\n    lnlt = addElement();\n\n    // grab the special div we generated in the beginning\n    // and put the mousmove output there\n    map.on('mousemove', function (e) {\n      lnlt.text(", 
                                           txt, ");\n    })\n  };\n}\n"))
  map
}

### iframe -----
popupIframe <- function(src, width = 300, height = 300) {
  paste0("<iframe src='",
         src,
         "' frameborder=0 width=",
         width,
         " height=",
         height,
         #" align=middle",
         "></iframe>")
}

cpadfcoords$Area <- round(cpadfcoords$Area,digits=2)


###### Add areas (sq.miles) to HAPC no regs table ######

HAPCnoregsdf$st_area_sh <- c(3049,107.4,17.4,10.7,14.6,11.9,14.6,26.5,6.6,46.4)



# > session_info()
# Session info ---------------------------------------------------
#   setting  value                       
# version  R version 3.3.0 (2016-05-03)
# system   x86_64, mingw32             
# ui       RStudio (0.99.875)          
# language (EN)                        
# collate  English_United States.1252  
# tz       America/New_York            
# date     2017-01-26                  
# 
# Packages -------------------------------------------------------
#   package        * version    date      
# assertthat       0.1        2013-12-06
# broom            0.4.1      2016-06-24
# car            * 2.1-3      2016-08-11
# chron            2.3-47     2015-06-24
# codetools        0.2-14     2015-07-15
# colorspace       1.2-6      2015-03-11
# data.table     * 1.9.6      2015-09-19
# DBI              0.4-1      2016-05-08
# devtools       * 1.11.1     2016-04-21
# digest           0.6.10     2016-08-02
# dplyr            0.4.3      2015-09-01
# DT             * 0.1        2015-06-09
# foreach          1.4.3      2015-10-13
# foreign          0.8-66     2015-08-19
# gdalUtils        2.0.1.7    2015-10-10
# highcharter    * 0.4.0      2016-07-15
# htmltools        0.3.5      2016-03-21
# htmlwidgets      0.8        2016-11-09
# httpuv           1.3.3      2015-08-04
# igraph           1.0.1      2015-06-26
# iterators        1.0.8      2015-10-13
# jsonlite         1.0        2016-07-01
# lattice          0.20-33    2015-07-14
# latticeExtra     0.6-28     2016-02-09
# leaflet        * 1.0.1.9004 2016-08-04
# lme4             1.1-12     2016-04-16
# lubridate        1.5.6      2016-04-06
# magrittr         1.5        2014-11-22
# mapview        * 1.1.0      2016-06-09
# markdown       * 0.7.7      2015-04-22
# MASS             7.3-45     2016-04-21
# Matrix           1.2-6      2016-05-02
# MatrixModels     0.4-1      2015-08-22
# memoise          1.0.0      2016-01-29
# mgcv             1.8-12     2016-03-03
# mime             0.5        2016-07-07
# minqa            1.2.4      2014-10-09
# mnormt           1.5-5      2016-10-15
# munsell          0.4.3      2016-02-13
# nlme             3.1-127    2016-04-16
# nloptr           1.0.4      2014-08-04
# nnet             7.3-12     2016-02-02
# pbkrtest         0.4-6      2016-01-27
# plyr             1.8.4      2016-06-08
# png              0.1-7      2013-12-03
# psych            1.6.12     2017-01-08
# purrr            0.2.1      2016-02-13
# quantmod         0.4-5      2015-07-24
# quantreg         5.26       2016-06-07
# R.methodsS3      1.7.1      2016-02-16
# R.oo             1.20.0     2016-02-17
# R.utils          2.3.0      2016-04-14
# R6               2.1.2      2016-01-26
# raster           2.5-8      2016-06-02
# RColorBrewer   * 1.1-2      2014-12-07
# Rcpp             0.12.7     2016-09-05
# reshape2         1.4.1      2014-12-06
# rgdal          * 1.1-10     2016-05-12
# rlist            0.4.6.1    2016-04-04
# rsconnect        0.4.1.11   2016-05-27
# satellite        0.2.0      2015-09-10
# scales           0.4.0      2016-02-26
# shiny          * 0.13.2     2016-03-28
# shinyBS        * 0.61       2015-03-31
# shinydashboard * 0.5.1      2015-09-09
# sp             * 1.2-3      2016-04-14
# SparseM          1.7        2015-08-15
# stringi          1.0-1      2015-10-22
# stringr          1.0.0      2015-04-30
# tibble           1.2        2016-08-26
# tidyr            0.4.1      2016-02-05
# TTR              0.23-1     2016-03-21
# viridisLite      0.1.3      2016-03-12
# webshot          0.3.2      2016-06-23
# withr            1.0.1      2016-02-04
# xtable           1.8-2      2016-02-05
# xts              0.9-7      2014-01-02
# yaml             2.1.13     2014-06-12
# zoo              1.7-13     2016-05-03
# source                            
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.2)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.2)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# Github (rstudio/leaflet@e2751ff)  
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.2)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.2)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.2)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# Github (rstudio/rsconnect@2419667)
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.1)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0)                    
# CRAN (R 3.3.0) 
