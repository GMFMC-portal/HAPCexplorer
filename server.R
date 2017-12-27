## server.R
## Modified 9-11-2015
##Version 0.31
## leaflet version 0.3-333

server <- function(input, output, session) {


  
  output$map <- renderLeaflet({  
    leaflet() %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
               options = providerTileOptions(noWrap = TRUE), group="World Imagery") %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/Mapserver/tile/{z}/{y}/{x}',
               options = providerTileOptions(noWrap = TRUE),group="Labels") %>%
#       addTiles('http://services.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}',
#                options = providerTileOptions(noWrap = TRUE),group="Oceans") %>%
      setView(-89.0, 27.75, zoom = 7) %>% 
      #addTiles(nautical, tileOptions(minZoom=6), group="Nautical Chart", attribution = mb_att) %>% 
      
      addEsriImageMapLayer(
        url = 'https://seamlessrnc.nauticalcharts.noaa.gov/arcgis/rest/services/RNC/NOAA_RNC/ImageServer',group="Nautical Chart",
        imageMapLayerOptions(position='back'))%>% 
         
    
  addPolygons(data=HAPCwRegs, fillOpacity=0, stroke=TRUE,
              fillColor="#1b9e77", #teal
              color="#1b9e77", #teal
              weight=4,
              popup= ~ paste(Name, round(st_area_sh,1), "sq. miles"),
              group="HAPCs with fishing restrictions") %>%

 # addPolygons(data=pHAPC, fillOpacity=0.75, stroke=FALSE, 
 #            fillColor='#7570b3', #purple
 #            popup= ~ paste(Feature, round(AREA_GEO,1), "sq. miles"),
 #            group="Recommended HAPCs") %>% 
     ## These polygons not included in area calculations
     
     addPolygons(data=HAPCnoregs, fillOpacity=0, stroke=TRUE, 
                 fillColor='#d95f02',
                 color='#d95f02',
                 weight=4,
                 popup= ~ paste(NAME, "No HAPC specific fishing regulations"),
                 group="HAPCs without fishing restrictions")  %>% 
      addPolygons(data=cpa, fillOpacity=0, stroke=TRUE,
                  fillColor='#7570b3', #purple
                  color='#7570b3',
                  weight=3,
                  popup= ~ paste(Name, "; Depth range:", Depth, " (m)"  ),
                  #popup = popupTable(cpa, zcol=c("Name", "Depth", "Area")),
                  group="Recommended (Updated Sept. 2016)")  %>%

      addPolygons(data=cpaVK906, fillOpacity=0, stroke=TRUE,
                  #color = "#888888",
                  color='#7570b3',
                  weight=4,
                  fillColor='#7570b3', #purple
                  #popup= ~ paste(Feature, "; Depth range:", depth__m_, " (m)"  ),
                  popup =  popupIframe("https://www.youtube.com/embed/VpKAOHQV2JE?autoplay=1", width = 300, height = 225),
                  group="Recommended (Updated Sept. 2016)")  %>%

      addPolygons(data=cpaVK826, fillOpacity=0, stroke=TRUE,
                  #color = "#888888",
                  color='#7570b3',
                  weight=4,
                  fillColor='#7570b3', #purple
                  #popup= ~ paste(Feature, "; Depth range:", depth__m_, " (m)"  ),
                  #popup =  mapview:::popupIframe("https://youtu.be/CoNcX8HT6r8", width = 300, height = 225),
                  popup =  popupIframe("https://www.youtube.com/embed/CoNcX8HT6r8?autoplay=1", width = 300, height = 225),
                  group="Recommended (Updated Sept. 2016)")  %>%

   ##Add polyline:
   addPolylines(data=depth50m, color = "#888888", weight = 4, group="50 m contour") %>%
 #   
  addMeasure(position=c("bottomleft"), primaryLengthUnit="miles",primaryAreaUnit="sqmiles") %>%
  addScaleBar(position=c("bottomright")) %>%
  #addMouseCoordinates(style='basic')  %>% 
  addMouseCoordinates2(style='basic')  %>% 
 #  
 #  
 #  
  ##New for version 0.20 Coral Mapper
  addCircleMarkers(data=BlackCoral,
                   # addCircleMarkers(data=BlackCoral[ which(BlackCoral$depth>-200), ],
                   #addCircleMarkers(data=BlackCoral[which(BlackCoral$depth>=input$integer[1] & BlackCoral$depth<=input$integer[2])]),
                   #addCircleMarkers(data=(BlackCoral[which(BlackCoral$depth>=input$integer[1]),]),
                   radius = ~3,
                   #color="FFFF00",
                   fillColor = "#a6d854",
                   #stroke = FALSE, fillOpacity = 0.8,
                   stroke = TRUE, fillOpacity = 0.8, color="gray", weight=1,
                   group="Coral locations",
                   popup = ~paste(" <b>Depth (m): </b>",  Depth, "<br/>",
                                  "<b>Common name: </b>", Name, sep = " ")) %>%
  ##New for version 0.20 Coral Mapper
  addCircleMarkers(data=Octocoral,
                   radius = ~3,
                   #color="FFFF00",
                   fillColor = "#66c2a5",
                   #stroke = FALSE, fillOpacity = 0.8,
                   stroke = TRUE, fillOpacity = 0.8, color="gray", weight=1,
                   #group="<em>Stony Coral</em>",
                   group= "Coral locations",

                   popup = ~paste(" <b>Depth (m): </b>",  Depth, "<br/>",
                                  "<b>Common name: </b>", Name, sep = " ")) %>%
 #  
 #  
 #  ##New for version 0.20 Coral Mapper
  addCircleMarkers(data=Seapen,
                   radius = ~3,
                   #color="FFFF00",
                   fillColor = "#fc8d62",
                   #stroke = FALSE, fillOpacity = 0.8,
                   stroke = TRUE, fillOpacity = 0.8, color="gray", weight=1,
                   group="Coral locations",
                   popup = ~paste(" <b>Depth (m): </b>",  Depth, "<br/>",
                                  "<b>Common name: </b>", Name, sep = " ")) %>%
  ##New for version 0.20 Coral Mapper
  addCircleMarkers(data=SoftCoral,
                   radius = ~3,
                   #color="FFFF00",
                   fillColor = "#8da0cb",
                   #stroke = FALSE, fillOpacity = 0.8,
                   stroke = TRUE, fillOpacity = 0.8, color="gray", weight=1,
                   group="Coral locations",
                   popup = ~paste(" <b>Depth (m): </b>",  Depth, "<br/>",
                                  "<b>Common name: </b>", Name, sep = " ")) %>%
  ##New for version 0.20 Coral Mapper
  addCircleMarkers(data=Sponge,
                   radius = ~3,
                   #color="FFFF00",
                   fillColor = "#ffd92f",
                   #stroke = FALSE, fillOpacity = 0.8,
                   stroke = TRUE, fillOpacity = 0.8, color="gray", weight=1,
                   group="Coral locations",
                   popup = ~paste(" <b>Depth (m): </b>",  Depth, "<br/>",
                                  "<b>Common name: </b>", Name, sep = " ")) %>%




  ##New for version 0.20 Coral Mapper
  addCircleMarkers(data=StonyCoral,
                   radius = ~3,
                   #color="FFFF00",
                   fillColor = "#e78ac3",
                   #stroke = FALSE, fillOpacity = 0.8,
                   stroke = TRUE, fillOpacity = 0.8, color="gray", weight=1,
                   #group="<em>Stony Coral</em>",
                   group= "Coral locations",

                   popup = ~paste(" <b>Depth (m): </b>",  Depth, "<br/>",
                                  "<b>Common name: </b>", Name, sep = " ")) %>%
    
 #  
  # addControl(  html='
  #          <!--Example adding point, line and polygon to create a custom legend-->
  #           <!--This needs to be HTML-->
  # 
  # 
  #              <table><tr><td class="shape"><svg width="24" height="18">
  #              <circle cx="5" cy="5" r="4" stroke="#888888" stroke-width="1.5" fill="#a6d854" /></svg></td><td class="value">Black Coral</td></tr></table>
  #              <table><tr><td class="shape"><svg width="24" height="18">
  #              <circle cx="5" cy="5" r="4" stroke="#888888" stroke-width="1.5" fill="#66c2a5" /></svg></td><td class="value">Octocoral</td></tr></table>
  # 
  #              <table><tr><td class="shape"><svg width="24" height="18">
  #              <circle cx="5" cy="5" r="4" stroke="#888888" stroke-width="1.5" fill="#fc8d62" /></svg></td><td class="value">Sea Pen</td></tr></table>
  # 
  #              <table><tr><td class="shape"><svg width="24" height="18">
  #              <circle cx="5" cy="5" r="4" stroke="#888888" stroke-width="1.5" fill="#8da0cb" /></svg></td><td class="value">Soft Coral</td></tr></table>
  # 
  #              <table><tr><td class="shape"><svg width="24" height="18">
  #              <circle cx="5" cy="5" r="4" stroke="#888888" stroke-width="1.5" fill="#ffd92f" /></svg></td><td class="value">Sponge</td></tr></table>
  # 
  #              <table><tr><td class="shape"><svg width="24" height="18">
  #              <circle cx="5" cy="5" r="4" stroke="#888888" stroke-width="1.5" fill="#e78ac3" /></svg></td><td class="value">Stony Coral</td></tr></table>
  # 
  #              <table><tr><td class="shape"><svg style="width:24px; height:18px;" xmlns="http://www.w3.org/2000/svg" version="1.1"><line class="ln" style="stroke: #888888; stroke-opacity: 0.8; stroke-width: 3;" x1="2.5" y1="9" x2="21.5" y2="9" /></svg></td><td class="value">50 m contour</td></tr></table>'
  # 
  # 
  # 
  #               ## End HTML
  #           , position= c("bottomleft"), className = "info legend", data = getMapData(map)
  # 
  # 
  #           ) %>%
  
## Add a legend.
  addLegend("bottomright", colors = c('#1b9e77',  '#d95f02', '#7570b3'),
            labels = c("HAPCs with fishing restrictions",  "HAPCs without fishing restrictions","Recommended (Updated Sept. 2016)"),
            title = "Legend (HAPCs)",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1
  )  %>%
###end legend
# Layers control
addLayersControl(
  #baseGroups = c("World Imagery", "Labels", "Oceans"),
  overlayGroups = c( "Nautical Chart","HAPCs with fishing restrictions", 
                    "HAPCs without fishing restrictions", "Coral locations", "Recommended (Updated Sept. 2016)"),
  position=c("bottomleft"),
  options = layersControlOptions(collapsed = FALSE)
                ) %>%
#   

  hideGroup("Coral locations")%>%
  hideGroup("HAPCs without fishing restrictions") %>%
  hideGroup("Nautical Chart")

    })
  

output$out <- renderPrint({
  validate(need(input$map_click, FALSE))
data.frame(input$map_click[1:2])
})

# ##test mouse coords
  ###reactive to get boundary box
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  HAPCInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(HAPCwregsdfcoords[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    HAPCinView <- subset(HAPCwregsdfcoords,
           y >= latRng[1] & y <= latRng[2] &
             x >= lngRng[1] & x <= lngRng[2])
    
  })

# HAPCInBoundspHAPC <- reactive({
#      if (is.null(input$map_bounds))
#           return(pHAPCdfcoords[FALSE,])
#      bounds <- input$map_bounds
#      latRng <- range(bounds$north, bounds$south)
#      lngRng <- range(bounds$east, bounds$west)
#      
#      HAPCinViewpHAPC <- subset(pHAPCdfcoords,
#                           y >= latRng[1] & y <= latRng[2] &
#                                x >= lngRng[1] & x <= lngRng[2])
#      
# })
  
HAPCInBoundspHAPC <- reactive({
  if (is.null(input$map_bounds))
    return(cpadfcoords[FALSE,])
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  
  HAPCinViewpHAPC <- subset(cpadfcoords,
                            y >= latRng[1] & y <= latRng[2] &
                              x >= lngRng[1] & x <= lngRng[2])
  
})
  
########### add layer on zoom
################################################################################  
## Experimental
# Remomves data at  zoom levels > 11
# observeEvent(input$map_zoom,{
#   proxy  <-  leafletProxy("map")
#   if(input$map_zoom < 11){
#     #proxy %>% clearImages()
#     proxy %>%addRasterImage(bathy, opacity = 0.8)
#   } else
#     if(input$map_zoom >= 11){
#       proxy %>% removeImages()
#         
# 
#     }
# })
################################################################################


###########

  
  
  
  ###reactive to get boundary box
  
  
  output$HAPCarea<- renderPlot({
    #If HAPCS are in view, don't plot
    #if (nrow(HAPCInBounds ()) == 0){
if (nrow(HAPCInBounds ()) == 0 & nrow(HAPCInBoundspHAPC ()) == 0 ){
      return(NULL)
    }
    
   #hist(  coralInBounds()$longitude,
#view <- sum(HAPCInBounds()$st_area_sh) + (sum(HAPCInBoundspHAPC()$AREA_GEO)*0.386102) #convert km to sq mi
    view <- sum(HAPCInBounds()$st_area_sh) + (sum(HAPCInBoundspHAPC()$Area)) #convert km to sq mi
    #view <- sum(HAPCInBounds()$st_area_sh) + (sum(HAPCInBoundspHAPC()$AREA_GEO)) #convert km to sq mi

current <- sum(HAPCInBounds()$st_area_sh)
proposed <- sum(HAPCInBoundspHAPC()$Area)

x <- data.frame(Current=c(current,0), Proposed=c(0,proposed), Total=c(current,proposed))



#   #barplot(c(sum(HAPCInBounds()$st_area_sh) + sum(HAPCInBoundspHAPC()$AREA_GEO), Area),
par(bg="#C8D7E6")
barplot(as.matrix(x),
#           #barplot(c(view, Area),     
#           barplot(c(current, proposed, view), 
         main = "Current and Recommended HAPCs\n (Bars = Area in view only)",
        cex.names = 0.85,  
         col=c('#1b9e77', '#7570b3'),
         # col = c('#9ecae1', '#2b8cbe', "red"),
          border = 'white',
         names.arg=c("Current", "Recommended", "Current +\n Recommended"),
         ylab="Area (sq. miles)", 
         sub=paste(round(view,0), " sq. miles in view", "\n", "(",
                 round(((view/2210)*100),0), "% of total area in view)" , sep=""),
         ylim=c(0,(2210+500)) 
         )
lines( x=c(0.1,1.9,3.5), y=c(2210,2210,2210), lwd=3,lty=2)
  

  })

output$tbl = DT::renderDataTable(
     HAPCwregsdf[,c(4,2)],  colnames=c("Name", "Area (sq.miles)"),options = list(lengthChange = FALSE, pageLength = 20))

output$tbl2 = DT::renderDataTable(
     HAPCnoregsdf[,c(2,6)], colnames=c("Name", "Area (sq.miles)"), options = list(lengthChange = FALSE))

output$tbl3 = DT::renderDataTable(
    cpadfcoords[,c(2,4,5)], colnames=c("Name",  "Depth range (m)", "Area (sq. miles)"),                                           #"Centroid.y"),
    options = list(lengthChange = FALSE,pageLength = 50),
    #colnames=c("Site","Area","Depth range"),
    filter=c("top"), selection=c("multiple"))



output$tbl4 = DT::renderDataTable(
  coralpoints2@data[,1:4], colnames = c("Species","Depth (m)", "Name", "Source"),
  options = list(lengthChange = TRUE,pageLength = 50),
  #coralpoints2@data[,c(1, as.numeric(as.character(2)),3,4)], options = list(lengthChange = TRUE,pageLength = 50),
  
  filter=c("top"), selection=c("multiple"))

}