library(rgdal)
cpa <- readOGR("X:/priority_coral_areas_August_2016.shp", layer="priority_coral_areas_August_2016")
#writeOGR(cpa, "X:/Data_John/shiny/coralhapcv050/coralhapcv050.shp", layer="coralhapcv050", driver="ESRI Shapefile")
cpaAlbers <- readOGR("X:/priority_coral_areas_August_2016_Albers.shp", layer="priority_coral_areas_August_2016_Albers")
#writeOGR(cpa, "X:/Data_John/shiny/coralhapcv050/coralhapcv050.shp", layer="coralhapcv050", driver="ESRI Shapefile")
cpadf <- cpa@data

cpadf <- cpa@data
colnames(cpadf) <- c("ID", "Name", "SR", "Depth", "Area")
cpadf$Area <- cpaAlbers@data$area_sqnm
cpa@data <- cpadf
#save(cpa, file="X:/Data_John/shiny/coralhapcv050/cpa.RData")


#save(depth50m, file="X:/Data_John/shiny/coralhapcv050/depth50m.RData")