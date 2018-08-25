#This script is to subset the POIs to Barking and Dagenham

remove(list = ls())


library(readr)
library(reshape)
library(devtools)
library(sf)
library(sp)
library(maptools)
library(RColorBrewer)
library(tmaptools)
library(tmap)
library(rgeos)
library(OpenStreetMap)
library(geojsonio)
library(classInt)
library(raster)
library(rgdal)
library(dplyr)
library(data.table)
library(igraph)
require(broom)
library(shp2graph)
library(tidyr)


setwd("~/Desktop/dissertation_ss")


#Points of Interest
poi <-read.table("data_be/Download_pointsofinterest_1003716/poi_2460824/poi.csv", sep="|", header=TRUE)

#Clean POIS

poi_london <- poi[grep("Greater London", poi$Geographic.County),]

#----Separate Point Classification 

poi_london_clean1 <- poi_london %>% separate(PointX.Classification.Code, into = c('group', 'rubbish'), sep = -7)

poi_london_clean2 <- poi_london_clean1 %>% separate(rubbish, into = c('category', 'class'), sep = -5)

num.cols <- c('group','category','class')
poi_london_clean2[num.cols] <- sapply(poi_london_clean2[num.cols], as.numeric)

`%notin%` <- function(x,y) !(x %in% y) 

poi_london_clean_category <- subset(poi_london_clean2, subset = category %notin% c(41, 42,49,53,54,55,56,34,59))

poi_london_clean_class_category <- subset(poi_london_clean_category, subset = class %notin% c(0811,0141,0142,0803,0806,0808,0807,0320,0361,0411,0424))

#Transform POIs into Spatial Object

UKBNG <- "+init=epsg:27700"

coordinates(poi_london_clean_class_category) <- ~ Feature.Easting + Feature.Northing
poi_london_clean_class_category@proj4string = CRS(UKBNG)


#Load London Boundaries

London <- readOGR("/data_be/statistical-gis-boundaries-london 3/ESRI/LSOA_2011_London_gen_MHW.shp")

barking <- London[grep("Barking", London$LSOA11NM),]


barking_bng <- spTransform(barking,CRS(UKBNG))


# Add a buffer around Barking
barking_buffer_bng <- gBuffer(barking_bng, width=1000)


#Overlay spatial boundries with POIs
poi_barking <- raster::intersect(poi_london_clean_class_category, barking_buffer_bng)

plot(poi_barking)
#download as csv

poi_barking_df <- as.data.frame(poi_barking)

write.csv(poi_barking_df, "/data_be/poi_barking_new.csv")
