#This script is the fun part - making maps!

remove(list = ls())


library(GGally)
library(ggplot2)#library("ggplot2", lib.loc="~/R/win-library/3.4")
library(reshape)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggfortify)
library(factoextra)
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(tmaptools)
library(tmap)
library(rgeos)
library(OpenStreetMap)
library(geojsonio)
library(classInt)
library(tidyverse)
library(leaflet)

setwd("~/Desktop/dissertation_ss")

#create a residential areas map

census <- read.csv("/data_si/lsoa-data.csv", header=TRUE,sep=",")

working <- read.csv("/data_si/working.csv", header=TRUE,sep=",")

#---dividing residential over employment
workpop <- merge(census, working, by="Lower.Super.Output.Area")

workpop$mix <-  (workpop$work / workpop$Mid.year.Population.Estimates.All.Ages.2011)*100



#merging with shapefile

lsoas<- readOGR("/data_be/statistical-gis-boundaries-london 3/ESRI/LSOA_2011_London_gen_MHW.shp")

UKBNG <- "+init=epsg:27700"

lsoas_bng <- spTransform(lsoas, CRS(UKBNG))

lsoas_b <- lsoas_bng[grep("^Barking",lsoas_bng@data$LSOA11NM),]

#join shapefile and data

dens_barking <- merge(x=lsoas_b, y=workpop, by.x='LSOA11CD', by.y="Lower.Super.Output.Area")

#add pois that are significant fromt the service area script

poi2 <- read.csv("/data_be/poi_barking_new.csv")

poi_sig2 <- poi2[poi2$category %in% c(3, 14, 15, 24, 37, 31),]

#try adding category names

category <- read.csv("/data_be/POI CATEGORIES.csv")

#---merge

poi_sig_df <- merge(poi_sig2, category, by="category")

poi_sig <- poi_sig_df
coordinates(poi_sig) <- ~ Feature.Easting + Feature.Northing
poi_sig@proj4string = CRS(UKBNG)

poi_barking_sig <- raster::intersect(poi_sig, lsoas_b)

#seperate into sig pois

poi_sig_3 <- poi_barking_sig[grep("Construction Services", poi_barking_sig$category.name),]
poi_sig_14 <- poi_barking_sig[grep("Research And Design",  poi_barking_sig$category.name),]
poi_sig_15 <- poi_barking_sig[grep("Transport, Storage And Delivery",  poi_barking_sig$category.name),]
poi_sig_24 <- poi_barking_sig[grep("Sports Complex",  poi_barking_sig$category.name),]
poi_sig_37 <- poi_barking_sig[grep("Consumer Products",  poi_barking_sig$category.name),]
poi_sig_31 <- poi_barking_sig[grep("Primary, Secondary And Tertiary Education",  poi_barking_sig$category.name),]





#see relationship between mixed use and si
si_b <- read.csv("data_be/newdata_no14_final.csv")


#join shapefile to si_b

si_poi_barking <- merge(x=dens_barking, y=si_b, by.x='LSOA11CD', by.y="Lower.Super.Output.Area")

#do each one individually

#---3
tmap_mode("view") +
  tm_shape(si_poi_barking) +
  tm_layout(title = "Construction Services") +
  tm_fill("social_isolation_pc1", palette="PuOr", title= "Social Isolation Index", midpoint = NA) +
  tm_borders(col = "azure4") +
  tm_shape(poi_sig_3) +
  tm_dots(size=0.05, col = "seagreen3", border.col=NA)+
  tm_facets(by="category.name", ncol = 2) +
  tmap_options(limits = c(facets.view = 6))


tmap_mode("view") +
  tm_shape(dens_barking) +
  tm_layout(title = "Construction Services") +
  tm_fill("mix", palette="YlGnBu", title= "% of Work to Residential Areas") +
  tm_borders(col = "gray") +
  tm_shape(poi_sig_3) +
  tm_dots(size=0.05, col = "purple", border.col=NA)


#---14


tmap_mode("view") +
  tm_shape(si_poi_barking) +
  tm_layout(title = "Research and Design") +
  tm_fill("social_isolation_pc1", palette="PuOr", title= "Social Isolation Index", midpoint = NA) +
  tm_borders(col = "azure4") +
  tm_shape(poi_sig_14) +
  tm_dots(size=0.05, col = "seagreen3", border.col=NA)+
  tm_facets(by="category.name", ncol = 2) +
  tmap_options(limits = c(facets.view = 6))


tmap_mode("view") +
  tm_shape(dens_barking) +
  tm_layout(title = "Research and Design") +
  tm_fill("mix", palette="YlGnBu", title= "% of Work to Residential Areas") +
  tm_borders(col = "gray") +
  tm_shape(poi_sig_14) +
  tm_dots(size=0.05, col = "purple", border.col=NA)

#----15


tmap_mode("view") +
  tm_shape(si_poi_barking) +
  tm_layout(title = "Transport, Storage and Delivery") +
  tm_fill("social_isolation_pc1", palette="PuOr", title= "Social Isolation Index", midpoint = NA) +
  tm_borders(col = "azure4") +
  tm_shape(poi_sig_15) +
  tm_dots(size=0.05, col = "seagreen3", border.col=NA)+
  tm_facets(by="category.name", ncol = 2) +
  tmap_options(limits = c(facets.view = 6))


tmap_mode("view") +
  tm_shape(dens_barking) +
  tm_layout(title = "Transport, Storage and Delivery") +
  tm_fill("mix", palette="YlGnBu", title= "% of Work to Residential Areas") +
  tm_borders(col = "gray") +
  tm_shape(poi_sig_15) +
  tm_dots(size=0.05, col = "purple", border.col=NA)


#----24


tmap_mode("view") +
  tm_shape(si_poi_barking) +
  tm_layout(title = "Sports Complex") +
  tm_fill("social_isolation_pc1", palette="PuOr", title= "Social Isolation Index", midpoint = NA) +
  tm_borders(col = "azure4") +
  tm_shape(poi_sig_24) +
  tm_dots(size=0.05, col = "seagreen3", border.col=NA)+
  tm_facets(by="category.name", ncol = 2) +
  tmap_options(limits = c(facets.view = 6))


tmap_mode("view") +
  tm_shape(dens_barking) +
  tm_layout(title = "Sports Complex") +
  tm_fill("mix", palette="YlGnBu", title= "% of Work to Residential Areas") +
  tm_borders(col = "gray") +
  tm_shape(poi_sig_24) +
  tm_dots(size=0.05, col = "purple", border.col=NA)

#----31

tmap_mode("view") +
  tm_shape(si_poi_barking) +
  tm_layout(title = "Primary, Secondary And Tertiary Education") +
  tm_fill("social_isolation_pc1", palette="PuOr", title= "Social Isolation Index", midpoint = NA) +
  tm_borders(col = "azure4") +
  tm_shape(poi_sig_31) +
  tm_dots(size=0.05, col = "seagreen3", border.col=NA)



tmap_mode("view") +
  tm_shape(dens_barking) +
  tm_layout(title = "Primary, Secondary And Tertiary Education") +
  tm_fill("mix", palette="YlGnBu", title= "% of Work to Residential Areas") +
  tm_borders(col = "gray") +
  tm_shape(poi_sig_31) +
  tm_dots(size=0.05, col = "purple", border.col=NA)

#----37

tmap_mode("view") +
  tm_shape(si_poi_barking) +
  tm_layout(title = "Consumer Products") +
  tm_fill("social_isolation_pc1", palette="PuOr", title= "Social Isolation Index", midpoint = NA) +
  tm_borders(col = "azure4") +
  tm_shape(poi_sig_37) +
  tm_dots(size=0.05, col = "seagreen3", border.col=NA)



tmap_mode("view") +
  tm_shape(dens_barking) +
  tm_layout(title = "Consumer Products") +
  tm_fill("mix", palette="YlGnBu", title= "% of Work to Residential Areas") +
  tm_borders(col = "gray") +
  tm_shape(poi_sig_37) +
  tm_dots(size=0.05, col = "purple", border.col=NA)


#NOW ALL OF THEM TA-DA

tmap_mode("view") +
  tm_shape(si_poi_barking) +
  tm_fill("social_isolation_pc1", palette="PuOr", title= "Social Isolation Index", midpoint = NA) +
  tm_borders(col = "azure4") +
  tm_shape(poi_barking_sig) +
  tm_dots(size=0.02, col = "dodgerblue", border.col=NA)


tmap_mode("plot")

tmap_mode("view") +
  tm_shape(dens_barking) +
  tm_fill("mix", palette="YlGnBu", title= "% of Work to Residential Areas") +
  tm_borders(col = "gray") +
  tm_shape(poi_barking_sig) +
  tm_dots(size=0.03, col = "coral2", border.col=NA)





qtm(si_poi_barking, fill = c("Tenure.Social.rented.....2011"                                                                  
                             ,"Tenure.Private.rented.....2011"                                                                 
                            ,"Dwelling.type.Household.spaces.with.no.usual.residents.....2011"                                
                              ,"Lone.Parents.Lone.parent.not.in.employment...2011"                                              
                             ,"Economic.Activity.Unemployment.Rate.2011"                                                       
                             ,"Qualifications...No.qualifications.2011"                                                        
                             ,"Qualifications...Highest.level.of.qualification..Level.1.qualifications.2011"                   
                             ,"Qualifications...Highest.level.of.qualification..Level.2.qualifications.2011"                   
                             ,"Qualifications...Highest.level.of.qualification..Apprenticeship.2011"                           
                             )
  , fill.palette="YlGnBu")

tmap_mode("plot")
qtm(si_poi_barking, fill = c("Qualifications...Highest.level.of.qualification..Other.qualifications.2011"                     
                             , "Health.Day.to.day.activities.limited.a.lot.....2011"                                            
                             ,"Health.Day.to.day.activities.limited.a.little.....2011"                                         
                             ,"Health.Fair.health.....2011"                                                                    
                             ,"Health.Bad.or.Very.Bad.health.....2011"                                                         
                             ,"Car.or.van.availability.1.car.or.van.in.household.....2011"
                             ,"Household.Composition...Lone.parent.household.2011"                                             
                             ,"Household.Composition...One.person.household.2011")
                             
, fill.palette="YlGnBu")


tmap_mode("plot")
qtm(si_poi_barking, fill = c("Qualifications...No.qualifications.2011"                      
                             , "Health.Day.to.day.activities.limited.a.lot.....2011")
    
    , fill.palette="Purples", borders = NULL)

tmap_mode("view") +
  tm_shape(si_poi_barking) +
  tm_fill("social_isolation_pc1", palette="PuOr", title= "Social Isolation Index", midpoint = NA) +
  tm_borders(col = "gray") 

