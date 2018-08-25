#This script creates the social isolation index using Principal Component Analysis

remove(list = ls())


library(GGally)
library(ggplot2)#library("ggplot2", lib.loc="~/R/win-library/3.4")
library(reshape)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggfortify)
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
library(PerformanceAnalytics)
library(car)

#Set Working Directory
setwd("~/Desktop/dissertation_ss")

#Upload census data
census <- read.csv("data_si/lsoa-data.csv", header=TRUE,sep=",")
colnames(census)

#Subset for social isolation index input variables
data <- census[,c(1,12,13,20,21,22,24,25,26,27:37,40)]

#Subset for controlled variables
controlled <- census[,c(1, 3:8, 14:19)]
colnames(controlled)
#Save controlled variables for the ana
write_csv(controlled, "data_analysis/controlled.csv")

#Scale data 

data_add <- data[,-c(1)]

stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

o_std = lapply(data_add, stdize, na.rm = T)

o_std_df <- as.data.frame(o_std)

#Add LSOA names back in
rownames(o_std_df)<- data$Lower.Super.Output.Area
o_std_df <- cbind(Lower.Super.Output.Area = rownames(o_std_df), o_std_df)
rownames(o_std_df) <- 1:nrow(o_std_df)


#Create correlations plots

correlation_before_pc1 <- as.data.frame(cor(o_std_df[2:21], method = "pearson", use = "complete.obs"))

write_csv(correlation_before_pc1, "data_si/correlation_before_pc1.csv")

chart.Correlation(o_std_df[2:21], histogram=TRUE, pch=19)

#Take out the highly correlated variables: Adult not employed with dependent children, mean income, no cars available in household
data_final <- o_std_df[,-c(21,19,7)]


#Perform PCA
data1.pca<-prcomp(na.omit(data_final[,c(2:18)]))
#Loadings(Rotation) for each variable.
#From these we can check which original variables influence the most in each PC(principal component)
#Usually, we are interested in PC1 and PC2
print(data1.pca)

summary(data1.pca)

data.1_pca_df <- as.data.frame(data1.pca)
scores <- as.data.frame(data1.pca$rotation)
scores<-cbind(1:17,scores)

lsoaindexall <- as.data.frame(data1.pca$x)

lsoasindex1 <- as.data.frame(lsoaindexall[,c(1)])

#joining data codes together
rownames(lsoasindex1) <- data$Lower.Super.Output.Area
lsoasindex1 <- cbind(Lower.Super.Output.Area = rownames(lsoasindex1), lsoasindex1)
rownames(lsoasindex1) <- 1:nrow(lsoasindex1)

#changing column name

colnames(lsoasindex1)[colnames(lsoasindex1)=="lsoaindexall[, c(1)]"] <- "social_isolation_pc1"

write.csv(scores,"data_si/scores.csv")

write.csv(lsoasindex1,"data_si/si_index1.csv")



#comparing with tombolo si 

tom_si <- read.csv("data_si/tombolo_si.csv")


#Merge two dataframes
tom_pca_si <- merge(tom_si, lsoasindex1, by="Lower.Super.Output.Area" )
colnames(tom_pca_si)[colnames(tom_pca_si)=="Tombolo_SI_Index"] <- "Tombolo"
colnames(tom_pca_si)[colnames(tom_pca_si)=="social_isolation_pc1"] <- "PCA_Index"


#Run linear regression
#--double check for outliers
outlierTest(tom_si_pca)
tom_si_pca <- lm(tom_pca_si$PCA_Index ~ tom_pca_si$Tombolo)
summary(tom_si_pca)

scatter.smooth(tom_pca_si$PCA_Index,tom_pca_si$Tombolo,ylab = 'Tombolo', xlab = 'PCA_Index')


#Plot chloropleth map

#---Merging with shapefile

lsoas<- readOGR("data_be/statistical-gis-boundaries-london 3/ESRI/LSOA_2011_London_gen_MHW.shp")

#---Project it
UKBNG <- "+init=epsg:27700"

lsoas_bng <- spTransform(lsoas, CRS(UKBNG))

lsoas_b <- lsoas_bng[grep("^Barking",lsoas_bng@data$LSOA11NM),]

#---Join shapefile and data

si_barking <- merge(x=lsoas_b, y=lsoasindex1, by.x='LSOA11CD', by.y="Lower.Super.Output.Area")


#---Map it out with tmap
tmap_mode("view")
tm_shape(si_barking) +
  tm_fill("social_isolation_pc1", palette="PRGn", midpoint = NA, title = "Social Isolation Index") +
  tm_borders(col = "gray")
