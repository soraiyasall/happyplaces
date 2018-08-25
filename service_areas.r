#This script is used for the following:
#1) Merge the service areas (walking catchment areas) with POIs
#2) Aggregate POIs to a LSOA level per category
#3) Run multi linear regression
#4) Run random forest

remove(list = ls())

#colors

purple <- "#555588"
yellow <- "#f4d06f"
turq <- "#58c4be"
darkp <- "#55566e"

library(broom)
require(maptools)
library(igraph)
library(rgdal)
library(rgeos)
library(ggplot2)
library(downloader)
library(geosphere)
library(rgdal)
library(rgeos)
library(sp)
library(tidyr)
library(dplyr)
library(plyr)
library(tmap)
library(data.table)
library(randomForest)
library(reshape2)

setwd("~/Desktop/dissertation_ss")

UKBNG <-"+init=epsg:27700"


#----Load Service Areas

service_area_2250 <- readOGR("/data_be/new_service_2250/new_service_2250.shp")

service_area_2250_df <- as.data.frame(service_area_2250)

service_area_2250_bng <- spTransform(service_area_2250, CRS(UKBNG))


#---Load POIs

poi <- read.csv("/data_be/poi_barking_new.csv")

#Project it
coordinates(poi) <- ~ Feature.Easting + Feature.Northing
poi@proj4string = CRS(UKBNG)

poi_df <- as.data.frame(poi)

#Merge it to service areas
poi_service <- raster::intersect(poi, service_area_2250_bng)

poi_service_df <- as.data.frame(poi_service)

#----Add facility ID with LSOA Reference

facilities <- readOGR("/data_be/new_service_2250/new_facilities.shp")
facilities_bng <- spTransform(facilities, CRS(UKBNG))

London <- readOGR("/data_be/statistical-gis-boundaries-london 3/ESRI/LSOA_2011_London_gen_MHW.shp")

barking <- subset(London, LAD11NM %in% c("Barking and Dagenham"))

havering <- subset(London, LAD11NM %in% c("Havering"))

newham <- subset(London, LAD11NM %in% c("Newham"))

RedBridge <- subset(London, LAD11NM %in% c("Redbridge"))

Greenwich <- subset(London, LAD11NM %in% c("Greenwich"))

Bexley <- subset(London, LAD11NM %in% c("Bexley"))

all_boroughs <- rbind(Bexley, Greenwich, RedBridge, newham, havering, barking)

all_boroughs_bng <- spTransform(all_boroughs, CRS(UKBNG))

facilities_lsoa <- raster::intersect(facilities_bng, all_boroughs_bng)


#change column name
facilities_lsoa_df <- as.data.frame(facilities_lsoa)
colnames(facilities_lsoa_df)[which(names(facilities_lsoa_df) == "ObjectID")] <- "FacilityID"

#merged poi_service with facilities
poi_service_2250_lsoa <- merge(poi_service_df, facilities_lsoa_df, by="FacilityID")



#----Group by Category 1-60


category <-  as.data.frame(table(poi_service_2250_lsoa$FacilityID, poi_service_2250_lsoa$category))

wide_DF <- category %>% spread(Var2, Freq)

hist(wide_DF$`11`)

#-----find mean for LSOA

category_unique_lsoa <- merge(wide_DF, facilities_lsoa_df, by.x="Var1", by.y="FacilityID")
summary(category_unique_lsoa)

#replace all 0s with NA

category_unique_lsoa[category_unique_lsoa == 0] <- NA

#try with mean
category_unique_lsoa_mean <- aggregate(category_unique_lsoa[, 2:61], list(category_unique_lsoa$LSOA11CD), mean)
summary(category_unique_lsoa_mean)




#-------count all pois
count_2250 <- aggregate(category~ObjectID,FUN=length,data=poi_service_2250_lsoa)

colnames(count_2250)[which(names(count_2250) == "category")] <- "count.category"

#Match with LSOA and do mean

category_count_lsoa <- merge(count_2250, facilities_lsoa_df, by.x="ObjectID", by.y="FacilityID")
category_count_lsoa_mean <- aggregate(category_count_lsoa [, 2], list(category_count_lsoa$LSOA11CD), mean)
colnames(category_count_lsoa_mean)[which(names(category_count_lsoa_mean) == "x")] <- "count.mean.category"




# Merge both categories

count_unique_2250 <- merge(category_count_lsoa_mean, category_unique_lsoa_mean, by="Group.1")



hist(count_unique_2250$count.mean.category)

######


controlled <- read.csv("/data_analysis/controlled.csv")

controlled_2250 <- merge(count_unique_2250, controlled, by.x="Group.1", by.y="Lower.Super.Output.Area")



#social isolation index

social_isolation <- read.csv("/Users/soraiyasalemohamed/Desktop/dissertation_ss/data_si/si_index1.csv")

si_2250 <- merge(social_isolation, controlled_2250, by.x="Lower.Super.Output.Area", by.y="Group.1")


#get rid of NA value columns

si_2250 <- si_2250[colSums(!is.na(si_2250)) > 0]


#also count NA values

apply(is.na(si_2250 ), 2, sum)

#taking out NA values that are above 10 except for transport - 57

si_2250 <- si_2250 [, -which(colMeans(is.na(si_2250 )) > 0.136)]

#further clean with

si_2250 <- si_2250[,-c(2, 8, 22,28,33)]



#subset for analysis

newdata <- si_2250[,c(2:28, 30:41)]




#descriptive statistics for sums
newdata_des <- newdata

newdata_sum <-as.data.frame(colSums(Filter(is.numeric, newdata_des), na.rm=TRUE))
library(data.table)
setDT(newdata_sum, keep.rownames =TRUE)[]
colnames(newdata_sum)[1]<- "category"

summary(newdata_des)
#Ethnicity is a % so don't use sum, use Median
newdata_eth <- newdata_des[,c(33:38)]
newdata_med_eth<-as.data.frame(colwise(median)(newdata_eth), na.rm=TRUE)
#transpose it 
newdata_med_eth_df <- as.data.frame(dcast(melt(as.matrix(newdata_med_eth)), Var2~paste0('r', Var1), value.var='value'))

barplot(newdata_med_eth_df$r1, names.arg= c("White", "Mixed Multiple", "Asian", "Black/African/Caribbean", "Other", "BAME"), main="Ethnic Makeup", xlab="Ethnicities", ylab="Median Percentage", cex.names = 0.6, col=turq, ylim = c(0, 75))

newdata_sum_poi <- newdata_sum[3:27,]

barplot(newdata_sum_poi$`colSums(Filter(is.numeric, newdata_des), na.rm = TRUE)`, names.arg= newdata_sum_poi$category, main="POI Distribution", xlab="POI Categories", ylab="Count", cex.names = 0.5, col=turq, ylim = c(0, 10000))

newdata_sum_age <- newdata_sum[28:32,]
barplot(newdata_sum_age$`colSums(Filter(is.numeric, newdata_des), na.rm = TRUE)`,  names.arg= c("0-15", "16-29", "30-44", "45-64","65+"), main="Age Distribution", xlab="Age Groups", ylab="Population Count", cex.names = 0.9, col=turq, ylim = c(0, 58000))



#---assessing outliers*************start from here


#running regression with variance



outlierTest
mod_o = lm(social_isolation_pc1 ~., data=newdata)
summary(mod_o)


# Assessing Outliers
outlierTest(mod_o) # Bonferonni p-value for most extreme obs
qqPlot(mod_o, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(mod_o) # leverage plots

#--- taking out 14

newdata_no14 <- newdata[-14,]

mod_o14_27 = lm(social_isolation_pc1 ~., data=newdata_no14_27)
summary(mod_o14_27)
outlierTest(mod_o14_27) # Bonferonni p-value for most extreme obs
qqPlot(mod_o14, main="QQ Plot") #qq plot for studentized resid 


plot(mod_o14_27)
#now it says

newdata_no44 <- newdata_no14[-c(44),]

mod_o44 = lm(social_isolation_pc1 ~. -Ethnic.Group.BAME.....2011, data=newdata_no44)
summary(mod_o44)
outlierTest(mod_o44) # Bonferonni p-value for most extreme obs
qqPlot(mod_o44, main="QQ Plot") #qq plot for studentized resid 

#won't take it out^

# take out row 14 from original df because we'll join lsoas later on si_2250
si_2250_14 <- si_2250[-14,]

#--- Took out control variables

newdata_mc <- newdata_no14[,c(1:27)]


#-Checking for mulicorrineality

mod_8 = lm(social_isolation_pc1 ~ count.mean.category +., data=newdata_mc)
summary(mod_8)

correlation_xvar <- as.data.frame(cor(newdata_mc, method = "pearson", use = "complete.obs"))
vif8_27 <-  as.data.frame(car::vif(mod_8_27))

vif8 <- as.data.frame(car::vif(mod_8))


#without the highest count.mean

mod_9  = lm(social_isolation_pc1 ~. -count.mean.category  , data=newdata_mc)
summary(mod_9)
vif9 <- as.data.frame(car::vif(mod_9))


#without the highest count.mean + 10

mod_10  = lm(social_isolation_pc1 ~. -count.mean.category  -`10` , data=newdata_mc)
summary(mod_10)
vif10 <- as.data.frame(car::vif(mod_10))


#without the highest count.mean + 10 + 9

mod_11  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9`, data=newdata_mc)
summary(mod_11)
vif11 <- as.data.frame(car::vif(mod_11))

#without the highest count.mean + 10 + 9 +2 

mod_12  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2`, data=newdata_mc)
summary(mod_12)
vif12 <- as.data.frame(car::vif(mod_12))

#without the highest count.mean + 10 + 9 +2 

mod_12  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2`, data=newdata_mc)
summary(mod_12)
vif12 <- as.data.frame(car::vif(mod_12))

# really interested 28, 47 so dont want to take those out. the next one is 48

mod_13  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48`, data=newdata_mc)
summary(mod_13)
vif13 <- as.data.frame(car::vif(mod_13))

# without the highest count.mean + 10 + 9 +2 + 48 +47

mod_14  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47`, data=newdata_mc)
summary(mod_14)
vif14 <- as.data.frame(car::vif(mod_14))

# without the highest count.mean + 10 + 9 +2 + 48 +47 really interested in 28 so will take out next big thing which is 4

mod_15  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47` -`4`, data=newdata_mc)
summary(mod_15)
vif15 <- as.data.frame(car::vif(mod_15))


#28 is still high but doesnt make much of difference in taking it out will take out next biggest thing 32
mod_16  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47` -`4` -`32`, data=newdata_mc)
summary(mod_16)
vif16 <- as.data.frame(car::vif(mod_16))

#28 is still high but doesnt make much of difference in taking it out will take out next biggest thing 46
mod_17  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47` -`4` -`32` -`46`, data=newdata_mc)
summary(mod_17)
vif17 <- as.data.frame(car::vif(mod_17))


#28 is still high lets test to take it out. makes a difference
mod_18  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47` -`4` -`32` -`46` -`28`, data=newdata_mc)
summary(mod_18)
vif18 <- as.data.frame(car::vif(mod_18))

#take out 8
mod_19  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47` -`4` -`32` -`46` -`28` -`8`, data=newdata_mc)
summary(mod_19)
vif19 <- as.data.frame(car::vif(mod_19))

#take out 13
mod_20  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47` -`4` -`32` -`46` -`28` -`8` -`13`, data=newdata_mc)
summary(mod_20)
vif20 <- as.data.frame(car::vif(mod_20))

#take out 11
mod_21  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47` -`4` -`32` -`46` -`28` -`8` -`13` -`11`, data=newdata_mc)
summary(mod_21)
vif21 <- as.data.frame(car::vif(mod_21))

#take out 18 still high so had to take it out
mod_22  = lm(social_isolation_pc1 ~. -count.mean.category  -`10`   -`9` -`2` -`48` -`47` -`4` -`32` -`46` -`28` -`8` -`13` -`11` -`18`-`35`, data=newdata_mc)
summary(mod_22)
vif22 <- as.data.frame(car::vif(mod_22))


#now add control variables and take out the high vif values

newdata_no14_final <- newdata_no14[,c(1, 4,6,7,13,14,16,17,23,19,21,27,28:38)]


#rename POIs
names(newdata_no14_final)[2:12] <- c("Construction Services", "Engineering Services", "Contract Services", "Research and Design", "Transport, Storage and Delivery", "Gambling", "Sports Complex", "Consumer Products", "Primary, Secondary and Tertiary Education", "Central and local Government", "Public Transport, Stations and Infrastructure")

#Run multi linear regression
mod_final  = lm(social_isolation_pc1 ~., newdata_no14_final)
summary(mod_final)


plot(mod_final, pch=16, which=1)

correlation_final_after <- as.data.frame(cor(newdata_no14_final, method = "pearson", use = "complete.obs"))

chart.Correlation(newdata_no14_final, histogram=TRUE, pch=19)



#Running random forest

#first rename column names because rf doesn't accept numbers

names(newdata_no14)[3:27] <- c("t.2","t.3", "t.4","t.6","t.7","t.8","t.9","t.10", "t.11","t.13","t.14","t.15", "t.18","t.22", "t.24","t.28","t.31","t.32", "t.33","t.35", "t.37","t.46","t.47","t.48", "t.57")


si.rf.wholedata=randomForest(social_isolation_pc1 ~ . , data = newdata_no14, na.action = na.roughfix)
si.rf.wholedata

#Find which variables are important (significant in regression)
importance(si.rf.wholedata, type=2)
df.si.rf.wholedata.Importance <- data.frame(variable = names(si.rf.wholedata$importance[,1]), importance = si.rf.wholedata$importance[,1])

varImpPlot(si.rf.wholedata,
           sort = T,
           main="Variable Importance")

write.csv(df.si.rf.wholedata.Importance, "data_be/rf-importance.csv")


#add lsoas 
rownames(newdata_no14_final)<- si_2250_14$Lower.Super.Output.Area
newdata_no14_final <- cbind(Lower.Super.Output.Area = rownames(newdata_no14_final), newdata_no14_final)
rownames(newdata_no14_final) <- 1:nrow(newdata_no14_final)

write.csv(newdata_no14_final,"data_be/newdata_no14_final.csv")

