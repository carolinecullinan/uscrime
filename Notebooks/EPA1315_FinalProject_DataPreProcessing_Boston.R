#Let's start by setting our directory and importing relevant packages
setwd("C:/Users/Carol/Documents/EPA 2019-2020/EPA1315/EPA1315_Assignments/EPA1315_FinalProject/EPA1315_FinalProject_Work")

#Install required For this project
#install.packages(c('wesanderson', 'RColorBrewer', 'sf', 'ggmap', 'maps', 'mapdata', 'rgeos', 'tidyverse', 'lwgeom', 'stringr', 'plotly', 'NLP'))

#Libraries required For this project
library(rgdal)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(sf)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgeos)
library(tidyverse)
library(lwgeom)
library(stringr)
library(dplyr)
library(NLP)


#Now let's import data for Boston

#Open Boston public streetlights
BO_streetlights <- st_read("Data/Boston/StreetLights/geo_export_6d166c39-621f-4ca5-8fa9-6de277c48821.shp")
#Open Boston trashcans
BO_trashcans <- read.csv("Data/Boston/TrashCans/big-belly-locations.csv")
#Open Boston trees
BO_trees <- st_read("Data/Boston/Trees/Trees.shp")
#Open Boston precincts
BO_prcnts <- st_read("Data/Boston/Precincts/Precincts_2017.shp")
#Open Boston crime
BO_crime <- read.csv("Data/Boston/Crime/tmp2a1yu35_.csv")





#Now let's clean up our layers to include only columns of interest

#Crime
#Subset BO_crime for 2018 and onwards
BO_crime <- subset(BO_crime, BO_crime['YEAR'] == '2018' | BO_crime['YEAR'] == '2019')
#Get rid of NA values for lat and long
BO_crime <-subset(BO_crime, is.na(BO_crime$Long)==FALSE)
BO_crime <- BO_crime[, c('Lat', 'Long', 'INCIDENT_NUMBER')]
#Convert crime csv to geospatial object
BO_crime <- st_as_sf(BO_crime, coords = c("Long", "Lat"), 
                     crs = 4326, agr = "constant")

#Check structure of BO_crime
str(BO_crime)
#Write and read BO_crime shapefile
st_write(BO_crime,'Data/Boston/Crime/Crime_Data_from_2018_to_Present.shp')
BO_crime <- st_read('Data/Boston/Crime/Crime_Data_from_2018_to_Present.shp')
#Add crime count column
BO_crime$crimecount <- 1

#Precincts layer
BO_prcnts <- select(BO_prcnts, 'WARD_PRECI', 'geometry')

#Trashcans
#Create columns "Lat" and "Long" from "Location"
BO_trashcans <- BO_trashcans %>% separate('Location', into = paste0('Location', 1:2 ), sep = ',')
BO_trashcans$Long <- stringr::str_replace(BO_trashcans$Location1, '\\(', '')
BO_trashcans$Lat <- stringr::str_replace(BO_trashcans$Location2, '\\)', '')
#Convert Long and Lat to numeric
BO_trashcans$Long<- as.numeric(BO_trashcans$Long)
BO_trashcans$Lat<- as.numeric(BO_trashcans$Lat)
#Remove unneeded columns
BO_trashcans <- select(BO_trashcans, 'description', 'Lat', 'Long')
#Get rid of NA values for location
BO_trashcans <- subset(BO_trashcans, is.na(BO_trashcans$Lat)==FALSE)
#Convert crime csv to geospatial object
BO_trashcans <- st_as_sf(BO_trashcans, coords = c("Lat", "Long"), 
                     crs = 4326, agr = "constant")
#Check structure of BO_trashcans
str(BO_trashcans)
#Write and read BO_trashcans shapefile
st_write(BO_trashcans,'Data/Boston/TrashCans/TrashCans.shp')
BO_trashcans <- st_read('Data/Boston/TrashCans/TrashCans.shp')
#Add trash count column
BO_trashcans$trashcount <- 1

#Streetlights
BO_streetlights <- select(BO_streetlights, 'objectid', 'geometry')
BO_streetlights$lightcount <- 1

#Trees
BO_trees <- select(BO_trees, 'OBJECTID', 'geometry')
BO_trees$treecount <- 1



#Check coordinate systems
st_crs(BO_streetlights)
st_crs(BO_trashcans)
st_crs(BO_trees)
st_crs(BO_prcnts)
st_crs(BO_crime)

#Reproject to common coordinate systems
#Reproject streetlights to common coordinate system
BO_streetlights <- st_transform(BO_streetlights, "+proj=longlat +datum=WGS84 +no_defs")

#Check that all spatial layers are in the same coordinate system
st_crs(BO_streetlights)
st_crs(BO_trashcans)
st_crs(BO_trees)
st_crs(BO_prcnts)
st_crs(BO_crime)



#Let's write a function that takes in all the variables, perform spatial joins on points inside voting precinct, and summarizes by voting precinct 
BO_prcnts_varsfunc <- function(geom, var1, var2, var3, var4) {
  #var1=trees; var2=crime; var3=streetlights; var4=trashcans
  
  #Join first variable
  geom_var1 <- st_join(geom, left = TRUE, var1[length(var1)])
  geom_var1 <- group_by(geom_var1, WARD_PRECI)
  geom_var1 <- summarise(geom_var1, treecount = sum(treecount))
  #Join second variable
  geom_var12 <- st_join(geom_var1, left = TRUE, var2[length(var2)])
  geom_var12 <- group_by(geom_var12, WARD_PRECI, treecount)
  geom_var12 <- summarise(geom_var12, crimecount = sum(crimecount))
  #Join third variable
  geom_var123 <- st_join(geom_var12, left = TRUE, var3[length(var3)])
  geom_var123 <- group_by(geom_var123, WARD_PRECI, treecount, crimecount)
  geom_var123 <- summarise(geom_var123, lightcount = sum(lightcount))
  #Join fourth variable
  geom_var1234 <- st_join(geom_var123, left = TRUE, var4[length(var4)])
  geom_var1234 <- group_by(geom_var1234, WARD_PRECI, treecount, crimecount, lightcount)
  geom_var1234 <- summarise(geom_var1234, trashcount = sum(trashcount))
  
  #Replace NA values with 0 (meaning that the count for a given var is 0 if there was no point within a polygon)
  geom_var1234[is.na(geom_var1234)] <- 0
  geom_vars <- geom_var1234
  return(geom_vars)
}

#Running our spatial join function for Washington DC
BO_prcnts_vars <- BO_prcnts_varsfunc(BO_prcnts, BO_trees, BO_crime, BO_streetlights, BO_trashcans)
#Save file
st_write(BO_prcnts_vars, "Data/Boston/ProcessedData/BO_prcnts_vars.shp")

#First we calculate the area of each precinct in km2
BO_prcnts_vars$km2 <- as.numeric((st_area(BO_prcnts_vars)/1000000))
#Next we divide each variable by the tract area
BO_prcnts_vars$crimecount <- BO_prcnts_vars$crimecount/BO_prcnts_vars$km2
BO_prcnts_vars$lightcount <- BO_prcnts_vars$lightcount/BO_prcnts_vars$km2
BO_prcnts_vars$trashcount <- BO_prcnts_vars$trashcount/BO_prcnts_vars$km2
BO_prcnts_vars$treecount <- BO_prcnts_vars$treecount/BO_prcnts_vars$km2
#Reorder dataframe
BO_prcnts_vars <- BO_prcnts_vars[,c('treecount','crimecount','lightcount','trashcount', 'WARD_PRECI', 'geometry', 'km2')]
#Save file
st_write(BO_prcnts_vars, "Data/Boston/ProcessedData/Boston_prcnts_vars_norm.shp")
