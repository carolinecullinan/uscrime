setwd("C:/Users/Carol/Documents/EPA 2019-2020/EPA1315/EPA1315_Assignments/EPA1315_FinalProject/EPA1315_FinalProject_Work")

library(sf)
library(ggplot2)
library(dplyr)
library(stringr)

#Read and explore San Francisco crime, assets, trees, and voting precincts
SF_crime <- st_read("Data/SF/Crime/geo_export_a2fb358b-e6e5-4c4b-818a-ed803c677a22.shp")
SF_assets<- st_read("Data/SF/Assets/geo_export_3ef3df6c-711b-4844-ac54-689c6612f710.shp")
SF_trees<- st_read("Data/SF/Trees/geo_export_238608c0-4b82-4317-8458-1fd216c4f7fb.shp")
SF_prcnts <- st_read("Data/SF/Precincts/geo_export_5dc474ad-572b-48a0-bf5c-405daa472de0.shp")

#Read and explore Los Angeles crime, trash cans, street lights, trees, and voting precincts
LA_crime <- read.csv("Data/LA/Crime/Crime_Data_from_2010_to_Present.csv")
LA_trashcans <- st_read("Data/LA/Trashcans/LA_City_Receptacles.shp")
LA_streetlights <- st_read("Data/LA/StreetLights/geo_export_1f03ddc3-260e-4190-91b3-982e850cd602.shp")
LA_trees <- st_read("Data/LA/Trees/output.shp")
LA_prcnts <- st_read("Data/LA/Precincts/geo_export_491b6183-9bea-418b-a555-e01c5db01ffc.shp")

#Read and explore Philadelphia crime, trash cans, street lights, trees, and voting precincts
PH_crime <- st_read("Data/Philadelphia/Crime/Crime_Data_from_2018_to_Present.shp")
PH_trashcans_bb <- st_read("Data/Philadelphia/TrashCans/WasteBaskets_Big_Belly.shp")
PH_trashcans_w <- st_read("Data/Philadelphia/TrashCans/WasteBaskets_Wire.shp")
PH_streetlights <- st_read("Data/Philadelphia/Streetlights/Street_Poles.shp")
PH_trees <- st_read("Data/Philadelphia/Trees/PPR_StreetTrees.shp")
PH_prcnts <- st_read("Data/Philadelphia/Precincts/Political_Divisions.shp")

#Read and explore Washington D.C. crime, trash cans, street lights, trees, and voting precincts
DC_crime_2018 <-st_read("Data/DC/Crime/Crime_Incidents_in_2018.shp")
DC_crime_2019 <- st_read("Data/DC/Crime/Crime_Incidents_in_2019.shp")
DC_trashcans <- st_read("Data/DC/TrashCans/Litter_Cans.shp")
DC_streetlights <- st_read("Data/DC/StreetLights/Street_Lights.shp")
DC_trees <- st_read("Data/DC/Trees/Urban_Forestry_Street_Trees.shp")
DC_prcnts <- st_read("Data/DC/Precincts/Voting_Precinct__2012.shp")

#Summarising Data : EX: Washington D.C.
summary(DC_crime_2018)
summary(DC_crime_2019)
summary(DC_trashcans)
summary(DC_streetlights)
summary(DC_trees)
summary(DC_prcnts)

#Get trashcans from SF
SF_trashcans <- SF_assets %>%
  dplyr::select(asset_type, geometry) %>%   
  filter(str_detect(asset_type, c('Trash Can')))

#Get streetlights from SF
SF_streetlights <- SF_assets %>%
  dplyr::select(asset_type, geometry) %>%   
  filter(str_detect(asset_type, c('Lamp Post')))


#Visualise: EX: San Francisco relevant data before data-cleaning

# SF voting precincts visualised by geometry
ggplot() + 
  geom_sf(data = SF_prcnts$geometry, fill = "transparent", color = "black", size = 1) + 
  coord_sf() +
  labs(title = "San Francisco Voting Precincts", 
       x = "", y="") +
  theme_void() + 
  theme(plot.title = element_text(hjust = -0.5))

# SF crime incidences visualised by geometry an on voting precinct
ggplot() + 
  geom_sf(data = SF_crime$geometry, fill = "transparent", color = "red", size = 0.10) + 
  geom_sf(data = SF_prcnts$geometry, fill = "transparent", color = "grey", size = 0.05) +
  coord_sf() +
  labs(title = "San Francisco Crime Incidences", 
       x = "", y="") +
  theme_void() + 
  theme(plot.title = element_text(hjust = -0.5))


# SF  trash can assets visualised by geometry and on voting precincts
ggplot() + 
  geom_sf(data = SF_trashcans$geometry, fill = "transparent", color = "blue", size = 0.10) + 
  geom_sf(data = SF_prcnts$geometry, fill = "transparent", color = "grey", size = 0.05) +
  coord_sf() +
  labs(title = "San Francisco Trash Cans per Voting Precinct", 
       x = "", y="") +
  theme_void() + 
  theme(plot.title = element_text(hjust = -0.5))

# SF street light assets visualised by geometry
ggplot() + 
  geom_sf(data = SF_streetlights$geometry, fill = "transparent", color = "yellow", size = 0.10) + 
  geom_sf(data = SF_prcnts$geometry, fill = "transparent", color = "grey", size = 0.05) +
  coord_sf() +
  labs(title = "San Francisco Streetlights per Voting Precinct", 
       x = "", y="") +
  theme_void() + 
  theme(plot.title = element_text(hjust = -0.5))


plot(SF_prcnts$geometry)


plot(SF_crime$geometry)
plot(SF_assets$geometry)
plot(SF_trees$geometry)
plot(SF_prcnts$geometry)

#Voting precincts visualised by geometry
ggplot() + 
  geom_sf(data = SF_prcnts$geometry, fill = "transparent", color = "black", size = 1) + 
  geom_sf(data = street, color = "blue", size = 0.5) +
  geom_sf(data = nbhd_stps_pop$geometry, fill = "transparent", color = "grey") + 
  geom_sf(data = ind_t, fill = "grey", size = 0.25) +
  coord_sf() +
  labs(title = "Philadelphia Mapped", 
       x = "", y="") +
  theme_void() + 
  theme(plot.title = element_text(hjust = -0.1))

