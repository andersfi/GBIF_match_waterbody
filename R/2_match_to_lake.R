################################################################################
#
#  Match GBIF occurrence to lakes
#
################################################################################

library(sf)
library(dplyr)
library(mapview)

#------------------------------------------------------------------------------
# load lake layer
# Example dataset used below is NVE lake database
# downloaded from NVE 2019-08-24. Data are realesed from NVE under a
# cc-by eqvivalent, site as "NVE Innsjødatabasen, downloaded from https://kartkatalog.nve.no/
# 2019-08-24". 
#------------------------------------------------------------------------------

# NVE innsjødatabsen convinience download -----------------------------------
temp <- tempdir()
download.file("https://ntnu.box.com/shared/static/6vu4de2birf9onmej2gorexwaxposuaa.zip",destfile = paste0(temp,"/NVE_innsjodatabasen.zip"))
unzip(paste0(temp,"/NVE_innsjodatabasen.zip"),exdir=temp)
lakes <- st_read(paste0(temp,"/Innsjo_Innsjo.shp"))
saveRDS(lakes,"./data/lake_polygons.rds")
unlink(temp)

lakes <- readRDS("./data/lake_polygons.rds")

#-------------------------------------------------------------------------------------------------
# load GBIF occurrence data and convert to EPSG:32633 (or whatever the same as the lake dataset)
#-------------------------------------------------------------------------------------------------

occ <- readRDS("./data/GBIF_download.rds") %>% 
  dplyr::select_if(~!all(is.na(.)))

occ_sf <- st_as_sf(occ, coords = c("decimalLongitude", "decimalLatitude"), 
                 crs = 4326)
occ_sf <- st_transform(occ_sf, 32633)
occ_sf <- occ_sf %>%
  dplyr::select(gbifID,occurrenceID,catalogNumber,geometry,species,taxonKey,datasetKey, locality,municipality,county,countryCode,locationID,
                eventDate,year,month,day,samplingProtocol,eventID,fieldNumber,
                recordedBy,dynamicProperties,collectionCode,datasetName,license,institutionCode)
# have a look at the data: mapview(occ_sf1)

#-------------------------------------------------------------------------------------------------
# find closest lake, distance to closest lake, and join
#-------------------------------------------------------------------------------------------------

# find closest lake - and join
occ_sf1 <- occ_sf[1:5,]
start_time <- Sys.time()
garg <- st_join(occ_sf, lakes, join = st_nearest_feature)
end_time <- Sys.time()
end_time - start_time

# find distance to closest lake

# selection for test purposes
occ_sf1 <- occ_sf#[1:100,]

# find closest lake
start_time <- Sys.time()
occ_with_lakes <- st_join(occ_sf1, lakes, join = st_nearest_feature)
end_time <- Sys.time()
end_time - start_time

# find distance to closest lake
start_time <- Sys.time()
index <- st_nearest_feature(x = occ_sf1, y = lakes) # index of closest lake
closest_lakes <- lakes %>% slice(index) # slice based on the index
dist_to_lake <- st_distance(x = occ_sf1, y= closest_lakes, by_element = TRUE) # get distance
occ_with_lakes$dist_to_lake <- as.numeric(dist_to_lake) # add the distance calculations to match data
end_time <- Sys.time()
end_time - start_time

occ_farfaraway <- occ_with_lakes %>% filter(dist_to_lake>0)
mapview(occ_farfaraway)

#-------------------------------------------------------------------------------------------------
# Filter out occurrence records not matching lakes (given certain criteria)
#-------------------------------------------------------------------------------------------------

occ_matched <- occ_with_lakes %>% filter(dist_to_lake<10) # example, 10 m




# # trying st_nn function of the nngeo package - seems to move slow
# #install.packages("nngeo")
# library(nngeo)
# occ_sf1 <- occ_sf[1:5,]
# start_time <- Sys.time()
# garg <- st_nn(occ_sf1, lakes, sparse = TRUE, k = 2, maxdist = 1500,
#       returnDist = TRUE, progress = TRUE)
# end_time <- Sys.time()
# end_time - start_time



