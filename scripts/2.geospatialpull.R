# This script will pull all of the geospatial data that we didn't get from GEE

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)  


# shapefile, benin
shape <- read_sf("shapefile/ben_admbnda_adm2_1m_salb_20190816.shp")

#gridded shapefile
shapegrid <- read_sf("shapefile/grid.shp")



# ACLED ------------------
# NOTE: I removed all countries except Benin from this data
# There are relatively few events, so only doing it to admin2, NOT based on grid
data <- read_csv("data/acled.csv")
# first, just keep some of the variables of interest
data <- data %>%
  dplyr::select(lon = longitude, lat = latitude, date = event_date, fatalities, disorder_type, event_type)
# use lubridate package to create date
data$date <- dmy(data$date)
data$events <- 1
# turn each row into a point (sf object)
data <- st_as_sf(data, coords = c("lon", "lat"), 
                   crs = crs(shape))
# extract shape info to ACLED
data <- st_join(data, shape)

# in Benin, there aren't that many events. As such, let's just count up total fatalities and total events in 2017 and 2018
data <- as_tibble(data) %>%
          group_by(admin2Pcod) %>%
          mutate(
                 fatalities = sum(fatalities, na.rm = T), # total fatalities
                 events = sum(events, na.rm = T) # total events
                 ) %>%
          filter(row_number()==1) %>%
          ungroup() %>%
          dplyr::select(admin2Pcod, fatalities, events) #just the variables we want
write_csv(data, "data/features/acled_clean.csv")





# Roads ------------------
# downloaded from here: https://www.globio.info/download-grip-dataset
# switch off spherical geometry (prevents an error)
sf_use_s2(FALSE)
# Also do to admin2 because of sparsity of roads in many areas
# NOTE: I have deleted the parts of this shapefile that are not in Benin in order to reduce the size of the shapefile.
data <- read_sf("data/GRIP4_Region3_vector_shp/GRIP4_region3.shp")
# right now, shapefile and roads are in lat/lon. However, we want them in METERS.
# As such, reproject to a different CRS (based on Benin)
data <- st_transform(data, crs = "EPSG:32631")
# same to shapefile
shapemeters <- st_transform(shape, crs = st_crs(data))

# now crop to make extraction quicker (this reduces the size of the data by a lot)
data <- st_crop(data, shapemeters)

# This loop will calculate the TOTAL LENGTH 
shapemeters$total_length <- 0
for (i in 1:nrow(shapemeters)) {
  shapemeters$road_length[i] <- sum(st_length(st_intersection(shapemeters[i, ], data)))
  print(i)
}
data <- as_tibble(shapemeters) %>%
  dplyr::select(admin2Pcod, road_length) %>%
  group_by(admin2Pcod) %>%
  mutate(road_length = sum(road_length, na.rm = T)) %>%
  filter(row_number()==1) %>%
  ungroup()
write_csv(data, "data/features/roads_clean.csv")
rm(data, shapemeters)





# Buildings ------------------
# download from google 
# this takes a while because it is 2 GB. If you are on a slow connection, you may want to download it manually
# OR ignore this feature
# Because this file is so large, you need a decent amount of RAM to load it. If you don't have much, I'd suggest completely
# skipping this section.
# increasing timeout because of how long it takes to download
# do this TO GRID
options(timeout = max(1000, getOption("timeout")))  
file <- "https://storage.googleapis.com/open-buildings-data/v3/polygons_s2_level_4_gzip/103_buildings.csv.gz"
download.file(url = paste0(file), "data/features/buildings1.csv.gz")
# second file (there are two to cover benin)
file <- "https://storage.googleapis.com/open-buildings-data/v3/polygons_s2_level_4_gzip/11d_buildings.csv.gz"
download.file(url = paste0(file), "data/features/buildings2.csv.gz")

# load first csv
data <- read_csv("data/features/buildings1.csv.gz")
# and second
data <- rbind(data, read_csv("data/features/buildings2.csv.gz"))

# easy way to drop things that fall outside of Benin
data <- data %>%
          filter(
                  longitude>=extent(shape)[1],
                  longitude<=extent(shape)[2],
                  latitude>=extent(shape)[3],
                  latitude<=extent(shape)[4]
                  ) %>%
          dplyr::select(latitude, longitude, area_in_meters, confidence)

# now turn data into sf object using coordinates
data <- st_as_sf(data, coords = c("longitude", "latitude"), 
                   crs = crs(shape))
# grid's crs is in METERS, not lat/lon. Create lat/lon to match crs of the data points
shapegridlatlon <- st_transform(shapegrid, crs = st_crs(shape))

# crop even more
data <- st_crop(data, shapegridlatlon)

# extract shapefile variables to buildings
data <- st_join(data, shapegridlatlon, join = st_intersects)

# now we want to calculate the total area of buildings in each admin2 and the number of buildings
data <- as_tibble(data) %>%
  # first totals at admin2
  group_by(admin2Pcod) %>%
  mutate(
         total_area_adm2 = sum(area_in_meters, na.rm = T), # sum of size in square meters
         total_buildings_adm2 = n()             # number of buildings
         ) %>%
  # now totals at id
  group_by(id) %>%
  mutate(
         total_area = sum(area_in_meters), # sum of size in square meters
         total_buildings = n()             # number of buildings
         ) %>%
  filter(row_number()==1) %>%
  dplyr::select(id, total_area, total_buildings, total_area_adm2, total_buildings_adm2)
data <- data %>% filter(is.na(id)==F)
write_csv(data, "data/features/buildings_clean.csv")

# now delete the csvs (they are large and we do'nt need them anymore)
unlink("data/features/buildings1.csv.gz")
unlink("data/features/buildings2.csv.gz")





# Distance to largest city in each country ------------------
# these are the ten largest cities in Benin
data <- read_csv("data/cities.csv")
colnames(data) <- c("city", "lat", "lon")
data <- st_as_sf(data, coords = c("lon", "lat"), 
                   crs = crs(shapegrid))

# get centroids of shapefile
# us shapegridlatlon to match cities
centroids <- st_centroid(shapegrid)
# turn both into meters
data <- st_transform(data, crs = "EPSG:32631")

# now we want to find the distance from each centroid to each city
# we will assume a flat surface because of the projection and how small Benin is.
for (j in 1:nrow(data)) {
  # create new variable
  centroids[[paste0("distance", j)]] <- NA
  # now calculate distance for each centroid (again assuming a flat surface)
  for (i in 1:nrow(centroids)) {
    centroids[[paste0("distance", j)]][i] <- sqrt( (centroids$geometry[i][[1]][1]-data$geometry[j][[1]][1])^2 + (centroids$geometry[i][[1]][2]-data$geometry[j][[1]][2])^2 )
  }
  print(j)
}
centroids <- as_tibble(centroids) %>%
          dplyr::select(id, distance1, distance2, distance3, distance4, distance5, distance6, distance7, distance8, distance9, distance10)
write_csv(centroids, "data/features/distancecities_clean.csv")







# Distance to nearest river ------------------
# comes from here: https://data.apps.fao.org/catalog/iso/b891ca64-4cd4-4efd-a7ca-b386e98d52e8
# I have deleted the parts of the shapefile that are NOT in Benin to reduce the size of the shapefile
data <- read_sf("data/rivers/rivers_africa_37333.shp")
# projected to benin
data <- st_transform(data, crs = "EPSG:32631")
# same to shapefile
shape <- st_transform(shape, crs = st_crs(data))

# first, length of rivers in each polygon (adm1)
data <- st_crop(data, shape)
# do length for each strahler type
shape$river_length_s7 <- 0
shape$river_length_s6 <- 0
shape$river_length_s5 <- 0
shape$river_length_s4 <- 0
shape$river_length_s3 <- 0
shape$river_length_s2 <- 0
shape$river_length_s1 <- 0
for (i in 1:nrow(shape)) {
  shape$river_length_s7[i] <- sum(st_length(st_intersection(shape[i, ], data %>% filter(Strahler==7))))
  shape$river_length_s6[i] <- sum(st_length(st_intersection(shape[i, ], data %>% filter(Strahler==6))))
  shape$river_length_s5[i] <- sum(st_length(st_intersection(shape[i, ], data %>% filter(Strahler==5))))
  shape$river_length_s4[i] <- sum(st_length(st_intersection(shape[i, ], data %>% filter(Strahler==4))))
  shape$river_length_s3[i] <- sum(st_length(st_intersection(shape[i, ], data %>% filter(Strahler==3))))
  shape$river_length_s2[i] <- sum(st_length(st_intersection(shape[i, ], data %>% filter(Strahler==2))))
  shape$river_length_s1[i] <- sum(st_length(st_intersection(shape[i, ], data %>% filter(Strahler==1))))
  print(i)
}
data <- as_tibble(shape) %>%
  dplyr::select(admin2Pcod, c(starts_with("river_length"))) %>%
  group_by(admin2Pcod) %>%
  # now add up each river length
  mutate(
    river_length_s7 = sum(river_length_s7),
    river_length_s6 = sum(river_length_s6),
    river_length_s5 = sum(river_length_s5),
    river_length_s4 = sum(river_length_s4),
    river_length_s3 = sum(river_length_s3),
    river_length_s2 = sum(river_length_s2),
    river_length_s1 = sum(river_length_s1)
  ) %>%
  filter(row_number()==1) %>%
  ungroup()
write_csv(data, "data/features/rivers_clean.csv")




# population ------------------
data <- rast("data/worldpop.tif")
# extract population data to grids
data <- exact_extract(data, shapegridlatlon, fun = "sum", append_cols = c("id", "admin2Pcod"))
# admin 2 pop
data <- data %>%
        rename(population = sum) %>%
        group_by(admin2Pcod) %>%
        mutate(population_adm2 = sum(population)) %>%
        ungroup()
write_csv(data, "data/features/population_clean.csv")



data <- exact_extract(data, shape, fun = "sum", append_cols = c("admin2Pcod"))



# GEE data ------------------
# note: I had to pull these separately using GEE's script editor (code.earthengine.google.com)
# the script is in the project folder (javascript)
## NDVI ------------------
data <- read_csv("data/features/ndvi.csv")
data <- data %>%
  mutate(date = as_date(imageId),
         month = month(date),
         year = year(date)) %>%
  dplyr::select(-c("imageId"))

# Let's keep this SIMPLE
# there are many ways we could do this, including using monthly values. Instead, let's just take averages, sds, max, and min
# admin2 first
data_adm2 <- data %>%
              group_by(admin2Pcod) %>%
              mutate(ndvi_mean_adm2 = mean(mean, na.rm=T),
                        ndvi_sd_adm2 = sd(mean, na.rm=T),
                        ndvi_max_adm2 = max(mean, na.rm=T),
                        ndvi_min_adm2 = min(mean, na.rm=T)) %>%
              filter(row_number()==1) %>%
              ungroup() %>%
              dplyr::select(admin2Pcod, ndvi_mean_adm2, ndvi_sd_adm2, ndvi_max_adm2, ndvi_min_adm2)
# and grid cell
data <- data %>%
              group_by(id) %>%
              mutate(ndvi_mean = mean(mean, na.rm=T),
                        ndvi_sd = sd(mean, na.rm=T),
                        ndvi_max = max(mean, na.rm=T),
                        ndvi_min = min(mean, na.rm=T)) %>%
              filter(row_number()==1) %>%
              ungroup() %>%
              dplyr::select(id, admin2Pcod, ndvi_mean, ndvi_sd, ndvi_max, ndvi_min)
# add admin2 data
data <- data %>% left_join(data_adm2, by="admin2Pcod")
# now save
write_csv(data, "data/features/ndvi_clean.csv")


## Nightlights ------------------
data <- read_csv("data/features/ntl.csv")
data <- data %>%
  mutate(date = ymd(imageId),
         month = month(date),
         year = year(date)) %>%
  dplyr::select(-c("imageId"))
# do the EXACT SAME THING as above
# admin2 first
data_adm2 <- data %>%
              group_by(admin2Pcod) %>%
              mutate(ntl_mean_adm2 = mean(mean, na.rm=T),
                        ntl_sd_adm2 = sd(mean, na.rm=T),
                        ntl_max_adm2 = max(mean, na.rm=T),
                        ntl_min_adm2 = min(mean, na.rm=T)) %>%
              filter(row_number()==1) %>%
              ungroup() %>%
              dplyr::select(admin2Pcod, ntl_mean_adm2, ntl_sd_adm2, ntl_max_adm2, ntl_min_adm2)
# and grid cell
data <- data %>%
              group_by(id) %>%
              mutate(ntl_mean = mean(mean, na.rm=T),
                        ntl_sd = sd(mean, na.rm=T),
                        ntl_max = max(mean, na.rm=T),
                        ntl_min = min(mean, na.rm=T)) %>%
              filter(row_number()==1) %>%
              ungroup() %>%
              dplyr::select(id, admin2Pcod, ntl_mean, ntl_sd, ntl_max, ntl_min)
# add admin2 data
data <- data %>% left_join(data_adm2, by = "admin2Pcod")
# now save
write_csv(data, "data/features/ntl_clean.csv")


## Terraclimate ------------------
### Precip ------------------
data <- read_csv("data/features/terraclimate.csv")
# Now define month and date
data <- data %>%
  mutate(date = ymd(paste0(imageId, "01")),
         month = month(date),
         year = year(date)) %>%
  dplyr::select(-c("imageId"))
# get 2018 (year of survey) data
data2018 <- data %>% 
            filter(year(date)==2018) %>%
            group_by(id) %>%
            mutate(precip_2018 = sum(pr, na.rm=T),
                    precip_2018_sd = sd(pr, na.rm=T)) %>%
            filter(row_number()==1) %>%
            ungroup() %>%
            dplyr::select(id, admin2Pcod, precip_2018, precip_2018_sd)
data2018_adm2 <- data2018 %>% 
                  group_by(admin2Pcod) %>%
                  mutate(precip_2018_adm2 = mean(precip_2018, na.rm=T), 
                         precip_2018_sd_adm2 = sd(precip_2018, na.rm=T)) %>%
                  filter(row_number()==1) %>%
                  ungroup() %>%
                  dplyr::select(admin2Pcod, precip_2018_adm2, precip_2018_sd_adm2)

# now collapse to YEAR (except 2018)
dataadm2 <- data %>%
            filter(year(date)!=2018) %>%
            group_by(year, admin2Pcod) %>%
            mutate(precip_year_adm2 = sum(pr, na.rm=T)) %>%
            filter(row_number()==1) %>%
            group_by(admin2Pcod) %>%
            mutate(precip_year_adm2 = mean(precip_year_adm2, na.rm = T)) %>%
            filter(row_number()==1) %>%
            dplyr::select(admin2Pcod, precip_year_adm2)
# and now for id
data <- data %>%
            filter(year(date)!=2018) %>%
            group_by(year, id) %>%
            mutate(precip_year = sum(pr, na.rm=T)) %>%
            filter(row_number()==1) %>%
            group_by(id) %>%
            mutate(precip_year = mean(precip_year, na.rm = T)) %>%
            filter(row_number()==1) %>%
            dplyr::select(id, precip_year)

# add others to data
data <- data %>% left_join(data2018, by = "id")
data <- data %>% left_join(data2018_adm2, by = "admin2Pcod")
data <- data %>% left_join(dataadm2, by = "admin2Pcod")
# a few NAs in SD, lets' replace with admin2 values
data$precip_2018_sd[is.na(data$precip_2018_sd)] <- data$precip_2018_sd_adm2[is.na(data$precip_2018_sd)]
# now save
write_csv(data, "data/features/precip_clean.csv")


# NOTE: Could do the same thing with temperatures, PDSI, etc., but for sake of parsimony, I'm not going to do that here.


## Pollution ------------------
# for pollution, we will just use means at different areas. You could imagine matching exact times to the survey data, but that's a bit more 
# complicated and I don't think it's worth it for an example script like this one.
### CO ------------------
data <- read_csv("data/features/pollution_co.csv")
# let's just take MEANS
data <- data %>%
        group_by(admin2Pcod) %>%
        mutate(co_adm2 = mean(CO_column_number_density, na.rm=T)) %>%
        group_by(id) %>%
        mutate(co = mean(CO_column_number_density, na.rm=T)) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        dplyr::select(id, co, co_adm2)
# now save
write_csv(data, "data/features/pollution_co_clean.csv")


### HCHO ------------------
data <- read_csv("data/features/pollution_hcho.csv")
# let's just take MEANS
data <- data %>%
        group_by(admin2Pcod) %>%
        mutate(hcho_adm2 = mean(HCHO_slant_column_number_density, na.rm=T)) %>%
        group_by(id) %>%
        mutate(hcho = mean(HCHO_slant_column_number_density, na.rm=T)) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        dplyr::select(id, hcho, hcho_adm2)
# now save
write_csv(data, "data/features/pollution_hcho_clean.csv")

### O3 ------------------
data <- read_csv("data/features/pollution_o3.csv")
# let's just take MEANS
data <- data %>%
        group_by(admin2Pcod) %>%
        mutate(o3_adm2 = mean(O3_column_number_density, na.rm=T)) %>%
        group_by(id) %>%
        mutate(o3 = mean(O3_column_number_density, na.rm=T)) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        dplyr::select(id, o3, o3_adm2)
# now save
write_csv(data, "data/features/pollution_o3_clean.csv")


### NO2 ------------------
data <- read_csv("data/features/pollution_no2.csv")
# let's just take MEANS
data <- data %>%
        group_by(admin2Pcod) %>%
        mutate(no2_adm2 = mean(NO2_column_number_density, na.rm=T)) %>%
        group_by(id) %>%
        mutate(no2 = mean(NO2_column_number_density, na.rm=T)) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        dplyr::select(id, no2, no2_adm2)
# now save
write_csv(data, "data/features/pollution_no2_clean.csv")

### SO2 ------------------
data <- read_csv("data/features/pollution_so2.csv")
# let's just take MEANS
data <- data %>%
        group_by(admin2Pcod) %>%
        mutate(so2_adm2 = mean(SO2_column_number_density, na.rm=T)) %>%
        group_by(id) %>%
        mutate(so2 = mean(SO2_column_number_density, na.rm=T)) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        dplyr::select(id, so2, so2_adm2)
# now save
write_csv(data, "data/features/pollution_so2_clean.csv")



## Landclass ------------------
# this is a bit more complicated because of how GEE stores the data. We will need to parse the histogram column.
data <- read_csv("data/features/lc.csv")
data$date <- as_date(data$imageDate)
data$year <- year(data$date)
# just one year here (2018)
data <- data %>%
  dplyr::select(id, admin2Pcod, histogram)
  
# Define the pattern to extract key-value pairs
pattern <- "\\{(.*?)\\}"

# Function to parse the key-value pairs
parseKeyValuePairs <- function(value) {
  # Extract the content within the curly braces
  content <- str_match(value, pattern)[, 2]
  
  # Split the content by commas
  pairs <- str_split(content, ", ")[[1]]
  
  # Split each pair into key and value
  pairs_split <- str_split(pairs, "=")
  
  # Create a named vector of key-value pairs
  parsed <- setNames(as.numeric(sapply(pairs_split, `[`, 2)), sapply(pairs_split, `[`, 1))
  
  return(parsed)
}
# Apply the parsing function to the column
parsed_column <- map(data$histogram, parseKeyValuePairs)

# this is a list. First, let's get a unique vector of all classes
classes <- c()
for (i in 1:length(parsed_column)){
  classes <- c(classes, names(parsed_column[[i]]))
}
classes <- unique(classes)
classes <- sort(classes)

# now go through parsed_column and add missing values
for (i in 1:length(parsed_column)){
  nametemp <- names(parsed_column[[i]])
  parsed_column[[i]] <- c(parsed_column[[i]], rep(0, length(classes[!(classes %in% names(parsed_column[[i]]))])))
  names(parsed_column[[i]]) <- c(nametemp, classes[!(classes %in% names(parsed_column[[i]]))])
  parsed_column[[i]] <- parsed_column[[i]][classes]  
}

# create new matrix that has rows of parsed_column length and columns based on classes
newmat <- c()
for (i in 1:length(parsed_column)){
  newmat <- rbind(newmat, parsed_column[[i]])
}
newmat <- as_tibble(newmat[,-c(1)])
# now calculate as proportion of total pixels
newmat <- newmat/apply(newmat, 1, FUN = "sum")
# col names
for (i in 1:ncol(newmat)){
  colnames(newmat)[i] <- paste0("lc_", classes[i])
}
# add to data
data <- cbind(data, newmat)
summary(data)
# some of these have VERY low mean. Drop if less than 0.5 percent on average.
data <- data %>%
  dplyr::select(c("id", "admin2Pcod", "lc_11", "lc_12", "lc_13", "lc_8"))

# also add means at higher levels
data <- data %>%
  group_by(admin2Pcod) %>%
  mutate(
         lc_11_adm2 = mean(lc_11, na.rm=T),
         lc_12_adm2 = mean(lc_12, na.rm=T),
         lc_13_adm2 = mean(lc_13, na.rm=T),
         lc_8_adm2 = mean(lc_8, na.rm=T),
         lc_11_max_adm2 = max(lc_11, na.rm=T),
         lc_12_max_adm2 = max(lc_12, na.rm=T),
         lc_13_max_adm2 = max(lc_13, na.rm=T),
         lc_8_max_adm2 = max(lc_8, na.rm=T)
         ) %>%
  ungroup()
# a few of the grid values are missing. replace with the admin2 value
data$lc_11[is.na(data$lc_11)] <- data$lc_11_adm2[is.na(data$lc_11)]
data$lc_12[is.na(data$lc_12)] <- data$lc_12_adm2[is.na(data$lc_12)]
data$lc_13[is.na(data$lc_13)] <- data$lc_13_adm2[is.na(data$lc_13)]
data$lc_8[is.na(data$lc_8)] <- data$lc_8_adm2[is.na(data$lc_8)]
write_csv(data, "data/features/landclass_clean.csv")

















