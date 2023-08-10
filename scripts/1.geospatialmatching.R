# This script creates a csv file with the household id and the admin2 code of the cluster of households
# we have to match the household coordinates to the shapefile of the admin2 clusters

library(tidyverse)  # general tools for data cleaning
library(sf)         # for shapefiles
library(raster)     # for rasters

# shapefile, admin2, benin
shape <- read_sf("shapefile/ben_admbnda_adm2_1m_salb_20190816.shp")
grid <- read_sf("shapefile/grid.shp")
# make sure int he same crs
grid <- st_transform(grid, crs = st_crs(shape))

# household basics
df <- read_csv("data/clean.csv")

# hosuehold coordinates
coords <- df %>%
          dplyr::select(hhid, lat, lon)
# as sf
coords <- st_as_sf(coords, coords = c("lon","lat"), crs = crs(grid))

# extract shapefile information to coordinates (clusters of households)
coords <- st_join(coords, grid, join = st_intersects)

# save coordinates as csv
coords <- as_tibble(coords) %>%
                dplyr::select(hhid, id)
coords <- coords %>%
            left_join(df, by = "hhid")
write_csv(coords, "data/coords.csv")





