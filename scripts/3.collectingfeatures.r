# This script will collect all of the grid-level features together

library(tidyverse)
library(sf)

# load the grid
grid <- read_sf("shapefile/grid.shp")

# just keep ids
grid <- as_tibble(grid) %>%
        dplyr::select(id, admin2Pcod)

# add features ----------------------

## ACLED ----------------------
data <- read_csv("data/features/acled_clean.csv")
grid <- grid %>%
        left_join(data, by = c("admin2Pcod"))
# missings mean zero
grid$fatalities[is.na(grid$fatalities)] <- 0
grid$events[is.na(grid$events)] <- 0

## Buildings ----------------------
data <- read_csv("data/features/buildings_clean.csv")
grid <- grid %>%
        left_join(data, by = c("id"))
# missings mean zero
grid$total_area[is.na(grid$total_area)] <- 0
grid$total_buildings[is.na(grid$total_buildings)] <- 0
grid$total_area_adm2[is.na(grid$total_area_adm2)] <- 0
grid$total_buildings_adm2[is.na(grid$total_buildings_adm2)] <- 0

## City distances ----------------------
data <- read_csv("data/features/distancecities_clean.csv")
grid <- grid %>%
        left_join(data, by = c("id"))

## Landclass ----------------------
data <- read_csv("data/features/landclass_clean.csv")
grid <- grid %>%
        left_join(data %>% dplyr::select(-c("admin2Pcod")), by = c("id"))

## NDVI ----------------------
data <- read_csv("data/features/ndvi_clean.csv")
grid <- grid %>%
        left_join(data %>% dplyr::select(-c("admin2Pcod")), by = c("id"))

## Nightlights ----------------------
data <- read_csv("data/features/ntl_clean.csv")
grid <- grid %>%
        left_join(data %>% dplyr::select(-c("admin2Pcod")), by = c("id"))

## Pollution CO ----------------------
data <- read_csv("data/features/pollution_co_clean.csv")
grid <- grid %>%
        left_join(data, by = c("id"))

## Pollution HCHO ----------------------
data <- read_csv("data/features/pollution_hcho_clean.csv")
grid <- grid %>%
        left_join(data, by = c("id"))

## Pollution NO2 ----------------------
data <- read_csv("data/features/pollution_no2_clean.csv")
grid <- grid %>%
        left_join(data, by = c("id"))

## Pollution O3 ----------------------
data <- read_csv("data/features/pollution_o3_clean.csv")
grid <- grid %>%
        left_join(data, by = c("id"))

## Pollution SO2 ----------------------
data <- read_csv("data/features/pollution_so2_clean.csv")
grid <- grid %>%
        left_join(data, by = c("id"))

## Population ----------------------
data <- read_csv("data/features/population_clean.csv")
grid <- grid %>%
        left_join(data %>% dplyr::select(-c("admin2Pcod")), by = c("id"))

## Precip ----------------------
data <- read_csv("data/features/precip_clean.csv")
grid <- grid %>%
        left_join(data %>% dplyr::select(-c("admin2Pcod")), by = c("id"))

## Rivers ----------------------
data <- read_csv("data/features/rivers_clean.csv")
grid <- grid %>%
        left_join(data, by = c("admin2Pcod"))

## Roads ----------------------
data <- read_csv("data/features/roads_clean.csv")
grid <- grid %>%
        left_join(data, by = c("admin2Pcod"))

write_csv(grid, "data/grid_clean.csv")

