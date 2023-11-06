# saereplication

This repository will allow you to fully reproduce an example using geospatial data with small area estimation. As part of this repository, there is a single outcome for Benin. We use a single indicator for "food insecurity", derived from the 2018 Enquête Harmonisée sur le Conditions de Vie des Ménages 2018-2019 (https://microdata.worldbank.org/index.php/catalog/4291).

We will be estimating a geospatial SAE model to estimate food insecurity in Benin at the admin2 (commune) level. However, the estimation will take place using GRIDS that cover the entire country. This shapefile consists of 3km-by-3km grids. We have already provided the admin2 identifier in the grid shapefile to make things a little easier.

# scripts

In the scripts folder are four scripts plus an additional folder, called "gee". The gee folder includes 9 scripts to pull data from Google Earth Engine. You MUST have the files in order to be able to run the rest of the scripts. The reason for this is that the files produced by Google Earth Engine are quite large (several GB) which makes hosting them here difficult. If you would like to pull the data from Google Earth Engine yourself, you must first do three things:
1. Create an account on https://earthengine.google.com/
2. Create a Google Drive account with the SAME id as 1. and create a folder within it called "unsae"
3. Upload the grid shapefile to Google Earth Engine with the name "grid"

As an alternative to pulling the data from Google Earth Engine, you can download them all directly from here: https://www.dropbox.com/s/v9ftyr4ziynhgew/downloaddata.zip?dl=0. After pulling the data from Google Earth Engine OR downloading them from the link, please place the files in the "features" folder. 

Make sure you have this entire repository downloaded and, importantly, **set the working directory in R to the location of the saereplication folder**. Then, run the following scripts, in order:

## 1.geospatialmatching

This script shows you how to match the GPS coordinates from the household survey to the shapefile and to pull the shapefile identifiers.

## 2.geospatialpull

This script does two things. First, it extracts non-Google Earth Engine data from the data files provided within this repository. These include the acled.csv, cities.csv, and worldpop.tif. The first two are point data, with GPS coordinates. This script will show you how to match those to the shapefile. The third is a raster file. This script also shows you how to extract data from the tif. Second, this cleans the already-extracted Google Earth Engine data. In each step, the script will save a .csv file for the cleaned data into the "features" folder, and the clean data has is always named *_clean.csv.

## 3.collectingfeatures

This script takes all of the clean data from the previous script and merges them all together into a single, clean file, called grid_clean.csv. This is the file we will use to estimate the model.

## 4.estimation

This script estimates a geospatial SAE model using the povmap package and EBP. It consists of the following steps:
- Some data transformation of distance variables (taking logs).
- Combining the clean geospatial data and the household data, collapsing the data to the GRID level for estimation.
- Estimating lasso in order to choose the best set of covariates.
  - We use cross validation to choose features. The details are documented in the script.
- Does some preliminary investigations to show some things that can go wrong. FOr example, in this script, we show that lasso ends up selecting some covariates that are perfectly collinear. We show you how to identify this and drop these in order to prevent errors when estimating the SAE model.
  - We also look at some diagnostics in order to ascertain whether the assumption of normality of the errors is reasonable. We decide on a transformation of the outcome variable here.
- We then estimate the SAE (EBP) model. We show that it improves on the direct survey estimates, at least in terms of precision. Since we do not know the "truth," we are not able to say anything about accuracy. 


# Other folders

## data

The data folder contains all of the data you need to run the scripts. Please take note of the warning above regarding the Google Earth Engine data. If you only want to do 4.estimation, then you can just use the repository as is.

## shapefile

This folder contains three shapefiles that you will use at one point during the process. The ben_admbnda_adm2_1m_salb_20190816.shp shapefile contains the commune boundaries for Benin. The grid.shp shapefile contains the grids that will form the basis of the estimation process. We pull some data at the commune level and some data at the grid level, depending on how sparse the data is. For example, the vast majority of grid cells do not contain rivers, so we extract river length to the commune level instead of the grid level.











