library(tidyverse)
library(sf)
library(mapview)

# ============================================
# set working folder
# setwd("YOUR/PATH/HERE")
# ============================================

# gis data sets
gpkg_file = "gis_datasets.gpkg"
csv_file = "aerodromes.csv"

# list gpkg layers
st_layers(gpkg_file)

# read GeoPackage layers
island = st_read(dsn = gpkg_file, layer = "vancouver_island")
places = st_read(dsn = gpkg_file, layer = "places")
watercourses = st_read(dsn = gpkg_file, layer = "watercourses")
samples = st_read(dsn = gpkg_file, layer = "samples")

