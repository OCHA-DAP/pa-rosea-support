# script for reading in waterways and creating geopackages for major rivers

# libraries
library(tidyverse)
library(sf)
library(zip)

# file path
waterways_path <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public", "raw", "moz", "waterways")

file_path <- file.path(waterways_path, "hotosm_moz_waterways_lines_geojson.zip")

# download file
download.file("https://s3.us-east-1.amazonaws.com/production-raw-data-api/ISO3/MOZ/waterways/lines/hotosm_moz_waterways_lines_geojson.zip",
              file_path)

# reading in file
unzip(file_path, 
      exdir = file.path(waterways_path, "hotosm_moz_waterways_lines_geojson"))
moz_waterways <- st_read(file.path(waterways_path, 
                                   "hotosm_moz_waterways_lines_geojson", 
                                   "hotosm_moz_waterways_lines_geojson.geojson"))
# filtering for rivers
moz_rivers <- moz_waterways %>%
  filter(waterway == "river")
# filtering for rivers with names
moz_named_rivers <- moz_waterways %>%
  filter(waterway == "river" & (!is.na(name)))
# writing to geopackage
st_write(moz_rivers, 
         dsn = file.path(waterways_path, "hotosm_moz_rivers_lines_geojson.gpkg"), 
         layer = "hotosm_moz_rivers_lines_geojson")

st_write(moz_rivers, 
         dsn = file.path(waterways_path, "hotosm_moz_named_rivers_lines_geojson.gpkg"), 
         layer = "hotosm_moz_named_rivers_lines_geojson")
