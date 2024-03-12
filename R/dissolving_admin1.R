# This script reads in the shapefile for East and Southern Africa and
# dissolves the admin 1s to admin 0s

## reading in libraries
library(tidyverse)
library(sf)
library(raster)
library(sp)
sf_use_s2(FALSE)
# path to admin 1 shp
shp_path <- file.path(
  Sys.getenv("AA_DATA_DIR"), 
  "public", "raw", "esa", "cod_ab", 
  "esa_admin_region", "ESA_admin1_region.shp")
esa_shp <- st_read(shp_path)
# dissolving to country level
sa_country_shp <- esa_shp %>%
  group_by(COUNTRY) %>% 
  summarize(geometry = st_union(geometry))
plot(sa_country_shp$geometry)
# writing out admin 0
st_write(sa_country_shp, file.path(Sys.getenv("AA_DATA_DIR"), 
  "public", "raw", "esa", "cod_ab", 
  "esa_admin_region", "ESA_admin0_region.shp"))
