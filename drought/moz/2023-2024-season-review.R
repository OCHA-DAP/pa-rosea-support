## libraries
library(tidyverse)
library(sf)

# rainfall

# reading in data
moz_rain_data <- read_csv("https://data.humdata.org/dataset/641a9118-4488-49e7-a0c6-efe9dc567987/resource/257d0484-49a8-4336-b87a-c4991decf42a/download/moz-rainfall-adm2-full.csv")
moz_ndvi_data <- read_csv("https://data.humdata.org/dataset/841f6fa9-7ba3-4e79-a335-db9e2608f8c3/resource/6e130522-c378-43a0-a843-be9256ec6d41/download/moz-ndvi-adm2-full.csv")
moz_shp <- st_read(file.path(
  Sys.getenv("AA_DATA_DIR"), 
  "public", "raw", 
  "moz", "cod_ab", 
  "moz_adm.shp.zip"), layer = "moz_admbnda_adm1_ine_20190607")
## aggregating the data to admin 1


## NDVI