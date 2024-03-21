## libraries
library(tidyverse)
library(sf)
library(gghdx)
gghdx()

# rainfall
moz_ssn <- c("October", "November", "December", "January", "February", "March")
# reading in data
moz_rain_data <- read_csv("https://data.humdata.org/dataset/641a9118-4488-49e7-a0c6-efe9dc567987/resource/257d0484-49a8-4336-b87a-c4991decf42a/download/moz-rainfall-adm2-full.csv")[-1,]
moz_ndvi_data <- read_csv("https://data.humdata.org/dataset/841f6fa9-7ba3-4e79-a335-db9e2608f8c3/resource/6e130522-c378-43a0-a843-be9256ec6d41/download/moz-ndvi-adm2-full.csv")[-1,]
moz_shp <- st_read(file.path(
  Sys.getenv("AA_DATA_DIR"), 
  "public", "raw", 
  "moz", "cod_ab", 
  "moz_adm.shp.zip"), layer = "moz_admbnda_adm2_ine_20190607")
## aggregating the data to admin 1
moz_rain_adm1 <- moz_rain_data %>%
  left_join((moz_shp %>%
               select(ADM2_PCODE, ADM1_PCODE, ADM1_PT)), 
            by = "ADM2_PCODE", suffix = c("", "_shp")) %>%
  mutate(month_total = as.numeric(r1h)) %>%
  group_by(ADM1_PCODE, ADM1_PT, date) %>%
  summarise(adm1_med = median(month_total, na.rm = T)) %>%
  mutate(dekad_start = substr(date, 9, 10),
         month = substr(date, 6, 7),
         month_name = month.name[as.numeric(month)],
         yr = as.numeric(substr(date, 1, 4)),
         season = case_when(
           month %in% c("10", "11", "12") ~ paste0(yr, "/", yr+1),
           month %in% c("01", "02", "03") ~ paste0(yr-1, "/", yr))) %>%
  filter(dekad_start == "21" & 
           month_name %in% moz_ssn) 

moz_rain_adm1 %>%
  mutate(month_name = as.factor(month_name, levels = moz_ssn))

ggplot(data = moz_rain_adm1) +
  geom_line(aes(x=month_name, y=adm1_med, group=season))
## NDVI

