# libraries
library(tidyverse)
library(lubridate)
library(sf)
library(raster)
library(rvest)
library(httr)
library(gganimate)
library(gghdx)
gghdx()

data_dir <- file.path(Sys.getenv("AA_DATA_DIR"), 
                      "public", "raw", "moz", "gpm")
moz_shp <- st_read(file.path(
  Sys.getenv("AA_DATA_DIR"), 
  "public", "raw", 
  "moz", "cod_ab", 
  "moz_adm.shp.zip"), layer = "moz_admbnda_adm2_ine_20190607")
moz_shp_adm1 <- st_read(file.path(
  Sys.getenv("AA_DATA_DIR"), 
  "public", "raw", 
  "moz", "cod_ab", 
  "moz_adm.shp.zip"), layer = "moz_admbnda_adm1_ine_20190607")

moz_maputo_shp <- moz_shp %>%
  filter(ADM1_PT %in% c("Maputo", "Maputo City"))
username <- Sys.getenv("GPM_USERNAME")

dates <- c("20240322", "20240323", "20240324", "20240325")
time_steps <- c("-S000000", "-S120000")
date_time_step <- outer(dates, time_steps, FUN = paste, sep = "")
date_time_step <- c(t(date_time_step))
gpm_url <- "https://jsimpsonhttps.pps.eosdis.nasa.gov/imerg/gis/2024/03/"
response <- GET(gpm_url, authenticate(username, username))
page <- read_html(content(response, "text"))
links <- page %>% 
  html_nodes("a") %>% 
  html_attr("href")
patterns <- paste0(dates, ".*\\min.tif")
matching_links <- c(links[grep(patterns[1], links)], 
                    links[grep(patterns[2], links)],
                    links[grep(patterns[3], links)],
                    links[grep(patterns[4], links)])
file_paths <- paste0(gpm_url, matching_links)

gpm_download <- function(file_url, username, download_dir) {
  response <- GET(file_url, authenticate(username, username))
  filename <- basename(file_url)
  file_path <- file.path(download_dir, filename)
  writeBin(content(response, "raw"), file_path)
  }
map(file_paths, gpm_download, username = username, download_dir = data_dir)

# reading in files
tif_files <- list.files(data_dir, full.names = TRUE, pattern = "min.tif")
raster_list <- list()
file_end <- c(24*c(1, 2, 3, 4, 5), 138)
file_start <- (file_end - 23)
file_start[6] <- 121
for(i in 1:length(date_time_step[1:6])){
  for(j in (file_start[i]):(file_end[i])){
    raster_list[[date_time_step[i]]][[j]] <- raster(tif_files[j])
    #print(paste(i, j))
  }
}
clean_raster_list <- map(raster_list, discard, is.null)

aggregate_rasters <- function(rasters) {
  # Stack the raster objects within the inner list
  raster_stack <- stack(rasters)
  # Calculate mean across layers of the raster stack
  mean_raster <- calc(raster_stack, sum, na.rm = T)
  return(mean_raster)
}
aggregated_list <- map(clean_raster_list, aggregate_rasters)
zonal_stats <- map_df(aggregated_list, raster::extract, moz_maputo_shp, fun = mean, na.rm = T)
zonal_stats <- zonal_stats / 10
shp_data <- cbind(moz_maputo_shp, zonal_stats)

plot_fxn <- function(data, fill_col, title){
  ggplot(data = data) +
    geom_sf(aes_string(fill = fill_col)) +
    geom_sf_text(aes(label = ADM2_PT), 
                 color = "black", size = 2.7, check_overlap = T) +
    scale_fill_gradient(low="#ffffff", high="steelblue") +
    labs(x="", y="", fill = "Precipitation Accumulation", 
         title = title,
         subtitle = "Precipitation Accumulation over 9 hours")
  
}
plots_all <- list(
plot_fxn(data = shp_data,
         fill_col = "X20240322.S000000",
         title = "Rainfall as at 20240322 00:00:00 to 12:00:00"),
plot_fxn(data = shp_data,
         fill_col = "X20240322.S120000",
         title = "Rainfall as at 20240322 12:00:00 to 24:00:00"),
plot_fxn(data = shp_data,
         fill_col = "X20240323.S000000",
         title = "Rainfall as at 20240323 00:00:00 to 12:00:00"),
plot_fxn(data = shp_data,
         fill_col = "X20240323.S120000",
         title = "Rainfall as at 20240323 12:00:00 to 24:00:00"),
plot_fxn(data = shp_data,
         fill_col = "X20240324.S000000",
         title = "Rainfall as at 20240324 00:00:00 to 12:00:00"),
plot_fxn(data = shp_data,
         fill_col = "X20240324.S120000",
         title = "Rainfall as at 20240324 12:00:00 to 21:00:00")
)
plots_all
shp_data %>%
  pivot_longer(cols = starts_with("X2024"),
               names_to = "date_time",
               values_to = "mean") %>%
  mutate(date_time = ymd_hms(paste(substr(date_time, 2, 9), 
                                   substr(date_time, 12, 17))),
         mean = mean / 10) %>%
  ggplot() +
  geom_line(aes(x = date_time, y = mean)) +
  facet_wrap(vars(ADM2_PT)) + 
  labs(title = "Rainfall Intensity from Mar 22nd", 
       x = "Date", 
       y = "Average Rainfall Intensity")
  
raster_list <- list()
for(i in 1:length(tif_files)){
    raster_list[[paste(substr(basename(tif_files[i]), 24, 31), 
                       substr(basename(tif_files[i]), 34, 39))]] <- 
      raster(tif_files[i])
}
zonal_statistics <- map_df(raster_list, raster::extract, moz_maputo_shp, fun = mean, na.rm = T)
shp_data <- cbind(moz_maputo_shp, zonal_statistics)
shp_data %>%
  pivot_longer(cols = starts_with("X2024"),
               names_to = "date_time",
               values_to = "mean") %>%
  mutate(date_time = ymd_hms(paste(substr(date_time, 2, 9), 
                                   substr(date_time, 11, 16))),
         mean = mean / 10) %>%
  ggplot() +
  geom_line(aes(x = date_time, y = mean)) +
  facet_wrap(vars(ADM2_PT)) + 
  labs(title = "Precipitation from Mar 22nd", 
       x = "Date", 
       y = "Precipitation Accumulation (mm)")
