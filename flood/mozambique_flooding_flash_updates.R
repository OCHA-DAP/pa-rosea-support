library(googledrive )
library(here)
library(terra)
library(sf)
library(tidyverse)
library(janitor)
source(
  file.path("R","googledrive.R")
)

drive_auth(
  path=Sys.getenv("FS_SA_JSON")
)

drive_dribble <- drive_ls(
  corpus = "user"
)

dl_log <- load_drive_file(drive_dribble,"FloodScan_zip_DL_log.csv")

df_latest_files_meta <- dl_log %>% 
  filter(download_date==max(download_date))


lr <- set_names(df_latest_files_meta$file_name,str_extract(df_latest_files_meta$file_name,"sfed|mfed")) %>% 
  map(
    \(tmp_file_name){
      cat("downloading zips\n")
      tmp_fp <- download_drive_zips(dribble = drive_dribble,tmp_file_name,overwrite=T) 
      cat("zip saved as ", tmp_fp,"\n")
      
      tmp_zip_contents <- unzip(zipfile = tmp_fp,list = T) %>% 
        tibble()
      
      cat("making meta data catalogue\n")
      latest_10_days_meta <- tmp_zip_contents %>% 
        clean_names() %>% 
        filter(
          str_detect(name, ".tif$")
        ) %>% 
        mutate(
          date = as_date(stringr::str_extract(name , "\\d{8}"),"%Y%M%D")
        ) %>% 
        slice_max(order_by = date, n = 10)
      
    cat("unzipping last 10\n")
    unzip(zipfile = tmp_fp,
          files = latest_10_days_meta$name,
          exdir = dirtmp <- tempdir(),
    )
    list.files(dirtmp)
    
    fps_tifs_last_10 <- list.files(
      file.path(
        dirtmp,
        "aer_floodscan_sfed_area_flooded_fraction_africa_90days"
      ),
      full.names = T
    )
    cat("reading raster\n")
      r <- rast(fps_tifs_last_10)
      lyr_names <- basename(fps_tifs_last_10)
      lyr_date_names <- as_date(stringr::str_extract(lyr_names , "\\d{8}"),"%Y%M%D")
      set.names(r, lyr_date_names)
      return(r)
      }
  )
