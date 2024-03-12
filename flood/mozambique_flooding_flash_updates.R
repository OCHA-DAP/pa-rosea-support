library(googledrive )
library(here)
library(terra)
library(sf)
library(tidyverse)
library(janitor)
library(tidyterra)
library(gghdx)
gghdx()
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

library(targets)
tar_load(lgdf_adm,store="flood_target_store")

lgdf_moz <- lgdf_adm %>% 
  map(
    ~.x %>% 
      filter(adm0_en =="Mozambique")
  )

lr_bounded <- lr %>% 
  map(
    \(rtmp){
      r_tmp_cropped<- crop(rtmp, lgdf_moz$adm0)
      r_tmp_cropped[r_tmp_cropped<0.02]<-NA
      return(r_tmp_cropped)
    }
  )




low_fill = "#E0F3F8"
med_fill ="#74ADD1" 
high_fill= "#4575B4"

lgdf_moz$adm0 %>% object.size()
adm0_simp<- st_simplify(lgdf_moz$adm0,4000)
adm1_simp<- st_simplify(lgdf_moz$adm1,4000)
adm0_simp %>% 
  object.size()
adm1_simp %>% 
  object.size()

ggplot()+
  geom_sf(data= adm0_simp,fill="white", linewidth=0.8)+
  geom_sf(data= adm1_simp,fill=NA,linewidth=0.5,color="grey")+
  geom_spatraster(data = lr_bounded$sfed)+
  scale_fill_steps2(
    breaks = seq(0,1,by=0.2),
    low = low_fill,
    mid = med_fill,
    high = high_fill,
    midpoint = 0.5,
    space = "Lab",
    na.value = NA,
    guide = "coloursteps",
    aesthetics = "fill",
    name= "Flooded\nFraction",labels=scales::label_percent()
  )+
  facet_wrap(~lyr)+
  
  labs(title = "Mozambique",
       subtitle = "East Africa: Flood Susceptibility")+
  theme(
    legend.text = element_text(angle=90)
  )
# ggsave("map_moz_SFED_flood_frac_last_10_days.png",height = 15)




