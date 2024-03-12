library(googledrive )
library(here)
library(terra)
library(sf)
library(tidyverse)
library(janitor)
library(tidyterra)
library(gghdx)
library(glue)
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
df_bbox <-  st_bbox(lgdf_moz$adm0)
lr_bounded <- lr %>% 
  map(
    \(rtmp){
      r_tmp_cropped<- crop(rtmp, lgdf_moz$adm0)
      r_tmp_cropped[r_tmp_cropped<0.02]<-NA
      return(r_tmp_cropped)
    }
  )



surrounding_countries<- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "esa",
  "cod_ab",
  "esa_admin_region"
)
gdf_esa<- st_read(
  "esa_admin_region",
  "ESA_admin0_region"
) %>% 
  clean_names()

gdf_surround <- gdf_esa %>% 
  filter(
    country %in% c("Zimbabwe","Zambia","Malawi","Tanzania","South Africa")
  ) %>% 
  st_make_valid() %>% 
  st_simplify(
    dTolerance = 50000
  )


low_fill = "#E0F3F8"
med_fill ="#74ADD1" 
high_fill= "#4575B4"

lgdf_moz$adm0 %>% object.size()
adm0_simp<- st_simplify(lgdf_moz$adm0,20000)
adm1_simp<- st_simplify(lgdf_moz$adm1,20000)
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
  geom_sf(
    data= gdf_surround, 
    fill="grey",alpha=0.8, color = "white"  )+
  coord_sf(xlim=df_bbox[c(1,3)],ylim=df_bbox[c(2,4)])+
  labs(title = "Mozambique Daily Flood Fraction (SFED)",
       subtitle = "Last 10 days")+
  theme(
    legend.text = element_text(angle=90)
  )
# ggsave("map_moz_SFED_flood_frac_last_10_days.png",height = 15)


countries_dl_wp <- c(Mozambique="MOZ")
r_wp <- map(countries_dl_wp,\(country_code){
  cat("downloading ",country_code,"\n")
  country_code_lower <- str_to_lower(country_code)
  url <- glue("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/{country_code}/{country_code_lower}_ppp_2020_1km_Aggregated_UNadj.tif")
  terra::rast(url)}
)

library(exactextractr)

# Expsure Calcs ####
# remember this is masked already . .... remember to think about

#flood frac thresh
threshold = 0.2
lr_exposure <- lr_bounded %>% 
  map(
    \(rtmp){
      cat("cropping\n")
      r_fs_max_crop <- crop(rtmp, r_wp$Mozambique)
      ext(r_fs_max_crop) <- ext( r_wp$Mozambique)
      
      cat("resampling floodscan to worldpop\n")
      r_fs_resampled <- terra::resample(x = r_fs_max_crop,r_wp$Mozambique)
      cat("binarizing floodscan\n")
      r_fs_resampled[r_fs_resampled<threshold,] <- NA
      cat("multiplying floodscan by worldpop for pop exposure\n")
      r_exposure <- r_fs_resampled*r_wp$Mozambique
      
      return(r_exposure)
      
    }
  )

# Zonal Stats ###
# exact_extract(lr_exposure$sfed,y = lgdf_moz$adm1)
df_adm2_exposure <- lr_exposure %>% 
  map(
    \(rtmp){
      exact_extract(x = rtmp,
                           y = lgdf_moz$adm2,
                           fun ="sum",
                           append_cols= c("adm0_en","adm1_pcode","adm1_en","adm2_en","adm2_pcode"),
                           force_df = TRUE
      ) %>%
        pivot_longer(cols = starts_with("sum"),
                     names_to = "sum_date",
                     values_to = "pop_exposed") %>%
        separate(col = sum_date, "\\.",into = c("stat","date")) %>%
        mutate(
          date= as_date(date)
        )
      
    }
  )


# select only adm2s that had some flood exposure for plotting
df_adm2_exposed_processed <- df_adm2_exposure$sfed %>% 
  group_by(
    across(matches("adm\\d"))
  ) %>% 
  filter(
    any(pop_exposed!=0)
  ) %>% 
  mutate(
    # was thinking could use this if we were going to put all 
    # on 1 plot to classify admins with greatest increase/decrease
    # NOT used yet
    diff = max(pop_exposed)- min(pop_exposed)
  ) %>% 
  ungroup()


# annoyingly there will be too many admins for having 1 color per adm2
# can't make a legend with 30 colors

# therefore we split the plots by admin1 give each plot it's own legend
adm1_w_exposure <- df_adm2_exposed_processed$adm1_en %>% 
  unique()

# requires patchwork and some fangling like this to only put dates
# on bottom plot
bottom_plot <- adm1_w_exposure[length(adm1_w_exposure)]

l_ps <-adm1_w_exposure %>% 
  map(
    \(adm1_tmp){
     p <-  df_adm2_exposed_processed %>% 
  filter(
    adm1_en == adm1_tmp
  ) %>%
        ggplot(
        aes(x= date,
            y= pop_exposed, 
            group = adm2_en,
            color= adm2_en)
      )+
        geom_point()+
        geom_line()+
        scale_color_brewer(type = "qual")+
        labs(
          subtitle = adm1_tmp
        )+
        scale_x_date(
          date_breaks = "day",date_labels = "%d-%b"
        )+
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          legend.text = element_text(size=8),
          legend.spacing =unit(0, 'cm'),
          legend.key.height = unit(0.2,'cm'),
          legend.position = "right"
        )
     if(adm1_tmp==bottom_plot){
       p <- p+theme(
         axis.text.x = element_text(angle=90),
       )
     }
     return(p)
    }
  
  )

patchwork::wrap_plots(l_ps,ncol = 1)+
  patchwork::plot_annotation(
    title= "Mozambique: population potentially exposed to flooding",
    subtitle = "FloodScan SFED + WorldPop")



