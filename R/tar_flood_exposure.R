# dir_wp <-  file.path(
#   Sys.getenv("AA_DATA_DIR"),
#   "public",
#   "raw",
#   "som",
#   "worldpop"
# )
#
# fp_fs <-   file.path(
#   Sys.getenv("AA_DATA_DIR"),
#   "private",
#   "raw",
#   "glb",
#   "floodscan",
#   "floodscan_flooded_fraction_africa_19980112-20221231_p00",
#   "aer_sfed_area_300s_19980112_20221231_v05r01.nc"
# )
#
#
# fp_wp <-  file.path(
#   dir_wp,
#   "som_ppp_2020_1km_Aggregated_UNadj.tif"
# )
#


zonal_pop_exposure_method1 <- function(floodscan_path=fp_fs,
                                       worldpop_path=fp_wp,
                                       flood_frac_thresh=0.005,
                                       adm = gdf_adm$adm2
){
  tnc_fs <- tidync(floodscan_path)
  tnc_fs_filt <- fs_filter_bounds(fs_obj = tnc_fs,geometry = adm)
  r_fs <- fs_to_raster(fs_obj = tnc_fs_filt,band = "SFED_AREA")
  r_wp <-  rast(worldpop_path)
  fs_lookup <-  floodscan_lookup(r_fs) %>%
    filter(!str_detect(fs_seas,"other"))

  # do the same, but this time per year
  lr_seas_max <- unique(fs_lookup$fs_seas) %>%
    map(
      \(seas_tmp){
        rname_temp<- fs_lookup %>%
          filter(fs_seas==seas_tmp) %>%
          pull(fs_name)
        r_fs_tmp <- r_fs[[names(r_fs) %in% rname_temp]]
        r_max <- max(r_fs_tmp)
        r_max %>%
          set.names(rname_temp[1])
        return(r_max)
      }
    )
  r_seas_max <- rast(lr_seas_max)

  r_fs_seas_max_crop <- crop(r_seas_max, r_wp)
  ext(r_fs_seas_max_crop) <- ext(r_wp)

  cat("resampling floodscan to worldpop\n")
  r_fs_seas_resampled <- terra::resample(x = r_fs_seas_max_crop,r_wp)

  lgl_thresh_mask <- ifel(r_fs_seas_resampled>=flood_frac_thresh,1,0)
  r_fs_seas_resampled_masked <- mask(r_fs_seas_resampled,lgl_thresh_mask)
  r_exposure <- r_fs_seas_resampled_masked * r_wp

  exact_extract(x = r_exposure,
                y = adm,
                fun ="sum",
                append_cols= c("adm1_en","adm1_pcode","adm2_en","adm2_pcode"),
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


zonal_pop_exposure_method2 <- function(floodscan_path=fp_fs,
                               worldpop_path=fp_wp,
                               flood_frac_thresh=0.2,
                               adm = gdf_adm$adm2
){
  tnc_fs <- tidync(floodscan_path)
  tnc_fs_filt <- fs_filter_bounds(fs_obj = tnc_fs,geometry = adm)
  r_fs <- fs_to_raster(fs_obj = tnc_fs_filt,band = "SFED_AREA")
  r_wp <-  rast(worldpop_path)
  fs_lookup <-  floodscan_lookup(r_fs) %>%
    filter(!str_detect(fs_seas,"other"))

  # do the same, but this time per year
  lr_seas_max <- unique(fs_lookup$fs_seas) %>%
    map(
      \(seas_tmp){
        rname_temp<- fs_lookup %>%
          filter(fs_seas==seas_tmp) %>%
          pull(fs_name)
        r_fs_tmp <- r_fs[[names(r_fs) %in% rname_temp]]
        r_max <- max(r_fs_tmp)
        r_max %>%
          set.names(rname_temp[1])
        return(r_max)
      }
    )
  r_seas_max <- rast(lr_seas_max)

  r_fs_seas_max_crop <- crop(r_seas_max, r_wp)
  ext(r_fs_seas_max_crop) <- ext(r_wp)

  cat("resampling floodscan to worldpop\n")
  r_fs_seas_resampled <- terra::resample(x = r_fs_seas_max_crop,r_wp)

  if(length(flood_frac_thresh)==1){
    lgl_thresh_mask <- ifel(r_fs_seas_resampled>=flood_frac_thresh,1,0)
    r_exposure <- lgl_thresh_mask * r_wp

    exact_extract(x = r_exposure,
                  y = adm,
                  fun ="sum",
                  append_cols= c("adm1_en","adm1_pcode","adm2_en","adm2_pcode"),
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
  if(length(flood_frac_thresh)>1){

    flood_frac_thresh %>%
      map( \(thresh_tmp){
        cat(thresh_tmp,"\n")
        r_fs_seas_resampled_copy <- deepcopy(r_fs_seas_resampled)
        lgl_thresh_mask <- ifel(r_fs_seas_resampled_copy>=thresh_tmp,1,0)
        r_exposure <- lgl_thresh_mask * r_wp

        exact_extract(x = r_exposure,
                      y = adm,
                      fun ="sum",
                      append_cols= c("adm1_en","adm1_pcode","adm2_en","adm2_pcode"),
                      force_df = TRUE
        ) %>%
          pivot_longer(cols = starts_with("sum"),
                       names_to = "sum_date",
                       values_to = "pop_exposed") %>%
          separate(col = sum_date, "\\.",into = c("stat","date")) %>%
          mutate(
            date= as_date(date),
            thresh=thresh_tmp
          )
      }
      ) %>%
      list_rbind()
  }
}

floodscan_pixel_values <-  function(floodscan_path,mask){

  tnc_fs <- tidync(floodscan_path)
  tnc_fs_filt <- fs_filter_bounds(fs_obj = tnc_fs,geometry = mask)
  r_fs <- fs_to_raster(fs_obj = tnc_fs_filt,band = "SFED_AREA")

  fs_lookup <-  floodscan_lookup(r_fs) %>%
    filter(!str_detect(fs_seas,"other"))

  # do the same, but this time per year
  lr_seas_max <- unique(fs_lookup$fs_seas) %>%
    map(
      \(seas_tmp){
        rname_temp<- fs_lookup %>%
          filter(fs_seas==seas_tmp) %>%
          pull(fs_name)
        r_fs_tmp <- r_fs[[names(r_fs) %in% rname_temp]]
        r_max <- max(r_fs_tmp)
        r_max %>%
          set.names(rname_temp[1])
        return(r_max)
      }
    )
  r_seas_max <- rast(lr_seas_max)
  r_seas_max_clipped <- mask(r_seas_max,mask)

  r_seas_max_clipped %>%
    values() %>%
    data.frame() %>%
    pivot_longer(everything())


}




floodscan_lookup <-  function(r_fs){
  # make a lookup table to be used for raster manipulations
  fs_mos<- floor_date(as_date(names(r_fs)),"month")

  fs_lookup <- tibble(
    fs_name = as_date(names(r_fs))
  ) %>%
    mutate(
      fs_mo = floor_date(fs_name,"month"),
      fs_yr = floor_date(fs_name,"year"),
      fs_seas = paste0(case_when(
        month(fs_mo) %in% c(3,4,5) ~ "MAM",
        month(fs_mo) %in% c(10,11,12) ~ "OND",
        .default ="other"

      ),"_",year(fs_yr))
    )
  return(fs_lookup)

}






