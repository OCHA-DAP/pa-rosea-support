

#' zonal_pop_exposure
#' @description calculates zonal stats of population exposed to flooding for each admin zone for each year of FloodScan data by combining FloodSCan and WorldPop data.
#' @param floodscan_path path to flood scan raster/nc file
#' @param worldpop_paths list of paths to all world pop rasters under AOI
#' @param flood_frac_thresh `numeric` threshold for flood fraction. 
#' @param binarize_floodscan `logical` if TRUE, floodscan is binarized based on `flood_frac_thresh`
#'   if TRUE values ≥ flood_frac_thresh are set to 1, values < flood_frac_thres set to 0 (prior to multiplying by worldpop rasters)
#'   if FALSE. any value ≥ flood_frac_threshold is multiplied by worldpop raster using original flood fraction value
#' @param adm `sf` class region/admin zone/polygon
#' @param cols_keep `character` vector of column names to keep from `adm` object
#' @return `data.frame` with zonal stats of population exposed to flooding for each admin zone for each year of FloodScan data.

zonal_pop_exposure <- function(floodscan_path=fp_fs,
                               worldpop_inputs =fp_wp,
                               flood_frac_thresh=0.005,
                               binarize_floodscan = T,
                               adm = lgdf_adm$adm0,
                               cols_keep = c("adm1_en", "adm1_pcode","adm0_en")
){
  tnc_fs <- tidync(floodscan_path)
  tnc_fs_filt <- fs_filter_bounds(fs_obj = tnc_fs,geometry = adm)
  r_fs <- fs_to_raster(fs_obj = tnc_fs_filt,band = "SFED_AREA")
  r_wp <-  mosaic_worldpop(worldpop_inputs,from_path=F)
  fs_lookup <-  floodscan_lookup(r_fs) 

  # do the same, but this time per year
  lr_max <- unique(fs_lookup$yr_int) %>%
    map(
      \(yr_tmp){
        rname_temp<- fs_lookup %>%
          filter(yr_int==yr_tmp) %>%
          pull(fs_name)
        r_fs_tmp <- r_fs[[names(r_fs) %in% rname_temp]]
        r_max <- max(r_fs_tmp)
        r_max %>%
          set.names(rname_temp[1])
        return(r_max)
      }
    )
  r_max <- rast(lr_max)

  r_fs_max_crop <- crop(r_max, r_wp)
  ext(r_fs_max_crop) <- ext(r_wp)

  cat("resampling floodscan to worldpop\n")
  r_fs_resampled <- terra::resample(x = r_fs_max_crop,r_wp)
  
 if(binarize_floodscan){
   lgl_thresh_mask <- ifel(r_fs_resampled>=flood_frac_thresh,1,0) 
   r_fs_resampled_masked <- mask(r_fs_resampled,lgl_thresh_mask)
   r_exposure <- r_fs_resampled_masked * r_wp
   
   ret <- exact_extract(x = r_exposure,
                 y = adm,
                 fun ="sum",
                 append_cols= cols_keep,
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
  if(!binarize_floodscan){
    if(length(flood_frac_thresh)>1){
      
      ret <- flood_frac_thresh %>%
        map( \(thresh_tmp){
          cat(thresh_tmp,"\n")
          r_fs_resampled_copy <- deepcopy(r_fs_resampled)
          lgl_thresh_mask <- ifel(r_fs_resampled_copy>=thresh_tmp,1,0)
          r_exposure <- lgl_thresh_mask * r_wp
          
          exact_extract(x = r_exposure,
                        y = adm,
                        fun ="sum",
                        append_cols= cols_keep,
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
  return(ret)

}



#' floodscan_lookup
#' @description
#' helper function used within `zonal_pop_exposure()` to help organize, group, and aggregate FloodScan raster
#' 
#' @param r_fs `spatRaster` object returned from fs_to_raster() (floodScan to raster)
#'
#' @return `tibble` lookup table with dates and months of floodscan data to be used to group and summarise FloodScan raster

floodscan_lookup <-  function(r_fs){
  # make a lookup table to be used for raster manipulations
  fs_mos<- floor_date(as_date(names(r_fs)),"month")

  fs_lookup <- tibble(
    fs_name = as_date(names(r_fs))
  ) %>%
    mutate(
      fs_mo = floor_date(fs_name,"month"),
      fs_yr = floor_date(fs_name,"year"),
      yr_int= year(fs_yr),
      # this is left over from SOM analysis... it's not used here... but we could add 
      # seasons at some point if we want
      fs_seas = paste0(case_when(
        month(fs_mo) %in% c(3,4,5) ~ "MAM",
        month(fs_mo) %in% c(10,11,12) ~ "OND",
        .default ="other"

      ),"_",year(fs_yr))
    )
  return(fs_lookup)

}

zonal_floodscan <- function(floodscan_path=fp_fs,
                            adm = lgdf_adm$adm1 %>% filter(adm0_en=="Mozambique"),
                            cols_keep = c("adm0_en","adm0_pcode","adm1_en","adm1_pcode"),
                            zonal_stat= "mean"
                            ){
  tnc_fs <- tidync(floodscan_path)
  tnc_fs_filt <- fs_filter_bounds(fs_obj = tnc_fs,geometry = adm)
  r_fs <- fs_to_raster(fs_obj = tnc_fs_filt,band = "SFED_AREA")
  
  ret <- exact_extract(x =r_fs,
                       y = adm,
                       fun =zonal_stat,
                       append_cols= cols_keep,
                       force_df = TRUE
  ) %>%
    pivot_longer(cols = matches('^mean.|^median.'),
                 names_to = "stat_date",
                 values_to = "flood_frac") %>%
    separate(col = stat_date, "\\.",into = c("stat","date")) %>%
    mutate(
      date= as_date(date)
    )
  
}






