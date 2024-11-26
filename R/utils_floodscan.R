#' Title
#'
#' @param fs_obj
#' @param date_range
#'
#' @return
#' @export
#'
#' @examples
fs_filter_date <- function(fs_obj,
                           date_range){

  date_range <- range(as_date(date_range)) # in case it's a seq
  date_seq <- seq(date_range[1],date_range[2], by ="day")

  df_time_idx_full <- fs_time_idx_lookup(
    fs_obj = fs_obj
  )

  df_time_idx <- df_time_idx_full %>%
    filter(date %in% date_seq)

  # hyper filter
  fs_obj %>%
    hyper_filter(
      time = between(time,min(df_time_idx$time_index), max(df_time_idx$time_index))
    )
}

fs_extent <-  function(fs_obj){
  lon_array<-  fs_obj |>
    tidync::activate("lon") |>
    tidync::hyper_array()
  
  lat_array<-  fs_obj |>
    tidync::activate("lat") |>
    tidync::hyper_array()
  
  # trick adapted from the OG https://github.com/rspatial/terra/blob/master/R/rast.R
  x <- list()
  x$x <- lon_array$lon
  x$y <- lat_array$lat
  resx <- abs(( x$x[length(x$x)] - x$x[1] ) / (length(x$x)-1))
  resy <- abs(( x$y[length(x$y)] - x$y[1] ) / (length(x$y)-1))
  xmn <- min(x$x) - 0.5 * resx
  xmx <- max(x$x) + 0.5 * resx
  ymn <- min(x$y) - 0.5 * resy
  ymx <- max(x$y) + 0.5 * resy
  
  r_ext <-  terra::ext(xmn, xmx, ymn, ymx)
  return(r_ext)
}

fs_time_idx_lookup <-  function(fs_obj){
  
  # pull time indices
  time_dim <- fs_obj |>
    tidync::activate("time") |>
    tidync::hyper_array()
  
  # create date - time idx lookup df
  fs_time_tibble <- dplyr::tibble(
    time_index = time_dim$time,
    date = as.Date(time_index, origin = "1998-01-12")
  )
  return(fs_time_tibble)
}
#' Title
#'
#' @param fs_obj
#' @param geometry
#'
#' @return
#' @export
#'
#' @examples
fs_filter_bounds <-  function(fs_obj,geometry){
  geo_bbox <- sf::st_bbox(geometry)
  
  fs_obj |>
    tidync::hyper_filter(
      lat = dplyr::between(lat, geo_bbox[2],geo_bbox[4]),
      lon = dplyr::between(lon, geo_bbox[1],geo_bbox[3]),
    )
}

#' Title
#'
#' @param fs_obj
#' @param band
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' fs %>%
#'   fs_filter_bounds(geometry = nga_adm$adm0) %>%
#'   fs_filter_date(c(
#'   "2019-08-26" ,"2019-09-20"
#'   )) %>%
#'   fs_to_raster(band = "SFED_AREA")
#'   }

fs_to_raster <- function(fs_obj, band){
  
  df_time_idx <- fs_time_idx_lookup(fs_obj = fs_obj)
  
  r_ext <-  fs_extent(fs_obj)
  
  fs_h_array <- tidync::hyper_array(
    x = fs_obj,
    select_var = band
  )
  
  # reorder dims -- would need a more flexible application to generalize
  fs_array <- aperm(fs_h_array[[band]], c(3, 2, 1))
  
  r <-   terra::rast(
    x=fs_array,
    extent =r_ext,
    crs= "EPSG:4326"
  )
  terra::set.names(x = r,df_time_idx$date)
  return(r)
}

#' floodscan_lookup
#' @description
#' helper function used within `zonal_pop_exposure()` to help organize, group, and aggregate FloodScan raster
#'
#' @param r_fs `spatRaster` object returned from fs_to_raster() (floodScan to raster)
#'
#' @return `tibble` lookup table with dates and months of floodscan data to be used to group and summarise FloodScan raster
#' @export


tar_flood_exposure <- function(fp_floodscan,
                               fp_population,
                               sf_zone,
                               date_range,
                               resample="population",
                               flood_band="SFED_AREA",
                               aggregate_binary_flood = T
){
  cat("reading raster floodscan\n")
  nc_fs <- tidync(fp_floodscan)

  cat("reading pop\n")
  r_pop <- rast(fp_population)

  cat("filtering floodscan\n")
  r_fs <- nc_fs %>%
    fs_filter_bounds(geometry = sf_zone) %>%
    fs_filter_date(c(
      date_range = date_range
    )) %>%
    fs_to_raster(band = flood_band)
  cat("cropping pop raster\n")
  r_pop_crop <- crop(r_pop, r_fs)
  if(resample == "population"){
    cat("resampling pop raster to floodscan\n")
    r_pop_resamp <- resample(x = r_pop_crop,
                             r_fs,
                             method="sum")
  }
  if(resample == "floodscan"){
    cat("downsampling floodscan raster to pop\n")
    r_fs_resamp <- resample(x = r_fs,
                            y=r_pop_crop,
                            method="near")
  }


  # specific for OCHA ADM CODs
  retain_col_names <- sf_zone %>%
    st_drop_geometry() %>%
    dplyr::select(matches("^ADM\\d_[EP]")) %>%
    colnames()

  if(aggregate_binary_flood){
    cat("replaceing 0 w/ NA in floodscan\n")
    r_fs <-  r_fs
    r_fs[r_fs==0] <- NA
    r_fs[!is.na(r_fs)]<-1
  }

  if(resample=="population"){
    r_flood_pop <- r_fs * r_pop_resamp
  }
  if(resample=="floodscan"){
    r_flood_pop <- r_fs_resamp * r_pop_crop
  }



  cat("zonal stats\n")
  exact_extract(
    x = r_flood_pop,
    y = sf_zone,
    append_cols = retain_col_names,
    fun = c("sum"),
    force_df = T,
    full_colnames = T
  ) %>% # wrangle
    pivot_longer(-matches("ADM")) %>%
    separate(name, into = c("stat", "date"), sep = "\\.") %>%
    pivot_wider(names_from = "stat", values_from = "value") %>%
    mutate(
      date= as_date(date)
    )



}

