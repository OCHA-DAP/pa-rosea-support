mosaic_worldpop <-  function(list_raster_paths ){
  lr <- list_raster_paths %>% 
    map(\(fp_temp){
      r_tmp <- rast(fp_temp)
      r_tmp %>% 
        set.names(
          "ppp_2020_1km_UNadj"
        )
      return(r_tmp)
    }
    
    )
  rc<- sprc(lr)
  mosaic(rc)
}