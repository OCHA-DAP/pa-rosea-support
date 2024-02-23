#' mosaic_worldpop
#'
#' @param input_list list of paths to raster files OR terra/raster spatRaster/Raster class objects. Specify which in from_path
#' @param from_path `logical` if T, function reads input_list as list of character file paths, if F function reads input_list as list of terra/raster spatRaster/Raster class objects
#'
#' @return single `sprc` raster object
#' @export
#'
#' @examples
mosaic_worldpop <-  function(input_list, from_path ){
  lr <- input_list %>% 
    map(\(input_tmp){
      if(from_path){
        r_tmp <- rast(input_tmp)
      }
      if(!from_path){
        r_tmp <- input_tmp
      }
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

