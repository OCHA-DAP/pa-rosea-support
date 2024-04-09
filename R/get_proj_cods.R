
#' get_proj_cods
#'
#' @return list of 3 sf class data.frames.
#'   return adm0, 1, & 2 for SOM, ETH, & KEN.
#'
#' @examples \dontrun{
#' lgdf_adm <-  get_proj_cods()
#' }
#' 
#' 
#' 
moz_cod_lookup <-  function(){
  df_lookup<- tibble::tribble(
    ~original, ~standardized,
    "adm3_pt", "adm3_en",
    "adm2_pt", "adm2_en",
    "adm1_pt","adm1_en",
  )
  set_names(df_lookup$original, df_lookup$standardized)
}


get_proj_cods <-  function(){
  lgdf <- load_proj_cods()
  
  # some special COD cleaning for Mozambique
  lgdf$Mozambique <- lgdf$Mozambique %>% 
    map(\(gdf_moz){
      gdf_moz %>%
        rename(
          any_of(moz_cod_lookup()) 
        )
    })
  compile_proj_cods(lgdf)
}



#' load_proj_cods
#'
#' @return list of sf class data.frames
#'
#' @examples \dontrun{
#' lgdf_adm <-  load_proj_cods()
#' }
load_proj_cods <-  function(){
  lnames <- get_proj_cod_layer_names()
  lnames %>% 
    imap(\(ds,nm_cod){
      ds %>% 
        map(
          ~ search_datasets(glue("{nm_cod} - Subnational Administrative Boundaries") )%>%
            pluck(1) %>%
            get_resource(2) %>%
            read_resource(layer = .x) %>%
            clean_names() %>%
            select(matches("^adm\\d_[ep]"))
        )
    }
    
    
    )
  
  
}



#' compile_proj_cods
#'
#' @param lgdf nested list returned from `get_proj_cods()`
#' @return list of 3 sf class spatiall data.frames (adm0, adm1, adm2) all compile from KEN,SOM,ETH CODs (HDX)
#' @examples \dontrun{
#' library(rhdx)
#' library(tidyyverse)
#' library(targets)
#' lgdf_adm_natl <-  load_proj_cods()
#' lgdf_adm <- compile_proj_cods(lgdf_adm_natl)
#' }

compile_proj_cods <-  function(lgdf){
  adm0= bind_rows(lgdf$Kenya$adm0,lgdf$Somalia$adm0,lgdf$Ethiopia$adm0, lgdf$Mozambique$adm0)  
  adm1= bind_rows(lgdf$Kenya$adm1,lgdf$Somalia$adm1,lgdf$Ethiopia$adm1,lgdf$Mozambique$adm1)  
  adm2= bind_rows(lgdf$Kenya$adm2,lgdf$Somalia$adm2,lgdf$Ethiopia$adm2,lgdf$Mozambique$adm2)  
  return(
    lst(adm0,adm1,adm2)
  )
}



#' get_proj_cod_layer_names
#'
#' @return list of AOI layer names so that they can be downloaded from HDX with `{rhdx}` package

get_proj_cod_layer_names <- function(){
  som <- list(
    adm0="som_admbnda_adm0_ocha_20230308",
    adm1="som_admbnda_adm1_ocha_20230308",
    adm2="som_admbnda_adm2_ocha_20230308"
  )
  
  eth <- list(
    adm0="eth_admbnda_adm0_csa_bofedb_itos_2021",
    adm1="eth_admbnda_adm1_csa_bofedb_2021",
    adm2= "eth_admbnda_adm2_csa_bofedb_2021"
  )
  
  ken <- list(
    adm0= "ken_admbnda_adm0_iebc_20191031",
    adm1="ken_admbnda_adm1_iebc_20191031",
    adm2= "ken_admbnda_adm2_iebc_20191031" 
  )
  moz <-  list(
    adm0="moz_admbnda_adm0_ine_20190607",
    adm1= "moz_admbnda_adm1_ine_20190607", 
    adm2= "moz_admbnda_adm2_ine_20190607",
    adm3 = "moz_admbnda_adm3_ine_20190607"
  )
  return(
    list(Somalia=som, Ethiopia=eth, Kenya=ken, Mozambique= moz)
  )
  
  
}

    
