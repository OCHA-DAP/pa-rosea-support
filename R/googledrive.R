
# google drive helpers ----------------------------------------------------


#' Title
#'
#' @param dribble 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
load_drive_file <-  function(
    dribble,
    file_name){
  file_id <-  get_drive_id(dribble = dribble,
                           file_name = file_name)
  tmp_file_path <-  file.path(
    tempdir(),file_name
  )
  
  cat("downloading ", file_name," to temp path\n")
  googledrive::drive_download(
    file = googledrive::as_id(file_id),
    path = tmp_file_path
  )
  cat("reading ", file_name," to memory\n")
  ret <- read_fun(tmp_file_path)
  unlink(tmp_file_path)
  return(ret)
}

#' Title
#'
#' @param dribble 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' get_drive_id(
#'   dribble = drive_dribble,
#'   file_name = "central_america_aoi_adm0.rds" 
#' )
#' }
get_drive_id <- function(dribble, file_name){
  dribble %>% 
    dplyr::filter(
      name == file_name
    ) %>% 
    dplyr::pull(id) %>% 
    googledrive::as_id()
}





#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
read_fun <- function(x){
  file_ext<- fs::path_ext(x)
  switch(file_ext,
         "rds"=readr::read_rds(x),
         "csv"= readr::read_csv(x)
  )
}

