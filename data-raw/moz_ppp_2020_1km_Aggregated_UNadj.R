#' Download world pop data for mozambique to repo data repo (.gitignored)
#' For NRT analysis it's downloaded each time- but this will help speed up the rendering of quarto for development/iteration
r_wp <- map(countries_dl_wp,\(country_code){
  cat("downloading ",country_code,"\n")
  country_code_lower <- str_to_lower(country_code)
  url <- glue("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/{country_code}/{country_code_lower}_ppp_2020_1km_Aggregated_UNadj.tif")
  terra::rast(url)
}
)
writeRaster(x = r_wp$Mozambique,
            file.path("data",
                      "moz_ppp_2020_1km_Aggregated_UNadj.tif"
            )
)
