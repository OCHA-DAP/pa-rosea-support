
file_name_country_wide <- "04_moz_flash_updates_viz_country_wide.qmd"
file_name_subset <- "05_moz_flash_updates_viz_subset.qmd"

run_date <- format(Sys.Date(),"%Y%m%d")
quarto::quarto_render(input = file.path("flood",file_name_country_wide),
                      execute_params = list(
                        "aoi" = "country_wide",
                        "update_data" = "true"
                      )
)


quarto::quarto_render(input = file.path("flood",file_name_subset),
                      execute_params = list(
                        "aoi" = "subset1",
                        "update_data"="true"
                        )
                      )



# quarto::quarto_render(input = file.path("flood",file_name_quarto),
#                       execute_params = list(
#                         "aoi" = "country_wide",
#                         "update_local" = T
#                       )
# )

