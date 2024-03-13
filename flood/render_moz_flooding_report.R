
file_name_quarto <- "04_moz_flash_updates_viz_country_wide.qmd"

run_date <- format(Sys.Date(),"%Y%m%d")
quarto::quarto_render(input = file.path("flood",file_name_quarto),
                      execute_params = list("aoi" = "country_wide",
                                            "subby"="Country Wide Analysis")
                      )


quarto::quarto_render(input = file.path("flood",file_name_quarto),
                      execute_params = list("aoi" = "subset1",
                                            "subby"="Analysis of Sofala & Inhambane Provinces")
                      # output_file = "flood\test.html",
                      # output_format = "html"
                      )

