library(targets)
tar_source()
# Flood Analysis ----------------------------------------------------------
# Set target options:
tar_option_set(
  packages = c("tibble",
               "sf",
               "gghdx",
               "tidyverse",
               "janitor",
               "rhdx",
               "glue",
               "tidync",
               "terra",
               "gt",
               "exactextractr"
  ) # packages that your targets need to run
)


# file path ocha-exploratory data
dir_ocha_explore <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "exploration",
  "som",
  "ocha"

)

dir_wp <-  file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "som",
  "worldpop"
)

list(
# Step 1. Get AOI ----------------------------------------------------------------
  tar_target(name = lgdf_adm, # list of geodataframes (lgdf)
             command=get_proj_cods()
             ),
  tar_target(
    name=fp_wp,
    command= file.path(dir_wp,"som_ppp_2020_1km_Aggregated_UNadj.tif" ),
    format = "file"
  ),

  tar_target(
    name=fp_fs,
    command= file.path(
      Sys.getenv("AA_DATA_DIR"),
      "private",
      "raw",
      "glb",
      "floodscan",
      "floodscan_flooded_fraction_africa_19980112-20221231_p00",
      "aer_sfed_area_300s_19980112_20221231_v05r01.nc"
    )
  )
)
  #   tar_target(
  #     name=fp_ocha_pop,
  #     command= file.path(dir_ocha_explore,"2024 Population Disaggregation.xlsx" ),
  #     format = "file"
  #   ),
  #   tar_target(
  #     name=fp_ocha_pin,
  #     command= file.path(dir_ocha_explore,"Workspace_3A-3B_PIN&Sev_2023-10-24_district level.xlsx"),
  #     format = "file"
  #   ),
# 
#     format = "file"
#   ),
# 
# 
#   # Load files --------------------------------------------------------------
# 
# 
#   tar_target(
#     name= df_ocha_pin,
#     command = readxl::read_xlsx(
#       path = fp_ocha_pin,
#       sheet = "WS - 3.1 Overall PiN",
#       skip=2
#     ) %>%
#       clean_names()
#   ),
#   tar_target(
#     name= df_ocha_pop,
#     command = readxl::read_xlsx(
#       path = fp_ocha_pop,
#       sheet ="Humanitarian Plannning 2024 pop",
#       skip = 1
#     ) %>%
#       clean_names()
#   ),
#   tar_target(
#     name = lgdf_adm,
#     command = get_proj_cod_layer_names()
#   ),
#   tar_target(
#     name = gdf_adm,
#     command = map(
#       set_names(c(
#         adm0="som_admbnda_adm0_ocha_20230308",
#         adm1="som_admbnda_adm1_ocha_20230308",
#         adm2="som_admbnda_adm2_ocha_20230308")),
# 
#       ~ search_datasets("Somalia - Subnational Administrative Boundaries") %>%
#         pluck(1) %>%
#         get_resource(2) %>%
#         read_resource(layer = .x) %>%
#         clean_names() %>%
#         select(matches("^adm\\d_"))
#     ) %>%
#       set_names(c("adm0", "adm1", "adm2"))
#     # format = "feather" # efficient storage for large data frames
#   ),
# 
#   # Wrangle OCHA data -------------------------------------------------------
# 
# 
#   tar_target(
#     name = df_ocha_pin_pop,
#     command = df_ocha_pop %>%
#       select(adm2_en= district,
#              adm2_pcode = district_pcode,
#              pop_24 = revised_population_methodology_2024_unfpa_adoptation) %>%
#       left_join(
#         df_ocha_pin %>%
#           select(adm1_en = admin_1,
#                  adm1_pcode = admin_1_p_code,
#                  adm2_en = admin_2,
#                  adm2_pcode = admin_2_p_code,
#                  final_pin=final_pi_n) %>%
#           filter(!is.na(adm1_en))
#       )
#   ),
#   tar_target(
#     name = df_adm1_ocha_pin_pop,
#     command = df_ocha_pop %>%
#       select(adm2_en= district,
#              adm2_pcode = district_pcode,
#              pop_24 = revised_population_methodology_2024_unfpa_adoptation) %>%
#       left_join(
#         df_ocha_pin %>%
#           select(adm1_en = admin_1,
#                  adm1_pcode = admin_1_p_code,
#                  adm2_en = admin_2,
#                  adm2_pcode = admin_2_p_code,
#                  final_pin=final_pi_n) %>%
#           filter(!is.na(adm1_en))
#       ) %>%
#       group_by(across(starts_with("adm1"))) %>%
#       summarise(
#         across(.cols= c("pop_24","final_pin"),~sum(.x,na.rm=T))
#       )
#   ),
# 
# 
#   # Historical Exposure Calcs -----------------------------------------------
#   ## Admin 2 Historical Exposure ####
# 
#   # look at distribution of pixels to set min threshold for method 2
#   tar_target(
#     name = df_fs_pixel_values,
#     command = floodscan_pixel_values(
#       floodscan_path = fp_fs,
#       mask = gdf_adm$adm0
#     )
#   ),
# 
#   ### Method 1 ####
#   # here we multiply flood frac by population, we still apply 1 very minimal threshold
#   # to eliminate noise
#   tar_target(
#     name = df_adm2_exposure_historical1,
#     command = zonal_pop_exposure_method1(floodscan_path=fp_fs,
#                                          worldpop_path=fp_wp,
#                                          flood_frac_thresh=0.005,
#                                          adm = gdf_adm$adm2
# 
#     )
#   ),
# 
#   tar_target(
#     name = df_adm2_exposure_historical2,
#     command = zonal_pop_exposure_method2(floodscan_path=fp_fs,
#                                          worldpop_path=fp_wp,
#                                          flood_frac_thresh=seq(0.02,0.2, by=0.02),
#                                          adm = gdf_adm$adm2
# 
#     )
#   ),
#   ## Admin 2 World Pop Summary ####
# 
#   tar_target(
#     name = df_adm2_worldpop_summary,
#     command= df_summary_adm2_wp <- exact_extract(x = rast(fp_wp),
#                                                  y = gdf_adm$adm2,
#                                                  fun ="sum",
#                                                  append_cols= c("adm1_en","adm1_pcode","adm2_en","adm2_pcode")) %>%
#       pivot_longer(-matches("adm")) %>%
#       select(-name) %>%
#       rename(
#         pop = "value"
#       )
# 
#   ),
# 
#   ## Merge world pop summary and exposure to get % exposed ####
#   tar_target(
#     name=ldf_adm2_historical_pct,
#     command = list(method1=df_adm2_exposure_historical1,method2 =df_adm2_exposure_historical2) %>%
#       map(
#         \(dft){
#           dft %>%
#             left_join(
#               df_adm2_worldpop_summary
#             ) %>%
#             mutate(
#               pct_exposed = pop_exposed/pop
#             ) %>%
# 
#             # apply % to OCHA adjusted pop for 2024
#             left_join(
#               df_ocha_pin_pop
#             ) %>%
#             mutate(
#               pop_exposed_24 = pop_24*pct_exposed
#             )
#         }
#       )
# 
#   ),
#   tar_target(name = df_adm2_historical_pct2_single_thresh,
#              command = ldf_adm2_historical_pct$method2 %>%
#                filter(round(thresh,3)==0.2)
#   ),
#   ## Admin 1 Historical Exposure ####
#   tar_target(
#     name = df_adm1_historical_pct2,
#     command = df_adm2_historical_pct2_single_thresh %>%
#       group_by(
#         across(starts_with("adm1")),date
#       ) %>%
#       summarise(
#         pop_wp = sum(pop),
#         pop_24= sum(pop_24),
#         pop_exposed_wp = sum(pop_exposed),
#         pop_exposed_24 = sum(pop_exposed_24),
#         pct_pop_exposed_24 = pop_exposed_24/pop_24,
#         .groups="drop"
#       )
# 
#   ),
# 
#   tar_target(
#     name = df_adm1_historical_pct1,
#     command = ldf_adm2_historical_pct$method1 %>%
#       group_by(
#         across(starts_with("adm1")),date
#       ) %>%
#       summarise(
#         pop_wp = sum(pop),
#         pop_24= sum(pop_24),
#         pop_exposed_wp = sum(pop_exposed),
#         pop_exposed_24 = sum(pop_exposed_24),
#         pct_pop_exposed_24 = pop_exposed_24/pop_24,
#         .groups="drop"
#       )
# 
#   ),
#   tar_target(
#     name = df_adm1_historical_pct_all_thresh2,
#     command = ldf_adm2_historical_pct$method2 %>%
#       group_by(
#         across(starts_with("adm1")),date,thresh
#       ) %>%
#       summarise(
#         pop_wp = sum(pop),
#         pop_24= sum(pop_24),
#         pop_exposed_wp = sum(pop_exposed),
#         pop_exposed_24 = sum(pop_exposed_24),
#         pct_pop_exposed_24 = pop_exposed_24/pop_24,
#         .groups="drop"
#       )
# 
#   ),
# 
# 
#   # Calculate Quantiles -----------------------------------------------------
# 
#   ## Admin 1 Quantiles #####
#   tar_target(
#     name = ldf_adm1_exposure_quantiles,
#     command =  list(method1=df_adm1_historical_pct1,
#                     method2=df_adm1_historical_pct2  )%>%
#       map(
#         \(dft){
#           dft %>%
#             mutate(mo= month(date)) %>%
#             group_by(across(starts_with("adm")),mo) %>%
#             reframe(
#               quibble(pct_pop_exposed_24,q=c(seq(0,0.95,0.05),0.99)),
#               quibble(pop_exposed_24,q=c(seq(0,0.95,0.05),0.99))
#             ) %>%
#             rename(
#               q= "pct_pop_exposed_24_q"
#             ) %>%
#             select(-ends_with("_q"))
#         }
#       )
# 
#   ),
#   tar_target(
#     name = ldf_adm2_exposure_quantiles,
#     command =  list(method1=df_adm2_historical_pct2_single_thresh,
#                     method2=ldf_adm2_historical_pct$method1  )%>%
#       map(
#         \(dft){
#           dft %>%
#             mutate(mo= month(date)) %>%
#             group_by(across(starts_with("adm")),mo) %>%
#             reframe(
#               quibble(pct_exposed,q=c(seq(0,0.95,0.05),0.99)),
#               quibble(pop_exposed_24,q=c(seq(0,0.95,0.05),0.99))
#             ) %>%
#             rename(
#               q= "pct_exposed_q"
#             ) %>%
#             select(-ends_with("_q"))
#         }
#       )
# 
#   ),
#  
#   # Create Range Table  -----------------------------------------------------
# 
#   # Admin 1 Ranges df #####
#   tar_target(
#     name = ldf_adm1_exposure_ranges,
#     command = ldf_adm1_exposure_quantiles %>%
#       map(
#         \(dft){
#           dft %>%
#             filter(
#               (mo==10 & q %in% c(0.25,0.75))|
#                 (mo== 3 & q %in% c(0.5,0.95)),
#             ) %>%
#             split(.$mo) %>%
#             imap(
#               \(dft,nm_mo){
#                 if(nm_mo==3){
#                   seas_label = "mam_"
#                 }
#                 if(nm_mo==10){
#                   seas_label="ond_"
#                 }
# 
#                 dft %>%
#                   group_by(across(starts_with("adm"))) %>%
#                   summarise(
#                     across(.cols =c("pop_exposed_24",
#                                     "pct_pop_exposed_24"),
#                            list(min=min, max= max)),
#                     .groups="drop"
#                   ) %>%
#                   # add seas_label to pop_exposed_24 and pct_pop_exposed_24 columns
#                   rename_with(
#                     .fn = ~paste0(seas_label,.x),
#                     .cols = matches("^pop_exposed_24*|^pct_pop_exposed_24*")
#                   )
#               }
#             ) %>%
#             reduce(left_join) %>%
#             left_join(df_adm1_ocha_pin_pop)
#         }
#       )
# 
#   ),
#   tar_target(
#     name = ldf_adm2_exposure_ranges,
#     command = ldf_adm2_exposure_quantiles %>%
#       map(
#         \(dft){
# 
#           dft %>%
#             filter(
#               (mo==10 & q %in% c(0.25,0.75))|
#               (mo== 3 & q %in% c(0.5,0.95)),
#             ) %>%
#             split(.$mo) %>%
#             imap(
#               \(dft2,nm_mo){
#                 if(nm_mo==3){
#                   seas_label = "mam_"
#                 }
#                 if(nm_mo==10){
#                   seas_label="ond_"
#                 }
# 
#                 dft2 %>%
#                   group_by(across(starts_with("adm"))) %>%
#                   summarise(
#                     across(.cols =c("pop_exposed_24",
#                                     "pct_exposed"),
#                            list(min=min, max= max)),
#                     .groups="drop"
#                   ) %>%
#                   # add seas_label to pop_exposed_24 and pct_pop_exposed_24 columns
#                   rename_with(
#                     .fn = ~paste0(seas_label,.x),
#                     .cols = matches("^pop_exposed*|^pct_exposed*")
#                   )
#               }
#             ) %>%
#             reduce(left_join) %>%
#             left_join(df_ocha_pin_pop)
#         }
#       )
# 
#   ),
#   tar_target(
#     name = df_adm1_max_range_table,
#     command = ldf_adm1_exposure_ranges %>%
#       imap(
#         \(dft,nm_tmp){
#           dft %>%
#             mutate(method=nm_tmp)
#         }
#       ) %>%
#       list_rbind() %>%
#       group_by(
#         across(
#           matches("adm")
#         )
#       ) %>%
#       summarise(
#         across(where(is.numeric),~max(.x)),
#         .groups="drop"
#       )
#   ),
#   tar_target(
#     name = df_adm2_max_range_table,
#     command = ldf_adm2_exposure_ranges %>%
#       imap(
#         \(dft,nm_tmp){
#           dft %>%
#             mutate(method=nm_tmp)
#         }
#       ) %>%
#       list_rbind() %>%
#       group_by(
#         across(
#           matches("adm")
#         )
#       ) %>%
#       summarise(
#         across(where(is.numeric),~max(.x)),
#         .groups="drop"
#       )
#   ),
#   tar_target(
#     name = gt_adm1_range_table,
#     command = df_adm1_max_range_table %>%
#       # and they said making tables in R would be fun
#       mutate(
#         across(matches("mam_pop_exposed|ond_pop_exposed"),~round(.x,-2))
#       ) %>%
#       gt() %>%
#       gt::cols_hide(columns = c("pop_24","adm1_pcode")) %>%
#       cols_merge(
#         columns = c("mam_pop_exposed_24_min","mam_pop_exposed_24_max"),
#         pattern= "{1}-{2}"
#       ) %>%
#       cols_merge(
#         columns = c("mam_pct_pop_exposed_24_min","mam_pct_pop_exposed_24_max"),
#         pattern= "{1}-{2}"
#       ) %>%
#       cols_merge(
#         columns = c("ond_pop_exposed_24_min","ond_pop_exposed_24_max"),
#         pattern= "{1}-{2}"
#       ) %>%
#       cols_merge(
#         columns = c("ond_pct_pop_exposed_24_min","ond_pct_pop_exposed_24_max"),
#         pattern= "{1}-{2}"
#       ) %>%
#       tab_spanner(
#         label = "MAM (50-95th percentile)",
#         columns = c(
#           "mam_pop_exposed_24_min",
#           "mam_pct_pop_exposed_24_min"
#         )
#       ) %>%
#       tab_spanner(
#         label = "OND (25-75th percentile)",
#         columns = c(
#           "ond_pop_exposed_24_min",
#           "ond_pct_pop_exposed_24_min"
#         )
#       ) %>%
#       gt::cols_label(
#         adm1_en = "Region",
#         mam_pop_exposed_24_min = "Population Estimates",
#         mam_pct_pop_exposed_24_min = "% Population Estimates",
#         ond_pop_exposed_24_min = "Population Estimates",
#         ond_pct_pop_exposed_24_min = "% Population ",
#         final_pin = "PiN"
#       ) %>%
#       fmt_number(
#         columns = c("mam_pop_exposed_24_min",
#                     "mam_pop_exposed_24_max",
#                     "ond_pop_exposed_24_min",
#                     "ond_pop_exposed_24_max"),
#         decimals = 0,
#         n_sigfig = 3
# 
#       ) %>%
#       fmt_number(
#         columns = "final_pin",
#         decimals=0
#       ) %>%
#       fmt_percent(
#         columns =c(
#           mam_pct_pop_exposed_24_min,
#           mam_pct_pop_exposed_24_max,
#           ond_pct_pop_exposed_24_min,
#           ond_pct_pop_exposed_24_max
#         ) ,
#         decimals = 0
#       ) %>%
#       data_color(
#         columns= c("mam_pct_pop_exposed_24_min",
#                    "mam_pop_exposed_24_min",
#                    "ond_pct_pop_exposed_24_min",
#                    "ond_pop_exposed_24_min",
#                    "final_pin"
#         ),
#         method = "numeric",
#         palette = "YlOrRd",
#         reverse=F
#       )
#   ),
# 
#   tar_target(
#     name = df_adm2_prioritization,
#     command = df_adm2_max_range_table %>%
#       select(
#         starts_with("adm"),
#         starts_with("mam_pop"),
#         starts_with("mam_pct"),
#         "final_pin"
#       ) %>%
#       left_join(
#         df_adm2_historical_pct2_single_thresh %>%
#       select(
#         "date",
#         starts_with("adm"),
#         "pop_exposed",
#       ) %>%
#       mutate(
#         seas_label = case_when(month(date)==3~"MAM",
#                               month(date)==10~"OND")
#       ) %>%
#       filter(seas_label=="MAM") %>%
#       group_by(
#         across(starts_with("adm")),
#         seas_label
#       ) %>%
#       summarise(
#         across("pop_exposed",list(mean= mean, median=median))
#       )
#       ) %>%
#       arrange(desc(pop_exposed_median)) %>%
#       mutate(
#         top_10_exposed =row_number()
#       ) %>%
#       arrange(desc(final_pin)) %>%
#       mutate(top_10_pin = row_number()) %>%
#       filter(
#         top_10_exposed<=15 &top_10_pin<=15
#       )
#   ),
# 
#   tar_target(
#     name = gt_adm2_range_table,
#     command = df_adm2_max_range_table %>%
#       # and they said making tables in R would be fun
#       mutate(
#         across(matches("mam_pop_exposed|ond_pop_exposed"),~round(.x,-2)),
# 
#         # not usingn this
#         # prioritization = adm2_pcode %in% df_adm2_prioritization$adm2_pcode
#       ) %>%
#       group_by(adm1_en) %>%
#       gt() %>%
#       gt::cols_hide(columns = c("pop_24","adm1_pcode","adm2_pcode")) %>%
#       cols_merge(
#         columns = c("mam_pop_exposed_24_min","mam_pop_exposed_24_max"),
#         pattern= "{1}-{2}"
#       ) %>%
#       cols_merge(
#         columns = c("mam_pct_exposed_min","mam_pct_exposed_max"),
#         pattern= "{1}-{2}"
#       ) %>%
#       cols_merge(
#         columns = c("ond_pop_exposed_24_min","ond_pop_exposed_24_max"),
#         pattern= "{1}-{2}"
#       ) %>%
#       cols_merge(
#         columns = c("ond_pct_exposed_min","ond_pct_exposed_max"),
#         pattern= "{1}-{2}"
#       ) %>%
#       tab_spanner(
#         label = "MAM (50-95th percentile)",
#         columns = c(
#           "mam_pop_exposed_24_min",
#           "mam_pct_exposed_min"
#         )
#       ) %>%
#       tab_spanner(
#         label = "OND (25-75th percentile)",
#         columns = c(
#           "ond_pop_exposed_24_min",
#           "ond_pct_exposed_min"
#         )
#       ) %>%
#       gt::cols_label(
#         adm1_en = "Region",
#         adm2_en ="District",
#         mam_pop_exposed_24_min = "Population Estimates",
#         mam_pct_exposed_min = "% Population Estimates",
#         ond_pop_exposed_24_min = "Population Estimates",
#         ond_pct_exposed_min = "% Population ",
#         final_pin = "PiN"
#       ) %>%
#       fmt_number(
#         columns = c("mam_pop_exposed_24_min",
#                     "mam_pop_exposed_24_max",
#                     "ond_pop_exposed_24_min",
#                     "ond_pop_exposed_24_max"),
#         decimals = 0,
#         n_sigfig = 3
# 
#       ) %>%
#       fmt_number(
#         columns = "final_pin",
#         decimals=0
#       ) %>%
#       fmt_percent(
#         columns =c(
#           mam_pct_exposed_min,
#           mam_pct_exposed_max,
#           ond_pct_exposed_min,
#           ond_pct_exposed_max
#         ) ,
#         decimals = 0
#       ) %>%
#       data_color(
#         columns= c("mam_pct_exposed_min",
#                    "mam_pop_exposed_24_min",
#                    "ond_pct_exposed_min",
#                    "ond_pop_exposed_24_min",
#                    "final_pin"
#         ),
#         method = "numeric",
#         palette = "YlOrRd",
#         reverse=F
#       ) %>%
#       # tab_style(
#       #   style = cell_fill(color = hdx_hex("tomato-hdx")),
#       #   locations = cells_body(
#       #     rows = prioritization == 1)
#       # ) %>%
# 
#       tab_options(
#         row_group.as_column = TRUE,
#         data_row.padding = px(0),
#         table.font.size = 12)
#   ),
# 
# 
# # "{pop_range_label}":=
#   #   paste0(formatC(round(min(pop_exposed_24),-2),format="d", big.mark=",")," - ",
#   #          formatC(round(max(pop_exposed_24),-2),format="d", big.mark=",")
#   #   ),
#   # "{pct_range_label}":=
#   #   paste0(formatC(round(min(pct_pop_exposed_24*100),0),format="d", big.mark=",")," - ",
#   #          formatC(round(max(pct_pop_exposed_24*100),0),format="d", big.mark=",")
#   #   ),
# 
#   #
#   #   ## Admin 2 Ranges df ####
#   #   tar_target(
#   #     name = df_adm2_exposure_ranges,
#   #     command = df_adm2_exposure_quantiles %>%
#   #       filter(mo==10 & q %in% c(0.25,0.75)|
#   #                mo== 3 & q %in% c(0.5,0.95),
#   #       ) %>%
#   #       split(.$mo) %>%
#   #       imap(
#   #         \(dft,nm_mo){
#   #
#   #           if(nm_mo==3){
#   #             pop_range_label = "mam_pop_50_95"
#   #             pct_range_label = "mam_pct_50_95"
#   #           }
#   #           if(nm_mo==10){
#   #             pop_range_label = "ond_pop_25_75"
#   #             pct_range_label = "ond_pct_25_75"
#   #           }
#   #           dft %>%
#   #             group_by(across(starts_with("adm"))) %>%
#   #             summarise(
#   #               "{pop_range_label}":=
#   #                 paste0(formatC(round(min(pop_exposed_24),-2),format="d", big.mark=",")," - ",
#   #                        formatC(round(max(pop_exposed_24),-2),format="d", big.mark=",")
#   #                 ),
#   #               "{pct_range_label}":=
#   #                 paste0(formatC(round(min(pct_exposed*100),0),format="d", big.mark=",")," - ",
#   #                        formatC(round(max(pct_exposed*100),0),format="d", big.mark=",")
#   #                 ),.groups="drop"
#   #             )
#   #         }
#   #       ) %>%
#   #       reduce(left_join) %>%
#   #       left_join(
#   #         df_ocha_pin_pop
#   #       )
#   #   ),
#   #
#   #   ## Gt Table Admin 1 ####
#   #   tar_target(
#   #     name = gt_adm1_pctiles,
#   #     command = df_adm1_exposure_ranges %>%
#   #       select(-adm1_pcode) %>%
#   #       gt() %>%
#   #       gt::cols_label(
#   #         adm1_en= "Region",
#   #         ond_pop_25_75="Population Estimates (25-75th percentile)",
#   #         ond_pct_25_75="% Estimates (25-75th percentile)",
#   #         mam_pop_50_95="Population Estimates (50-95th percentile)",
#   #         mam_pct_50_95= "% Estimates (50-95th percentile)",
#   #         final_pin = "PiN"
#   #       ) %>%
#   #       fmt_number(
#   #         columns = c("final_pin"),
#   #         decimals = 0
#   #       ) %>%
#   #       tab_spanner(
#   #         label = "MAM",
#   #         columns = c("mam_pop_50_95","mam_pct_50_95")
#   #       ) %>%
#   #       tab_spanner(
#   #         label = "OND",
#   #         columns = c("ond_pop_25_75","ond_pct_25_75")
#   #       )
#   #   ),
#   #   ## gt ADMIN 2 Table ####
#   #   tar_target(
#   #     name = gt_adm2_pctiles,
#   #     command = df_adm2_exposure_ranges %>%
#   #       select(
#   #         adm1_en,adm2_en, everything(), - ends_with("_pcode")
#   #       ) %>%
#   #       group_by(adm1_en) %>%
#   #       gt() %>%
#   #       cols_label(
#   #         adm1_en= "Region",
#   #         adm2_en= "District",
#   #         ond_pop_25_75="Population Estimates (25-75th percentile)",
#   #         ond_pct_25_75="% Estimates (25-75th percentile)",
#   #         mam_pop_50_95="Population Estimates (50-95th percentile)",
#   #         mam_pct_50_95= "% Estimates (50-95th percentile)",
#   #         final_pin = "PiN"
#   #       ) %>%
#   #       fmt_number(
#   #         columns = c("final_pin"),
#   #         decimals = 0
#   #       ) %>%
#   #       tab_spanner(
#   #         label = "MAM",
#   #         columns = c("mam_pop_50_95","mam_pct_50_95")
#   #       ) %>%
#   #       tab_spanner(
#   #         label = "OND",
#   #         columns = c("ond_pop_25_75","ond_pct_25_75")
#   #       ) %>%
#   #       tab_options(
#   #         row_group.as_column = TRUE,
#   #         data_row.padding = px(0),
#   #         table.font.size = 12)
#   #   ),
#   #
#   #
#   #   # Extra analysis for Map Shading ------------------------------------------
#   #
#   #   ## Admin 1 pt estimates ####
#     tar_target(
#       name=df_adm1_pt_estimates_map,
#       command = bind_rows(
#         df_adm1_historical_pct2 %>%
#           mutate(
#             method = "method2"
#           ),
#         df_adm1_historical_pct1 %>%
#           mutate(
#             method= "method1"
#           )
#       ) %>%
#         group_by(
#           across(starts_with("adm1")),
#           seas_label = if_else(month(date) ==3,"MAM","OND"),
#           method
#         ) %>%
#         summarise(
#           mean_pop_exposed = mean(pop_exposed_24),
#           pop_24 = unique(pop_24),
#           .groups="drop_last"
#         ) %>%
#         summarise(
#           mean_pop_exposed = max(mean_pop_exposed),
#           pop_24 = unique(pop_24),
#         ) %>%
#         ungroup() %>%
#         mutate(
#           mean_pct_exposed = mean_pop_exposed/pop_24,
#         ) %>%
#         select(-pop_24) %>%
#         pivot_wider(names_from= seas_label,
#                     values_from= c("mean_pop_exposed","mean_pct_exposed"))
#     ),
#     tar_target(
#       name=gdf_adm1_summary_ranges_map ,
#       command=
#         # gdf
#         gdf_adm$adm1 %>%
#         left_join(
#           # percentile ranges + PiN
#           df_adm1_max_range_table %>%
#             mutate(
#               ond_pop_25_75 = paste0(
#                 formatC(round(ond_pop_exposed_24_min,-2),format="d", big.mark=","),"-",
#                 formatC(round(ond_pop_exposed_24_max,-2),format="d", big.mark=",")
#               ),
#               mam_pop_50_95 = paste0(
#                 formatC(round(mam_pop_exposed_24_min,-2),format="d", big.mark=","),"-",
#                 formatC(round(mam_pop_exposed_24_max,-2),format="d", big.mark=",")
#               )
#             ) %>%
#             select(starts_with("adm"),ond_pop_25_75,mam_pop_50_95,final_pin) %>%
#             # pt estimates for shading
#             left_join(df_adm1_pt_estimates_map) %>%
#             mutate(
#               mam_pct_pin = mean_pct_exposed_MAM*final_pin,
#               ond_pct_pin = mean_pct_exposed_OND*final_pin
#             )
#         )
#     )



