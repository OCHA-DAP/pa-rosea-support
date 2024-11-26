#' @title East Africa Flood Exposure Estimates
#' @description
#' This analysis uses FloodScan (1998-2022) & WorldPop (2022) to generate flood estimates for our specified AOI in East Africa.
#' Navigate to run.R file to run pipeline


library(targets)

# just need these 3 libraries separate to DL World Pop rasters from HDX before pipeline begins
library(tidyverse) # for mapping/downloading in beginning
library(glue) # for downloading
library(terra)
re_download_worldpop <- c(T,F)[1]

tar_source()
# Flood Analysis ----------------------------------------------------------
# Set target options:
tar_option_set(
  packages = c(
    "tibble",
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

# File paths --------------------------------------------------------------

## File Paths World Pop
fp_wp_som <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "som",
  "worldpop",
  "som_ppp_2020_1km_Aggregated_UNadj.tif"
)

fp_wp_eth <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "eth",
  "worldpop",
  "eth_ppp_2020_1km_Aggregated_UNadj.tif"
)

fp_wp_ken <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "ken",
  "worldpop",
  "ken_ppp_2020_1km_Aggregated_UNadj.tif" # this is 100 m?
)

# switchingn to read directly from HDX

# "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/KEN/ken_ppp_2020_1km_Aggregated_UNadj.tif"

if(re_download_worldpop){
  cat("downloading worldpop raster from HDX\n")
  countries_dl_wp <- c(Kenya="KEN",Ethiopia="ETH",Somalia="SOM",Mozambique="MOZ")
  lr_wp <- map(countries_dl_wp,\(country_code){
    cat("downloading ",country_code,"\n")
    country_code_lower <- str_to_lower(country_code)
    url <- glue("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/{country_code}/{country_code_lower}_ppp_2020_1km_Aggregated_UNadj.tif")
    terra::rast(url)
  }
  )
  
  
}





fp_wp_moz <- file.path(
  "public",
  "raw",
  "moz",
  "worldpop",
  "moz_ppp_2020_1km_Aggregated_UNadj.tif"
)

## defining countries and seasons
ssn_by_country <- list("Somalia" = c("MAM", "OND", "Annual"),
                    "Ethiopia" = c("MAM", "AMJ", "OND", "Annual", "JJAS"),
                    "Kenya" = c("MAM", "OND", "Annual"),
                    "Mozambique" = c("NDJ", "Annual"))
seasons <- tibble(
  season = c("MAM", "AMJ", "OND", "Annual", "NDJ", "JJAS"),
  start_month = c(3, 4, 10, 1, 11, 6),
  end_month = c(5, 6, 12, 12, 1, 9)
)

list(
  # Load Inputs -------------------------------------------------------------

  ## AOI ####
  tar_target(
    name = lgdf_adm, # list of geodataframes (lgdf)
    command = get_proj_cods()
  ),
  ## FloodScan ####
  # Loading FloodScan as a target - not necessary really in this context, but it's also fine

  tar_target(
    name = fp_fs,
    command = file.path(
      Sys.getenv("AA_DATA_DIR"),
      "private",
      "raw",
      "glb",
      "floodscan",
      "floodscan_flooded_fraction_africa_19980112-20221231_p00",
      "aer_sfed_area_300s_19980112_20221231_v05r01.nc"
    )
  ),
  # need to fix finish this up -- if we have a wrapped raster func
  # it will make our next steps more generic
  # this isn't necessary at this stage.
  # tar_target(
  # name = r_fs,
  # command =  tidync(fp_fs) %>% 
  #   fs_filter_bounds(geometry = lgdf_adm$adm0) %>% 
  #   fs_to_raster(band = "SFED_AREA")
  # ),

  # Zonal Stats World Pop ---------------------------------------------------

  # Notes `mosaic_worlpop()` isa function i made to read in the the different world pop file paths and mosaic the rasters
  # It's wasteful to store raster files as targets so I just do this step in memory.
  tar_target(
    name = df_adm2_worldpop_summary,
    command = exact_extract(
      x = mosaic_worldpop(lr_wp, from_path =F),
      y = lgdf_adm$adm2,
      fun = "sum",
      append_cols = c("adm1_en", "adm1_pcode", "adm2_en", "adm2_pcode")
    ) %>%
      # just tidy up the extracted data by going long
      pivot_longer(-matches("adm")) %>%
      dplyr::select(-name) %>%
      rename(
        pop = "value"
      )
  ),
  tar_target(
    name = df_adm3_worldpop_summary,
    command = exact_extract(
      x = mosaic_worldpop(lr_wp, from_path =F),
      y = lgdf_adm$adm3,
      fun = "sum",
      append_cols = c("adm1_en", "adm1_pcode", "adm2_en", "adm2_pcode", "adm3_en", "adm3_pcode")
    ) %>%
      # just tidy up the extracted data by going long
      pivot_longer(-matches("adm")) %>%
      dplyr::select(-name) %>%
      rename(
        pop = "value"
      )
  ),
  # Zonal Stats - Historical Pop Exposed Flooding ---------------------------------

  ## Admin 2 level ####

  ### Admin 2 - method 1: binary reclassification ####
  
  # need to separate Mozambique from the rest as running into memory issues
  ## Doing this by country and season
  #tar_target(
  #  name = df_adm2_stats_test,
  #  command = c("Kenya", "Ethiopia", "Somalia", "Mozambique") %>%
  #    map_df(
  #      \(country_name){
  #        zonal_pop_exposure(
  #          floodscan_path = fp_fs,
  #          worldpop_inputs = keep_at(lr_wp, country_name),
  #          flood_frac_thresh = 0.2,
  #          binarize_floodscan = T,
  #          adm = lgdf_adm$adm2 %>% 
  #            filter(adm0_en == country_name),
  #          country_seasons = pluck(ssn_by_country, country_name),
  #          cols_keep = c("adm0_en", "adm1_en", "adm1_pcode", "adm2_en", "adm2_pcode")
  #        )
  #        print(paste0("Completed processing for: ", country_name))
  #      }
  #    ) %>%
  #    bind_rows()
  #),

  # Target for Somalia at ADM2 level
  tar_target(
    name = df_som_adm2_stats,
    command = zonal_pop_exposure(
      floodscan_path = fp_fs,
      worldpop_inputs = keep_at(lr_wp, c("Somalia")),
      flood_frac_thresh = 0.005,
      binarize_floodscan = FALSE,
      adm = lgdf_adm[["adm2"]] %>%
        dplyr::filter(adm0_pcode %in% c("SO")),
      country_seasons = c("MAM", "OND", "Annual"),
      cols_keep = c("adm0_en", "adm1_en", "adm1_pcode", "adm2_en", "adm2_pcode")
    )
  ),
  # Target for Ethiopia at ADM2 level
  tar_target(
    name = df_eth_adm2_stats,
    command = zonal_pop_exposure(
      floodscan_path = fp_fs,
      worldpop_inputs = keep_at(lr_wp, c("Ethiopia")),
      flood_frac_thresh = 0.05,
      binarize_floodscan = F,
      adm = lgdf_adm[["adm2"]] %>%
        dplyr::filter(adm0_pcode %in% c("ET")),
      country_seasons = c("MAM", "AMJ", "OND", "Annual", "JJAS"),
      cols_keep = c("adm0_en", "adm1_en", "adm1_pcode", "adm2_en", "adm2_pcode")
    )
  ),
  # Target for Kenya at ADM2 level
  tar_target(
    name = df_ken_adm2_stats,
    command = zonal_pop_exposure(
      floodscan_path = fp_fs,
      worldpop_inputs = keep_at(lr_wp, c("Kenya")),
      flood_frac_thresh = 0.005,
      binarize_floodscan = FALSE,
      adm = lgdf_adm[["adm2"]] %>%
        dplyr::filter(adm0_pcode %in% c("KE")),
      country_seasons = c("MAM", "OND", "Annual"),
      cols_keep = c("adm0_en", "adm1_en", "adm1_pcode", "adm2_en", "adm2_pcode")
    )
  ),
  # Target for Mozambique at ADM2 level
  tar_target(
    name = df_moz_adm2_stats,
    command = zonal_pop_exposure(
      floodscan_path = fp_fs,
      worldpop_inputs = keep_at(lr_wp, c("Mozambique")),
      flood_frac_thresh = 0.005,
      binarize_floodscan = FALSE,
      adm = lgdf_adm[["adm2"]] %>%
        dplyr::filter(adm0_pcode %in% c("MZ")),
      country_seasons = c("MAM", "OND", "Annual"),
      cols_keep = c("adm0_en", "adm1_en", "adm1_pcode", "adm2_en", "adm2_pcode")
    )
  ),
  tar_target(
    name = df_adm2_stats,
    command = bind_rows(
      df_eth_adm2_stats,
      df_som_adm2_stats,
      df_ken_adm2_stats,
      df_moz_adm2_stats
    ) %>%
      mutate(season = case_when(
        month(date) == 1 ~ "Annual",
        month(date) == 3 ~ "MAM",
        month(date) == 4 ~ "AMJ",
        month(date) == 10 ~ "OND",
        month(date) == 6 ~ "JJAS",
        .default = "Other"
      ))
  ),
  # Target for Ethiopia at ADM3 level
  tar_target(
    name = df_eth_adm3_stats,
    command = zonal_pop_exposure(
      floodscan_path = fp_fs,
      worldpop_inputs = keep_at(lr_wp, c("Ethiopia")),
      flood_frac_thresh = 0.05,
      binarize_floodscan = F,
      adm = lgdf_adm[["adm3"]] %>%
        dplyr::filter(adm0_pcode %in% c("ET")),
      country_seasons = c("MAM", "AMJ", "OND", "Annual", "JJAS"),
      cols_keep = c("adm0_en", "adm1_en", "adm1_pcode", "adm2_en", "adm2_pcode", "adm3_en", "adm3_pcode")
    )
  ),
  tar_target(
    name= df_adm3_stats,
    command= bind_rows(df_eth_adm3_stats) %>%
      mutate(season = case_when(
        month(date) == 1 ~ "Annual",
        month(date) == 3 ~ "MAM",
        month(date) == 4 ~ "AMJ",
        month(date) == 10 ~ "OND",
        month(date) == 6 ~ "JJAS",
        .default = "Other"))
  ),
  # Join w/ world pop zonal stats (adm2) to calculate % populations
  tar_target(
    name = df_adm2_stats_bin_method,
    command = left_join(df_adm2_worldpop_summary, df_adm2_stats) %>%
      rename(
        total_pop_wp = pop,
        pop_exposed_wp = pop_exposed
      ) %>%
      mutate(
        pct_exposed = pop_exposed_wp / total_pop_wp
      )
  ),
  tar_target(
    name = df_adm3_stats_bin_method,
    command = left_join(df_adm3_worldpop_summary, df_adm3_stats) %>%
      rename(
        total_pop_wp = pop,
        pop_exposed_wp = pop_exposed
      ) %>%
      mutate(
        pct_exposed = pop_exposed_wp / total_pop_wp
      )
  ),

  ## Admin 1 Level ####
  ### Admin 1 - method 1: binary reclassification ####

  # Can aggregate admin 2 level data to admin 1 directly (no need for rasters)
  tar_target(
    name = df_adm1_stats_bin_method,
    command = df_adm2_stats_bin_method %>%
      group_by(
        across(matches("^adm[01]")), date, season, thresh
      ) %>%
      summarise(
        total_pop_wp = sum(total_pop_wp, na.rm = T),
        pop_exposed_wp = sum(pop_exposed_wp, na.rm = T),
        .groups = "drop"
      ) %>%
      mutate(
        pct_exposed = pop_exposed_wp / total_pop_wp
      )
  ),

  # Temporal Summarization --------------------------------------------------

  ## Admin 1 mean ####
  tar_target(
    name = df_adm1_mean_stat,
    df_adm1_stats_bin_method %>%
      group_by(across(matches("adm")), season, thresh) %>%
      summarise(
        mean_exposed = mean(pop_exposed_wp),
        total_pop = unique(total_pop_wp),
        .groups = "drop"
      ) %>%
      mutate(pct_exposed = mean_exposed / total_pop)
  ),

  ## Admin 2 mean ####
  tar_target(
    name = df_adm2_mean_stat,
    df_adm2_stats_bin_method %>%
      group_by(across(matches("adm")), season, thresh) %>%
      summarise(
        mean_exposed = mean(pop_exposed_wp),
        total_pop = unique(total_pop_wp),
        .groups = "drop"
      ) %>%
      mutate(pct_exposed = mean_exposed / total_pop)
  ),
  ## Admin 3 mean ####
  tar_target(
    name = df_adm3_mean_stat,
    df_adm3_stats_bin_method %>%
      group_by(across(matches("adm")), season) %>%
      summarise(
        mean_exposed = mean(pop_exposed_wp),
        total_pop = unique(total_pop_wp),
        .groups = "drop"
      ) %>%
      mutate(pct_exposed = mean_exposed / total_pop)
  ),

  # Make Summary `{gt}` Tables ---------------------------------------------------
  ## Admin 3 Table ####
  tar_target(
    name = lgt_adm3_bin_method,
    command = df_adm3_mean_stat %>%
      split(.$adm0_en, .$season) %>%
      imap(\(dft, country_name){
        dft %>%
          #dplyr::filter(case_when(
          #  country_name %in% c("Ethiopia", "Somalia") ~ season == "AMJ",
          #  T ~ season == "Annual")) %>%
          dplyr::select(adm0_en, adm1_en, adm2_en, adm3_en, season, mean_exposed, total_pop, pct_exposed) %>%
          group_by(adm1_en) %>%
          gt() %>%
          cols_hide(columns = "adm0_en") %>%
          cols_label(
            # adm0_en = "Country",
            adm1_en = "Region",
            adm2_en = "District",
            adm3_en = "Woreda",
            season = "Time Period",
            mean_exposed = "Average population exposed",
            total_pop = "Total population",
            pct_exposed = "Percent population exposed"
          ) %>%
          fmt_number(
            columns = c(
              #"thresh",
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            decimals = 0, n_sigfig = 3
          ) %>%
          fmt_percent(columns = pct_exposed, decimals = 0) %>%
          #fmt_percent(columns = thresh, decimals = 1) %>%
          data_color(
            columns = c(
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            method = "numeric",
            palette = "YlOrRd",
            reverse = F
          ) %>%
          tab_options(
            row_group.as_column = TRUE
          ) %>%
          tab_header(
            title = paste0("Approximate population susceptible to flooding: ", country_name)
          )
      })
  ),
  ## Admin 2 Table ####
  tar_target(
    name = lgt_adm2_bin_method,
    command = df_adm2_mean_stat %>%
      split(.$adm0_en, .$season) %>%
      imap(\(dft, country_name){
        dft %>%
          #dplyr::filter(case_when(
          #  country_name %in% c("Ethiopia", "Somalia") ~ season == "AMJ",
          #  T ~ season == "Annual")) %>%
          dplyr::select(adm0_en, adm1_en, adm2_en, season, mean_exposed, total_pop, pct_exposed) %>%
          group_by(adm1_en) %>%
          gt() %>%
          cols_hide(columns = "adm0_en") %>%
          cols_label(
            # adm0_en = "Country",
            adm1_en = "Region",
            adm2_en = "District",
            season = "Time Period",
            mean_exposed = "Average population exposed",
            total_pop = "Total population",
            pct_exposed = "Percent population exposed"
          ) %>%
          fmt_number(
            columns = c(
              #"thresh",
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            decimals = 0, n_sigfig = 3
          ) %>%
          fmt_percent(columns = pct_exposed, decimals = 0) %>%
          #fmt_percent(columns = thresh, decimals = 1) %>%
          data_color(
            columns = c(
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            method = "numeric",
            palette = "YlOrRd",
            reverse = F
          ) %>%
          tab_options(
            row_group.as_column = TRUE
          ) %>%
          tab_header(
            title = paste0("Approximate population susceptible to flooding: ", country_name)
          )
      })
  ),

  ## Admin 1 table ####
  tar_target(
    name = lgt_adm1_bin_method,
    command = df_adm1_mean_stat %>%
      split(.$adm0_en, .$season) %>%
      imap(\(dft, country_name){
        dft %>%
          #dplyr::filter(case_when(
          #  country_name %in% c("Ethiopia", "Somalia") ~ season == "AMJ",
          #  T ~ season == "Annual")) %>%
          dplyr::select(adm0_en, adm1_en, season, mean_exposed, total_pop, pct_exposed) %>%
          gt() %>%
          cols_hide(columns = "adm0_en") %>%
          cols_label(
            # adm0_en = "Country",
            adm1_en = "Region",
            season = "Time Period",
            mean_exposed = "Average population exposed",
            total_pop = "Total population",
            pct_exposed = "Percent population exposed"
          ) %>%
          fmt_number(
            columns = c(
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            decimals = 0, n_sigfig = 3
          ) %>%
          fmt_percent(columns = pct_exposed, decimals = 0) %>%
          #fmt_percent(columns = thresh, decimals = 1) %>%
          data_color(
            columns = c(
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            method = "numeric",
            palette = "YlOrRd",
            reverse = F
          ) %>%
          tab_options(
            row_group.as_column = TRUE
          ) %>%
          tab_header(
            title = paste0("Approximate population susceptible to flooding: ", country_name)
          )
      })
  ),
  tar_target(
    name = lgt_adm2_bin_method_annual,
    command = df_adm2_mean_stat %>%
      split(.$adm0_en, .$season) %>%
      imap(\(dft, country_name){
        dft %>%
          dplyr::filter(season == "Annual") %>%
          dplyr::select(adm0_en, adm1_en, adm2_en, thresh, mean_exposed, total_pop, pct_exposed) %>%
          group_by(adm1_en) %>%
          gt() %>%
          cols_hide(columns = "adm0_en") %>%
          cols_label(
            # adm0_en = "Country",
            adm1_en = "Region",
            thresh = "Threshold",
            adm2_en = "District",
            mean_exposed = "Average population exposed",
            total_pop = "Total population",
            pct_exposed = "Percent population exposed"
          ) %>%
          fmt_number(
            columns = c(
              "thresh",
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            decimals = 0, n_sigfig = 3
          ) %>%
          fmt_percent(columns = pct_exposed, decimals = 0) %>%
          fmt_percent(columns = thresh, decimals = 1) %>%
          data_color(
            columns = c(
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            method = "numeric",
            palette = "YlOrRd",
            reverse = F
          ) %>%
          tab_options(
            row_group.as_column = TRUE
          ) %>%
          tab_header(
            title = paste0("Approximate population susceptible to flooding: ", country_name)
          )
      })
  ),
  
  ## Admin 1 table ####
  tar_target(
    name = lgt_adm1_bin_method_annual,
    command = df_adm1_mean_stat %>%
      split(.$adm0_en, .$season) %>%
      imap(\(dft, country_name){
        dft %>%
          dplyr::filter(season == "Annual") %>%
          dplyr::select(adm0_en, adm1_en, mean_exposed, total_pop, pct_exposed) %>%
          gt() %>%
          cols_hide(columns = "adm0_en") %>%
          cols_label(
            # adm0_en = "Country",
            adm1_en = "Region",
            mean_exposed = "Average population exposed",
            total_pop = "Total population",
            pct_exposed = "Percent population exposed"
          ) %>%
          fmt_number(
            columns = c(
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            decimals = 0, n_sigfig = 3
          ) %>%
          fmt_percent(columns = pct_exposed, decimals = 0) %>%
          data_color(
            columns = c(
              "mean_exposed",
              "total_pop",
              "pct_exposed"
            ),
            method = "numeric",
            palette = "YlOrRd",
            reverse = F
          ) %>%
          tab_options(
            row_group.as_column = TRUE
          ) %>%
          tab_header(
            title = paste0("Approximate population susceptible to flooding: ", country_name)
          )
      })
  )
  
  # quick flood scan analysis for cyclones
  #tar_target(
  #  name = moz_adm1_floodscan_unprocessed,
  #  command= zonal_floodscan(floodscan_path = fp_fp,
  #                           geom = lgdf_adm$adm1 %>% filter(adm0_en=="Mozambique"),
  #                           cols_keep = c("adm0_en","adm0_pcode","adm1_en","adm1_pcode"),
  #                           zonal_stat= c("mean","median")
  #                           )
    
  #)
)
