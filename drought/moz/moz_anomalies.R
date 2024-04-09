library(tidyverse)
library(sf)
library(gghdx)
gghdx()
options(scipen = 99999)
prec_file <- read_csv("https://data.humdata.org/dataset/641a9118-4488-49e7-a0c6-efe9dc567987/resource/351016c4-6e0f-42a5-8bf7-4162cba6ad2e/download/moz-rainfall-adm2-5ytd.csv",
                      skip=0)
prec_file <- prec_file[-1,] %>%
  mutate(r3h_anm = as.numeric(r3h) - as.numeric(r3h_avg))

moz_shp <- st_read(
    file.path(
    Sys.getenv("AA_DATA_DIR"), 
    "public", "raw", "moz", "cod_ab", 
    "moz_admbnda_adm2_ine_20190607.shp"))

bin_breaks <- c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)
col_vals_fn <- colorRampPalette(c("red", "white", "navyblue"))
col_vals <- setNames(col_vals_fn(length(bin_breaks)-1), cut(bin_breaks, breaks = bin_breaks)[-1])

plot_fxn <- function(moz_shp, prec_file, subtitle, mon_date){
    data_df <- prec_file %>%
        filter(date == mon_date) %>%
        merge(moz_shp, by = "ADM2_PCODE", all = TRUE) %>%
        mutate(r3q_binned = cut(as.numeric(r3q), breaks = bin_breaks))
    ggplot() +
        geom_sf(data = data_df, aes(geometry = geometry, fill = r3q_binned)) + 
        ggtitle(label = "3-Month Rainfall Anomalies in Percentage",
                subtitle = subtitle) +
        scale_fill_manual(values = col_vals, na.value = "white") +
        labs(fill = "3-month anomaly in %") + 
        theme(legend.key.width = unit(0.5, "cm"))
}
plot_fxn(moz_shp, prec_file, 
         subtitle = "November 2023 to February 2024", 
         mon_date = "2024-02-11")

plot_fxn(moz_shp, prec_file, 
         subtitle = "November 2023 to February 2024", 
         mon_date = "2024-02-01")

plot_fxn(moz_shp, prec_file, 
         subtitle = "November 2023 to January 2024", 
         mon_date = "2024-01-21")

plot_fxn(moz_shp, prec_file, 
         subtitle = "October 2023 to January 2024", 
         mon_date = "2024-01-11")

plot_fxn(moz_shp, prec_file, 
         subtitle = "October 2023 to January 2024", 
         mon_date = "2024-01-01")

plot_fxn(moz_shp, prec_file, 
         subtitle = "October 2023 to December 2023", 
         mon_date = "2023-12-21")

#bin_breaks <- c(0, 50, 100, 150, 200, 300, 400, 500, 650, 800, 1000)
col_vals_fn <- colorRampPalette(c("red", "white", "navyblue"))
#col_vals <- setNames(col_vals_fn(length(bin_breaks)-1), cut(bin_breaks, breaks = bin_breaks)[-1])

plot_fxn <- function(moz_shp, prec_file, subtitle, mon_date){
  data_df <- prec_file %>%
    filter(date == mon_date) %>%
    merge(moz_shp, by = "ADM2_PCODE", all = TRUE) %>%
    mutate(r3q_binned = cut(as.numeric(r3h_avg), breaks = bin_breaks))
  ggplot() +
    geom_sf(data = data_df, aes(geometry = geometry, fill = as.numeric(r3h_avg))) + 
    ggtitle(label = "Long Term Average in mm",
            subtitle = subtitle) +
    scale_fill_gradient2(low="#bb3f3f", mid="white", high = "#49759c", midpoint = 400, na.value = "white") +
    labs(fill = "long term average [mm] ") + 
    theme(legend.key.width = unit(1.4, "cm"))
}
plot_fxn(moz_shp, prec_file, 
         subtitle = "November to February", 
         mon_date = "2024-02-11")


plot_fxn <- function(moz_shp, prec_file, subtitle, mon_date){
  data_df <- prec_file %>%
    filter(date == mon_date) %>%
    merge(moz_shp, by = "ADM2_PCODE", all = TRUE) %>%
    mutate(r3q_binned = cut(as.numeric(r3h_anm), breaks = bin_breaks))
  ggplot() +
    geom_sf(data = data_df, aes(geometry = geometry, fill = as.numeric(r3h_anm))) + 
    ggtitle(label = "3-Month Anomalies in mm",
            subtitle = subtitle) +
    scale_fill_gradient2(low="#bb3f3f", mid="white", high = "#49759c", midpoint = 0, na.value = "white") +
    labs(fill = "3-month absolute anomalies [mm] ") + 
    theme(legend.key.width = unit(1.4, "cm"))
}
plot_fxn(moz_shp, prec_file, 
         subtitle = "November 2023 to February 2024", 
         mon_date = "2024-02-11")
