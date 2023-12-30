### R code for making some maps from IPC data for Southern Africa
### setup
library(tidyverse)
library(sf)
library(gghdx)
library(countrycode)

# reading in data
shp_path <- file.path(
  Sys.getenv("AA_DATA_DIR"), 
  "public", "raw", "esa", "cod_ab", 
  "esa_admin1_region", "ESA_admin1_region.shp")
esa_shp <- st_read(shp_path)
ipc_data <- read_csv("https://data.humdata.org/dataset/7a7e7428-b8d7-4d2e-91d3-19100500e016/resource/f6e7954c-3717-487a-b0d9-787881831634/download/ipc_global_level1_long.csv")
ipc_data_latest <- read_csv("https://data.humdata.org/dataset/7a7e7428-b8d7-4d2e-91d3-19100500e016/resource/741a6164-c8b8-413b-b021-53d250a5814b/download/ipc_global_level1_long_latest.csv")
ipc_data_wide <- read_csv("https://data.humdata.org/dataset/7a7e7428-b8d7-4d2e-91d3-19100500e016/resource/6c61ca6f-bd79-4956-9887-624bd38d9daa/download/ipc_global_national_wide_latest.csv")
gghdx()
options(scipen = 999)
sf_use_s2(FALSE)

# getting country codes and names
esa_iso2 <- c("AO", "SZ", "LS", "MG", "MW", "MZ", "NA", "MW", "MZ", "ZA", "ZM", "ZW")
esa_iso3 <- countrycode(esa_iso2, origin = "iso2c", destination = "iso3c")
esa_countries <- countrycode(esa_iso2, origin = "iso2c", destination = "country.name")

# filtering for only SA countries
sa_shp <- esa_shp %>%
  filter(COUNTRY %in% esa_countries) %>%
  group_by(COUNTRY) %>% 
  summarize(geometry = st_union(geometry))

sa_country_shp <- esa_shp %>%
  filter(COUNTRY %in% esa_countries) %>%
  # group_by(COUNTRY) %>% 
  # summarize(geometry = st_union(geometry)) %>%
  mutate(iso3 = countrycode(COUNTRY, origin = "country.name", destination = "iso3c")) %>%
  unite(admin_id, COUNTRY, Admin, sep = "_", remove = FALSE)

# Phase 4 equals 4+ as Phase 5 are zero
sa_ipc_data <- ipc_data_latest %>%
  filter(Country %in% esa_iso3  & From >= "2023-01-01") %>%
  mutate(country_name = countrycode(Country, origin = "iso3c", destination = "country.name")) %>%
  unite(admin_id, country_name, `Level 1`, sep = "_", remove = FALSE) %>%
  mutate(admin_fixed = case_when(admin_id %in% sa_country_shp$admin_id ~ admin_id,
                                 admin_id == "Zambia_Muchiga" ~ "Zambia_Northern",
                                 admin_id == "Mozambique_Zambezia" ~ "Mozambique_Zambézia",
                                 admin_id == "Madagascar_Vatovavy" ~ "Madagascar_Fianarantsoa",
                                 admin_id == "Madagascar_Fitovivany" ~ "Madagascar_Fianarantsoa",
                                 admin_id == "Madagascar_Androy" ~ "Madagascar_Toliara",
                                 admin_id == "Madagascar_Anosy" ~ "Madagascar_Toliara",
                                 admin_id == "Madagascar_Atsimo Andrefana" ~ "Madagascar_Toliara",
                                 admin_id == "Madagascar_Atsimo Atsinanana" ~ "Madagascar_Fianarantsoa",
                                 .default = ""),
         percent_pop = (as.numeric(Number) / as.numeric(`Total country population`))*100)
  

sa_country_ipc <- merge(sa_country_shp, sa_ipc_data, by.x = "admin_id", by.y = "admin_fixed", all = TRUE)

### Phase 3+ plot for current projections
total_plot_fxn <- function(geodata1, geodata2, phase, period, text){
  
  # plot
  phase_df <- geodata1 %>%
    filter((Phase == phase | is.na(phase)) & (`Validity period` == period | is.na(`Validity period`))) %>%
    mutate(Number_t = as.numeric(Number))
  
  ggplot() +
    geom_sf(data = phase_df, aes(fill = Number_t)) +
    geom_sf(data = geodata2, fill = "transparent", linewidth=1) +
    geom_sf_text(data = geodata2, aes(label = COUNTRY)) +
    annotate(geom = "text", x = 40, y = -35, label = text, 
             color = "grey22", size = 4) +
    # geom_sf_label(aes(label = country_name)) +
    ggtitle(label = paste0("Acute Food Insecurity IPC Phase ", phase, " (", str_to_title(period), ")"), 
            subtitle = "Population at Risk") +
    scale_fill_gradient(low="pink", high="red", 
                        labels = scales::label_number(scale = 1e-3, suffix = "k")) +
    labs(fill = "Population", x = "", y = "")
  
}

# 3+ and current period
total_plot_fxn(geodata1 = sa_country_ipc, geodata2 = sa_shp, 
         phase = "3+", period = "current", 
         text = "Zambia - Aug 2023 to Sep 2023\nMadagascar - May 2023 to Sep 2023\nMozambique - Jul 2023 to Sep 2023")

# 4+ and current period
total_plot_fxn(geodata1 = sa_country_ipc, geodata2 = sa_shp, 
         phase = "4", period = "current", 
         text = "Zambia - Aug 2023 to Sep 2023\nMadagascar - May 2023 to Sep 2023\nMozambique - Jul 2023 to Sep 2023")

# 3+ and projected period
total_plot_fxn(geodata1 = sa_country_ipc, geodata2 = sa_shp, 
         phase = "3+", period = "projected", 
         text = "Zambia - Oct 2023 to Mar 2024\nMadagascar - Oct 2023 to Dec 2023\nMozambique - Oct 2023 to Mar 2024")

# 4+ and projected period
total_plot_fxn(geodata1 = sa_country_ipc, geodata2 = sa_shp, 
         phase = "4", period = "projected", 
         text = "Zambia - Oct 2023 to Mar 2024\nMadagascar - Oct 2023 to Dec 2023\nMozambique - Oct 2023 to Mar 2024")

### Percent of Population
### Phase 3+ plot for current projections
perc_plot_fxn <- function(geodata1, geodata2, phase, period, text){
  
  # plot
  phase_df <- geodata1 %>%
    filter((Phase == phase | is.na(phase)) & (`Validity period` == period | is.na(`Validity period`))) 
  
  ggplot() +
    geom_sf(data = phase_df, aes(fill = percent_pop)) +
    geom_sf(data = geodata2, fill = "transparent", linewidth=1) +
    geom_sf_text(data = geodata2, aes(label = COUNTRY)) +
    annotate(geom = "text", x = 40, y = -35, label = text, 
             color = "grey22", size = 4) +
    # geom_sf_label(aes(label = country_name)) +
    ggtitle(label = paste0("Acute Food Insecurity IPC Phase ", phase, " (", str_to_title(period), ")"), 
            subtitle = "Percent of Population at Risk") +
    scale_fill_gradient(low="pink", high="red",
                        labels = scales::label_percent(scale = 1)) +
    labs(fill = "% of Population", x = "", y = "")
  
}

# 3+ and current period
perc_plot_fxn(geodata1 = sa_country_ipc, geodata2 = sa_shp, 
               phase = "3+", period = "current", 
               text = "Zambia - Aug 2023 to Sep 2023\nMadagascar - May 2023 to Sep 2023\nMozambique - Jul 2023 to Sep 2023")

# 4+ and current period
perc_plot_fxn(geodata1 = sa_country_ipc, geodata2 = sa_shp, 
               phase = "4", period = "current", 
               text = "Zambia - Aug 2023 to Sep 2023\nMadagascar - May 2023 to Sep 2023\nMozambique - Jul 2023 to Sep 2023")

# 3+ and projected period
perc_plot_fxn(geodata1 = sa_country_ipc, geodata2 = sa_shp, 
               phase = "3+", period = "projected", 
               text = "Zambia - Oct 2023 to Mar 2024\nMadagascar - Oct 2023 to Dec 2023\nMozambique - Oct 2023 to Mar 2024")

# 4+ and projected period
perc_plot_fxn(geodata1 = sa_country_ipc, geodata2 = sa_shp, 
               phase = "4", period = "projected", 
               text = "Zambia - Oct 2023 to Mar 2024\nMadagascar - Oct 2023 to Dec 2023\nMozambique - Oct 2023 to Mar 2024")

