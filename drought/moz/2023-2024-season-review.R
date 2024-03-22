## libraries
library(tidyverse)
library(sf)
library(ggrepel)
library(fitdistrplus)
library(gghdx)
gghdx()

# rainfall
moz_ssn <- c("October", "November", "December", "January", "February", "March")
# reading in data
moz_rain_data <- read_csv("https://data.humdata.org/dataset/641a9118-4488-49e7-a0c6-efe9dc567987/resource/257d0484-49a8-4336-b87a-c4991decf42a/download/moz-rainfall-adm2-full.csv")[-1,]
moz_ndvi_data <- read_csv("https://data.humdata.org/dataset/841f6fa9-7ba3-4e79-a335-db9e2608f8c3/resource/6e130522-c378-43a0-a843-be9256ec6d41/download/moz-ndvi-adm2-full.csv")[-1,]
moz_shp <- st_read(file.path(
  Sys.getenv("AA_DATA_DIR"), 
  "public", "raw", 
  "moz", "cod_ab", 
  "moz_adm.shp.zip"), layer = "moz_admbnda_adm2_ine_20190607")
## aggregating the data to admin 1
moz_rain_adm1 <- moz_rain_data %>%
  left_join((moz_shp %>%
               dplyr::select(ADM2_PCODE, ADM1_PCODE, ADM1_PT)), 
            by = "ADM2_PCODE", suffix = c("", "_shp")) %>%
  mutate(month_total = as.numeric(r1h),
         month_total_avg = as.numeric(r1h_avg),
         month_anom = as.numeric(r1q)) %>%
  group_by(ADM1_PCODE, ADM1_PT, date) %>%
  summarise(adm1_tot_med = median(month_total, na.rm = T),
            adm1_avg_med = median(month_total_avg, na.rm = T),
            adm1_anom_med = median(month_anom, na.rm = T)) %>%
  mutate(dekad_start = substr(date, 9, 10),
         month = substr(date, 6, 7),
         month_name = month.name[as.numeric(month)],
         month_abb = month.abb[as.numeric(month)],
         yr = as.numeric(substr(date, 1, 4)),
         season = case_when(
           month %in% c("10", "11", "12") ~ paste0(yr, "/", yr+1),
           month %in% c("01", "02", "03") ~ paste0(yr-1, "/", yr))) %>%
  filter(dekad_start == "21" & 
           month_name %in% moz_ssn) %>%
  mutate(month_name = factor(month_name, levels = moz_ssn),
         month_abb = factor(month_abb, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")),
         highlighted_ssn = case_when(
           season == "2015/2016" ~ season,
           season == "2022/2023" ~ season,
           season == "2023/2024" ~ season,
           .default = "Long Term Average"),
         ssn_values = case_when(
           season == "2015/2016" ~ adm1_tot_med,
           season == "2022/2023" ~ adm1_tot_med,
           season == "2023/2024" ~ adm1_tot_med,
           .default = adm1_avg_med))

moz_subsets <- split(moz_rain_adm1, moz_rain_adm1$ADM1_PT)
line_width <- c("2015/2016" = 1, "2022/2023" = 1, "2023/2024" = 1, "Long Term Average" = 0.8)

ggplot(data = moz_rain_adm1) +
  geom_line(aes(x=month_abb, y=adm1_tot_med, 
                group=season, color=highlighted_ssn, 
                size=highlighted_ssn)) +
  scale_color_manual(values = c("maroon", "steelblue", "#8B8000", "darkgrey")) +
  scale_size_manual(values = line_width, guide = "none") +
  labs(title="Total Monthly Rainfall by Province", 
       x="Month", y="Total Monthly Rainfall (mm)", color = "Seasons") + 
  facet_wrap(~ADM1_PT) + 
  guides(color = guide_legend(override.aes = list(size = 10)), size = "none")

ggplot(data = moz_rain_adm1) +
  geom_line(aes(x=month_abb, y=ssn_values, 
                group=highlighted_ssn, color=highlighted_ssn, 
                size=highlighted_ssn)) +
  scale_color_manual(values = c("maroon", "steelblue", "#8B8000", "grey")) +
  scale_size_manual(values = line_width, guide = "none") +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(title="Total Monthly Rainfall by Province", 
       x="Month", y="Total Monthly Rainfall (mm)", color = "Seasons") + 
  facet_wrap(~ADM1_PT)


map(moz_subsets, 
    ~ ggplot(.x) +
      geom_line(aes(x=month_name, y=adm1_tot_med, 
                    group=season, color=highlighted_ssn, size=highlighted_ssn)) +
      scale_color_manual(values = c("maroon", "steelblue", "#8B8000", "grey")) +
      scale_size_manual(values = line_width, guide = "none") +
      labs(title=paste("Total Monthly Rainfall for ", unique(.x$ADM1_PT)), 
           x="Month", y="Total Monthly Rainfall (mm)",
           color = "Seasons") +
      guides(colour = guide_legend(override.aes = list(size = 3)))) 


## looking at anomalies
moz_rain_adm1 %>%
  filter(season == "2023/2024") %>%
  ggplot() +
    geom_line(aes(x=month_name, y=adm1_anom_med, 
                  group=ADM1_PT, color=ADM1_PT), size = 1) +
    geom_text_repel(data = moz_rain_adm1 %>%
                filter(season == "2023/2024") %>%
                filter(date == "2024-02-21"), 
              aes(x = "February", y = adm1_anom_med, label = ADM1_PT), 
            size = 4, hjust = 0, color = "black",
            position = position_nudge(x = 0.02, y = 0.3)) +
    labs(title="Monthly Rainfall Anomalies by Province", 
         x="Month", y="Monthly Rainfall Anomalies (%)", color = "Provinces") +
    guides(size = "none")

## ranking seasons based on total rainfall by month and overall
moz_rank <- moz_rain_adm1 %>%
  filter(month_name != "March") %>%
  group_by(ADM1_PT, season) %>%
  summarise(adm1_ssn_avg = sum(adm1_tot_med, na.rm = T)) %>%
  group_by(season) %>%
  summarise(moz_ssn_avg = median(adm1_ssn_avg, na.rm = T)) %>%
  mutate(rank = rank(moz_ssn_avg))

ggplot(moz_rank) + 
  geom_bar(aes(x=season, y=moz_ssn_avg), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title=paste("Average Seasonal Rainfall"), 
       x="Season", y="Average Season Rainfall (mm)")
  
View(moz_rain_adm1 %>%
  group_by(ADM1_PT, season) %>%
  summarise(adm1_ssn_avg = sum(adm1_tot_med, na.rm = T)))

adm1_rank <- moz_rain_adm1 %>%
  filter(month_name != "March") %>%
  group_by(ADM1_PT, season) %>%
  summarise(adm1_ssn_avg = sum(adm1_tot_med, na.rm = T)) %>%
  mutate(rank = rank(adm1_ssn_avg),
         mean = mean(adm1_ssn_avg),
         sd = sd(adm1_ssn_avg),
         prob = pnorm(adm1_ssn_avg, mean = mean, sd = sd),
         rp = paste("1 in ", round(1/prob), " yr"),
         rp_rank = paste("1 in ", round(45 / rank), " yr"))

## NDVI
mon_dekad <- paste(rep(substr(moz_ssn, 1, 3), 
                       each = length(c("01", "11", "21"))), 
                   c("01", "11", "21"))
moz_ndvi_adm1 <- moz_ndvi_data %>%
  left_join((moz_shp %>%
               dplyr::select(ADM2_PCODE, ADM1_PCODE, ADM1_PT)), 
            by = "ADM2_PCODE", suffix = c("", "_shp")) %>%
  mutate(dekad_val = as.numeric(vim),
         dekad_lta = as.numeric(vim_avg),
         dekad_anom = as.numeric(viq)) %>%
  group_by(ADM1_PCODE, ADM1_PT, date) %>%
  summarise(adm1_val_med = median(dekad_val, na.rm = T),
            adm1_lta_med = median(dekad_lta, na.rm = T),
            adm1_anom_med = median(dekad_anom, na.rm = T)) %>%
  mutate(dekad_start = substr(date, 9, 10),
         month = substr(date, 6, 7),
         month_name = month.name[as.numeric(month)],
         month_abb = month.abb[as.numeric(month)],
         yr = as.numeric(substr(date, 1, 4)),
         season = case_when(
           month %in% c("10", "11", "12") ~ paste0(yr, "/", yr+1),
           month %in% c("01", "02", "03") ~ paste0(yr-1, "/", yr))) %>%
  filter(month_name %in% moz_ssn) %>%
  mutate(month_name = factor(month_name, levels = moz_ssn),
         month_abb = factor(month_abb, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")),
         highlighted_ssn = case_when(
           season == "2015/2016" ~ season,
           season == "2022/2023" ~ season,
           season == "2023/2024" ~ season,
           .default = "Long Term Average"),
         ssn_values = case_when(
           season == "2015/2016" ~ adm1_val_med,
           season == "2022/2023" ~ adm1_val_med,
           season == "2023/2024" ~ adm1_val_med,
           .default = adm1_lta_med),
         mon_dekad = factor(paste(substr(month_name, 1, 3), dekad_start), 
                            levels = mon_dekad))

ndvi_subsets <- split(moz_ndvi_adm1, moz_ndvi_adm1$ADM1_PT)
x_labs <- mon_dekad[seq(1, length(mon_dekad), by = 3)]

ggplot(data = moz_ndvi_adm1) +
  geom_line(aes(x=mon_dekad, y=adm1_val_med, 
                group=season, color=highlighted_ssn, 
                size=highlighted_ssn)) +
  scale_color_manual(values = c("maroon", "steelblue", "#8B8000", "grey")) +
  scale_size_manual(values = line_width, guide = "none") +
  labs(title="NDVI Over the Season by Province", 
       x="Date", y="NDVI", color = "Seasons") + 
  scale_x_discrete(breaks = x_labs) +
  facet_wrap(~ADM1_PT)

ggplot(data = moz_ndvi_adm1) +
  geom_line(aes(x=mon_dekad, y=ssn_values, 
                group=highlighted_ssn, color=highlighted_ssn, 
                size=highlighted_ssn)) +
  scale_color_manual(values = c("maroon", "steelblue", "#8B8000", "darkgrey")) +
  scale_size_manual(values = line_width, guide = "none") +
  labs(title="NDVI Over the Season by Province", 
       x="Date", y="NDVI", color = "Seasons") + 
  scale_x_discrete(breaks = x_labs) +
  facet_wrap(~ADM1_PT)


map(ndvi_subsets, 
    ~ ggplot(.x) +
      geom_line(aes(x=mon_dekad, y=adm1_val_med, 
                    group=season, color=highlighted_ssn, size=highlighted_ssn)) +
      scale_color_manual(values = c("maroon", "steelblue", "#8B8000", "grey")) +
      scale_size_manual(values = line_width, guide = "none") +
      labs(title=paste("NDVI Over the Season for ", unique(.x$ADM1_PT)), 
           x="Date", y="NDVI",
           color = "Seasons") +
      scale_x_discrete(breaks = x_labs) +
      guides(colour = guide_legend(override.aes = list(size = 3)))) 

# anomalies
moz_ndvi_adm1 %>%
  filter(season == "2023/2024") %>%
  ggplot() +
  geom_line(aes(x=mon_dekad, y=adm1_anom_med, 
                group=ADM1_PT, color=ADM1_PT), size = 1) +
  labs(title="NDVI Over the 2023/2024 Season by Province", 
       x="Date", y="NDVI Anomaly", color = "Provinces") +
  scale_x_discrete(breaks = x_labs) +
  guides(size = "none")

## ranking seasons based on total rainfall by month and overall
ndvi_rank <- moz_ndvi_adm1 %>%
  filter(month_name != "March") %>%
  group_by(ADM1_PT, season) %>%
  summarise(adm1_ssn_avg = mean(adm1_val_med, na.rm = T)) %>%
  group_by(season) %>%
  summarise(moz_ssn_avg = median(adm1_ssn_avg, na.rm = T)) %>%
  mutate(rank = rank(moz_ssn_avg))

ggplot(ndvi_rank) + 
  geom_bar(aes(x=season, y=moz_ssn_avg), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title=paste("Average NDVI"), 
       x="Season", y="Average NDVI")

adm1_ndvi <- moz_ndvi_adm1 %>%
  filter(month_name != "March") %>%
  group_by(ADM1_PT, season) %>%
  summarise(adm1_ssn_avg = mean(adm1_val_med, na.rm = T)) %>%
  mutate(rank = rank(adm1_ssn_avg),
         mean = mean(adm1_ssn_avg),
         sd = sd(adm1_ssn_avg),
         prob = pnorm(adm1_ssn_avg, mean = mean, sd = sd),
         rp = paste("1 in ", round(1/prob), " yr"),
         rp_rank = paste("1 in ", round(23 / rank), " yr"))

adm1_dkd_ndvi <- moz_ndvi_adm1 %>%
  filter(mon_dekad == "Feb 21") %>%
  group_by(ADM1_PT, mon_dekad) %>%
  #summarise(adm1_ssn_avg = mean(adm1_val_med, na.rm = T)) %>%
  mutate(rank = rank(adm1_val_med),
         mean = mean(adm1_val_med),
         sd = sd(adm1_val_med),
         prob = pnorm(adm1_val_med, mean = mean, sd = sd),
         rp = paste("1 in ", round(1/prob), " yr"),
         rp_rank = paste("1 in ", round(23 / rank), " yr"))
