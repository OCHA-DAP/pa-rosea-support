library(tidyverse)
library(cumulus)
library(DBI)
con <- pg_con()
df_lookup <- cumulus::blob_load_admin_lookup()
df_lookup_aoi <- df_lookup |> 
  filter(
    ADM_LEVEL ==1,
    ISO3 == "MOZ",
    ADM1_NAME %in% c("Maputo","Sofala","Gaza")
  ) 

DBI::dbListTables(con)

imerg <- tbl(con, "imerg") |> 
  filter(
    adm_level == 1, 
    pcode %in% df_lookup_aoi$ADM1_PCODE
  ) |> 
  collect()

imerg <- imerg |>
  left_join(
    df_lookup_aoi |>
      select(pcode =ADM1_PCODE, adm1_name = ADM1_NAME)

  ) |>
  select(iso3, pcode, adm1_name,everything())

# Calculate 3-day rolling mean per ADM1
imerg_rolling <- imerg |>
  arrange(adm1_name, valid_date) |>
  group_by(adm1_name) |>
  mutate(
    precip_roll3 = zoo::rollmean(mean, k = 3, fill = NA, align = "right")
  ) |>
  ungroup()

# Plot the 3-day rolling mean over time
ggplot(imerg_rolling, aes(x = valid_date, y = precip_roll3, color = adm1_name)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "3-Day Rolling Mean Precipitation by Province",
    subtitle = "Mozambique (Maputo, Sofala, Gaza)",
    x = "Date",
    y = "Precipitation (mm, 3-day rolling mean)",
    color = "Province"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  facet_wrap(~adm1_name)

# Return period analysis per ADM1
# Calculate empirical return periods using Weibull plotting position: T = (n+1)/rank

return_period_analysis <- imerg_rolling |>

  filter(!is.na(precip_roll3)) |>
  group_by(adm1_name) |>
  mutate(
    n = n(),
    rank = rank(-precip_roll3, ties.method = "first"),  # rank from highest
    return_period = (n + 1) / rank,
    exceedance_prob = rank / (n + 1)
  ) |>
  ungroup()

# Get the latest 2 days per ADM1
latest_2_days <- return_period_analysis |>
  group_by(adm1_name) |>
  slice_max(valid_date, n = 2) |>
  select(adm1_name, valid_date, precip_roll3, rank, n, return_period, exceedance_prob) |>
  arrange(adm1_name, desc(valid_date))

print("Return Period Analysis - Latest 2 Days per Province:")
print(latest_2_days)

# Plot return period curve with latest 2 days highlighted
ggplot(return_period_analysis, aes(x = return_period, y = precip_roll3)) +

geom_point(alpha = 0.3, size = 1) +
  geom_point(
    data = latest_2_days,
    aes(color = as.factor(valid_date)),
    size = 3
  ) +
  geom_text(
    data = latest_2_days,
    aes(label = format(valid_date, "%b %d"), color = as.factor(valid_date)),
    vjust = -1, size = 3
  ) +
  scale_x_log10() +
  facet_wrap(~adm1_name, scales = "free_y") +
  labs(
    title = "Return Period Analysis - 3-Day Rolling Mean Precipitation",
    subtitle = "Latest 2 days highlighted",
    x = "Return Period (days)",
    y = "Precipitation (mm, 3-day rolling mean)",
    color = "Date"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Annual maxima-based return period analysis (standard approach)
# Extract annual maximum 3-day rolling precip per ADM1
annual_maxima <- imerg_rolling |>
  filter(!is.na(precip_roll3)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm1_name, year) |>
  slice_max(precip_roll3, n = 1) |>
  ungroup()

# Calculate return periods based on annual maxima
annual_maxima_rp <- annual_maxima |>
  group_by(adm1_name) |>
  mutate(
    n_years = n(),
    rank = rank(-precip_roll3, ties.method = "first"),
    return_period_years = (n_years + 1) / rank
  ) |>
  ungroup()

# Get the latest 2 days and find where they fall in the annual maxima distribution
latest_2_days_values <- imerg_rolling |>
  filter(!is.na(precip_roll3)) |>
  group_by(adm1_name) |>
  slice_max(valid_date, n = 3) |>
  select(adm1_name, valid_date, precip_roll3) |>
  ungroup()

# For each latest day, estimate return period by interpolating against annual maxima
latest_rp <- latest_2_days_values |>
  left_join(
    annual_maxima_rp |>
      select(adm1_name, annual_max = precip_roll3, return_period_years) |>
      group_by(adm1_name) |>
      arrange(desc(annual_max)) |>
      mutate(row_id = row_number()),
    by = "adm1_name",
    relationship = "many-to-many"
  ) |>
  group_by(adm1_name, valid_date, precip_roll3) |>
  summarise(
    # Find how many annual maxima the current value exceeds
    n_years_exceeded = sum(precip_roll3 > annual_max),
    n_total_years = n(),
    # Rank = how many annual maxima are >= this value (i.e., n - n_exceeded)
    # Return period = (n + 1) / rank
    estimated_rp_years = (n_total_years + 1) / pmax(1, n_total_years - n_years_exceeded),
    .groups = "drop"
  )

print("Annual Maxima-Based Return Period Analysis - Latest 2 Days:")
print(latest_rp)

# Get top 10 years per ADM1 for labeling
top_10_years <- annual_maxima_rp |>
  group_by(adm1_name) |>
  slice_min(rank, n = 10) |>
  ungroup()

# Plot: annual maxima return period curve with latest 2 days
ggplot(annual_maxima_rp, aes(x = return_period_years, y = precip_roll3)) +
  geom_point(size = 2) +
  geom_line() +
  geom_text(
    data = top_10_years,
    aes(label = paste0(substr(year, 3, 4), "'")),
    hjust = -0.3, vjust = 0.5, size = 2.5
  ) +
  geom_hline(
    data = latest_2_days_values,
    aes(yintercept = precip_roll3, color = as.factor(valid_date)),
    linetype = "dashed", linewidth = 1
  ) +
  geom_label(
    data = latest_rp,
    aes(x = Inf, y = precip_roll3,
        label = paste0(format(valid_date, "%b %d"), " ~", round(estimated_rp_years, 1), " yr"),
        color = as.factor(valid_date)),
    hjust = 1.05, vjust = 0.5, size = 2.5, label.size = 0, fill = "white", alpha = 0.8
  ) +
  scale_x_log10() +
  facet_wrap(~adm1_name, scales = "free_y") +
  labs(
    title = "Return Period Analysis (Annual Maxima Method)",
    subtitle = "3-Day Rolling Mean Precipitation - Latest 2 days shown as dashed lines",
    x = "Return Period (years)",
    y = "Precipitation (mm, 3-day rolling mean)",
    color = "Recent Date"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ---- Rolling Max Version ----

# Calculate 3-day rolling max per ADM1
imerg_rolling_max <- imerg |>
  arrange(adm1_name, valid_date) |>
  group_by(adm1_name) |>
  mutate(
    precip_roll3_max = zoo::rollmax(mean, k = 3, fill = NA, align = "right")
  ) |>
  ungroup()

# Annual maxima of rolling max
annual_maxima_max <- imerg_rolling_max |>
  filter(!is.na(precip_roll3_max)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm1_name, year) |>
  slice_max(precip_roll3_max, n = 1) |>
  ungroup()

# Calculate return periods based on annual maxima
annual_maxima_rp_max <- annual_maxima_max |>
  group_by(adm1_name) |>
  mutate(
    n_years = n(),
    rank = rank(-precip_roll3_max, ties.method = "first"),
    return_period_years = (n_years + 1) / rank
  ) |>
  ungroup()

# Get the latest 3 days
latest_days_max <- imerg_rolling_max |>
  filter(!is.na(precip_roll3_max)) |>
  group_by(adm1_name) |>
  slice_max(valid_date, n = 3) |>
  select(adm1_name, valid_date, precip_roll3_max) |>
  ungroup()

# Estimate return period for latest days
latest_rp_max <- latest_days_max |>
  left_join(
    annual_maxima_rp_max |>
      select(adm1_name, annual_max = precip_roll3_max, return_period_years),
    by = "adm1_name",
    relationship = "many-to-many"
  ) |>
  group_by(adm1_name, valid_date, precip_roll3_max) |>
  summarise(
    n_years_exceeded = sum(precip_roll3_max > annual_max),
    n_total_years = n(),
    estimated_rp_years = (n_total_years + 1) / pmax(1, n_total_years - n_years_exceeded),
    .groups = "drop"
  )

print("Annual Maxima-Based Return Period Analysis (Rolling Max) - Latest 3 Days:")
print(latest_rp_max)

# Get top 10 years per ADM1 for labeling
top_10_years_max <- annual_maxima_rp_max |>
  group_by(adm1_name) |>
  slice_min(rank, n = 10) |>
  ungroup()

# Plot: annual maxima return period curve (rolling max)
ggplot(annual_maxima_rp_max, aes(x = return_period_years, y = precip_roll3_max)) +
  geom_point(size = 2) +
  geom_line() +
  geom_text(
    data = top_10_years_max,
    aes(label = paste0(substr(year, 3, 4), "'")),
    hjust = -0.3, vjust = 0.5, size = 2.5
  ) +
  geom_hline(
    data = latest_days_max,
    aes(yintercept = precip_roll3_max, color = as.factor(valid_date)),
    linetype = "dashed", linewidth = 1
  ) +
  geom_label(
    data = latest_rp_max,
    aes(x = Inf, y = precip_roll3_max,
        label = paste0(format(valid_date, "%b %d"), " ~", round(estimated_rp_years, 1), " yr"),
        color = as.factor(valid_date)),
    hjust = 1.05, vjust = 0.5, size = 2.5, label.size = 0, fill = "white", alpha = 0.8
  ) +
  scale_x_log10() +
  facet_wrap(~adm1_name, scales = "free_y") +
  labs(
    title = "Return Period Analysis (Annual Maxima Method)",
    subtitle = "3-Day Rolling Max Precipitation - Latest 3 days shown as dashed lines",
    x = "Return Period (years)",
    y = "Precipitation (mm, 3-day rolling max)",
    color = "Recent Date"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Quick plot: last 2 months of daily rainfall
imerg_recent <- imerg |>
  filter(valid_date >= max(valid_date) - 60)

# Get top 3 days per province
top_3_days <- imerg_recent |>
  group_by(adm1_name) |>
  slice_max(mean, n = 3) |>
  ungroup()

ggplot(imerg_recent, aes(x = valid_date, y = mean, color = adm1_name)) +
  geom_line() +
  geom_point(size = 1.5) +
  geom_point(data = top_3_days, size = 3) +
  geom_text(
    data = top_3_days,
    aes(label = format(valid_date, "%b %d")),
    vjust = -1, size = 2.5
  ) +
  facet_wrap(~adm1_name) +
  labs(
    title = "Daily Rainfall - Last 2 Months",
    x = "Date",
    y = "Precipitation (mm)",
    color = "Province"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 7-Day Rolling Sum Analysis for Jan 2026 ----

# Calculate 7-day rolling sum for all data
imerg_roll7_sum <- imerg |>
  arrange(adm1_name, valid_date) |>
  group_by(adm1_name) |>
  mutate(
    precip_roll7_sum = zoo::rollsum(mean, k = 7, fill = NA, align = "right"),
    start_date = valid_date - 6  # start date of 7-day window
  ) |>
  ungroup()

# Get max 7-day rolling sum in Jan 2026 per province
jan_2026_max_7day <- imerg_roll7_sum |>
  filter(
    !is.na(precip_roll7_sum),
    year(valid_date) == 2026,
    month(valid_date) == 1
  ) |>
  group_by(adm1_name) |>
  slice_max(precip_roll7_sum, n = 1) |>
  ungroup()

print("Max 7-Day Rolling Sum in January 2026 per Province:")
print(jan_2026_max_7day |> select(adm1_name, start_date, valid_date, precip_roll7_sum))

# Annual maxima of 7-day rolling sum (for RP calculation)
annual_maxima_roll7_sum <- imerg_roll7_sum |>
  filter(!is.na(precip_roll7_sum)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm1_name, year) |>
  slice_max(precip_roll7_sum, n = 1) |>
  ungroup()

# Calculate return periods
annual_maxima_rp_roll7_sum <- annual_maxima_roll7_sum |>
  group_by(adm1_name) |>
  mutate(
    n_years = n(),
    rank = rank(-precip_roll7_sum, ties.method = "first"),
    return_period_years = (n_years + 1) / rank
  ) |>
  ungroup()

# Estimate return period for Jan 2026 max
jan_2026_rp_7day <- jan_2026_max_7day |>
  left_join(
    annual_maxima_rp_roll7_sum |>
      select(adm1_name, annual_max = precip_roll7_sum),
    by = "adm1_name",
    relationship = "many-to-many"
  ) |>
  group_by(adm1_name, start_date, valid_date, precip_roll7_sum) |>
  summarise(
    n_years_exceeded = sum(precip_roll7_sum > annual_max),
    n_total_years = n(),
    estimated_rp_years = (n_total_years + 1) / pmax(1, n_total_years - n_years_exceeded),
    .groups = "drop"
  )

print("Return Period Analysis - Max 7-Day Rolling Sum in Jan 2026:")
print(jan_2026_rp_7day)

# Get top 10 years for labeling
top_10_years_roll7_sum <- annual_maxima_rp_roll7_sum |>
  group_by(adm1_name) |>
  slice_min(rank, n = 10) |>
  ungroup()

# Plot: 7-day rolling sum return period curve
ggplot(annual_maxima_rp_roll7_sum, aes(x = return_period_years, y = precip_roll7_sum)) +
  geom_point(size = 2) +
  geom_line() +
  geom_text(
    data = top_10_years_roll7_sum,
    aes(label = paste0(substr(year, 3, 4), "'")),
    hjust = -0.3, vjust = 0.5, size = 2.5
  ) +
  geom_hline(
    data = jan_2026_max_7day,
    aes(yintercept = precip_roll7_sum),
    color = "red", linetype = "dashed", linewidth = 1
  ) +
  geom_label(
    data = jan_2026_rp_7day,
    aes(x = Inf, y = precip_roll7_sum,
        label = paste0(format(start_date, "%b %d"), "-", format(valid_date, "%b %d"), " ~", round(estimated_rp_years, 1), " yr")),
    hjust = 1.05, vjust = 0.5, size = 2.5, label.size = 0, fill = "white", color = "red"
  ) +
  scale_x_log10() +
  facet_wrap(~adm1_name, scales = "free_y") +
  labs(
    title = "Return Period Analysis (Annual Maxima Method)",
    subtitle = "7-Day Rolling Sum - Max value in Jan 2026 shown as dashed line",
    x = "Return Period (years)",
    y = "Precipitation (mm, 7-day rolling sum)"
  ) +
  theme_minimal()

# ---- 3-Day Rolling Sum - Max in Jan 2026 ----

# Calculate 3-day rolling sum for all data
imerg_roll3_sum <- imerg |>
  arrange(adm1_name, valid_date) |>
  group_by(adm1_name) |>
  mutate(
    precip_roll3_sum = zoo::rollsum(mean, k = 3, fill = NA, align = "right"),
    start_date = valid_date - 2  # start date of 3-day window
  ) |>
  ungroup()

# Get max 3-day rolling sum in Jan 2026 per province
jan_2026_max_3day_sum <- imerg_roll3_sum |>
  filter(
    !is.na(precip_roll3_sum),
    year(valid_date) == 2026,
    month(valid_date) == 1
  ) |>
  group_by(adm1_name) |>
  slice_max(precip_roll3_sum, n = 1) |>
  ungroup()

print("Max 3-Day Rolling Sum in January 2026 per Province:")
print(jan_2026_max_3day_sum |> select(adm1_name, start_date, valid_date, precip_roll3_sum))

# Annual maxima of 3-day rolling sum (for RP calculation)
annual_maxima_roll3_sum <- imerg_roll3_sum |>
  filter(!is.na(precip_roll3_sum)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm1_name, year) |>
  slice_max(precip_roll3_sum, n = 1) |>
  ungroup()

# Calculate return periods
annual_maxima_rp_roll3_sum <- annual_maxima_roll3_sum |>
  group_by(adm1_name) |>
  mutate(
    n_years = n(),
    rank = rank(-precip_roll3_sum, ties.method = "first"),
    return_period_years = (n_years + 1) / rank
  ) |>
  ungroup()

# Estimate return period for Jan 2026 max
jan_2026_rp_3day_sum <- jan_2026_max_3day_sum |>
  left_join(
    annual_maxima_rp_roll3_sum |>
      select(adm1_name, annual_max = precip_roll3_sum),
    by = "adm1_name",
    relationship = "many-to-many"
  ) |>
  group_by(adm1_name, start_date, valid_date, precip_roll3_sum) |>
  summarise(
    n_years_exceeded = sum(precip_roll3_sum > annual_max),
    n_total_years = n(),
    estimated_rp_years = (n_total_years + 1) / pmax(1, n_total_years - n_years_exceeded),
    .groups = "drop"
  )

print("Return Period Analysis - Max 3-Day Rolling Sum in Jan 2026:")
print(jan_2026_rp_3day_sum)

# Get top 10 years for labeling
top_10_years_roll3_sum <- annual_maxima_rp_roll3_sum |>
  group_by(adm1_name) |>
  slice_min(rank, n = 10) |>
  ungroup()

# Plot: 3-day rolling sum return period curve with Jan 2026 max
ggplot(annual_maxima_rp_roll3_sum, aes(x = return_period_years, y = precip_roll3_sum)) +
  geom_point(size = 2) +
  geom_line() +
  geom_text(
    data = top_10_years_roll3_sum,
    aes(label = paste0(substr(year, 3, 4), "'")),
    hjust = -0.3, vjust = 0.5, size = 2.5
  ) +
  geom_hline(
    data = jan_2026_max_3day_sum,
    aes(yintercept = precip_roll3_sum),
    color = "red", linetype = "dashed", linewidth = 1
  ) +
  geom_label(
    data = jan_2026_rp_3day_sum,
    aes(x = Inf, y = precip_roll3_sum,
        label = paste0(format(start_date, "%b %d"), "-", format(valid_date, "%b %d"), " ~", round(estimated_rp_years, 1), " yr")),
    hjust = 1.05, vjust = 0.5, size = 2.5, label.size = 0, fill = "white", color = "red"
  ) +
  scale_x_log10() +
  facet_wrap(~adm1_name, scales = "free_y") +
  labs(
    title = "Return Period Analysis (Annual Maxima Method)",
    subtitle = "3-Day Rolling Sum - Max value in Jan 2026 shown as dashed line",
    x = "Return Period (years)",
    y = "Precipitation (mm, 3-day rolling sum)"
  ) +
  theme_minimal()

