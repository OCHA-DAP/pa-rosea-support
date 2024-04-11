library(readxl)
library(tidyverse)
library(scales)
library(gghdx)
# remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
gghdx()
options(scipen=99999)

cerf_allocations <- read_excel("C:/Users/pauni/Desktop/Work/OCHA/Trips/MPM/cerf allocations.xlsx", sheet = 2)
cerf_allocations$year <- year(cerf_allocations$`Date of Most Recent Submission`)
cerf_allocations$`Date of Most Recent Submission ` <- as.character(cerf_allocations$`Date of Most Recent Submission`)
cerf_allocations$`Date of Most Recent Submission` <- as.Date(cerf_allocations$`Date of Most Recent Submission`)

cerf_allocations %>%
  mutate(`Date of Most Recent Submission` = format(`Date of Most Recent Submission`, "%Y-%m")) %>%
  group_by(`Date of Most Recent Submission`, `Emergency Type`) %>%
  summarise(`Amount Approved` = mean(`Amount Approved`)) %>%
  ggplot() +
    geom_bar(stat = "identity",
             aes(x=as.Date(paste0(`Date of Most Recent Submission`, "-01")), 
                 y = `Amount Approved`, 
                 fill = `Emergency Type`)) +
    scale_y_continuous(labels = comma) + 
    labs(y = "Amount Approved (USD)", title = "CERF Allocations by Emergency over time") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")


ggplot(cerf_allocations) +
  geom_point(aes(x=`Date of Most Recent Submission`, 
               y = `Amount Approved`, 
               color = `Emergency Type`)) +
  scale_y_continuous(labels = comma) + 
  labs(y = "Amount Approved (USD)", title = "CERF Allocations by Emergency over time")

ggplot(cerf_allocations) +
  geom_bar_pattern(stat = "identity",
           aes(x=`Date of Most Recent Submission `, 
               y = `Amount Approved`, 
               fill = `Emergency Type`,
               pattern = Predictable),
           position = position_dodge(preserve = "single"),
           color = "black", 
           pattern_fill = "black",
           pattern_angle = 45,
           pattern_density = 0.1,
           pattern_spacing = 0.025,
           pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorRampPalette(c("brown","orange","red", "lightblue", "darkgreen", "navyblue", "pink"))(7)) +
  scale_y_continuous(labels = comma) +
  scale_pattern_manual(values = c(Yes = "none", No = "stripe")) +
  labs(y = "Amount Approved (USD)", 
       title = "CERF Allocations by Emergency over time",
       pattern = "Predictable") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1, 
                                   margin = margin(t = -15, r = 0, b = 20, l = 0))) + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
