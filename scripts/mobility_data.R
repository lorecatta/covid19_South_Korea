
library(tidyverse)

source(file.path("R", "utilities.R"))

mobility_data <- read.csv(file.path("data", "Global_Mobility_Report.csv"))

brks_labs <- c("2020-02-15", 
               "2020-03-01", 
               "2020-03-15", 
               "2020-04-01",
               "2020-04-15",
               "2020-04-30",
               "2020-05-15")

root_column_names <- "_percent_change_from_baseline"

countries <- c("South Korea", "Italy", "United Kingdom")

brks <- as.Date(brks_labs)

col_names <- names(mobility_data)[grepl(root_column_names, names(mobility_data))]

new_col_names <- gsub(pattern = root_column_names, replacement = "", x = col_names)
  
new_col_names <- setNames(new_col_names, col_names)

data_to_plot <- filter(mobility_data, (country_region %in% countries) & sub_region_1 == "" & sub_region_2 == "") %>%
  pivot_longer(cols = ends_with("baseline"), names_to = "type") %>%
  mutate(date = as.Date(date)) %>%
  mutate(type = as.factor(type)) %>%
  mutate(type = recode(type, !!!new_col_names)) %>%
  mutate(country_region = as.factor(country_region)) %>%
  mutate(country_region = droplevels(country_region)) %>%
  mutate(country_region = fct_relevel(country_region, "South Korea", "Italy"))
  
p <- ggplot(data = data_to_plot) +
  geom_line(aes(x = date, y = value, color = type)) +
  facet_wrap(~ country_region, ncol = 1) +
  scale_y_continuous("percent change from baseline") +
  scale_x_date(breaks = brks, date_labels = "%b %d") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(ncol = 3))

save_plot(p, "figures/mobility", "mobility_comparison", wdt = 15, hgt = 20)
