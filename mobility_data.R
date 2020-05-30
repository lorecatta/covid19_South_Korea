library(dplyr)
library(ggplot2)
library(tidyr)

source(file.path("R", "utility_functions.R"))

mobility_data <- read.csv(file.path("data", "Global_Mobility_Report.csv"))

brks_labs <- c("2020-02-15", 
               "2020-03-01", 
               "2020-03-15", 
               "2020-04-01",
               "2020-04-15",
               "2020-04-30",
               "2020-05-15")

brks <- as.Date(brks_labs)


SK_mobility_data <- filter(mobility_data, country_region == "South Korea" & sub_region_1 == "" & sub_region_2 == "") %>%
  pivot_longer(cols = ends_with("baseline"), names_to = "type") %>%
  mutate(date = as.Date(date)) %>%
  mutate(type = as.factor(type))

p_SK <- ggplot(data = SK_mobility_data) +
  geom_line(aes(x = date, y = value)) +
  facet_wrap(~ type, ncol = 1) +
  scale_x_date(breaks = brks, date_labels = "%b %d") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

IT_mobility_data <- filter(mobility_data, country_region == "Italy" & sub_region_1 == "" & sub_region_2 == "") %>%
  pivot_longer(cols = ends_with("baseline"), names_to = "type") %>%
  mutate(date = as.Date(date)) %>%
  mutate(type = as.factor(type))

p_IT <- ggplot(data = IT_mobility_data) +
  geom_line(aes(x = date, y = value)) +
  facet_wrap(~ type, ncol = 1) +
  scale_x_date(breaks = brks, date_labels = "%b %d") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

UK_mobility_data <- filter(mobility_data, country_region == "United Kingdom" & sub_region_1 == "" & sub_region_2 == "") %>%
  pivot_longer(cols = ends_with("baseline"), names_to = "type") %>%
  mutate(date = as.Date(date)) %>%
  mutate(type = as.factor(type))

p_UK <- ggplot(data = UK_mobility_data) +
  geom_line(aes(x = date, y = value)) +
  facet_wrap(~ type, ncol = 1) +
  scale_x_date(breaks = brks, date_labels = "%b %d") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_SK, "figures/mobility", "South_Korea_mobility", wdt = 10, hgt = 20)
save_plot(p_IT, "figures/mobility", "Italy_mobility", wdt = 10, hgt = 20)
save_plot(p_UK, "figures/mobility", "UK_mobility", wdt = 10, hgt = 20)
