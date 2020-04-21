
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "plotting.R"))


# define parameters -----------------------------------------------------------


brks_labs <- c("2020-01-20", 
               "2020-02-01", 
               "2020-02-15", 
               "2020-03-01", 
               "2020-03-15", 
               "2020-04-01",
               "2020-04-16")


# load data -------------------------------------------------------------------


case_data <- readRDS(file.path("output", "KCDC_line_list.rds"))


# pre processing --------------------------------------------------------------


brks <- as.Date(brks_labs)

label_x <- "2020-01-20"


# -----------------------------------------------------------------------------


case_data_2 <- case_data %>%
  mutate_at(.funs = list(inc = ~. - lag(., default = first(.))), 
            .vars = c("Total", "Confirmed", "Discharged", "Deceased"))
            
sec_axis_brks <- seq(0, 500000, 100000)

sec_axis_labels <- vapply(sec_axis_brks, 
                          scientific_format, 
                          character(1),
                          FALSE)

SK_total_plot <- ggplot(data = case_data_2) +
  geom_col(aes(x = Date, y = Total_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Total/25)) + 
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Daily incidence", 
                     sec.axis = sec_axis(trans = ~.*25, 
                                         name = "Cumulative",
                                         breaks = sec_axis_brks,
                                         labels = sec_axis_labels)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  geom_label(x = as.Date(label_x), y = max(case_data$Total)*0.95/25, label = "Tested", hjust = 0)

SK_case_plot <- ggplot(data = case_data_2) +
  geom_col(aes(x = Date, y = Confirmed_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Confirmed/10)) + 
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Daily incidence", 
                     sec.axis = sec_axis(trans = ~.*10, 
                                         name = "Cumulative")) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  geom_label(x = as.Date(label_x), y = max(case_data$Confirmed)*0.95/10, label = "Confirmed", hjust = 0)

SK_deaths_plot <- ggplot(data = case_data_2) +
  geom_col(aes(x = Date, y = Deceased_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Deceased/20)) + 
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Daily incidence", 
                     sec.axis = sec_axis(trans = ~.*20, 
                                         name = "Cumulative")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.margin = unit(c(0,1,0.1,0.5), "cm")) +
  geom_label(x = as.Date(label_x), y = max(case_data$Deceased)*0.95/20, label = "Deceased", hjust = 0)

g1 <- ggplotGrob(SK_total_plot)
g2 <- ggplotGrob(SK_case_plot)
g4 <- ggplotGrob(SK_deaths_plot)

g <- rbind(g2, g4, g1, size = "first")

g$widths <- unit.pmax(g2$widths, g4$widths, g1$widths)


# isolation plot --------------------------------------------------------------


case_data_3 <- pivot_longer(case_data, 
                            cols = c("Confirmed", "Discharged"), 
                            names_to = "type", 
                            values_to = "value")

isolation_plot <- ggplot(data = case_data_3) +
  geom_col(aes(x = Date, y = value, fill = type), width = 0.7, position = "stack") +
  scale_fill_manual(values = c("Confirmed" = "firebrick1",
                               "Discharged" = "steelblue3")) +
  geom_line(aes(x = Date, y = Isolated, color = "Under isolation")) +
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_color_manual(name = NULL, values = c("Under isolation" = "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm"),
        legend.position = c(0.2, 0.7),
        legend.title = element_blank())


# save ------------------------------------------------------------------------


save_plot(g, "figures", "korea_case_data", wdt = 15, hgt = 15)

save_plot(isolation_plot, "figures", "korea_isolation_data", wdt = 15, hgt = 7)
