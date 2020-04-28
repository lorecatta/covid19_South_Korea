
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
               "2020-04-15",
               "2020-04-28")


# load data -------------------------------------------------------------------


case_data <- readRDS(file.path("output", "KCDC_case_data.rds"))


# pre processing --------------------------------------------------------------


brks <- as.Date(brks_labs)

label_x <- "2020-01-20"


# -----------------------------------------------------------------------------


case_data_2 <- case_data %>%
  mutate_at(.funs = list(inc = ~. - lag(., default = first(.))), 
            .vars = c("Total", "Confirmed", "Discharged", "Deceased"))
            
sec_axis_brks <- seq(0, 600000, 100000)

sec_axis_labels <- vapply(sec_axis_brks, 
                          scientific_format, 
                          character(1),
                          FALSE)

key_date <- data.frame(x = as.Date(c("2020-01-28", "2020-02-20", "2020-02-23", "2020-03-22")), 
                       xend = as.Date(c("2020-01-28", "2020-02-20", "2020-02-23", "2020-03-22")),
                       y = 0,
                       yend = 950,
                       lab = factor(c("Testing travellers\nfrom China", 
                                      "Testing regardless\nof travel history", 
                                      "Daegu quarantine", 
                                      "Strict Social\nDistancing"), 
                                    levels = c("Testing travellers\nfrom China", 
                                               "Testing regardless\nof travel history", 
                                               "Daegu quarantine", 
                                               "Strict Social\nDistancing")))

points <- data.frame(x = as.Date(c("2020-01-28", "2020-02-20", "2020-02-23", "2020-03-22")), 
                     y = 950,
                     lab = factor(c("Testing travellers\nfrom China", 
                                    "Testing regardless\nof travel history", 
                                    "Daegu quarantine", 
                                    "Strict Social\nDistancing"), 
                                  levels = c("Testing travellers\nfrom China", 
                                             "Testing regardless\nof travel history", 
                                             "Daegu quarantine", 
                                             "Strict Social\nDistancing")))

SK_case_plot <- ggplot(data = case_data_2) +
  geom_col(aes(x = Date, y = Confirmed_inc), width = 0.7, fill = "gray65") +
  geom_line(aes(x = Date, y = Confirmed/10)) +
  geom_segment(data = key_date,
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               linetype = 2) +
  geom_point(data = points,
             mapping = aes(x = x, y = y, shape = lab), size = 2.5) +
  scale_shape_manual(NULL,
                     values = c("Testing travellers\nfrom China" = 15,
                                "Testing regardless\nof travel history" = 16,
                                "Daegu quarantine" = 17,
                                "Strict Social\nDistancing" = 18)) +
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Daily incidence",
                     sec.axis = sec_axis(trans = ~.*10,
                                         name = "Cumulative")) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm"),
        legend.position = c(0.83, 0.5),
        legend.background = element_rect(fill = "gray90", 
                                         colour = "black", 
                                         size = 0.3),
        legend.key = element_rect(fill = "transparent")) +
  labs(tags = "A")

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
        plot.margin = unit(c(0,1,0.1,0.5), "cm")) +
  labs(tags = "B")

g1 <- ggplotGrob(SK_case_plot)
g2 <- ggplotGrob(SK_deaths_plot)

g <- rbind(g1, g2, size = "first")

g$widths <- unit.pmax(g1$widths, g2$widths)


# number of tests -------------------------------------------------------------


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
        plot.margin = unit(c(0,1,0,0.5), "cm"))
 

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
  scale_y_continuous(limits = c(0, 21000)) +
  scale_color_manual(name = NULL, values = c("Under isolation" = "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm"),
        legend.position = c(0.2, 0.7),
        legend.title = element_blank())


# save ------------------------------------------------------------------------


save_plot(SK_total_plot, "figures", "test_data", wdt = 18, hgt = 7)

save_plot(g, "figures", "korea_case_data_v1", wdt = 18, hgt = 15)

save_plot(isolation_plot, "figures", "korea_isolation_data", wdt = 18, hgt = 7)
