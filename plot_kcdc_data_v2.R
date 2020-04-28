
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(patchwork)

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

key_date <- data.frame(x = as.Date(c("2020-01-28", "2020-02-20", "2020-02-23", "2020-03-22")), 
                       xend = as.Date(c("2020-01-28", "2020-02-20", "2020-02-23", "2020-03-22")),
                       y = 0,
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
               mapping = aes(x = x, y = y, xend = xend, yend = 950, linetype = lab),
               size = 0.5) +
  scale_linetype_manual(NULL,
                        values = c("Testing travellers\nfrom China" = 2, 
                                   "Testing regardless\nof travel history" = 3, 
                                   "Daegu quarantine" = 4, 
                                   "Strict Social\nDistancing" = 6)) +
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
  ggtitle("Cases") +
  labs(tags = "A")

SK_deaths_plot <- ggplot(data = case_data_2) +
  geom_col(aes(x = Date, y = Deceased_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Deceased/20)) + 
  geom_segment(data = key_date, 
               mapping = aes(x = x, y = y, xend = xend, yend = 11, linetype = lab),
               size = 0.5) +
  scale_linetype_manual(NULL,
                        values = c("Testing travellers\nfrom China" = 2, 
                                   "Testing regardless\nof travel history" = 3, 
                                   "Daegu quarantine" = 4, 
                                   "Strict Social\nDistancing" = 6)) +
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Daily incidence", 
                     sec.axis = sec_axis(trans = ~.*20, 
                                         name = "Cumulative")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,1,0.1,0.5), "cm")) +
  ggtitle("Deaths") +
  labs(tags = "B")

g <- SK_case_plot / SK_deaths_plot & theme(legend.position = "bottom")

g2 <- g + plot_layout(guides = "collect")

save_plot(g2, "figures", "korea_case_data_v2", wdt = 18, hgt = 16)
