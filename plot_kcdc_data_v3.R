
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(RColorBrewer)

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

seq_brew_palette <- "Oranges"

my_palette_1 <- brewer.pal(n = 9, name = seq_brew_palette)[c(4, 5, 6, 7, 8, 9)]

my_alpha <- 0.5


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

# phases are intensity levels 
# could there be two consecutive time periods with same phases?
Bord_contr_dates <- data.frame(type = "Border_control",
                               intensity = c("A", "B", "C"),
                               label = c("Imm Meas from China", "Imm Meas from Japan", "Imm Meas from anywhere"),
                               start = as.Date(c("2020-01-28", "2020-03-09", "2020-03-19")),
                               end = as.Date(c("2020-03-09", "2020-03-19", "2020-04-28")))

testing_dates <- data.frame(type = "Testing",
                            intensity = c("A", "B", "C", "D", "E", "F"),
                            label = c("Test from Hubei", 
                                      "Test from China", 
                                      "Test regardless of travel", 
                                      "Drive through",
                                      "Test incoming from Europe",
                                      "Test incoming from US"),
                            start = as.Date(c("2020-01-20", "2020-01-28", "2020-02-20", "2020-03-05", "2020-03-22", "2020-03-27")),
                            end = as.Date(c("2020-01-28", "2020-02-20", "2020-03-05", "2020-03-22", "2020-03-27", "2020-04-28")))

isolation_dates <- data.frame(type = "Isolation",
                              intensity = c("A", "B", "C"),
                              label = c("Hospital", "Home", "RTC"),
                              start = as.Date(c("2020-01-20", "2020-02-01", "2020-03-01")),
                              end = as.Date(c("2020-02-01", "2020-03-01", "2020-04-28")))

SS_dates <- data.frame(type = "Social_distancing",
                       intensity = c("A", "B", "C"),
                       label = c("Daegu quarantine", "SSD", "RSD"),
                       start = as.Date(c("2020-02-23", "2020-03-22", "2020-04-19")),
                       end = as.Date(c("2020-03-22", "2020-04-19", "2020-04-28")))


prim_axis_brks <- seq(0, 1000, 200)

prim_axis_labels <- vapply(prim_axis_brks, 
                           scientific_format, 
                           character(1),
                           FALSE)

sec_axis_brks <- seq(0, 10000, 2000)

sec_axis_labels <- vapply(sec_axis_brks, 
                          scientific_format, 
                          character(1),
                          FALSE)

SK_case_plot <- ggplot() +
  geom_col(data = case_data_2, aes(x = Date, y = Confirmed_inc), 
           width = 0.7, 
           fill = "gray65") +
  geom_line(data = case_data_2, aes(x = Date, y = Confirmed/10)) +
  geom_rect(data = SS_dates,
            aes(xmin = start, xmax = end, ymin = 0, ymax = 275, fill = intensity)) +
  geom_rect(data = isolation_dates,
            aes(xmin = start, xmax = end, ymin = 275, ymax = 550, fill = intensity), show.legend = FALSE) +
  geom_rect(data = testing_dates,
            aes(xmin = start, xmax = end, ymin = 550, ymax = 825, fill = intensity), show.legend = FALSE) +
  geom_rect(data = Bord_contr_dates,
            aes(xmin = start, xmax = end, ymin = 825, ymax = 1100, fill = intensity), show.legend = FALSE) +
  scale_x_date(breaks = brks, date_labels = "%b %d", 
               limits = as.Date(c('2020-01-19','2020-05-01'))) +
  scale_y_continuous(name = "Daily incidence",
                     breaks = prim_axis_brks,
                     labels = prim_axis_labels,
                     limits = c(0,1100),
                     sec.axis = sec_axis(trans = ~.*10,
                                         name = "Cumulative",
                                         breaks = sec_axis_brks,
                                         labels = sec_axis_labels)) +
  scale_fill_manual(values = adjustcolor(my_palette_1, alpha.f = my_alpha),
                    guide = guide_legend(override.aes = list(alpha = my_alpha),
                                         nrow = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm"),
        legend.position = "bottom")
  # labs(tags = "A")

# SK_deaths_plot <- ggplot(data = case_data_2) +
#   geom_col(aes(x = Date, y = Deceased_inc), width = 0.7, fill = "gray65") +
#   geom_line(aes(x = Date, y = Deceased/20)) +
#   scale_x_date(breaks = brks, date_labels = "%b %d") +
#   scale_y_continuous(name = "Daily incidence",
#                      sec.axis = sec_axis(trans = ~.*20,
#                                          name = "Cumulative")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_blank(),
#         plot.margin = unit(c(0,1,0.1,0.5), "cm")) +
#   labs(tags = "B")
# 
# g1 <- ggplotGrob(SK_case_plot)
# g2 <- ggplotGrob(SK_deaths_plot)
# 
# g <- rbind(g1, g2, size = "first")
# 
# g$widths <- unit.pmax(g1$widths, g2$widths)


# isolation plot --------------------------------------------------------------


# case_data_3 <- case_data %>%
#   mutate(Isolated_cum = cumsum(Isolated)) %>%
#   mutate(Isolated_inc = Isolated_cum - lag(Isolated_cum, default = first(Isolated_cum))) %>%
#   mutate(Confirmed_bw = Confirmed - lag(Confirmed, n = 14, default = 0)) %>%
#   mutate(Isolation_case = Isolated_inc / Confirmed_bw)
# 
# isolation_plot <- ggplot(data = case_data_3) +
#   geom_col(aes(x = Date, y = Isolation_case), width = 0.7) +
#   # scale_fill_manual(values = c("Confirmed" = "firebrick1",
#   #                              "Discharged" = "steelblue3")) +
#   geom_line(aes(x = Date, y = Isolated, color = "Under isolation")) +
#   scale_x_date(breaks = brks, date_labels = "%b %d") +
#   scale_y_continuous(name = "Daily incidence", 
#                      sec.axis = sec_axis(trans = ~.*25,
#                                          name = "Cumulative",
#                                          breaks = sec_axis_brks,
#                                          labels = sec_axis_labels)) +
#   scale_color_manual(name = NULL, values = c("Under isolation" = "black")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         plot.margin = unit(c(0,1,0,0.5), "cm"),
#         legend.position = c(0.2, 0.7),
#         legend.title = element_blank())
# 
# 
# # save ------------------------------------------------------------------------
# 
# 
save_plot(SK_case_plot, "figures", "timeline", wdt = 18, hgt = 8)
# 
# save_plot(g, "figures", "korea_case_data_v1", wdt = 18, hgt = 15)
# 
# save_plot(isolation_plot, "figures", "korea_isolation_data", wdt = 18, hgt = 7)
