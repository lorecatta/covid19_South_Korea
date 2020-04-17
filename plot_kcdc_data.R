
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


brks_labs <- c("2020-01-20", 
               "2020-02-01", 
               "2020-02-15", 
               "2020-03-01", 
               "2020-03-15", 
               "2020-04-01",
               "2020-04-13")


# load data -------------------------------------------------------------------


case_data <- readRDS(file.path("output", "KCDC_line_list.rds"))


# -----------------------------------------------------------------------------


tot_cum <- case_data$Total
tot_inc <- c(0, diff(tot_cum))

conf_cum <- case_data$Confirmed
conf_inc <- c(0, diff(conf_cum))

iso_cum <- case_data$Isolated
iso_inc <- c(0, diff(iso_cum))
iso_inc[iso_inc < 0] <- 0

disch_cum <- case_data$Discharged
disch_inc <- c(0, diff(disch_cum))

deaths_cum <- case_data$Deceased
deaths_inc <- c(0, diff(deaths_cum))

case_data$tot_inc <- tot_inc
case_data$conf_inc <- conf_inc
case_data$iso_inc <- iso_inc
case_data$disch_inc <- disch_inc
case_data$deaths_inc <- deaths_inc
  
brks <- as.Date(brks_labs)

label_x <- "2020-01-20"
  
SK_total_plot <- ggplot(data = case_data) +
  geom_col(aes(x = Date, y = tot_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Total/25)) + 
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Incidence", 
                     sec.axis = sec_axis(trans = ~.*25, 
                                         name = "Cumulative",
                                         labels = c(0, 10000, 20000, 30000, 40000, 50000))) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  geom_label(x = as.Date(label_x), y = 19900, label = "Tested", hjust = 0)

SK_case_plot <- ggplot(data = case_data) +
  geom_col(aes(x = Date, y = conf_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Confirmed/10)) + 
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Incidence", 
                     sec.axis = sec_axis(trans = ~.*10, 
                                         name = "Cumulative")) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  geom_label(x = as.Date(label_x), y = 1000, label = "Confirmed", hjust = 0)

SK_case_isolation_plot <- ggplot(data = case_data) +
  geom_col(aes(x = Date, y = iso_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Isolated/5)) + 
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Incidence", 
                     sec.axis = sec_axis(trans = ~.*5, 
                                         name = "Cumulative")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  geom_label(x = as.Date(label_x), y = 1400, label = "In isolation", hjust = 0)

SK_discharged_plot <- ggplot(data = case_data) +
  geom_col(aes(x = Date, y = disch_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Discharged/10)) + 
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Incidence", 
                     sec.axis = sec_axis(trans = ~.*10, 
                                         name = "Cumulative")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.margin = unit(c(0,1,0.1,0.5), "cm")) +
  geom_label(x = as.Date(label_x), y = 700, label = "Discharged", hjust = 0)

SK_deaths_plot <- ggplot(data = case_data) +
  geom_col(aes(x = Date, y = deaths_inc), width = 0.7, fill = "gray65") + 
  geom_line(aes(x = Date, y = Deceased/20)) + 
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous(name = "Incidence", 
                     sec.axis = sec_axis(trans = ~.*20, 
                                         name = "Cumulative")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,1,0.1,0.5), "cm")) +
  geom_label(x = as.Date(label_x), y = 10, label = "Deceased", hjust = 0)

g1 <- ggplotGrob(SK_total_plot)
g2 <- ggplotGrob(SK_case_plot)
g3 <- ggplotGrob(SK_discharged_plot)
g4 <- ggplotGrob(SK_deaths_plot)

g <- rbind(g1, g2, g3, g4, size = "first")

g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)

save_plot(g, "figures", "korea_case_data", wdt = 15, hgt = 18)
