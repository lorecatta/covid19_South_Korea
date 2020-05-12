
library(ggplot2)
library(dplyr)
library(patchwork)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "plotting.R"))


# define parameters -----------------------------------------------------------


x_brks_1 <- seq(as.Date("2020-01-02"), as.Date("2020-05-11"), by = 4)

x_brks_labs <- c("2020-01-20", 
                 "2020-02-01", 
                 "2020-02-15", 
                 "2020-03-01", 
                 "2020-03-15", 
                 "2020-04-01",
                 "2020-04-15",
                 "2020-04-30",
                 "2020-05-11")

label_x <- "2020-01-20"


# load data -------------------------------------------------------------------


case_data <- readRDS(file.path("output", "KCDC_case_data.rds"))


# pre processing --------------------------------------------------------------


x_axis_max_lim <- last(x_brks_1) + 3

prim_axis_brks <- seq(0, 1000, 200)

prim_axis_labels <- vapply(prim_axis_brks, 
                           scientific_format, 
                           character(1),
                           FALSE)

prim_axis_brks_2 <- seq(0, 10, 2)

x_brks_2 <- as.Date(x_brks_labs)


# -----------------------------------------------------------------------------


case_data_2 <- case_data %>%
  mutate_at(.funs = list(inc = ~. - lag(., default = first(.))), 
            .vars = c("Total", "Confirmed", "Discharged", "Deceased"))

SK_case_plot <- ggplot(data = case_data_2) +
  geom_col(aes(x = Date, y = Confirmed_inc), width = 0.7, fill = "gray65") + 
  scale_x_date(limits = c(as.Date("2020-01-01"), x_axis_max_lim), 
               breaks = x_brks_1, 
               date_labels = "%e", 
               expand = expansion(add = 1)) +
  scale_y_continuous(name = "Daily number of cases",
                     breaks = prim_axis_brks,
                     labels = prim_axis_labels,
                     limits = c(0, 1000),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(tags = "A")

SK_deaths_plot <- ggplot(data = case_data_2) +
  geom_col(aes(x = Date, y = Deceased_inc), width = 0.7, fill = "gray65") +
  #geom_line(aes(x = Date, y = Deceased)) +
  scale_x_date(limits = c(as.Date("2020-01-01"), x_axis_max_lim),
               breaks = x_brks_1, 
               date_labels = "%e",
               expand = expansion(add = 1)) +
  scale_y_continuous(name = "Daily number of deaths",
                     breaks = prim_axis_brks_2,
                     labels = prim_axis_brks_2,
                     limits = c(0, 10),
                     expand = c(0, 0)) +
                     # sec.axis = sec_axis(trans = ~.*20,
                     #                     name = "Cumulative")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(0.5,1,0.1,0.5), "cm")) +
  labs(tags = "B")
 
g <- SK_case_plot / SK_deaths_plot


# isolation plot --------------------------------------------------------------


case_data_3 <- case_data %>%
  mutate(Isolation_cum = cumsum(Isolated)) %>%
  mutate(Isolation_inc = Isolation_cum - lag(Isolation_cum, default = first(Isolation_cum))) %>%
  mutate(Confirmed_bw_inc = Confirmed - lag(Confirmed, default = first(Confirmed))) %>%
  mutate(Isolation_per_case = ifelse(Confirmed_bw_inc == 0, 0, Isolation_inc / Confirmed_bw_inc)) 

isolation_plot <- ggplot(data = case_data_3) +
  geom_line(aes(x = Date, y = Isolated)) +
  scale_x_date(breaks = x_brks_2, date_labels = "%b %d") +
  scale_y_continuous(name = "Cases currently under isolation",
                     breaks = seq(0, 8000, 2000),
                     limits = c(0, 8000)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"))


# save ------------------------------------------------------------------------


save_plot(SK_case_plot, "figures", "korea_case_data_simple", wdt = 18, hgt = 12)

save_plot(isolation_plot, "figures", "korea_isolation_data_v3", wdt = 18, hgt = 7)

save_plot(SK_deaths_plot, "figures", "korea_deaths_data_simple", wdt = 18, hgt = 8)
