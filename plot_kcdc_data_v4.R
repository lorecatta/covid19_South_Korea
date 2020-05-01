
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(patchwork)

source(file.path("R", "utility_functions.R"))
source(file.path("R", "plotting.R"))


# define parameters -----------------------------------------------------------


# brks_labs <- c("2020-01-20", 
#                "2020-02-01", 
#                "2020-02-15", 
#                "2020-03-01", 
#                "2020-03-15", 
#                "2020-04-01",
#                "2020-04-15",
#                "2020-04-28")

brks_labs <- seq(as.Date("2020-01-20"), as.Date("2020-04-28"), by = 3)


# load data -------------------------------------------------------------------


case_data <- readRDS(file.path("output", "KCDC_case_data.rds"))


# pre processing --------------------------------------------------------------


brks <- as.Date(brks_labs)

label_x <- "2020-01-20"

prim_axis_brks <- seq(0, 1000, 200)

prim_axis_labels <- vapply(prim_axis_brks, 
                           scientific_format, 
                           character(1),
                           FALSE)


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
  scale_x_date(breaks = brks, date_labels = "%e", expand = expansion(add = 1)) +
  scale_y_continuous(name = "Daily incidence",
                     breaks = prim_axis_brks,
                     labels = prim_axis_labels,
                     limits = c(0,1000),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# SK_deaths_plot <- ggplot(data = case_data_2) +
#   geom_col(aes(x = Date, y = Deceased_inc), width = 0.7, fill = "gray65") + 
#   geom_line(aes(x = Date, y = Deceased/20)) + 
#   geom_segment(data = key_date, 
#                mapping = aes(x = x, y = y, xend = xend, yend = 11, linetype = lab),
#                size = 0.5) +
#   scale_linetype_manual(NULL,
#                         values = c("Testing travellers\nfrom China" = 2, 
#                                    "Testing regardless\nof travel history" = 3, 
#                                    "Daegu quarantine" = 4, 
#                                    "Strict Social\nDistancing" = 6)) +
#   scale_x_date(breaks = brks, date_labels = "%b %d") +
#   scale_y_continuous(name = "Daily incidence", 
#                      sec.axis = sec_axis(trans = ~.*20, 
#                                          name = "Cumulative")) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         plot.margin = unit(c(0,1,0.1,0.5), "cm")) +
#   ggtitle("Deaths") +
#   labs(tags = "B")
# 
# g <- SK_case_plot / SK_deaths_plot & theme(legend.position = "top")
# 
# g2 <- g + plot_layout(guides = "collect")
# 
# save_plot(g2, "figures", "korea_case_data_v3", wdt = 18, hgt = 16)

save_plot(SK_case_plot, "figures", "korea_case_data_simple", wdt = 20, hgt = 12)
