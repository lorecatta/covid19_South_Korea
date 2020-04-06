
library(ggplot2)
library(reshape2)

source(file.path("R", "utility_functions.R"))


# load data -------------------------------------------------------------------


death <- read.csv(file.path("data", 
                            "csse_covid_19_time_series", 
                            "time_series_covid19_deaths_global.csv"))

confirmed <- read.csv(file.path("data", 
                                "csse_covid_19_time_series", 
                                "time_series_covid19_confirmed_global.csv"))

recovered <- read.csv(file.path("data", 
                                "csse_covid_19_time_series",
                                "time_series_covid19_recovered_global.csv"))


# define parameters -----------------------------------------------------------


country_name <- "Korea, South"

brks_labs <- c("2020-01-22", 
               "2020-02-01", 
               "2020-02-15", 
               "2020-02-15", 
               "2020-03-01", 
               "2020-03-15", 
               "2020-04-01")


# run -------------------------------------------------------------------------


death_sub <- subset(death, Country.Region %in% country_name)
death_cum <- as.numeric(death_sub[1,5:ncol(death_sub)])
death_inc <- c(0, diff(death_cum))

conf_sub <- subset(confirmed, Country.Region %in% country_name)
conf_cum <- as.numeric(conf_sub[1,5:ncol(conf_sub)])
conf_inc <- c(0, diff(conf_cum))

rec_sub <- subset(recovered, Country.Region %in% country_name)
rec_cum <- as.numeric(rec_sub[1,5:ncol(rec_sub)])
rec_inc <- c(0, diff(rec_cum))

df_cum_wide <- data.frame(time = seq_len(length(death_inc)),
                          date = names(death[5:ncol(death)]),
                          death = death_cum, 
                          confirmed = conf_cum, 
                          recovered = rec_cum)

df_inc_wide <- data.frame(time = seq_len(length(death_inc)),
                          date = names(death[5:ncol(death)]),
                          death = death_inc, 
                          confirmed = conf_inc, 
                          recovered = rec_inc)

df_cum_wide$date <- sub('.', '', df_cum_wide$date)

df_cum_long <- melt(df_cum_wide, 
                    id.vars = c("time", "date"),
                    variable.name = "type")

df_cum_long$date <- paste0(df_cum_long$date, "20")
df_cum_long$date <- as.Date(df_cum_long$date, '%m.%d.%Y')

df_inc_wide$date <- sub('.', '', df_inc_wide$date)

df_inc_long <- melt(df_inc_wide, 
                    id.vars = c("time", "date"),
                    variable.name = "type")

df_inc_long$date <- paste0(df_inc_long$date, "20")
df_inc_long$date <- as.Date(df_inc_long$date, '%m.%d.%Y')

brks <- as.Date(brks_labs)


# plotting --------------------------------------------------------------------


p_cum <- ggplot(data = df_cum_long, aes(x = date, y = value)) +
  geom_line() + 
  geom_point(size = 0.7) +
  facet_wrap(~ type, ncol = 1, scales = "free_y") +
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous("Cumulative number") +
  ggtitle("South Korea") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

p_inc <- ggplot(data = df_inc_long, aes(x = date, y = value)) +
  geom_line() + 
  geom_point(size = 0.7) +
  facet_wrap(~ type, ncol = 1, scales = "free_y") +
  scale_x_date(breaks = brks, date_labels = "%b %d") +
  scale_y_continuous("Daily number") +
  ggtitle("South Korea") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())


# save ------------------------------------------------------------------------


save_plot(p_cum, "figures", "cumulative", wdt = 15, hgt = 13)
save_plot(p_inc, "figures", "daily", wdt = 15, hgt = 13)
