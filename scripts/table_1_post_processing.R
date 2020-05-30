
library(stringr)
library(dplyr)

source(file.path("R", "post_processing.R"))
source(file.path("R", "utilities.R"))


# load data -------------------------------------------------------------------


all_tables_1 <- readRDS(file.path("output", "all_tables_1.rds"))
all_tables_2 <- readRDS(file.path("output", "all_tables_2.rds"))
all_tables_3 <- readRDS(file.path("output", "all_tables_3.rds"))
all_tables_4 <- readRDS(file.path("output", "all_tables_4.rds"))

all_dates <- readRDS(file.path("output", "all_target_dates.rds"))

# from 2020-01-20 to 2020-02-06
manual <- read.csv(file.path("data", "manual.csv"), stringsAsFactors = FALSE)


# -----------------------------------------------------------------------------


all_tables_1_num <- all_tables_1
all_tables_2_num <- lapply(all_tables_2, char_to_num)
all_tables_3_num <- lapply(all_tables_3, char_to_num)
all_tables_4_num <- lapply(all_tables_4, char_to_num)


all_tables_2_processed <- lapply(all_tables_2_num, format_results_byCityProvince)

to_replace <- lapply(all_tables_3_num[1:6], fill_fields)

all_tables_3_num[1:6] <- to_replace

all_tables_4_num_2 <- lapply(all_tables_4_num, remove_subtotals)

to_replace_1 <- lapply(all_tables_4_num_2[1], fill_fields_2)

to_replace_2 <- lapply(all_tables_4_num_2[2:8], fill_fields_3)

all_tables_4_num_2[1] <- to_replace_1

all_tables_4_num_2[2:8] <- to_replace_2

all_tables_1_num_final <- lapply(all_tables_1_num, add_num_time)
all_tables_2_num_final <- lapply(all_tables_2_processed, add_num_time)
all_tables_3_num_final <- lapply(all_tables_3_num, add_num_time)
all_tables_4_num_final <- lapply(all_tables_4_num_2, add_num_time)

all_dates_order_I_want <- order(all_dates)

all_reports <- c(all_tables_1_num_final,
                 all_tables_2_num_final,
                 all_tables_3_num_final,
                 all_tables_4_num_final)

all_reports_2 <- all_reports[all_dates_order_I_want]

all_reports_2_df <- as.data.frame(do.call("rbind", all_reports_2))

manual_dates <- as.Date(manual$Date, "%d/%m/%Y")

final_output <- select(manual, -Date) %>%
  bind_rows(all_reports_2_df) %>%
  filter(Period == 2) %>%
  mutate(Date = c(manual_dates, all_dates[all_dates_order_I_want])) %>%
  select(-Period)


# -----------------------------------------------------------------------------
# linear interpolation of NA Being tested


na_idxs <- which(is.na(final_output$Being_tested))

my_look_up_table <- data.frame(Confirmed = final_output$Confirmed[-na_idxs],
                               Total = final_output$Total[-na_idxs],
                               Being_tested = final_output$Being_tested[-na_idxs],
                               stringsAsFactors = FALSE)

values_to_look_up <- final_output$Confirmed[na_idxs]

Being_tested_vals <- approx(my_look_up_table[, "Confirmed"], 
                            my_look_up_table[, "Being_tested"], 
                            xout = values_to_look_up)$y

final_output$Being_tested[na_idxs] <- round(Being_tested_vals)

# interpolate Total as well. Otherwise how do I get the number of negative tested?

Total_vals <- approx(my_look_up_table[, "Confirmed"], 
                     my_look_up_table[, "Total"], 
                     xout = values_to_look_up)$y

final_output$Total[na_idxs] <- round(Total_vals)

# now calculate the number of tested negative
final_output$Tested_negative <- final_output$Total - (final_output$Confirmed + final_output$Being_tested)

write_out_rds(final_output, "output", "KCDC_case_data")
write_out_csv(final_output, "output", "KCDC_case_data")
