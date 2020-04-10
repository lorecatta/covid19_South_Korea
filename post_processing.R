
library(stringr)

source(file.path("R", "post_processing_functions.R"))
source(file.path("R", "utility_functions.R"))


# load data -------------------------------------------------------------------


all_tables_1 <- readRDS(file.path("output", "all_tables_1.rds"))
all_tables_2 <- readRDS(file.path("output", "all_tables_2.rds"))
all_tables_3 <- readRDS(file.path("output", "all_tables_3.rds"))
all_tables_4 <- readRDS(file.path("output", "all_tables_4.rds"))

# from 2020-01-20 to 2020-02-06
manual <- read.csv(file.path("data", "manual.csv"), stringsAsFactors = FALSE)


# -----------------------------------------------------------------------------


all_tables_1_num <- lapply(all_tables_1, char_to_num)
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

target_1_dates <- c(as.Date(c("2020-03-10", "2020-03-12", "2020-03-14")),
                    seq(as.Date("2020-03-18"), as.Date("2020-04-07"), 1))
target_2_dates <- as.Date(c("2020-03-09", "2020-03-11", "2020-03-13", "2020-03-15", "2020-03-16", "2020-03-17"))
target_3_dates <- c(as.Date(c("2020-02-09", "2020-02-11", "2020-02-13", "2020-02-15", "2020-02-17", "2020-02-19", "2020-02-21")), 
                    seq(as.Date("2020-02-23"), as.Date("2020-03-08"), 1))
target_4_dates <- c(seq(as.Date("2020-02-07"), as.Date("2020-02-08"), 1), 
                    as.Date(c("2020-02-10", "2020-02-12", "2020-02-14", "2020-02-16", "2020-02-18", "2020-02-20", "2020-02-22"))) 

all_dates_current_order <- c(target_1_dates,
                             target_2_dates,
                             target_3_dates,
                             target_4_dates)

all_dates_order_I_want <- order(all_dates_current_order)

all_reports <- c(all_tables_1_num_final,
                 all_tables_2_num_final,
                 all_tables_3_num_final,
                 all_tables_4_num_final)

all_repors_2 <- all_reports[all_dates_order_I_want]

all_reports_2_df <- do.call("rbind", all_repors_2)

manual_dates <- as.Date(manual$Date, "%d/%m/%Y")

final_output <- rbind(manual[,-1], all_reports_2_df)

final_output_2 <- subset(final_output, Period == 2)

final_output_2$Date <- c(manual_dates, all_dates_current_order[all_dates_order_I_want])


# -----------------------------------------------------------------------------
# linear interpolation of NA Being tested


na_idxs <- which(is.na(final_output_2$Being_tested))

my_look_up_table <- data.frame(Confirmed = final_output_2$Confirmed[-na_idxs],
                               Total = final_output_2$Total[-na_idxs],
                               Being_tested = final_output_2$Being_tested[-na_idxs],
                               stringsAsFactors = FALSE)

values_to_look_up <- final_output_2$Confirmed[na_idxs]

Being_tested_vals <- approx(my_look_up_table[, "Confirmed"], 
                            my_look_up_table[, "Being_tested"], 
                            xout = values_to_look_up)$y

final_output_2$Being_tested[na_idxs] <- round(Being_tested_vals)

# interpolate Total as well. Otherwise how do I get the number of negative tested?

Total_vals <- approx(my_look_up_table[, "Confirmed"], 
                     my_look_up_table[, "Total"], 
                     xout = values_to_look_up)$y

final_output_2$Total[na_idxs] <- round(Total_vals)

# now calculate the number of tested negative
final_output_2$Tested_negative <- final_output_2$Total - (final_output_2$Confirmed + final_output_2$Being_tested)

write_out_csv(final_output_2, "output", "KCDC_line_list")
