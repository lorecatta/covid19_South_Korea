
library(pdftools)
library(stringr)
library(purrr)

source(file.path("R", "case_table.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


dir_in <- file.path("data", "korea_archive")

last_day <- "2020-05-11"


# preprocess ------------------------------------------------------------------


all_pdf_file_names <- list.files(file.path(dir_in))

all_pdf_names <- gsub("\\..*", "", all_pdf_file_names)
  
all_pdf_dates <- as.Date(all_pdf_names)

all_paths <- list.files(file.path(dir_in), full.names = TRUE)


# -----------------------------------------------------------------------------


target_1_dates <- c(as.Date(c("2020-03-10", "2020-03-12", "2020-03-14")),
                    seq(as.Date("2020-03-18"), as.Date(last_day), 1))

target_1_idx <- which(all_pdf_dates %in% target_1_dates)

target_1 <- all_paths[target_1_idx]

all_pdfs_1 <- imap(target_1, ~ pdf_text(pdf = .x))

all_tables_1 <- imap(all_pdfs_1, ~ grab_table_1(test_1 = .x, index = .y))

# grab_table_1(all_pdfs_1[[33]], index = 33)


# -----------------------------------------------------------------------------


# these press releases only report isolated, discharged and deceased (and confirmed therefore), I THINK
target_2_dates <- as.Date(c("2020-03-09", "2020-03-11", "2020-03-13", "2020-03-15", "2020-03-16", "2020-03-17"))

target_2_idx <- which(all_pdf_dates %in% target_2_dates)
  
target_2 <- all_paths[target_2_idx] 

all_pdfs_2 <- imap(target_2, ~ pdf_text(pdf = .x))

all_tables_2 <- imap(all_pdfs_2, ~ grab_table_1_byCityProvince(test_1 = .x, index = .y))

# grab_table_1_byCityProvince(all_pdfs_2[[6]])


# -----------------------------------------------------------------------------


target_3_dates <- c(as.Date(c("2020-02-09", "2020-02-11", "2020-02-13", "2020-02-15", "2020-02-17", "2020-02-19", "2020-02-21")), 
                    seq(as.Date("2020-02-23"), as.Date("2020-03-08"), 1))

target_3_idx <- which(all_pdf_dates %in% target_3_dates)

target_3 <- all_paths[target_3_idx]  

all_pdfs_3 <- imap(target_3, ~ pdf_text(pdf = .x))

all_tables_3 <- imap(all_pdfs_3, ~ grab_table_1_oneRow(test_1 = .x, 
                                                       index = .y))

# grab_table_1_oneRow(all_pdfs_3[[3]],
#                     index = 3)


# -----------------------------------------------------------------------------


# 2020-02-07 is the last report automatically extracted
# the remaining 10 are  manually extracted

target_4_dates <- c(seq(as.Date("2020-02-07"), as.Date("2020-02-08"), 1), 
                    as.Date(c("2020-02-10", "2020-02-12", "2020-02-14", "2020-02-16", "2020-02-18", "2020-02-20", "2020-02-22"))) 

target_4_idx <- which(all_pdf_dates %in% target_4_dates)

target_4 <- all_paths[target_4_idx]

all_pdfs_4 <- imap(target_4, ~ pdf_text(pdf = .x))

all_tables_4 <- imap(all_pdfs_4, ~ grab_table_1_oneDate(test_1 = .x, 
                                                        index = .y))

# grab_table_1_oneDate(all_pdfs_4[[2]],
#                      index = 2)


# concatenate all dates together ----------------------------------------------


all_target_dates <- c(target_1_dates,
                      target_2_dates,
                      target_3_dates,
                      target_4_dates)

# save ------------------------------------------------------------------------


write_out_rds(all_tables_1, "output", "all_tables_1")
write_out_rds(all_tables_2, "output", "all_tables_2")
write_out_rds(all_tables_3, "output", "all_tables_3")
write_out_rds(all_tables_4, "output", "all_tables_4")

write_out_rds(all_target_dates, "output", "all_target_dates")
