
library(pdftools)
library(stringr)
library(purrr)

source(file.path("R", "epi_links_table.R"))
source(file.path("R", "utilities.R"))


# define parameters -----------------------------------------------------------


dir_in <- file.path("data", "korea_archive")

last_day <- "2020-05-28"


# preprocess ------------------------------------------------------------------


all_pdf_file_names <- list.files(file.path(dir_in))

all_pdf_names <- gsub("\\..*", "", all_pdf_file_names)

all_pdf_dates <- as.Date(all_pdf_names)

all_paths <- list.files(file.path(dir_in), full.names = TRUE)


# -----------------------------------------------------------------------------


target_1_dates <- c(seq(as.Date("2020-03-25"), as.Date(last_day), 1))

target_1_idx <- which(all_pdf_dates %in% target_1_dates)

target_1 <- all_paths[target_1_idx]

all_pdfs_1 <- imap(target_1, ~ pdf_text(pdf = .x))

all_tables_1 <- imap(all_pdfs_1, ~ grab_table_epilinks(test_1 = .x, index = .y))

grab_table_epilinks(all_pdfs_1[[59]], index = 59)



# -----------------------------------------------------------------------------
# post processing

library(dplyr)

final_output <- do.call("rbind", all_tables_1)

dates_look_up <- data.frame(id = seq_len(length(target_1_dates)), target_1_dates)

final_output_2 <- left_join(final_output, dates_look_up)

write.csv(final_output_2, file.path("output", "epi_links.csv"), row.names = FALSE)
