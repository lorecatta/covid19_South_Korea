
library(tabulizer)

source(file.path("R", "utility_functions.R"))

in_dir <- file.path("data", "korea_archive")

my_pdf <- extract_tables(file.path(in_dir, "2020-02-20.pdf"))

table_1 <- my_pdf[[2]][-1, ]
table_2 <- my_pdf[[3]][-c(1:4), ]

date <- table_1[, 3]
case_no <- table_1[, 1]
contacts <- table_1[, 16]

table_2[6, 10] <- table_2[6, 11]
table_2[6, 2] <- table_2[6, 3]

date_2 <- table_2[, 2]
case_no_2 <- table_2[, 1]
contacts_2 <- table_2[, 10]

data_table <- data.frame(case_no = c(case_no, case_no_2), 
                         date_lab_conf = as.Date(c(date, date_2)),
                         contacts = c(contacts, contacts_2), 
                         stringsAsFactors = FALSE)

write_out_csv(data_table, "output", "contacts_of_first_30")
write_out_rds(data_table, "output", "contacts_of_first_30")
