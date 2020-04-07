
library(purrr)
library(tabulizer)
library(dplyr)

source(file.path("R", "pdf_extraction_functions.R"))


# -----------------------------------------------------------------------------


urls <- c(
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=UtBXAvY7Xi53DZ1QktH4rWjJ3FSvonjq3nXlJV3/yYQ=&comfile_fs=20200406201621499680011&comfile_fn=Press+Release+%28April6%29_Afternoon.pdf&comfile_c=www1&comfile_fd=1586187382458",
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=/5c0C96D8Bd/xHhZKAQWnx9kwptDRMBgp0||^||IFywdX/w=&comfile_fs=20200405173450804679345&comfile_fn=Press_Release_%28April5%29_Afternoon.pdf&comfile_c=www1&comfile_fd=1586187352362",
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=||^||0zMHW||^||zfy3nyz7Lq7Sqs769t89CDlUUzX6FXk66Kyg=&comfile_fs=20200404175228484679250&comfile_fn=Press+Release+%28April4%29_afternoon.pdf&comfile_c=www1&comfile_fd=1586186166975",
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=TkVJW30lGikr6SgGl5JC/wvUxeSND83BpExsQIP||^||vdM=&comfile_fs=20200403175410867679028&comfile_fn=Press_Release_%28April3%29_Afternoon.pdf&comfile_c=www1&comfile_fd=1586188305590")

no_reports <- length(urls)

dest_files <- paste0("report_", seq_len(no_reports), ".pdf") 

imap(urls, ~ download.file(url = .x, destfile = dest_files[.y], mode = "wb"))

# extract from pdf four tables 
my_pdf <- extract_tables("report_1.pdf")

tables_report_1 <- scrap_tables(my_pdf)
