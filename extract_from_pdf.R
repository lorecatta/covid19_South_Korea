
library(purrr)
library(tabulizer)

source(file.path("R", "pdf_extraction_functions.R"))


# define parameters -----------------------------------------------------------


first_day <- "2020-04-03"

# oldest one on top
urls <- c(
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=TkVJW30lGikr6SgGl5JC/wvUxeSND83BpExsQIP||^||vdM=&comfile_fs=20200403175410867679028&comfile_fn=Press_Release_%28April3%29_Afternoon.pdf&comfile_c=www1&comfile_fd=1586188305590",
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=||^||0zMHW||^||zfy3nyz7Lq7Sqs769t89CDlUUzX6FXk66Kyg=&comfile_fs=20200404175228484679250&comfile_fn=Press+Release+%28April4%29_afternoon.pdf&comfile_c=www1&comfile_fd=1586186166975",
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=/5c0C96D8Bd/xHhZKAQWnx9kwptDRMBgp0||^||IFywdX/w=&comfile_fs=20200405173450804679345&comfile_fn=Press_Release_%28April5%29_Afternoon.pdf&comfile_c=www1&comfile_fd=1586187352362",
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=UtBXAvY7Xi53DZ1QktH4rWjJ3FSvonjq3nXlJV3/yYQ=&comfile_fs=20200406201621499680011&comfile_fn=Press+Release+%28April6%29_Afternoon.pdf&comfile_c=www1&comfile_fd=1586187382458",
  "http://is.cdc.go.kr/upload_comm/refile.do?cmd=fileDownloadC&comfile_se=6cQx||^||yeB/TIhRBMx7EjLbEeCAahquv65GZBpSB7vC6I=&comfile_fs=20200407173909186680622&comfile_fn=Press_Release_%28April7%29_Afternoon.pdf&comfile_c=www1&comfile_fd=1586254203260")


# test ------------------------------------------------------------------------


first_day_of_report <- as.Date(first_day) # 3 of April

today_is <- Sys.Date()

seq_dates <- seq(first_day_of_report, today_is, 1)

no_reports <- length(urls)

dest_files <- paste0(seq_dates, ".pdf") 

imap(urls, ~ download.file(url = .x, destfile = dest_files[.y], mode = "wb"))

# load pdfs into R
my_pdfs <- map(dest_files, ~ extract_tables(file = .x))

# extract four tables from each pdf 
# unfortuntely this only works ATM for report in position 4 
scrap_tables(my_pdfs[[4]])
