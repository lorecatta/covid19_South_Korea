format_table_1 <- function(my_pdf) {
  
  ret_1 <- my_pdf[[1]]
  
  data_table <- as.data.frame(ret_1, stringsAsFactors = FALSE)
  
  spl_1 <- strsplit(data_table$V4[7], " ")
  spl_2 <- strsplit(data_table$V4[10], " ")
  
  raw_dates <- data_table$V1[c(8, 11)]
  
  spl_3 <- strsplit(raw_dates, " ")
  
  new_dates <- fix_date(spl_3)
  
  data.frame(date = new_dates,
             total = data_table$V2[c(7, 10)],
             pos_confirmed = data_table$V3[c(7, 10)],
             pos_discharged = c(spl_1[[1]][1], spl_2[[1]][1]),
             pos_isolated = c(spl_1[[1]][2], spl_2[[1]][2]),
             pos_deaths = data_table$V5[c(7, 10)],
             being_tested = data_table$V6[c(7, 10)],
             neg_tested = data_table$V7[c(7, 10)], 
             stringsAsFactors = FALSE)
  
}

fix_date <- function(spl_3) {
  
  full_date <- c()
  
  for (i in seq_along(spl_3)) {
    
    day_code <- sprintf("%02d", as.numeric(spl_3[[i]][1]))
    ret <- which(month.name %in% spl_3[[i]][2])
    month_code <- sprintf("%02d", ret)
    full_date[i] <- paste(day_code, month_code, "2020", sep = "-")
    
  }
  
  full_date
  
}

format_table_2 <- function(my_pdf) {
  
  ret_1 <- my_pdf[[2]]
  
  table_no_2 <- as.data.frame(ret_1, stringsAsFactors = FALSE)
  
  spl_1 <- strsplit(table_no_2$V4, " ")
  
  spl_2 <- strsplit(table_no_2$V7, " ")
  
  spl_3 <- strsplit(table_no_2$V8, " ")
  
  origin_names <- c(table_no_2$V3[3], 
                    paste0(spl_1[[3]][1], "(", spl_1[[3]][2], ")"),
                    spl_1[[3]][3],
                    table_no_2$V5[3],
                    table_no_2$V6[3])
  
  where_confirmed_names <- c(spl_2[[3]]) 
  
  col_names <- c("total", 
                 paste0("from_", origin_names),
                 paste0("confirmed_in_", spl_2[[3]]),
                 paste0(spl_3[[3]], "_national"))
  
  data_table <- data.frame(table_no_2$V2[c(4, 6)],
                           table_no_2$V3[c(4, 5)],
                           c(spl_1[[4]][1], spl_1[[5]][1]),
                           c(spl_1[[4]][2], spl_1[[5]][2]),
                           table_no_2$V5[c(4, 5)],
                           table_no_2$V6[c(4, 5)],
                           c(spl_2[[4]][1], spl_2[[5]][1]),
                           c(spl_2[[4]][2], spl_2[[5]][2]),
                           c(spl_3[[4]][1], spl_3[[5]][1]),
                           c(spl_3[[4]][2], spl_3[[5]][2]))
  
  set_names(data_table, col_names)
  
}

