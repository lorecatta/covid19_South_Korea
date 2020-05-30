choose_cleaning_fun <- function(my_pdf) {
  
  browser()
  
  for (i in seq_along(my_pdf)) {
    
    message(i)
    
    one_tab <- my_pdf[[i]]
    
    if(dim(one_tab)[1] >= 3 & dim(one_tab)[2] >= 4) {
      
      if(one_tab[3,1] == "Region" & one_tab[1,4] == "Confirmed cases") {
        
        table_3 <- format_table_3(one_tab)
        
      }
      
    }
    
    if(dim(one_tab)[1] >= 2 & dim(one_tab)[2] >= 2) {
      
      if(one_tab[1,1] == "Period" & one_tab[2,2] == "Total") {
        
        table_1 <- format_table_1(one_tab)
        
      }
      
    }
    
  }
  
  list(table_1, table_3)
  
}


scrap_tables <- function(my_pdf) {
  
  choose_cleaning_fun(my_pdf)
  
  # total confirmed and suspected cases 
  # table_1 <- format_table_1(my_pdf)
  
  # imported cases 
  # table_2 <- format_table_2(my_pdf) 
  
  # epi cluster tables
  # table_3 <- format_table_3(my_pdf)
  
  # severe cases 
  # table_4 <- format_table_4(my_pdf)
  
  # list(table_1, table_2, table_3, table_4)
  
}

format_table_1 <- function(ret_1) {
  
  #ret_1 <- my_pdf[[1]]
  
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
