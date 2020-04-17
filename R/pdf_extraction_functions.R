grab_table_1 <- function(test_1, index) {
  
  message(index)
  
  # browser()
  
  test_1_table_1 <- test_1[[1]]
  table_1 <- str_split(test_1_table_1, "\n", simplify = TRUE)
  table_start <- stringr::str_which(table_1, "Period |        Period\r|       Period\r")
  table_end <- stringr::str_which(table_1, "Difference|Differences|Change")
  if(length(table_end) > 1){
    table_end <- table_end[1]  
  }
  table_1 <- table_1[1, (table_start +1 ):(table_end - 1)]
  table_1 <- str_replace_all(table_1, "\\s{2,}", "|")
  
  idx_1 <- 4
  idx_2 <- 7
  
  spl_4 <- strsplit(table_1[idx_1], "\\|")
  
  if(spl_4[[1]][2] == "total" | spl_4[[1]][2] == "isolation isolation") {
    
    idx_1 <- idx_1 + 2
    idx_2 <- idx_2 + 2
    spl_4 <- strsplit(table_1[idx_1], "\\|")
  
  }
  
  spl_7 <- strsplit(table_1[idx_2], "\\|")  
  l_spl_4 <- length(spl_4[[1]])
  l_spl_7 <- length(spl_7[[1]])
  if(l_spl_4 == 17) {
    spl_4[[1]] <- spl_4[[1]][1:8]
    table_1[idx_1] <- str_c(spl_4[[1]], collapse = "|")
  }
  if(l_spl_7 == 9 & l_spl_4 == 8) {
    spl_7[[1]] <- spl_7[[1]][-length(spl_7[[1]])]
    table_1[idx_2] <- str_c(spl_7[[1]], collapse = "|")
  }
  
  text_con <- textConnection(table_1[c(idx_1, idx_2)])
  data_table <- read.csv(text_con, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  
  if(is.na(data_table[1, 1])) { 
    ret <- data_table[, -1]
  } else{
    ret <- data_table
  }
  
  if(ncol(ret) == 8) {
    
    ret <- ret[-6]  
    
  }
  
  col_names <- c("Total", "Confirmed", "Discharged", "Isolated", "Deceased", "Being_tested", "Tested_negative")
  
  out <- setNames(ret, col_names)
  
  data.frame(lapply(out, as.character), stringsAsFactors = FALSE)
  
}

grab_table_1_byCityProvince <- function(test_1, index) {
  
  message(index)
  
  #browser()
  
  test_1_table_1 <- test_1[[1]]
  table_1 <- str_split(test_1_table_1, "\n", simplify = TRUE)
  table_start <- stringr::str_which(table_1, "Total")
  table_end <- stringr::str_which(table_1, "Total")
  if(length(table_start) > 1){
    table_start <- table_start[1]  
  }
  if(length(table_end) > 1){
    table_end <- table_end[3]  
  }
  table_1 <- table_1[1, (table_start +1 ):(table_end - 1)]
  table_1 <- str_replace_all(table_1, "\\s{2,}", "|")
  
  spl <- strsplit(table_1, "\\|") 
  
  # browser()
  
  if(spl[[9]][2]== "gi") {
    
    idxs <- c(1,2,3,11,12,13) 
    
    spl_sub_2 <- spl
    
    spl_sub_2[[idxs[1]]] <- spl_sub_2[[idxs[1]]][3:length(spl_sub_2[[idxs[1]]])]
    spl_sub_2[[idxs[2]]] <- spl_sub_2[[idxs[2]]][2:length(spl_sub_2[[idxs[2]]])]
    spl_sub_2[[idxs[3]]] <- spl_sub_2[[idxs[3]]][2:length(spl_sub_2[[idxs[3]]])]
    spl_sub_2[[idxs[4]]] <- spl_sub_2[[idxs[4]]][3:length(spl_sub_2[[idxs[4]]])]
    spl_sub_2[[idxs[5]]] <- spl_sub_2[[idxs[5]]][2:length(spl_sub_2[[idxs[5]]])]
    spl_sub_2[[idxs[6]]] <- spl_sub_2[[idxs[6]]][2:length(spl_sub_2[[idxs[6]]])]
    
    spl_sub_c <- lapply(spl_sub_2, str_c, collapse = "|")
    
  } else {
    
    idxs <- c(1,2,3,9,10,11) 
    
    spl_sub <- lapply(spl, function(x) x[3:length(x)])
    
    spl_sub_2 <- lapply(spl_sub, str_replace, "\\*", "")
    
    spl_sub_c <- lapply(spl_sub_2, str_c, collapse = "|")
    
  }
  
  spl_sub_c_rbind_1 <- do.call("rbind", spl_sub_c[idxs[1:3]])
  
  spl_sub_c_rbind_2 <- do.call("rbind", spl_sub_c[idxs[4:6]])
  
  text_con_1 <- textConnection(spl_sub_c_rbind_1)
  
  text_con_2 <- textConnection(spl_sub_c_rbind_2)
  
  data_table_1 <- read.csv(text_con_1, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  
  data_table_2 <- read.csv(text_con_2, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  
  ret <- cbind(data_table_1, data_table_2)
  
  if(ncol(ret) == 18) {
    
    col_names <- c("Total", "Seoul", "Busan", "Daegu", "Incheon", "Gwangju", "Daejeon", "Ulsan", "Sejong",
                   "Gyeonggi", "Gangwon", "Chung-buk", "Chung-nam", "Jeon-buk", "Jeon-nam", "Gyeong-buk", "Gyeong-nam", "Jeju")
  
  } else {
    
    col_names <- c("Total", "Seoul", "Busan", "Daegu", "Incheon", "Gwangju", "Daejeon", "Ulsan", "Sejong",
                   "Gyeonggi", "Gangwon", "Chung-buk", "Chung-nam", "Jeon-buk", "Jeon-nam", "Gyeong-buk", "Gyeong-nam", "Jeju", "Airport-screening")
    
  }
  
  out <- setNames(ret, col_names)
  
  data.frame(lapply(out, as.character), stringsAsFactors = FALSE)
  
}

grab_table_1_oneRow <- function(test_1, index) {
  
  message(index)
  
  # browser()
  
  test_1_table_1 <- test_1[[1]]
  table_1 <- str_split(test_1_table_1, "\n", simplify = TRUE)
  table_start <- stringr::str_which(table_1, "Period |        Period\r|       Period\r")
  table_end <- stringr::str_which(table_1, "Difference|Differences|Change")
  table_start_2 <- table_start + 1
  
  if(length(table_end) > 1) {
    table_end_2 <- table_end[1] - 1 
  } else {
    table_end_2 <- table_end - 1
  }
  
  table_1 <- table_1[1, table_start_2:table_end_2]
  table_1 <- str_replace_all(table_1, "\\s{1,}", "|")
  
  idx_1 <- 5
  idx_2 <- 8
  idx_3 <- 2
  
  spl <- strsplit(table_1, "\\|")
  
  if((length(spl) >= 8 | length(spl) == 5) | length(spl) == 6) {
    
    if(spl[[idx_1]][2] == "As")  {
      
      idx_1 <- 4
      idx_2 <- 5
      idx_3 <- 7
      
    }
    
  }
  
  if(length(spl) == 6) {
    
    if(spl[[5]][2] == "As" & spl[[6]][2] == "As") {
      
      idx_1 <- 5
      idx_2 <- 6
      idx_3 <- 7
      
    }
  }
  
  if(length(spl) > 9) {
    
    idx_1 <- 9
    idx_2 <- 10
    idx_3 <- 7
    
  }
  
  if(length(spl) == 9) {
    
    if(spl[[9]][2] == "As") {
      
      idx_1 <- 8
      idx_2 <- 9
      idx_3 <- 7
      
    }
    
  }
  
  if(length(spl) == 7) {
    
    idx_1 <- 4
    idx_2 <- 6
    idx_3 <- 7
    spl[[idx_2]] <- spl[[idx_2]][1:14]
    
  }
  
  if(length(spl) == 4) {
    
    idx_1 <- 3
    idx_2 <- 4
    idx_3 <- 7
    
    if(spl[[idx_1]][5] == "Feb.") {
      idx_3 <- 6  
    }
    
  }
  
  if(length(spl) == 3) {
    
    idx_1 <- 2
    idx_2 <- 3
    idx_3 <- 6
    
    if(spl[[idx_2]][idx_3] == "Feb.") {
      idx_3 <- 7  
    }
    
  }
  
  spl[[idx_1]] <- spl[[idx_1]][idx_3:length(spl[[idx_1]])]
  spl[[idx_2]] <- spl[[idx_2]][idx_3:length(spl[[idx_2]])]
  
  spl_sub <- spl[c(idx_1, idx_2)]
  
  spl_sub_2 <- lapply(spl_sub, str_replace, "\\*", "")
  
  spl_sub_c <- lapply(spl_sub_2, str_c, collapse = "|")
  
  spl_sub_c_rbind <- do.call("rbind", spl_sub_c)
  
  text_con <- textConnection(spl_sub_c_rbind)
  data_table <- read.csv(text_con, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  
  if(is.na(data_table[1, 1])) { 
    ret <- data_table[, -1]
  } else{
    ret <- data_table
  }
  
  ret_2 <- ret
  
  if(ncol(ret) == 8) {
    
    ret_2 <- ret[-6]
    
  }

  if(ncol(ret) == 7) {
    
    ret_2 <- ret[-c(2, 5)]
    
  }

  if(ncol(ret) == 6) {
    
    ret_2 <- ret[-4]
    
  }

  if(ncol(ret_2) == 7) {
    
    col_names <- c("Total", "Confirmed", "Discharged", "Isolated", "Deceased", "Being_tested", "Tested_negative")
    
  } 
  
  if(ncol(ret_2) == 5) {
    
    col_names <- c("Total", "Isolated", "Discharged", "Being_tested", "Tested_negative")
    
  }
  
  out <- setNames(ret_2, col_names)
  
  data.frame(lapply(out, as.character), stringsAsFactors = FALSE)

}

grab_table_1_oneDate <- function(test_1, index) {
  
  message(index)
  
  # browser()
  
  test_1_table_1 <- test_1[[1]]
  table_1 <- str_split(test_1_table_1, "\n", simplify = TRUE)
  table_start <- stringr::str_which(table_1, "Period |        Period\r|       Period\r")
  table_end <- stringr::str_which(table_1, "Out|The newly|KCDC also provided|In addition|KCDC released the results of epidemiological investigation|About")
  table_start_2 <- table_start + 1
  
  if(length(table_end) > 1){
    table_end_2 <- table_end[1] - 1
  } else {
    table_end_2 <- table_end - 1
  }
  
  table_1 <- table_1[1, table_start_2:table_end_2]
  table_1 <- str_replace_all(table_1, "\\s{1,}", "|")
  
  idx_1 <- 5
  idx_3 <- 1
  
  spl <- strsplit(table_1, "\\|")
  
  if(length(spl) == 4) {
    idx_1 <- 4
    idx_3 <- 7
  }
  
  if(length(spl) == 3) {
    idx_1 <- 3
    idx_3 <- 7
    
    if(length(spl[[idx_1]]) == 10){
      idx_3 <- 6  
    }
    
  }

  if(length(spl) == 2) {
    idx_1 <- 2
    idx_3 <- 7
  }

  spl[[idx_1]] <- spl[[idx_1]][idx_3:length(spl[[idx_1]])]

  spl_sub <- spl[idx_1]
  
  # spl_sub <- lapply(spl[c(idx_1, idx_2)], function(x) {
  #   x[1] <- word(x[1], 6, 6)
  #   x})
  
  spl_sub_c <- str_c(spl_sub[[1]], collapse = "|")
  
  text_con <- textConnection(spl_sub_c)
  data_table <- read.csv(text_con, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  
  if(is.na(data_table[1, 1])) { 
    ret <- data_table[, -1]
  } else{
    ret <- data_table
  }
  
  if(ncol(ret) == 7) {
    
    ret <- ret[-2]
    
  }
  
  if(ncol(ret) == 8) {
    
    col_names <- c("Total", "Confirmed", "Discharged", "Isolated", "Deceased", "Subtotal", "Being_tested", "Tested_negative")
    
  } 
  
  if(ncol(ret) == 6) {
    
    col_names <- c("Total", "Isolated", "Discharged", "Subtotal", "Being_tested", "Tested_negative")
    
  }
  
  if(ncol(ret) == 5) {
    
    col_names <- c("Total", "Isolated", "Subtotal", "Being_tested", "Tested_negative")
    
  }

  out <- setNames(ret, col_names)
  
  data.frame(lapply(out, as.character), stringsAsFactors = FALSE)
  
}

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

format_table_2 <- function(ret_1) {
  
  # ret_1 <- my_pdf[[2]]
  
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
                           c(spl_3[[4]][2], spl_3[[5]][2]),
                           stringsAsFactors = FALSE)
  
  set_names(data_table, col_names)
  
}

format_table_3 <- function(my_pdf) {
  
  ret_1 <- my_pdf[[4]]
  
  table_no_3 <- as.data.frame(ret_1, stringsAsFactors = FALSE)
  
  # for names col
  idx <- which(table_no_3$V1 != "" & 
                 table_no_3$V1 != "Region")
  
  raw_nms <- table_no_3$V1[idx]
  
  name_1 <- paste0(raw_nms[11], raw_nms[12])
  name_2 <- paste0(raw_nms[13], raw_nms[14])
  name_3 <- paste0(raw_nms[17], raw_nms[18])
  name_4 <- paste0(raw_nms[19], raw_nms[20])
  
  raw_nms[11] <- name_1
  raw_nms[13] <- name_2
  raw_nms[17] <- name_3
  raw_nms[19] <- name_4 
  
  reg_names <- raw_nms[-c(12,14,18,20)]
  
  # for tot col 
  idx_2 <- which(table_no_3$V2 != "" & 
                   table_no_3$V2 != "Total")
  
  tot_values <- table_no_3$V2[idx_2] 
  
  # for imported cases col
  idx_3 <- which(!grepl('Imported|Sub-cases|total|\\(|^$', table_no_3$V3))
  
  raw_imp_cases <- table_no_3$V3[idx_3]
  
  spl_1 <- strsplit(raw_imp_cases, " ")
  
  imp_cases <- vapply(spl_1, "[[", character(1), 1)
  sub_total <- vapply(spl_1, "[[", character(1), 2)
  
  # for cluster 1
  idx_4 <- which(!grepl('Confirmed cases|Clusters|Contacts|Shin- Small of|cheonji clusters confirmed|cases|\\(|^$', table_no_3$V4))
  
  raw_cluster_1 <- table_no_3$V4[idx_4]
  
  spl_2 <- strsplit(raw_cluster_1, " ")
  
  cl_1 <- vapply(spl_2, "[[", character(1), 1)
  cl_2 <- vapply(spl_2, "[[", character(1), 2)
  cl_3 <- vapply(spl_2, "[[", character(1), 3)
  
  # for imported cases
  idx_5 <- which(!grepl('Imported|cases|\\(|^$', table_no_3$V5))
  
  imported_cases <- table_no_3$V5[idx_5]
  
  # for other
  idx_6 <- which(!grepl('Other*|\\(|^$', table_no_3$V6)) 
  
  other <- table_no_3$V6[idx_6]
  
  # for new cases 
  idx_7 <- which(!grepl('New|cases|\\(|^$', table_no_3$V7)) 
  
  new_cases <- table_no_3$V7[idx_7]
  
  data.frame(region = reg_names,
             total = tot_values,
             imported_cases = imp_cases,
             sub_total = sub_total,
             Shin_cheonji = cl_1,
             small_clusters = cl_2,
             contacts_of_cases = cl_3,
             imported_cases_to_cluster = imported_cases,
             other = other, 
             new_cases = new_cases,
             stringsAsFactors = FALSE)
  
}

format_table_4 <- function(my_pdf) {
  
  ret_1 <- my_pdf[[5]]
  
  table_no_4 <- as.data.frame(ret_1, stringsAsFactors = FALSE)
  
  age_group_nms <- table_no_4$V1[-1]
  total_values <- table_no_4$V2[-1]
  severe_values <- table_no_4$V3[-1]
  very_severe_values <- table_no_4$V7[-1]
  
  data.frame(age_group = age_group_nms,
             total = total_values,
             severe = severe_values,
             very_severe = very_severe_values,
             stringsAsFactors = FALSE)
  
}
