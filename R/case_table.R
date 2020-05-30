number_finder <- function(x) {
  
  #ret <- str_replace_all(x, ",", "")
  
  logic <- grepl('\\d{6}', x)
  
  any(logic)

}

grab_table_1 <- function(test_1, index) {
  
  message(index)
  
  test_1_table_1 <- test_1[[1]]
  table_1 <- str_split(test_1_table_1, "\n", simplify = TRUE)
  table_start <- stringr::str_which(table_1, "Period |        Period\r|       Period\r")
  table_end <- stringr::str_which(table_1, "Difference|Differences|Change")
  if(length(table_end) > 1){
    table_end <- table_end[1]  
  }
  table_1 <- table_1[1, (table_start +1 ):(table_end - 1)]
  table_1 <- str_replace_all(table_1, "\\s{2,}", "|")
  table_1 <- str_replace_all(table_1, ",", "")
  table_1 <- str_replace_all(table_1, "\r$", "")
  
  # browser()
  
  # alternative - more general?
  end <- 8
  
  if(index == 1 | index == 2) {
    
    end <- 9
    
  }
  
  test <- lapply(table_1, str_split, "\\|", simplify = TRUE)
  test_2 <- vapply(test, number_finder, logical(1))
  test_3 <- table_1[test_2]
  spl_1 <- str_split(test_3[1], "\\|")
  spl_2 <- str_split(test_3[2], "\\|")
  test_3[1] <- str_c(spl_1[[1]][1:end], collapse = "|")
  test_3[2] <- str_c(spl_2[[1]][1:end], collapse = "|")
  text_con <- textConnection(test_3)

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
  
  setNames(ret, col_names)

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
