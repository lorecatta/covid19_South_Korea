nineORmore_number_finder <- function(x) {
  
  #browser()
  
  # This just allow positive integers.
    
  logic <- grepl('[0-9]+', x) 
  # logic <- grepl('^[0-9]*[1-9][0-9]*$', x) 
  
  sum(logic) >= 8
  
}

grab_table_epilinks <- function(test_1, index) {
  
  message(index)
  
  #browser()
  table_name <- "Regional distribution and epidemiological links of the confirmed cases|Regional distribution and epidemiological links of confirmed cases"
  
  idx <- stringr::str_which(test_1, table_name)
  
  test_1_table_1 <- test_1[[idx]]
  table_1 <- str_split(test_1_table_1, "\n", simplify = TRUE)
  table_start <- stringr::str_which(table_1, table_name)
  table_end <- stringr::str_which(table_1, "Not classified into a cluster or under investigation")
  if(length(table_end) > 1){
    table_end <- table_end[1]  
  }
  browser()
  
  table_1 <- table_1[1, (table_start +1 ):(table_end - 1)]
  table_1 <- str_replace_all(table_1, "\\s{2,}", "|")
  table_1 <- str_replace_all(table_1, ",", "")
  table_1 <- str_replace_all(table_1, "\r$", "")
  
  test <- lapply(table_1, str_split, "\\|", simplify = TRUE)
  test_2 <- vapply(test, nineORmore_number_finder, logical(1))
  test_3 <- table_1[test_2]
  
  ret <- matrix(0, ncol = 9, nrow = 18)
  #browser()
  if(index == 25 | index == 61) {
    
    test_3 <- test_3[-c(7,9,11)]
    
  }
    
  browser()
  for (i in seq_len(18)){
    
    message(i)
    spl <- str_split(test_3[i], "\\|", simplify = T)  
    spl_2 <- spl[spl != ""]
    indices_of_numbers_only <- grep("\\D", spl_2, invert = T)
    first_idx <- indices_of_numbers_only[1]
    if(length(indices_of_numbers_only) > 15){
      first_idx <- indices_of_numbers_only[8]
    }
    last_idx <- first_idx + 8
    ret[i,] <- spl_2[first_idx:last_idx]
    # numbers_only <- !grepl("\\D", spl_2)
    # ret[i,] <- spl_2[numbers_only]
    
  }
  # browser()
  data_table <- as.data.frame(ret)
  
  region_names <- c("Seoul",
                    "Busan",
                    "Daegu",
                    "Incheon",
                    "Gwangju",
                    "Daejeon",
                    "Ulsan",
                    "Sejong",
                    "Gyeonggi",
                    "Gangwon",
                    "Chung-buk",
                    "Chung-nam",
                    "Jeon-buk",
                    "Jeon-nam",
                    "Gyeong-buk",
                    "Gyeong-nam",
                    "Jeju",
                    "Airport")
  
  data_table <- cbind(index, region_names, data_table)
    
  col_names <- c("id",
                 "region",
                 "total_cc",
                 "imported_cc",
                 "sub_total_epilinks_cc",
                 "shincheonji_cc",
                 "small_clusters_cc",
                 "contacts_cc",
                 "epi_link_imported_cc",
                 "other_cc",
                 "new_cc")
  
  colnames(data_table) <- col_names
  
  data_table
  
}

grab_table_epilinks_2 <- function(test_1, index, page_dims) {
  
  message(index)
  
  table_name <- "Confirmed cases"
  
  idx <- lapply(test_1, function(x, pattern) {
    out <- stringr::str_detect(as.vector(x), pattern)
    any(out)
  })
  
  test_1_table_1 <- test_1[unlist(idx)]
  
  check_page_dims <- vapply(page_dims[[24]], function(x) x[1] > 595, logical(1))
  
  browser()
  
  table_1 <- str_split(test_1_table_1, "\n", simplify = TRUE)
  table_start <- stringr::str_which(table_1, table_name)
  table_end <- stringr::str_which(table_1, "Not classified into a cluster or under investigation")
  if(length(table_end) > 1){
    table_end <- table_end[1]  
  }
  browser()
  
  table_1 <- table_1[1, (table_start +1 ):(table_end - 1)]
  table_1 <- str_replace_all(table_1, "\\s{2,}", "|")
  table_1 <- str_replace_all(table_1, ",", "")
  table_1 <- str_replace_all(table_1, "\r$", "")
  
  test <- lapply(table_1, str_split, "\\|", simplify = TRUE)
  test_2 <- vapply(test, nineORmore_number_finder, logical(1))
  test_3 <- table_1[test_2]
  
  ret <- matrix(0, ncol = 9, nrow = 18)
  
  for (i in seq_len(18)){
    
    message(i)
    spl <- str_split(test_3[i], "\\|", simplify = T)  
    spl_2 <- spl[spl != ""]
    indices_of_numbers_only <- grep("\\D", spl_2, invert = T)
    first_idx <- indices_of_numbers_only[1]
    last_idx <- first_idx + 8
    ret[i,] <- spl_2[first_idx:last_idx]
    # numbers_only <- !grepl("\\D", spl_2)
    # ret[i,] <- spl_2[numbers_only]
    
  }
  
  data_table <- as.data.frame(ret)
  
  region_names <- c("Seoul",
                    "Busan",
                    "Daegu",
                    "Incheon",
                    "Gwangju",
                    "Daejeon",
                    "Ulsan",
                    "Sejong",
                    "Gyeonggi",
                    "Gangwon",
                    "Chung-buk",
                    "Chung-nam",
                    "Jeon-buk",
                    "Jeon-nam",
                    "Gyeong-buk",
                    "Gyeong-nam",
                    "Jeju",
                    "Airport")
  
  data_table <- cbind(region_names, data_table)
  
  col_names <- c("region",
                 "total_cc",
                 "imported_cc",
                 "sub_total_epilinks_cc",
                 "shincheonji_cc",
                 "small_clusters_cc",
                 "contacts_cc",
                 "epi_link_imported_cc",
                 "other_cc",
                 "new_cc")
  
  colnames(data_table) <- col_names
  
  data_table
  
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

