# post processing extraction

format_results_byCityProvince <- function(x) {
  
  # browser()
  
  sums <- rowSums(x[, -1])  
  
  confirmed <- sum(sums)
  
  out <- c(NA, confirmed, sums[2], sums[1], sums[3], NA, NA)
  
  col_names <- c("Total", "Confirmed", "Discharged", "Isolated", "Deceased", "Being_tested", "Tested_negative")
  
  setNames(out, col_names)
  
}

char_to_num <- function(x){
  
  # browser()
  
  col_names <- names(x)
  
  helper <- function(y) {
    
    as.numeric(str_replace(y, ",", ""))
    
  }
    
  apply(x, 2, helper)
  
  # setNames(t(out), col_names)
  
}

fill_fields <- function(x) {
  
  # browser()
  
  x <- cbind(x, Confirmed = x[, "Discharged"] + x[, "Isolated"]) 
  x <- cbind(x, Deceased = c(0, 0))
  x[, c("Total", "Confirmed", "Discharged", "Isolated", "Deceased", "Being_tested", "Tested_negative")]
  
}

remove_subtotals <- function(x) {
  
  # browser()
  
  x[setdiff(names(x), "Subtotal")]
  
}

fill_fields_2 <- function(x) {
  
  #browser()
  
  x <- c(x, Confirmed = as.numeric(x["Isolated"])) 
  x <- c(x, Discharged = 0)
  x <- c(x, Deceased = 0)
  x[c("Total", "Confirmed", "Discharged", "Isolated", "Deceased", "Being_tested", "Tested_negative")]
  
}

fill_fields_3 <- function(x) {
  
  #browser()
  
  x <- c(x, Confirmed = as.numeric(x["Discharged"]) + as.numeric(x["Isolated"])) 
  x <- c(x, Deceased = 0)
  x[c("Total", "Confirmed", "Discharged", "Isolated", "Deceased", "Being_tested", "Tested_negative")]
  
}

add_num_time <- function(x) {
  
  if(is.matrix(x)){
    
    out <- cbind(Period = c(1, 2), x)
    
  } else {
    
    out <- c(Period = 2, x)  
  }
  
  out
  
} 
