# Do the statistics work based on the intersect and union results
levels=c(3,4,5,10,20,30,40,50)
category <- c("Values = 0","Values > 0 AND <= 0.5","Values > 0.5 AND < 1","Values = 1","Have intersect and union length is 1","Have intersect and union length is NOT 1")
results<-data.frame("category"=category)
for(i in levels){
  freq_final_removed <- read.csv(paste0("cut_results/freq_final_l",i,"_removed.csv"))
  res_removed <- read.csv(paste0("cut_results/res_l",i,"_removed.csv"))
  
  temp1 <- ""
  sum1 <- 0
  temp2 <- ""
  sum2 <- 0
  temp3 <- ""
  sum3 <- 0
  temp4 <- ""
  sum4 <- 0
  sta_result_removed <- data.frame("Category"=category,"Count"=rep(0,length(category)), stringsAsFactors = F)
  
  for (m in 1:nrow(freq_final_removed)){
    if (freq_final_removed$Value[m] == 0){
      temp1 <- freq_final_removed$freq[m]
      sum1 <- sum1 + temp1
      sta_result_removed[1,2] <- sum1
      
    }
    else if (freq_final_removed$Value[m] > 0 && freq_final_removed$Value[m] <= 0.5){
      temp2 <- freq_final_removed$freq[m]
      sum2 <- sum2 + temp2
      sta_result_removed[2,2] <- sum2
      
    }
    else if(freq_final_removed$Value[m] > 0.5 && freq_final_removed$Value[m] < 1){
      temp3 <- freq_final_removed$freq[m]
      sum3 <- sum3 + temp3
      sta_result_removed[3,2] <- sum3
    }
    
    else if (freq_final_removed$Value[m] == 1){
      temp4 <- freq_final_removed$freq[m]
      sum4 <- sum4 + temp4
      sta_result_removed[4,2] <- sum4
      
    }
  }
  
  # :: use for load the functions
  library(dplyr)
  sta_result_removed[is.na(sta_result_removed)] <- 0
  
  # Create another statistic result based on the union length
  # For union length equals to 1
  index_removed_union_1 <- which(res_removed$union_length == "1")
  new_dataset_removed_union_1 <- res_removed[index_removed_union_1,]
  rows_removed_length_1 <- nrow(new_dataset_removed_union_1)
  sta_result_removed[5,2] <- rows_removed_length_1
  
  
  # For union length DOES NOT equal to 1 and ALSO HAS intersect
  index_removed_union_NOT_1 <- which(res_removed$union_length != "1" & res_removed$intersect != "") # Should always use one '&' (list may can use two '&')
  new_dataset_removed_union_NOT_1 <- res_removed[index_removed_union_NOT_1,]
  rows_removed_length_NOT_1 <- nrow(new_dataset_removed_union_NOT_1)
  sta_result_removed[6,2] <- rows_removed_length_NOT_1
  
  
  # Calculate the percentage
  sta_result_removed$Count <- as.numeric(sta_result_removed$Count)
  for (c in 1:nrow(sta_result_removed)){
    sta_result_removed$Percentage[c] <- sta_result_removed$Count[c] * 100 / sum(sta_result_removed$Count[1:4])
  }
  results <- cbind(results,sta_result_removed$Percentage)
}
colnames(results) <- c("Category",paste0("Level",levels,"Percentage"))

write.csv(results, file ="cut_results/sta_result_all_levels_removed.csv", row.names =F)


# Conclusion: Level 3 is the optimal level





