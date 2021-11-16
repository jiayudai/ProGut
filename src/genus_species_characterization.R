setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
combined_pathway_aggregate_genusName_reference <- read.csv("normalized_genus_ec_count.csv")
combined_pathway_aggregate_speciesName_reference <- read.csv("normalized_species_ec_count.csv")

# 5.4 Pick the TOP 10 pathways based on enzyme counts (Genus level)
# 5.4.1 Extract neccessary columns
combined_pathway_aggregate_genusName_reference_sub <- combined_pathway_aggregate_genusName_reference[,c(1,4,7)]
# 5.4.2 Obtain Top 10 pathways for each genus
path_all_genus_normalized <- as.character(unique(combined_pathway_aggregate_genusName_reference_sub$Genus))
path_pw_table_normalized <- data.frame(genus=NULL,pathway_name=NULL)

for (d in 1:length(path_all_genus_normalized)){
  genus <- path_all_genus_normalized[d]
  path_ind_genus_normalized <- combined_pathway_aggregate_genusName_reference_sub[combined_pathway_aggregate_genusName_reference_sub$Genus==genus,]
  path_genus_normalized_subgroup_aggregate <-  path_ind_genus_normalized[order(path_ind_genus_normalized$count_after_normalized, decreasing = TRUE), ]  %>% top_n(10)
  path_pw_table_normalized <- rbind(path_pw_table_normalized,cbind(genus,paste(path_genus_normalized_subgroup_aggregate$pathway_name,collapse = ",")))
  
}
colnames(path_pw_table_normalized) <- c("Genus","Pathway_list")



# 6.4 Pick the TOP 10 pathways based on enzyme counts (Species level)
# 6.4.1 Extract neccessary columns
combined_pathway_aggregate_speciesName_reference_sub <- combined_pathway_aggregate_speciesName_reference[,c(1,2,5,8)]
# 6.4.2 Merge Genus information with Species information
combined_pathway_aggregate_speciesName_reference_sub$Genus_Species <- paste(combined_pathway_aggregate_speciesName_reference_sub$Genus,combined_pathway_aggregate_speciesName_reference_sub$Species)
combined_pathway_aggregate_speciesName_reference_sub_sub <- combined_pathway_aggregate_speciesName_reference_sub[,c(5,3,4)]





# 5.5 Do the intersect and union between genus
path_pw_table_normalized_copy <- path_pw_table_normalized
path_pw_table_normalized_copy$Pathway_list <- as.character(path_pw_table_normalized_copy$Pathway_list)
path_intersect_pathway_genus_normalized_matrix <- matrix(0, nrow = nrow(path_pw_table_normalized_copy), ncol=nrow((path_pw_table_normalized_copy)))
path_res_class_normalized <- data.frame("r","w", "intersect", "union", "intersect_length","union_length",stringsAsFactors = F)
for (r in 1:nrow(path_pw_table_normalized_copy)){
  
  path_genus_normalized <- path_pw_table_normalized_copy$Genus[r]
  
  path_genus_normalized_comp <- path_pw_table_normalized_copy[r,2]
  path_genus_normalized_comp_vector <- c(path_genus_normalized_comp)
  path_genus_normalized_comp_split <- unlist(strsplit(path_genus_normalized_comp_vector, ","))
  
  for (w in 1:nrow(path_pw_table_normalized_copy)){
    #print(f)
    path_genus_normalized2 <- path_pw_table_normalized_copy$Genus[w]
    
    path_genus_normalized_comp2 <- path_pw_table_normalized_copy[w,2]
    path_genus_normalized_comp_vector2 <- c(path_genus_normalized_comp2)
    path_genus_normalized_comp_split2 <- unlist(strsplit(path_genus_normalized_comp_vector2, ","))
    
    path_intersect_normalized3 <- intersect(path_genus_normalized_comp_split, path_genus_normalized_comp_split2)
    path_intersect_normalized_length <- length(path_intersect_normalized3)
    
    
    path_union_normalized3 <- union(path_genus_normalized_comp_split, path_genus_normalized_comp_split2)
    path_union_normalized_length <- length(path_union_normalized3)
    
    # ji = nm; r=o
    path_nm_normalized <- path_intersect_normalized_length / path_union_normalized_length
    
    #o <- c(as.character(x),as.character(y), paste(intersect3,collapse = ","), paste(union3, collapse = ","), intersect_length,union_length)
    p <- c(as.character(path_genus_normalized),as.character(path_genus_normalized2), paste(path_intersect_normalized3,collapse = ","), paste(path_union_normalized3, collapse = ","), path_intersect_normalized_length,path_union_normalized_length)
    #r <- c(e,f,"a","b")
    
    path_res_class_normalized <- rbind(path_res_class_normalized, p)
    
    path_intersect_pathway_genus_normalized_matrix[r,w] <- path_nm_normalized
    
  }
  
}
path_intersect_pathway_genus_normalized_matrix_df <- as.data.frame(path_intersect_pathway_genus_normalized_matrix)
path_res_class_normalized <- path_res_class_normalized[-1,]
path_res_class_normalized$X.intersect_length. <- as.numeric(path_res_class_normalized$X.intersect_length.)
path_res_class_normalized$X.union_length. <- as.numeric(path_res_class_normalized$X.union_length.)
colnames(path_res_class_normalized) <- c("GenusR","GenusW","intersect","union","intersect_length","union_length") 


# 6.4 Pick the TOP 10 pathways based on enzyme counts (Species level)
# 6.4.1 Extract neccessary columns
combined_pathway_aggregate_speciesName_reference_sub <- combined_pathway_aggregate_speciesName_reference[,c(1,2,5,8)]
# 6.4.2 Merge Genus information with Species information
combined_pathway_aggregate_speciesName_reference_sub$Genus_Species <- paste(combined_pathway_aggregate_speciesName_reference_sub$Genus,combined_pathway_aggregate_speciesName_reference_sub$Species)
combined_pathway_aggregate_speciesName_reference_sub_sub <- combined_pathway_aggregate_speciesName_reference_sub[,c(5,3,4)]

# 6.5 Pick the TOP 10 pathways based on enzyme counts (Species level)
path_all_genus_species_normalized <- unique(combined_pathway_aggregate_speciesName_reference_sub_sub$Genus_Species)
path_pw_table_species_normalized <- data.frame(genus_species=NULL,pathway_name=NULL)
for (v in 1:length(path_all_genus_species_normalized)){
  path_genus_species_normalized_temp <- path_all_genus_species_normalized[v]
  path_ind_genus_species_normalized <- which(Pie_chart_input_removed_new_aggregate_speicesName_reference_sub_sub$Genus_Species==path_genus_species_normalized_temp)
  path_genus_species_normalized_subgroup <- Pie_chart_input_removed_new_aggregate_speicesName_reference_sub_sub[path_ind_genus_species_normalized,]
  path_genus_species_normalized_subgroup_aggregate <- path_genus_species_normalized_subgroup[order(path_genus_species_normalized_subgroup$count_after_normalized, decreasing = TRUE), ]
  path_genus_species_normalized_subgroup_aggregate2 <- path_genus_species_normalized_subgroup_aggregate %>% top_n(10)
  path_newrow_species_normalized <- cbind(path_genus_species_normalized_temp,paste(path_genus_species_normalized_subgroup_aggregate2$pathway_name,collapse = ","))
  path_pw_table_species_normalized <- rbind(path_pw_table_species_normalized,path_newrow_species_normalized)
  
}
colnames(path_pw_table_species_normalized) <- c("Genus_Species","Pathway_list")


# 6.6 Do the intersect and union between genus-species
path_pw_table_species_normalized_copy <- path_pw_table_species_normalized
path_pw_table_species_normalized_copy$Pathway_list <- as.character(path_pw_table_species_normalized_copy$Pathway_list)
path_intersect_pathway_genus_species_normalized_matrix <- matrix(0, nrow = nrow(path_pw_table_species_normalized_copy), ncol=nrow((path_pw_table_species_normalized_copy)))
#res <- as.data.frame(matrix(ncol=4, nrow = 0),stringsAsFactors = F)
path_res_class_species_normalized <- data.frame("q","l", "intersect", "union", "intersect_length","union_length",stringsAsFactors = F)
for (q in 1:nrow(path_pw_table_species_normalized_copy)){
  
  path_genus_species_normalized <- path_pw_table_species_normalized_copy$Genus_Species[q]
  
  path_genus_species_normalized_comp <- path_pw_table_species_normalized_copy[q,2]
  path_genus_species_normalized_comp_vector <- c(path_genus_species_normalized_comp)
  path_genus_species_normalized_comp_split <- unlist(strsplit(path_genus_species_normalized_comp_vector, ","))
  
  for (l in 1:nrow(path_pw_table_species_normalized_copy)){
    #print(f)
    path_genus_species_normalized2 <- path_pw_table_species_normalized_copy$Genus_Species[l]
    
    path_genus_species_normalized_comp2 <- path_pw_table_species_normalized_copy[l,2]
    path_genus_species_normalized_comp_vector2 <- c(path_genus_species_normalized_comp2)
    path_genus_species_normalized_comp_split2 <- unlist(strsplit(path_genus_species_normalized_comp_vector2, ","))
    
    path_intersect3_species_normalized <- intersect(path_genus_species_normalized_comp_split, path_genus_species_normalized_comp_split2)
    path_intersect_length_species_normalized <- length(path_intersect3_species_normalized)
    
    
    path_union3_species_normalized <- union(path_genus_species_normalized_comp_split, path_genus_species_normalized_comp_split2)
    path_union_length_species_normalized <- length(path_union3_species_normalized)
    
    # ji = nm; r=o
    path_nm_species_normalized <- path_intersect_length_species_normalized / path_union_length_species_normalized
    
    #o <- c(as.character(x),as.character(y), paste(intersect3,collapse = ","), paste(union3, collapse = ","), intersect_length,union_length)
    z <- c(as.character(path_genus_species_normalized),as.character(path_genus_species_normalized2), paste(path_intersect3_species_normalized,collapse = ","), paste(path_union3_species_normalized, collapse = ","), path_intersect_length_species_normalized,path_union_length_species_normalized)
    #r <- c(e,f,"a","b")
    
    path_res_class_species_normalized <- rbind(path_res_class_species_normalized, z)
    
    path_intersect_pathway_genus_species_normalized_matrix[q,l] <- path_nm_species_normalized

  }

}

path_intersect_pathway_genus_species_normalized_matrix_df <- as.data.frame(path_intersect_pathway_genus_species_normalized_matrix)
path_res_class_species_normalized <- path_res_class_species_normalized[-1,]

path_res_class_species_normalized$X.intersect_length. <- as.numeric(path_res_class_species_normalized$X.intersect_length.)
path_res_class_species_normalized$X.union_length. <- as.numeric(path_res_class_species_normalized$X.union_length.)
colnames(path_res_class_species_normalized) <- c("Genus_SpeciesQ","Genus_SpeciesL","intersect","union","intersect_length","union_length")

path_res_class_species_normalized$GenusQ <- gsub(" .*", "", path_res_class_species_normalized$Genus_SpeciesQ)
path_res_class_species_normalized$GenusL <- gsub(" .*", "", path_res_class_species_normalized$Genus_SpeciesL)

for (e in 1:nrow(path_res_class_species_normalized)){
  if(path_res_class_species_normalized$GenusQ[e] == path_res_class_species_normalized$GenusL[e]){
    path_res_class_species_normalized$Flag[e] <- "TRUE"
  }
  else{
    path_res_class_species_normalized$Flag[e] <- "FALSE"
  }
}

path_indexgenus_species_normalized_true <- which(path_res_class_species_normalized$Flag=="TRUE")
path_Genus_species_normalized_True <- path_res_class_species_normalized[path_indexgenus_species_normalized_true,]

path_indexgenus_species_normalized_false <- which(path_res_class_species_normalized$Flag=="FALSE")
path_Genus_species_normalized_False <- path_res_class_species_normalized[path_indexgenus_species_normalized_false,]

# 7. Genus table analysis
# 7.1 Clean the Genus table
# 7.1.1 Remove the same genus lines 
path_res_class_normalized_copy <- path_res_class_normalized
for (e in 1:nrow(path_res_class_normalized_copy)){
  if(path_res_class_normalized_copy$GenusR[e] == path_res_class_normalized_copy$GenusW[e]){
    path_res_class_normalized_copy$Flag_genus[e] <- "TRUE"
  }
  else{
    path_res_class_normalized_copy$Flag_genus[e] <- "FALSE"
  }
}

indpath_res_class_normalized_copy_true <- which(path_res_class_normalized_copy$Flag_genus=="TRUE")
path_res_class_normalized_copy_True <- path_res_class_normalized_copy[indpath_res_class_normalized_copy_true,]
# We need to keep this one since they are from different genus
indpath_res_class_normalized_copy_false <- which(path_res_class_normalized_copy$Flag_genus=="FALSE")
path_res_class_normalized_copy_False <- path_res_class_normalized_copy[indpath_res_class_normalized_copy_false,]
# 7.1.2 Remove AB-BA
path_res_class_normalized_copy_False_copy <- path_res_class_normalized_copy_False
path_res_class_normalized_copy_False_unique <- path_res_class_normalized_copy_False_copy[!duplicated(t(apply(path_res_class_normalized_copy_False_copy[1:2], 1, sort))),]

# 7.2 Calculate the percentage
path_res_class_normalized_copy_False_unique_percentage <- path_res_class_normalized_copy_False_unique
path_res_class_normalized_copy_False_unique_percentage$percentage <- (path_res_class_normalized_copy_False_unique_percentage$intersect_length / path_res_class_normalized_copy_False_unique_percentage$union_length) * 100

# 8. Species table analysis
# 8.1 Clean the Species table
# 8.1.1 Remove the same genus-species lines 
path_Genus_species_normalized_True_copy <- path_Genus_species_normalized_True
for (e in 1:nrow(path_Genus_species_normalized_True_copy)){
  if(path_Genus_species_normalized_True_copy$Genus_SpeciesQ[e] == path_Genus_species_normalized_True_copy$Genus_SpeciesL[e]){
    path_Genus_species_normalized_True_copy$Flag_species[e] <- "TRUE"
  }
  else{
    path_Genus_species_normalized_True_copy$Flag_species[e] <- "FALSE"
  }
}
indpath_Genus_species_normalized_True_copy_true <- which(path_Genus_species_normalized_True_copy$Flag_species=="TRUE")
path_Genus_species_normalized_True_copy_True <- path_Genus_species_normalized_True_copy[indpath_Genus_species_normalized_True_copy_true,]
# We need to keep this one since they are from same genus but different species
indpath_Genus_species_normalized_True_copy_false <- which(path_Genus_species_normalized_True_copy$Flag_species=="FALSE")
path_Genus_species_normalized_True_copy_False <- path_Genus_species_normalized_True_copy[indpath_Genus_species_normalized_True_copy_false,]
# 8.1.2 Remove AB-BA
path_Genus_species_normalized_True_copy_False_unique <- path_Genus_species_normalized_True_copy_False[!duplicated(t(apply(path_Genus_species_normalized_True_copy_False[1:2], 1, sort))),]
# 8.2 Calculate the percentage
path_Genus_species_normalized_True_copy_False_unique_percentage <- path_Genus_species_normalized_True_copy_False_unique
path_Genus_species_normalized_True_copy_False_unique_percentage$percentage <- (path_Genus_species_normalized_True_copy_False_unique_percentage$intersect_length / path_Genus_species_normalized_True_copy_False_unique_percentage$union_length) * 100

# 9. Create overlapping density plot
# 9.1 Input
# 9.1.1 Genus
path_res_class_normalized_copy_False_unique_percentage_density <- path_res_class_normalized_copy_False_unique_percentage
path_res_class_normalized_copy_False_unique_percentage_density$level <- "Genus"
path_res_class_normalized_copy_False_unique_percentage_density_sub <- path_res_class_normalized_copy_False_unique_percentage_density[,c(8,9)]
# 9.1.2 Species
path_Genus_species_normalized_True_copy_False_unique_percentage_density <- path_Genus_species_normalized_True_copy_False_unique_percentage
path_Genus_species_normalized_True_copy_False_unique_percentage_density$level <- "Species"
path_Genus_species_normalized_True_copy_False_unique_percentage_density_sub <- path_Genus_species_normalized_True_copy_False_unique_percentage_density[,c(11,12)]
# 9.1.3 Combine
path_res_class_normalized_genus_species_density <- rbind(path_res_class_normalized_copy_False_unique_percentage_density_sub,path_Genus_species_normalized_True_copy_False_unique_percentage_density_sub)
# 9.2 Density plot
library(ggplot2)
ggplot(path_res_class_normalized_genus_species_density,aes(x=percentage)) + 
  geom_density(data=subset(path_res_class_normalized_genus_species_density,level == 'Genus'),aes(fill = "Genus"), alpha = 0.2) +
  geom_density(data=subset(path_res_class_normalized_genus_species_density,level == 'Species'),aes(fill = "Species"),alpha = 0.2)


# Test
# For genus, pick the random 366 rows
seed(1) # To make the result of sample() is same. But you have to run this one before the sample()
path_res_class_normalized_copy_False_unique_percentage_density_sub_random <- path_res_class_normalized_copy_False_unique_percentage_density_sub[sample(nrow(path_res_class_normalized_copy_False_unique_percentage_density_sub),nrow(path_Genus_species_normalized_True_copy_False_unique_percentage_density_sub)),]

path_res_class_normalized_genus_species_density_random <- rbind(path_res_class_normalized_copy_False_unique_percentage_density_sub_random,path_Genus_species_normalized_True_copy_False_unique_percentage_density_sub)

ggplot(path_res_class_normalized_genus_species_density_random,aes(x=percentage)) + 
  geom_histogram(data=subset(path_res_class_normalized_genus_species_density_random,level == 'Genus'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(path_res_class_normalized_genus_species_density_random,level == 'Species'),fill = "blue", alpha = 0.2)

ggplot(path_res_class_normalized_genus_species_density_random,aes(x=percentage)) + 
  geom_density(data=subset(path_res_class_normalized_genus_species_density_random,level == 'Genus'),fill = "red", alpha = 0.2) +
  geom_density(data=subset(path_res_class_normalized_genus_species_density_random,level == 'Species'),fill = "blue", alpha = 0.2)


# 10. Chi-Square test among percentages on Genus and Strain levels
path_res_class_normalized_genus_species_density_chi <- path_res_class_normalized_genus_species_density




h1_genus <- hist(path_res_class_normalized_copy_False_unique_percentage_density_sub$percentage)
h1_genus_breaks <- h1_genus$breaks
h1_genus_counts <- h1_genus$counts

h2_species <- hist(path_Genus_species_normalized_True_copy_False_unique_percentage_density_sub$percentage)
h2_species_breaks <- h2_species$breaks
h2_species_counts <- h2_species$counts

h1_genus_counts_df <- as.data.frame(h1_genus_counts)
h2_species_counts_df <- as.data.frame(h2_species_counts)
h1_h2_counts <- cbind(h1_genus_counts_df,h2_species_counts_df)
colnames(h1_h2_counts) <- c("Genus","Species")
rownames(h1_h2_counts) <- c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")
chisq.test(h1_h2_counts) # This is the one for showing the chi-test















