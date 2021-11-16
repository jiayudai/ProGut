path_pw_table_normalized <- read.csv("top10_genus_pathway.csv",stringsAsFactors = F)
path_pw_table_species_normalized <- read.csv("top10_genus_species_pathway.csv",stringsAsFactors = F)

# 1. Do the intersect and union between genus
path_res_class_normalized <- data.frame("r","w", "intersect", "union", "intersect_length","union_length","jaccard_index_percentage",stringsAsFactors = F)
counts = nrow(path_pw_table_normalized)
for (r in 1:(counts-1)){
  
  path_genus_normalized <- path_pw_table_normalized$Genus[r]
  path_genus_normalized_comp_split <- unlist(strsplit(path_pw_table_normalized[r,2], ","))
  
  for (w in (r+1):nrow(path_pw_table_normalized)){
    #print(f)
    path_genus_normalized2 <- path_pw_table_normalized$Genus[w]
    path_genus_normalized_comp_split2 <- unlist(strsplit(path_pw_table_normalized[w,2], ","))
    
    path_intersect_normalized3 <- intersect(path_genus_normalized_comp_split, path_genus_normalized_comp_split2)
    path_intersect_normalized_length <- length(path_intersect_normalized3)
    
    path_union_normalized3 <- union(path_genus_normalized_comp_split, path_genus_normalized_comp_split2)
    path_union_normalized_length <- length(path_union_normalized3)
    path_nm_normalized <- path_intersect_normalized_length * 100 / path_union_normalized_length
    
    p <- c(path_genus_normalized,path_genus_normalized2, paste(path_intersect_normalized3,collapse = ","),
           paste(path_union_normalized3, collapse = ","), path_intersect_normalized_length,
           path_union_normalized_length, path_nm_normalized)
    
    path_res_class_normalized <- rbind(path_res_class_normalized, p)
  }
}


path_res_class_normalized <- path_res_class_normalized[-1,]
colnames(path_res_class_normalized) <- c("GenusR","GenusW","intersect","union","intersect_length","union_length","jaccard_index_percentage")
write.csv(path_res_class_normalized,"intersect_union_genus_pathway.csv", row.names = F)


# 2. Do the intersect and union between genus-species
path_res_class_species_normalized <- data.frame("q","l", "intersect", "union", "intersect_length","union_length","jaccard_index_percentage",stringsAsFactors = F)
counts_species = nrow(path_pw_table_species_normalized)
for (q in 1:(counts_species-1)){
  
  path_genus_species_normalized <- path_pw_table_species_normalized$Genus_Species[q]
  path_genus_species_normalized_comp_split <- unlist(strsplit(path_pw_table_species_normalized[q,2], ","))
  
  for (l in (q+1):nrow(path_pw_table_species_normalized)){
    #print(f)
    path_genus_species_normalized2 <- path_pw_table_species_normalized$Genus_Species[l]
    path_genus_species_normalized_comp_split2 <- unlist(strsplit(path_pw_table_species_normalized[l,2], ","))
    
    path_intersect_species_normalized3 <- intersect(path_genus_species_normalized_comp_split, path_genus_species_normalized_comp_split2)
    path_intersect_species_normalized_length <- length(path_intersect_species_normalized3)
    
    
    path_union_species_normalized3 <- union(path_genus_species_normalized_comp_split, path_genus_species_normalized_comp_split2)
    path_union_species_normalized_length <- length(path_union_species_normalized3)
    
    path_nm_species_normalized <- path_intersect_species_normalized_length * 100 / path_union_species_normalized_length
    
    z <- c(path_genus_species_normalized,path_genus_species_normalized2, paste(path_intersect_species_normalized3,collapse = ","), paste(path_union_species_normalized3, collapse = ","), path_intersect_species_normalized_length,path_union_species_normalized_length,path_nm_species_normalized)
    
    path_res_class_species_normalized <- rbind(path_res_class_species_normalized, z)
    
  }
}
path_res_class_species_normalized <- path_res_class_species_normalized[-1,]
colnames(path_res_class_species_normalized) <- c("Genus_SpeciesQ","Genus_SpeciesL","intersect","union","intersect_length","union_length","jaccard_index_percentage")

# 3. Pick the records for same genus but different species
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
path_Genus_species_normalized_sameGenus <- path_res_class_species_normalized[which(path_res_class_species_normalized$Flag=="TRUE"),]
path_Genus_species_normalized_sameGenus <- path_Genus_species_normalized_sameGenus[,c(1:7)]
write.csv(path_Genus_species_normalized_sameGenus,"intersect_union_sameGenus_species_pathway.csv", row.names = F)
