library("dplyr")

combined_pathway_aggregate_genusName_reference <- read.csv("normalized_genus_ec_count.csv")
combined_pathway_aggregate_speciesName_reference <- read.csv("normalized_species_ec_count.csv")

# 1. Pick the TOP 10 pathways based on enzyme counts (Genus level)
# 1.1 Extract neccessary columns
combined_pathway_aggregate_genusName_reference_sub <- combined_pathway_aggregate_genusName_reference[,c(1,4,7)]
# 1.2 Obtain Top 10 pathways for each genus
path_all_genus_normalized <- as.character(unique(combined_pathway_aggregate_genusName_reference_sub$Genus))
path_pw_table_normalized <- data.frame(genus=NULL,pathway_name=NULL)

for (d in 1:length(path_all_genus_normalized)){
  genus <- path_all_genus_normalized[d]
  path_ind_genus_normalized <- combined_pathway_aggregate_genusName_reference_sub[combined_pathway_aggregate_genusName_reference_sub$Genus==genus,]
  path_genus_normalized_subgroup_aggregate <-  path_ind_genus_normalized[order(path_ind_genus_normalized$count_after_normalized, decreasing = TRUE), ]  %>% top_n(10)
  path_pw_table_normalized <- rbind(path_pw_table_normalized,cbind(genus,paste(path_genus_normalized_subgroup_aggregate$pathway_name,collapse = ",")))
  
}
colnames(path_pw_table_normalized) <- c("Genus","Pathway_list")
write.csv(path_pw_table_normalized,"top10_genus_pathway.csv", row.names = F)

# 2. Pick the TOP 10 pathways based on enzyme counts (Species level)
# 2.1 Extract neccessary columns
combined_pathway_aggregate_speciesName_reference_sub <- combined_pathway_aggregate_speciesName_reference[,c(1,2,5,8)]
# 2.2 Merge Genus information with Species information
combined_pathway_aggregate_speciesName_reference_sub$Genus_Species <- paste(combined_pathway_aggregate_speciesName_reference_sub$Genus,combined_pathway_aggregate_speciesName_reference_sub$Species)
combined_pathway_aggregate_speciesName_reference_sub_sub <- combined_pathway_aggregate_speciesName_reference_sub[,c(5,3,4)]

path_all_genus_species_normalized <- as.character(unique(combined_pathway_aggregate_speciesName_reference_sub_sub$Genus_Species))
path_pw_table_species_normalized <- data.frame(genus_species=NULL,pathway_name=NULL)

for (v in 1:length(path_all_genus_species_normalized)){
  genus_species <- path_all_genus_species_normalized[v]
  path_ind_genus_species_normalized <- combined_pathway_aggregate_speciesName_reference_sub_sub[combined_pathway_aggregate_speciesName_reference_sub_sub$Genus_Species==genus_species,]
  path_genus_species_normalized_subgroup_aggregate <- path_ind_genus_species_normalized[order(path_ind_genus_species_normalized$count_after_normalized, decreasing = TRUE), ] %>% top_n(10)
  path_pw_table_species_normalized <- rbind(path_pw_table_species_normalized, cbind(genus_species,paste(path_genus_species_normalized_subgroup_aggregate$pathway_name,collapse = ",")))
  
}
colnames(path_pw_table_species_normalized) <- c("Genus_Species","Pathway_list")
write.csv(path_pw_table_species_normalized,"top10_genus_species_pathway.csv", row.names = F)

