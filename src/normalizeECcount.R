# 1. Read the input files
# stringsAsFactors can transform a string to a factor. So it should be false in this case.
combined_pathway <- read.csv("combined_pathway.csv",stringsAsFactors = F)


# 2. Extract the Genus, Species and Strain for each bacteria
combined_pathway$Genus <- gsub(" .*", "", combined_pathway$Bacteria)
combined_pathway$SpeciesAndStrain <- sub(".*? ", "", combined_pathway$Bacteria)
combined_pathway$Species <- gsub(" .*", "", combined_pathway$SpeciesAndStrain)
combined_pathway$Strain <- sub(".*? ", "", combined_pathway$SpeciesAndStrain)
combined_pathway$SpeciesAndStrain <- NULL

# 3. Calculate the reference count for normalization
combined_pathway_reference <- aggregate(combined_pathway$EC_number, list(combined_pathway$Pathway),function(x) length(unique(x)))
colnames(combined_pathway_reference) <- c("pathway","count_reference")
combined_pathway_reference$pathway <- gsub("path:","",combined_pathway_reference$pathway,fixed = TRUE)

################# 4. Genus Level Charecterization #################
# 4.1 Count the enzymes for each genus-pathway
combined_pathway_aggregate_genus <- aggregate(combined_pathway$EC_number, list(combined_pathway$Genus, combined_pathway$Pathway),function(x) length(unique(x)))
colnames(combined_pathway_aggregate_genus) <- c("Genus","pathway","count")
combined_pathway_aggregate_genus$pathway <- gsub("path:","",combined_pathway_aggregate_genus$pathway,fixed = TRUE)

# 4.2 Connect with pathway names
Pathway_Class <- read.csv("keggPathway.csv", stringsAsFactors = F)
library(dplyr)
path_genusToKeep <- intersect(Pathway_Class$Map , combined_pathway_aggregate_genus$pathway)
path_genus_intersect <- Pathway_Class[Pathway_Class$Map %in% path_genusToKeep,]
combined_pathway_aggregate_genusName <- combined_pathway_aggregate_genus %>% left_join(Pathway_Class, by=c("pathway"="Map"))
colnames(combined_pathway_aggregate_genusName) <- c("Genus","pathway","count","pathway_name","pathway_class")

# 4.3 Normalized the enzyme counts
# 4.3.1 Connect the genus table with the reference count table
combined_pathway_aggregate_genusName_reference <- combined_pathway_aggregate_genusName %>% left_join(combined_pathway_reference, by=c("pathway"="pathway"))
colnames(combined_pathway_aggregate_genusName_reference) <- c("Genus","map_ID","count_before_normalized","pathway_name","pathway_class","count_reference")
# 4.3.2. Calculate the normalized count
combined_pathway_aggregate_genusName_reference$count_after_normalized <- combined_pathway_aggregate_genusName_reference$count_before_normalized / combined_pathway_aggregate_genusName_reference$count_reference
write.csv(combined_pathway_aggregate_genusName_reference,"normalized_genus_ec_count.csv", row.names=F)

################# 5. Species Level Charecterization #################
# 5.1 Count the enzymes for each genus-species-pathway
combined_pathway_aggregate_species <- aggregate(combined_pathway$EC_number, list(combined_pathway$Genus,combined_pathway$Species,combined_pathway$Pathway),function(x) length(unique(x)))
colnames(combined_pathway_aggregate_species) <- c("Genus","Species","pathway","count")
combined_pathway_aggregate_species$pathway <- gsub("path:","",combined_pathway_aggregate_species$pathway,fixed = TRUE)

# 5.2 Connect with pathway names
path_speciesToKeep <- intersect(Pathway_Class$Map , combined_pathway_aggregate_species$pathway)
path_species_intersect <- Pathway_Class[Pathway_Class$Map %in% path_speciesToKeep,]
combined_pathway_aggregate_speciesName <- combined_pathway_aggregate_species %>% left_join(Pathway_Class, by=c("pathway"="Map"))
colnames(combined_pathway_aggregate_speciesName) <- c("Genus","Species","pathway","count","pathway_name","pathway_class")

# 5.3 Normalized the enzyme counts
# 5.3.1 Connect the species table with the reference count table
combined_pathway_aggregate_speciesName_reference <- combined_pathway_aggregate_speciesName %>% left_join(combined_pathway_reference, by=c("pathway"="pathway"))
colnames(combined_pathway_aggregate_speciesName_reference) <- c("Genus","Species","map_ID","count_before_normalized","pathway_name","pathway_class","count_reference")
# 5.3.2 Calculate the normalized count
combined_pathway_aggregate_speciesName_reference$count_after_normalized <- combined_pathway_aggregate_speciesName_reference$count_before_normalized / combined_pathway_aggregate_speciesName_reference$count_reference

write.csv(combined_pathway_aggregate_speciesName_reference,"normalized_species_ec_count.csv",row.names = F)

