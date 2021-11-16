library("dplyr")
library("stringr")
# Combine three groups together
primary_pathway <- read.csv("primary_pathway.csv", header = F)
secondary_pathway <- read.csv("secondary_pathway.csv", header = F)
tnumber_pathway <- read.csv("tnumber_pathway.csv", header = F)
all_pathway <- rbind(primary_pathway,secondary_pathway,tnumber_pathway)
colnames(all_pathway) <- c("EC_number","Pathway","Bacteria")

# Remove duplicated bacteria (Bifidobacterium longum subsp. Infantis ATCC 15697)
all_pathway$Bacteria <- as.character(all_pathway$Bacteria)
all_pathway_159 <- all_pathway[-which(all_pathway$Bacteria == "Bifidobacterium longum subsp. Infantis ATCC 15697"),]

# Remove ec from path
all_pathway_159$Pathway <- as.character(all_pathway_159$Pathway)
for (k in 1:nrow(all_pathway_159)){
  if(str_starts(all_pathway_159$Pathway[k], "path:map", negate = FALSE)){
    all_pathway_159$Flag[k] <- "TRUE"
  }
  else{
    all_pathway_159$Flag[k] <- ""
  }
}
indexall_pathway_159_TRUE <- which(all_pathway_159$Flag == "TRUE")
all_pathway_159_map <- all_pathway_159[indexall_pathway_159_TRUE,]
all_pathway_159_map <- all_pathway_159_map[,c(1,2,3)]

# Make each row unique
all_pathway_159_map_unique <- all_pathway_159_map %>% distinct()
# Save the files all_pathway_159_map_unique as a csv file - combined_pathway.csv
write.csv(all_pathway_159_map_unique, file ="combined_pathway.csv",row.names =F)

# # Bact + EC
all_pathway_159_bact_ec <- cbind(all_pathway_159_map_unique[,c(3,1)],1)
all_pathway_159_bact_ec <- all_pathway_159_bact_ec %>% distinct()
colnames(all_pathway_159_bact_ec) <- c("Bacterium","Gene","Value")
write.csv(all_pathway_159_bact_ec,"Bact_EC.csv",quote=F,row.names =F)
# # EC + pathway
all_pathway_159_ec_path <- cbind(all_pathway_159_map_unique[,c(1,2)],1)
all_pathway_159_ec_path <- all_pathway_159_ec_path %>% distinct()
colnames(all_pathway_159_ec_path) <- c("Gene","Pathway","Value")
write.csv(all_pathway_159_ec_path,"EC_Pathway.csv",quote=F,row.names =F)
# Need two files:
# Bact + unique EC they have
# EC + pathway
# Unique rows first

















