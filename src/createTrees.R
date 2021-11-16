library(stats)
# Reorganize the fuzzy ranking results
bact_pathway_fuzzy_ranking <- read.csv("Bact_Pathway_Fuzzy_Ranking.csv", stringsAsFactors = F)
bact_pathway_fuzzy_ranking$Pathway <- gsub("path:","",bact_pathway_fuzzy_ranking$Pathway,fixed = TRUE)
# Add the pathway names
Pathway_Class <- read.csv("keggPathway.csv", stringsAsFactors = F)
colnames(Pathway_Class) <- c("Map", "Pathway_Name", "Pathway_Class")

library(dplyr)
bact_pathway_fuzzy_ranking_pathnames <- bact_pathway_fuzzy_ranking %>% left_join(Pathway_Class, by=c("Pathway"="Map"))
bact_pathway_fuzzy_ranking_pathnames_sub <- bact_pathway_fuzzy_ranking_pathnames[,c(4,1,3)]
colnames(bact_pathway_fuzzy_ranking_pathnames_sub) <- c("Pathway","Bacterium", "Value")
write.csv(bact_pathway_fuzzy_ranking_pathnames_sub, file ="Pathway_Bacteria_Fuzzy_Ranking.csv", quote = F,row.names =F)

# 1. Create LONG matrix
# 1.1 Bacteria-EC matrix
all_ec_159_unique <- read.csv("combined_ec.csv",stringsAsFactors = F)
# Create a Long to Wide enzyme matrix
library(reshape2)
all_ec_159_unique$Bacteria <- factor(all_ec_159_unique$Bacteria)
all_ec_159_unique_wide <- dcast(all_ec_159_unique, Bacteria ~ EC_number)
library(dplyr)
all_ec_159_unique_wide[is.na(all_ec_159_unique_wide)] <- 0
all_ec_159_unique_wide[-1] <- as.integer(all_ec_159_unique_wide[-1] != 0)

# 1.2 Bacteria-Pathway matrix
all_path_fuzzy <- read.csv("Pathway_Bacteria_Fuzzy_Ranking.csv",stringsAsFactors = F)
Pathway_Class <- read.csv("keggPathway.csv", stringsAsFactors = F)
colnames(Pathway_Class) <- c("Map", "Pathway_Name", "Pathway_Class") # Remove spaces in the column names

all_path_fuzzy_pathnames <- all_path_fuzzy %>% left_join(Pathway_Class, by=c("Pathway"="Pathway_Name"))
# Sort based on the bacteria names (ascending) and values (descending)
all_path_fuzzy_pathnames_sorted <- all_path_fuzzy_pathnames[order(all_path_fuzzy_pathnames$Bacterium, -all_path_fuzzy_pathnames$Value),] 
# Save the file for the shiny app
write.csv(all_path_fuzzy_pathnames_sorted, file ="shiny_app/all_path_fuzzy_pathnames_sorted.csv", row.names =F)

all_path_fuzzy_pathnames_sorted$Bacterium <- factor(all_path_fuzzy_pathnames_sorted$Bacterium)
all_path_fuzzy_pathnames_sorted_wide <- dcast(all_path_fuzzy_pathnames_sorted, Bacterium ~ Map, value.var="Value")
# Replace NAs with 0
library(dplyr)
all_path_fuzzy_pathnames_sorted_wide[is.na(all_path_fuzzy_pathnames_sorted_wide)] <- 0
# Save the original pathway matrix - for heatmap
all_path_fuzzy_pathnames_sorted_wide_before_removed <- all_path_fuzzy_pathnames_sorted_wide
save(all_path_fuzzy_pathnames_sorted_wide_before_removed,file="all_path_fuzzy_pathnames_sorted_wide_before_removed.rfile")

# preHeatmap
library(pheatmap)
rownames(all_path_fuzzy_pathnames_sorted_wide_before_removed) <- all_path_fuzzy_pathnames_sorted_wide_before_removed$Bacterium
all_path_fuzzy_pathnames_sorted_wide_before_removed <- all_path_fuzzy_pathnames_sorted_wide_before_removed[,2:ncol(all_path_fuzzy_pathnames_sorted_wide_before_removed)]
all_path_fuzzy_pathnames_sorted_wide_before_removed_matrix <- as.matrix(all_path_fuzzy_pathnames_sorted_wide_before_removed)
pheatmap(all_path_fuzzy_pathnames_sorted_wide_before_removed_matrix)
#Make the figures readable
pheatmap(all_path_fuzzy_pathnames_sorted_wide_before_removed_matrix,fontsize=3, height=9, width=9,filename="heatmaps/heatmap_before_removed.pdf")
pheatmap(all_path_fuzzy_pathnames_sorted_wide_before_removed_matrix,fontsize=3, height=9, width=9,filename="heatmaps/heatmap_before_removed.jpg")

# Remove three big pathways
all_path_fuzzy_pathnames_sorted_wide_removed <- all_path_fuzzy_pathnames_sorted_wide
all_path_fuzzy_pathnames_sorted_wide_removed$map01100 = NULL
all_path_fuzzy_pathnames_sorted_wide_removed$map01110 = NULL
all_path_fuzzy_pathnames_sorted_wide_removed$map01120 = NULL
save(all_path_fuzzy_pathnames_sorted_wide_removed,file="all_path_fuzzy_pathnames_sorted_wide_removed.rfile")

# 2. Create Trees
# 2.1 Gene (EC) tree
library(ape)
rownames(all_ec_159_unique_wide) <- all_ec_159_unique_wide$Bacteria
all_ec_159_unique_wide <- all_ec_159_unique_wide[,c(2:ncol(all_ec_159_unique_wide))]
all_ec_159_unique_matrix <- as.matrix(all_ec_159_unique_wide)

distance <- dist(all_ec_159_unique_matrix)
hc <- hclust(distance)
plot(hc)

#34-Create the data frame based on the cluster id 
################# Make the figures readable
pdf("Bact_EC_Tree.pdf",height=9, width=18)
plot(hc,cex=0.7)
dev.off()
###################

# 2.2 Function tree
rownames(all_path_fuzzy_pathnames_sorted_wide_removed) <- all_path_fuzzy_pathnames_sorted_wide_removed$Bacterium
all_path_fuzzy_pathnames_sorted_wide_removed <- all_path_fuzzy_pathnames_sorted_wide_removed[,2:ncol(all_path_fuzzy_pathnames_sorted_wide_removed)]
all_path_fuzzy_pathnames_sorted_wide_removed_matrix <- as.matrix(all_path_fuzzy_pathnames_sorted_wide_removed)

distance_pathway_removed <- dist(all_path_fuzzy_pathnames_sorted_wide_removed_matrix)
hc_pathway_removed <- hclust(distance_pathway_removed)
plot(hc_pathway_removed)

################# Make the figures readable
pdf("Function_Tree.pdf",height=9, width=18)
plot(hc_pathway_removed,cex=0.7)
dev.off()
###################

save(hc,file="hc.rfile")
save(hc_pathway_removed,file="hc_pathway_removed.rfile")













