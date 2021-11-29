#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library("dplyr")
# Level3 is the optimal level
load("all_path_fuzzy_pathnames_sorted_wide_removed.rfile")
#inputFile <- "cut_results/res_l3_removed.csv"
inputFile <- commandArgs(trailingOnly = TRUE)[1]


# 1. Extract the genus names based on bacteria names
all_path_fuzzy_pathnames_sorted_wide_removed$Genus = gsub(" .*","",all_path_fuzzy_pathnames_sorted_wide_removed$Bacterium)
all_path_fuzzy_pathnames_sorted_wide_removed <- all_path_fuzzy_pathnames_sorted_wide_removed[,c(1,ncol(all_path_fuzzy_pathnames_sorted_wide_removed),2:(ncol(all_path_fuzzy_pathnames_sorted_wide_removed)-1))]

# 2. Extract the Genus, Species and Strain for each bacteria
#combined_pathway$Genus <- gsub(" .*", "", combined_pathway$Bacteria)
all_path_fuzzy_pathnames_sorted_wide_removed$SpeciesAndStrain <- sub(".*? ", "", all_path_fuzzy_pathnames_sorted_wide_removed$Bacterium)
all_path_fuzzy_pathnames_sorted_wide_removed$Species <- gsub(" .*", "", all_path_fuzzy_pathnames_sorted_wide_removed$SpeciesAndStrain)
all_path_fuzzy_pathnames_sorted_wide_removed$Strain <- sub(".*? ", "", all_path_fuzzy_pathnames_sorted_wide_removed$SpeciesAndStrain)
all_path_fuzzy_pathnames_sorted_wide_removed$SpeciesAndStrain <- NULL
all_path_fuzzy_pathnames_sorted_wide_removed <- all_path_fuzzy_pathnames_sorted_wide_removed[,c(1,2,(ncol(all_path_fuzzy_pathnames_sorted_wide_removed)-1),3:(ncol(all_path_fuzzy_pathnames_sorted_wide_removed)-2),ncol(all_path_fuzzy_pathnames_sorted_wide_removed))]
all_path_fuzzy_pathnames_sorted_wide_removed$GenusSpecies <- paste(all_path_fuzzy_pathnames_sorted_wide_removed$Genus, all_path_fuzzy_pathnames_sorted_wide_removed$Species, sep=" ")
all_path_fuzzy_pathnames_sorted_wide_removed <- all_path_fuzzy_pathnames_sorted_wide_removed[,c(1,ncol(all_path_fuzzy_pathnames_sorted_wide_removed),2:(ncol(all_path_fuzzy_pathnames_sorted_wide_removed)-1))]
all_path_fuzzy_pathnames_sorted_wide_removed$Strain <- NULL

# Do the sum for each genus aggregate
aggregate_result_species <- aggregate(x=all_path_fuzzy_pathnames_sorted_wide_removed[,5:ncol(all_path_fuzzy_pathnames_sorted_wide_removed)], 
                              by = list(all_path_fuzzy_pathnames_sorted_wide_removed$GenusSpecies), FUN= sum)

aggregate_result_species$GenusSpecies_ID <- rownames(aggregate_result_species)
aggregate_result_species <- aggregate_result_species[,c(1,ncol(aggregate_result_species),2:(ncol(aggregate_result_species)-1))]

Pathway_Class <- read.csv("keggPathway.csv", stringsAsFactors = F,row.names = 1)
newcolnames <- as.character(Pathway_Class[colnames(aggregate_result_species)[-1:-2],1])
colnames(aggregate_result_species)[3:ncol(aggregate_result_species)] <- newcolnames

# 2. For each species, extract the column names whose values are not zero
aggregate_sub_species <- aggregate_result_species[,2:ncol(aggregate_result_species)]
aggregate_sub_species$GenusSpecies_ID <- as.numeric(aggregate_result_species$GenusSpecies_ID)
library(purrr)

aggregate_name_list_species <- aggregate_sub_species %>% split(.$GenusSpecies_ID) %>% map(~names(.x)[!!.x][-1])
names(aggregate_name_list_species) <- aggregate_result_species$Group.1

research_res_l3_removed_species <- read.csv(inputFile,stringsAsFactors = F)

# 3. Create a unique genus list based on the intersect column and
# calculate the length of each unique genus list
bact_list <- lapply(strsplit(research_res_l3_removed_species$intersect,","), strsplit, split=" ")
species_list_unique <- rep(NA,length(bact_list))
common_path <- rep(NA,length(bact_list))
species_list_length <- rep(0,length(bact_list))
common_path_length <- rep(0,length(bact_list))

for(i in 1:length(bact_list)){
  species_list <- unique(unlist(lapply(bact_list[[i]],function(x) paste(x[1:2],collapse=" "))))
  #print(bact_list[[i]])
  #print(genus_list)
  if(length(species_list) > 0){
    common_list <- aggregate_name_list_species[[species_list[1]]]
    if(length(species_list) > 1) {
      for(j in 2:length(species_list)){
        pathway_list <- aggregate_name_list_species[[species_list[j]]]
        common_list <- intersect(common_list, pathway_list)
      }
    }
    common_path[i] <- paste(common_list, collapse=',')
    common_path_length[i] <- length(common_list)
  }
  species_list_unique[i] <- paste(species_list,collapse = ",")
  species_list_length[i] <- length(species_list)
  
}
research_res_l3_removed_species$species_list_unique <- species_list_unique
research_res_l3_removed_species$species_length <- species_list_length
research_res_l3_removed_species$common_path <- common_path
research_res_l3_removed_species$common_path_length <- common_path_length

# 4. Calculate the length of metabolic pathways and biosynthesis pathways
library(stringr)
research_res_l3_removed_species$metabolism_pathway_count <- str_count(tolower(research_res_l3_removed_species$common_path),"metabolism")
research_res_l3_removed_species$biosynthesis_pathway_count <- str_count(tolower(research_res_l3_removed_species$common_path),"biosynthesis")
research_res_l3_removed_species$metabolism_pathway_count[is.na(research_res_l3_removed_species$metabolism_pathway_count)] <- 0
research_res_l3_removed_species$biosynthesis_pathway_count[is.na(research_res_l3_removed_species$biosynthesis_pathway_count)] <- 0

outputFile <- gsub("removed.csv","removed_length_species.csv",gsub(".*/res","optimal_level/research_res",inputFile))
write.csv(research_res_l3_removed_species, file =outputFile, row.names =F)



##################### 5. Find the common pathway among bacteria groups #####################
research_res_l3_removed_species <- read.csv(outputFile,stringsAsFactors = F)
research_res_l3_removed_nonempty_species <- research_res_l3_removed_species[nchar(research_res_l3_removed_species$intersect)>0,c("intersect","species_list_unique","common_path")]
research_res_l3_removed_nonempty_species  <- cbind("Group"=paste0("Group",1:nrow(research_res_l3_removed_nonempty_species)),research_res_l3_removed_nonempty_species)
# unique(unlist(strsplit(research_res_l3_removed_nonempty_species$species_list_unique[2],",")))
outputFile <- gsub("_length_species.csv","_nonempty_species.csv",outputFile)
write.csv(research_res_l3_removed_nonempty_species, file = outputFile, row.names =F)

# Do the intersect and union
research_res_l3_removed_compared_species <- data.frame("GroupA_ID","GroupB_ID","GroupA_Path","GroupB_Path", "intersect", "union", "intersect_length","union_length", "jaccard_index_percentage", "uniqueA","uniqueB",stringsAsFactors = F)
counts_groups = nrow(research_res_l3_removed_nonempty_species)
for (q in 1:(counts_groups-1)){
  
  groupA <- research_res_l3_removed_nonempty_species$common_path[q]
  groupA_ID <- as.character(research_res_l3_removed_nonempty_species$Group[q])
  groupA_split <- unlist(strsplit(research_res_l3_removed_nonempty_species[q,4], ","))
  
  for (l in (q+1):nrow(research_res_l3_removed_nonempty_species)){
    #print(f)
    groupB <- research_res_l3_removed_nonempty_species$common_path[l]
    groupB_ID <- as.character(research_res_l3_removed_nonempty_species$Group[l])
    groupB_split <- unlist(strsplit(research_res_l3_removed_nonempty_species[l,4], ","))
    
    groupAB_intersect <- intersect(groupA_split, groupB_split)
    groupAB_intersect_length <- length(groupAB_intersect)
    
    '%notin%' <- Negate('%in%')
    uniqueA <- paste(groupA_split[which(groupA_split %notin% groupAB_intersect)], collapse = ",")
    uniqueB <- paste(groupB_split[which(groupB_split %notin% groupAB_intersect)], collapse = ",")
    
    groupAB_union <- union(groupA_split, groupB_split)
    groupAB_union_length <- length(groupAB_union)
    
    groupAB_jaccard_index <- groupAB_intersect_length * 100 / groupAB_union_length
    
    z <- c(groupA_ID,groupB_ID,groupA,groupB, paste(groupAB_intersect,collapse = ","), paste(groupAB_union, collapse = ","), groupAB_intersect_length,groupAB_union_length,groupAB_jaccard_index,uniqueA,uniqueB)
    
    research_res_l3_removed_compared_species <- rbind(research_res_l3_removed_compared_species, z)
    
    
  }
}
research_res_l3_removed_compared_species <- research_res_l3_removed_compared_species[-1,]
colnames(research_res_l3_removed_compared_species) <- c("GroupA_ID","GroupB_ID","GroupA","GroupB","intersect","union","intersect_length","union_length","jaccard_index_percentage","uniqueA","uniqueB")
outputFile <- gsub("_nonempty_species.csv","_compared_species.csv",outputFile)
write.csv(research_res_l3_removed_compared_species, file =outputFile, row.names =F)


##################### 6. Venn Diagram #####################
# Find pathways that are unique for every individual group between multiple groups at the species level
research_res_l3_removed_nonempty_path_species <- research_res_l3_removed_nonempty_species[,c(1,2,4)]
group1_paths_species <- unlist(strsplit(research_res_l3_removed_nonempty_path_species$common_path[1], ","))
group2_paths_species <- unlist(strsplit(research_res_l3_removed_nonempty_path_species$common_path[2], ","))
group3_paths_species <- unlist(strsplit(research_res_l3_removed_nonempty_path_species$common_path[3], ","))
group4_paths_species <- unlist(strsplit(research_res_l3_removed_nonempty_path_species$common_path[4], ","))

library(VennDiagram)
library(RColorBrewer)
myCol <- brewer.pal(4, "Pastel2")
# group2_paths_species
group_paths_all_species <- list("group1Species"=group1_paths_species, "group2Species"=group2_paths_species, "group3Species"=group3_paths_species, "group4Species"=group4_paths_species)
venn.diagram(group_paths_all_species,filename = 'data_validation_results/Venn_diagram_groups_species.png',
             lwd = 2,
             lty = 'blank',
             fill = myCol)


# Dot plot
plot(33,42,xlim=c(1,100),ylim=c(30,130),pch=2,xlab="Genus/Species length",ylab="Common pathway length")
points(19,72,pch=2)
points(30,65,pch=4)
points(86,36,pch=4)
legend("topright",legend=c("Genus","Species"),pch=c(2,4))
