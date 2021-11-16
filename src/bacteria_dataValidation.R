inputs <- commandArgs(trailingOnly = T)[1]
# inputs should be "optimal_level/research_res_l3_removed_nonempty.csv"

library("dplyr")
library("stringr")
# Obtain bacteria pathways for each group
research_res_l3_removed_nonempty <- read.csv(inputs, stringsAsFactors = F)
bact_pathway_fuzzy_validation <- read.csv("Pathway_Bacteria_Fuzzy_Ranking.csv", stringsAsFactors = F)
for (q in 1:nrow(research_res_l3_removed_nonempty)){
  groupList <- unlist(strsplit(research_res_l3_removed_nonempty$intersect[q], ","))
  index <- which(bact_pathway_fuzzy_validation$Bacterium %in% groupList)
  group_fuzzy <- bact_pathway_fuzzy_validation[index,]
  group_path <- unique(group_fuzzy$Pathway)
  research_res_l3_removed_nonempty$common_path[q] <- paste(group_path,collapse=",")
}

outputFile <- gsub("_nonempty.csv","_genus_species.csv",inputs)
write.csv(research_res_l3_removed_nonempty, file =outputFile, row.names =F)
#write.csv(research_res_l3_removed_nonempty, file="optimal_level/research_res_l3_removed_genus_species.csv",row.names = F)

# Compare the pathways between every two groups, including calculating the lengths
research_res_l3_removed_compared_GS <- data.frame("GroupA_ID","GroupB_ID","GroupA_Path","GroupB_Path", "intersect", "union", "intersect_length","union_length", "jaccard_index_percentage", "uniqueA","uniqueA_length","uniqueB","uniqueB_length",stringsAsFactors = F)
counts_groups <- nrow(research_res_l3_removed_nonempty)
for (k in 1:(counts_groups-1)){
  
  groupA <- research_res_l3_removed_nonempty$common_path[k]
  groupA_ID <- as.character(research_res_l3_removed_nonempty$Group[k])
  groupA_split <- unlist(strsplit(research_res_l3_removed_nonempty[k,4], ","))
  
  for (l in (k+1):nrow(research_res_l3_removed_nonempty)){
    #print(f)
    groupB <- research_res_l3_removed_nonempty$common_path[l]
    groupB_ID <- as.character(research_res_l3_removed_nonempty$Group[l])
    groupB_split <- unlist(strsplit(research_res_l3_removed_nonempty[l,4], ","))
    
    groupAB_intersect <- intersect(groupA_split, groupB_split)
    groupAB_intersect_length <- length(groupAB_intersect)
    
    '%notin%' <- Negate('%in%')
    uniqueA_paths <- groupA_split[which(groupA_split %notin% groupAB_intersect)]
    uniqueA_length <- length(uniqueA_paths)
    uniqueA <- paste(uniqueA_paths, collapse = ",")
    
    uniqueB_paths <- groupB_split[which(groupB_split %notin% groupAB_intersect)]
    uniqueB_length <- length(uniqueB_paths)
    uniqueB <- paste(uniqueB_paths, collapse = ",")
    
    groupAB_union <- union(groupA_split, groupB_split)
    groupAB_union_length <- length(groupAB_union)
    
    groupAB_jaccard_index <- groupAB_intersect_length * 100 / groupAB_union_length
    
    z <- c(groupA_ID,groupB_ID,groupA,groupB, paste(groupAB_intersect,collapse = ","), paste(groupAB_union, collapse = ","), groupAB_intersect_length,groupAB_union_length,groupAB_jaccard_index,uniqueA,uniqueA_length,uniqueB,uniqueB_length)
    
    research_res_l3_removed_compared_GS <- rbind(research_res_l3_removed_compared_GS, z)
    
    
  }
}
research_res_l3_removed_compared_GS <- research_res_l3_removed_compared_GS[-1,]
colnames(research_res_l3_removed_compared_GS) <- c("GroupA_ID","GroupB_ID","GroupA","GroupB","intersect","union","intersect_length","union_length","jaccard_index_percentage","uniqueA","uniqueA_length","uniqueB","uniqueB_length")

outputFile <- gsub("_genus_species.csv","_genus_species_compared.csv",outputFile)
write.csv(research_res_l3_removed_compared_GS, file =outputFile, row.names =F)
#write.csv(research_res_l3_removed_compared_GS, file="optimal_level/research_res_l3_removed_genus_species_compared.csv",quote=T,row.names = F)

# Attach 159 bacteria names with group ids
research_res_l3_removed_nonempty_sub <- research_res_l3_removed_nonempty[,c(1,2)]
bacteria_names <- strsplit(research_res_l3_removed_nonempty_sub$intersect,",")

allBool <- F # To know if we have already started the first step
for(i in 1:nrow(research_res_l3_removed_nonempty_sub)) {
  x <- cbind("Group"=as.character(research_res_l3_removed_nonempty_sub$Group[i]), "bacteria" = bacteria_names[[i]])
  
  if (allBool == F){
    all <- x
    allBool <- T
  }
  else{
    all <- rbind(all, x)
  }
}
all <- data.frame(all)

# For each comparison group, now extract the bacteria that have these unique pathways
research_res_l3_removed_compared_GS_sub <- research_res_l3_removed_compared_GS[,c(1,2,10,11)] 
# Left join to attach the Group information to the Fuzzy Ranking Table
bact_pathway_fuzzy_validation_all <- bact_pathway_fuzzy_validation %>% left_join(all, by=c("Bacterium"="bacteria"))
# Have a count for intersect, unique length - size of the pathway doesn't matter

# Find pathways that are unique for every individual group between multiple groups
research_res_l3_removed_nonempty_path <- research_res_l3_removed_nonempty[,c(1,2,4)]
group1_paths <- unlist(strsplit(research_res_l3_removed_nonempty_path$common_path[1], ","))
group2_paths <- unlist(strsplit(research_res_l3_removed_nonempty_path$common_path[2], ","))
group3_paths <- unlist(strsplit(research_res_l3_removed_nonempty_path$common_path[3], ","))
group4_paths <- unlist(strsplit(research_res_l3_removed_nonempty_path$common_path[4], ","))

library(VennDiagram)
library(RColorBrewer)
myCol <- brewer.pal(4, "Pastel2")
group_paths_all <- list("group1_paths"=group1_paths, "group2_paths"=group2_paths, "group3_paths"=group3_paths, "group4_paths"=group4_paths)
venn.diagram(group_paths_all,filename = 'data_validation_results/Venn_diagram_groups.png',
             lwd = 2,
             lty = 'blank',
             fill = myCol)

# Extract the unique pathway names and combine with KEGG sizes
group1_unique <- setdiff(group1_paths,union(union(group2_paths,group3_paths),group4_paths))
group2_unique <- setdiff(group2_paths,union(union(group1_paths,group3_paths),group4_paths))
group3_unique <- setdiff(group3_paths,union(union(group1_paths,group2_paths),group4_paths))
group4_unique <- setdiff(group4_paths,union(union(group1_paths,group2_paths),group3_paths))
Group <- c("Group1","Group2","Group3","Group4")

groups_unique <- data.frame(Group=c("Group1","Group2","Group3","Group4"),
                            paths=c(paste(group1_unique,collapse = ","),
                                    paste(group2_unique,collapse = ","),
                                    paste(group3_unique,collapse = ","),
                                    paste(group4_unique,collapse = ",")))
groups_unique$paths <- as.character(groups_unique$paths)
for (u in 1:nrow(groups_unique)){
  groups_unique$unique_length[u] <- length(unlist(strsplit(groups_unique$paths[u], ",")))
}

fuzzy_bact_unique_list <- data.frame()
for (f in 1:nrow(groups_unique)){
  group_path_unique <- unlist(strsplit(groups_unique$paths[f], ","))
  index_path_unique_bact <- which(bact_pathway_fuzzy_validation_all$Pathway %in% group_path_unique & bact_pathway_fuzzy_validation_all$Group == groups_unique$Group[f])
  fuzzy_bact_unique <- bact_pathway_fuzzy_validation_all[index_path_unique_bact,]
  fuzzy_bact_unique_list <- rbind(fuzzy_bact_unique_list,fuzzy_bact_unique)
}

pwslast <- unique(fuzzy_bact_unique_list$Pathway)
Pathway_Class <- read.csv("keggPathway.csv", stringsAsFactors = F)
mapslast <- Pathway_Class[match(pwslast,Pathway_Class$Pathway.Name),"Map"]

library(httr)
fuzzy_bact_unique_list_test <- fuzzy_bact_unique_list
keggcount <- c()
keggtable <- data.frame(pwslast,mapslast)
for (mapid in mapslast) {
  print(mapid)
  urlpath = paste0("http://rest.kegg.jp/link/ec/",mapid)
  r <- GET(url = urlpath)
  kegg = str_count(content(r),"\n")
  keggcount <- c(keggcount,kegg)
}
keggtable$KEGG <- keggcount
# Join with unique bacteria pathways
fuzzy_bact_unique_list_aggregate <- fuzzy_bact_unique_list %>% left_join(keggtable, by=c("Pathway"="pwslast"))
fuzzy_bact_unique_list_aggregate <- fuzzy_bact_unique_list_aggregate[,c(1,5,2,4,3,6)]
colnames(fuzzy_bact_unique_list_aggregate) <- c("Pathway","Map","Bacteria","Group","Value","KEGG")
write.csv(fuzzy_bact_unique_list_aggregate, file ="data_validation_results/unique_bact_paths.csv", row.names =F)

# Jaccard index Venn diagram
load("trees/geneTree_l3.rfile")
dfGene_l3 <- dfBacteria
load("trees/pathwayTree_l3.rfile")
dfPath_l3 <- dfBacteria
# Extract cluster1 from gene tree
dfGene_l3_c1 <- dfGene_l3$Bacteria[1]
# Extract cluster1 and cluster2 from function tree
dfPath_l3_c1 <- dfPath_l3$Bacteria[1]
dfPath_l3_c2 <- dfPath_l3$Bacteria[2]

dfGene_l3_c1_list <- unlist(strsplit(dfGene_l3_c1,","))
dfPath_l3_c1_list <- unlist(strsplit(dfPath_l3_c1,","))
dfPath_l3_c2_list <- unlist(strsplit(dfPath_l3_c2,","))

myCol2 <- brewer.pal(3, "Pastel2")
clusters_all <- list("dfGene_l3_c1"=dfGene_l3_c1_list, "dfPath_l3_c1"=dfPath_l3_c1_list, "dfPath_l3_c2"=dfPath_l3_c2_list)
venn.diagram(clusters_all,filename = 'data_validation_results/Venn_diagram_clusters.png',
             lwd = 2,
             lty = 'blank',
             fill = myCol2,
             scaled = TRUE)

