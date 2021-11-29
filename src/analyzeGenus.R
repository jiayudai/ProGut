library("dplyr")
# Level3 is the optimal level
load("all_path_fuzzy_pathnames_sorted_wide_removed.rfile")
#inputFile <- "cut_results/res_l3_removed.csv"
inputFile <- commandArgs(trailingOnly = TRUE)[1]


# 1. Extract the genus names based on bacteria names
all_path_fuzzy_pathnames_sorted_wide_removed$Genus = gsub(" .*","",all_path_fuzzy_pathnames_sorted_wide_removed$Bacterium)
all_path_fuzzy_pathnames_sorted_wide_removed <- all_path_fuzzy_pathnames_sorted_wide_removed[,c(1,ncol(all_path_fuzzy_pathnames_sorted_wide_removed),2:(ncol(all_path_fuzzy_pathnames_sorted_wide_removed)-1))]

# Do the sum for each genus aggregate
aggregate_result <- aggregate(x=all_path_fuzzy_pathnames_sorted_wide_removed[,3:ncol(all_path_fuzzy_pathnames_sorted_wide_removed)], 
                              by = list(all_path_fuzzy_pathnames_sorted_wide_removed$Genus), FUN= sum)

aggregate_result$Genus_ID <- rownames(aggregate_result)
aggregate_result <- aggregate_result[,c(1,ncol(aggregate_result),2:(ncol(aggregate_result)-1))]

Pathway_Class <- read.csv("keggPathway.csv", stringsAsFactors = F,row.names = 1)
newcolnames <- as.character(Pathway_Class[colnames(aggregate_result)[-1:-2],1])
colnames(aggregate_result)[3:ncol(aggregate_result)] <- newcolnames

# 2. For each genus, extract the column names whose values are not zero
aggregate_sub <- aggregate_result[,2:ncol(aggregate_result)]
aggregate_sub$Genus_ID <- as.numeric(aggregate_sub$Genus_ID)
library(purrr)
aggregate_name_list <- aggregate_sub %>% split(.$Genus_ID) %>% map(~names(.x)[!!.x][-1])
names(aggregate_name_list) <- aggregate_result$Group.1

research_res_l3_removed <- read.csv(inputFile,stringsAsFactors = F)

# 3. Create a unique genus list based on the intersect column and
# calculate the length of each unique genus list
bact_list <- lapply(strsplit(research_res_l3_removed$intersect,","), strsplit, split=" ")
genus_list_unique <- rep(NA,length(bact_list))
common_path <- rep(NA,length(bact_list))
genus_list_length <- rep(0,length(bact_list))
common_path_length <- rep(0,length(bact_list))

for(i in 1:length(bact_list)){
  genus_list <- unique(unlist(lapply(bact_list[[i]],function(x)x[1])))
  #print(bact_list[[i]])
  #print(genus_list)
  if(length(genus_list) > 0){
    common_list <- aggregate_name_list[[genus_list[1]]]
    if(length(genus_list) > 1) {
      for(j in 2:length(genus_list)){
        pathway_list <- aggregate_name_list[[genus_list[j]]]
        common_list <- intersect(common_list, pathway_list)
      }
    }
    common_path[i] <- paste(common_list, collapse=',')
    common_path_length[i] <- length(common_list)
  }
  genus_list_unique[i] <- paste(genus_list,collapse = ",")
  genus_list_length[i] <- length(genus_list)
  
}
research_res_l3_removed$genus_list_unique <- genus_list_unique
research_res_l3_removed$genus_length <- genus_list_length
research_res_l3_removed$common_path <- common_path
research_res_l3_removed$common_path_length <- common_path_length

# 4. Calculate the length of metabolic pathways and biosynthesis pathways
library(stringr)
research_res_l3_removed$metabolism_pathway_count <- str_count(tolower(research_res_l3_removed$common_path),"metabolism")
research_res_l3_removed$biosynthesis_pathway_count <- str_count(tolower(research_res_l3_removed$common_path),"biosynthesis")
research_res_l3_removed$metabolism_pathway_count[is.na(research_res_l3_removed$metabolism_pathway_count)] <- 0
research_res_l3_removed$biosynthesis_pathway_count[is.na(research_res_l3_removed$biosynthesis_pathway_count)] <- 0

outputFile <- gsub("removed.csv","removed_length.csv",gsub(".*/res","optimal_level/research_res",inputFile))
write.csv(research_res_l3_removed, file =outputFile, row.names =F)


##################### 5. Find the common pathway among bacteria groups #####################
research_res_l3_removed <- read.csv(outputFile,stringsAsFactors = F)

research_res_l3_removed_nonempty <- research_res_l3_removed[nchar(research_res_l3_removed$intersect)>0,c("intersect","genus_list_unique","common_path")]
research_res_l3_removed_nonempty  <- cbind("Group"=paste0("Group",1:nrow(research_res_l3_removed_nonempty)),research_res_l3_removed_nonempty)
outputFile <- gsub("_length.csv","_nonempty.csv",outputFile)
write.csv(research_res_l3_removed_nonempty, file = outputFile, row.names =F)


# Do the intersect and union
research_res_l3_removed_compared <- data.frame("GroupA_ID","GroupB_ID","GroupA_Path","GroupB_Path", "intersect", "union", "intersect_length","union_length", "jaccard_index_percentage", "uniqueA","uniqueB",stringsAsFactors = F)
counts_groups = nrow(research_res_l3_removed_nonempty)
for (q in 1:(counts_groups-1)){
  
  groupA <- research_res_l3_removed_nonempty$common_path[q]
  groupA_ID <- as.character(research_res_l3_removed_nonempty$Group[q])
  groupA_split <- unlist(strsplit(research_res_l3_removed_nonempty[q,4], ","))
  
  for (l in (q+1):nrow(research_res_l3_removed_nonempty)){
    #print(f)
    groupB <- research_res_l3_removed_nonempty$common_path[l]
    groupB_ID <- as.character(research_res_l3_removed_nonempty$Group[l])
    groupB_split <- unlist(strsplit(research_res_l3_removed_nonempty[l,4], ","))
    
    groupAB_intersect <- intersect(groupA_split, groupB_split)
    groupAB_intersect_length <- length(groupAB_intersect)
    
    '%notin%' <- Negate('%in%')
    uniqueA <- paste(groupA_split[which(groupA_split %notin% groupAB_intersect)], collapse = ",")
    uniqueB <- paste(groupB_split[which(groupB_split %notin% groupAB_intersect)], collapse = ",")
    
    groupAB_union <- union(groupA_split, groupB_split)
    groupAB_union_length <- length(groupAB_union)
    
    groupAB_jaccard_index <- groupAB_intersect_length * 100 / groupAB_union_length
    
    z <- c(groupA_ID,groupB_ID,groupA,groupB, paste(groupAB_intersect,collapse = ","), paste(groupAB_union, collapse = ","), groupAB_intersect_length,groupAB_union_length,groupAB_jaccard_index,uniqueA,uniqueB)
    
    research_res_l3_removed_compared <- rbind(research_res_l3_removed_compared, z)
    

  }
}
research_res_l3_removed_compared <- research_res_l3_removed_compared[-1,]
colnames(research_res_l3_removed_compared) <- c("GroupA_ID","GroupB_ID","GroupA","GroupB","intersect","union","intersect_length","union_length","jaccard_index_percentage","uniqueA","uniqueB")
outputFile <- gsub("_nonempty.csv","_compared.csv",outputFile)
write.csv(research_res_l3_removed_compared, file =outputFile, row.names =F)

##################### 6. Venn Diagram #####################
# Find pathways that are unique for every individual group between multiple groups at the genus level
research_res_l3_removed_nonempty <- read.csv("optimal_level/research_res_l3_removed_nonempty.csv",stringsAsFactors = F)
research_res_l3_removed_nonempty_path_genus <- research_res_l3_removed_nonempty[,c(1,2,4)]
group1_paths_genus <- unlist(strsplit(research_res_l3_removed_nonempty_path_genus$common_path[1], ","))
group2_paths_genus <- unlist(strsplit(research_res_l3_removed_nonempty_path_genus$common_path[2], ","))
group3_paths_genus <- unlist(strsplit(research_res_l3_removed_nonempty_path_genus$common_path[3], ","))
group4_paths_genus <- unlist(strsplit(research_res_l3_removed_nonempty_path_genus$common_path[4], ","))

library(VennDiagram)
library(RColorBrewer)
myCol <- brewer.pal(4, "Pastel2")
# group2_paths_genus
group_paths_all_genus <- list("group1Genus"=group1_paths_genus, "group2Genus"=group2_paths_genus, "group3Genus"=group3_paths_genus, "group4Genus"=group4_paths_genus)
venn.diagram(group_paths_all_genus,filename = 'data_validation_results/Venn_diagram_groups_genus.png',
             lwd = 2,
             lty = 'blank',
             fill = myCol)















