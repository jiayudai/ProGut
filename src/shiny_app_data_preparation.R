library("dplyr")
inputFiles <- commandArgs(trailingOnly = T)
# inputFiles[1] should be "optimal_level/research_res_l3_removed_length.csv"
# inputFiles[2] should be "optimal_level/research_res_l3_removed_genus_species.csv"

############## Pie Charts Tab
pie_chart_data <- read.csv("combined_pathway.csv")

pie_chart_data$Pathway <- as.character(pie_chart_data$Pathway)

pie_chart_data$Pathway <- gsub("path:","",pie_chart_data$Pathway,fixed = TRUE)

Pathway_Class <- read.csv("keggPathway.csv", stringsAsFactors = F)

pie_chart_data_aggregate <- pie_chart_data %>% left_join(Pathway_Class, by=c("Pathway"="Map"))


count_class <- c()
nameList_class <- c()
pathwayList_class <- c()
for (name_class in unique(pie_chart_data_aggregate$Bacteria)){
  for (pathway_class in unique(pie_chart_data_aggregate$Pathway.Class)){
    nameList_class <- c(nameList_class, name_class)
    pathwayList_class <- c(pathwayList_class, pathway_class)
    count_class <- c(count_class,length(unique(pie_chart_data_aggregate$EC_number[pie_chart_data_aggregate$Pathway.Class == pathway_class & pie_chart_data_aggregate$Bacteria == name_class])))
  }
}

pie_chart_data_aggregate_count <- as.data.frame(count_class)
pie_chart_data_aggregate_count$bacteria <- nameList_class
pie_chart_data_aggregate_count$pathway <- pathwayList_class

# Aggregate the ec counts based on the bacteria
pie_chart_data_aggregate_count_bacteria <- aggregate(pie_chart_data_aggregate_count$count_class, list(pie_chart_data_aggregate_count$bacteria),function(x) sum(x))
colnames(pie_chart_data_aggregate_count_bacteria) <- c("bacteria","count_sum")

pie_chart_data_aggregate_count_aggregate <- pie_chart_data_aggregate_count %>% left_join(pie_chart_data_aggregate_count_bacteria, by=c("bacteria"="bacteria"))

pie_chart_data_aggregate_count_aggregate$count_percentage <- pie_chart_data_aggregate_count_aggregate$count_class * 100 / pie_chart_data_aggregate_count_aggregate$count_sum 
colnames(pie_chart_data_aggregate_count_aggregate) <- c("count_class","bacteria","pathway_class","count_sum","count_percentage%")
write.csv(pie_chart_data_aggregate_count_aggregate, file ="shiny_app/shiny_pie_chart_data.csv", row.names =F)


############## "Start from Pathways" Tab - Genus level
# Pathway, bacteria group, unique genus group
research_res_l3_removed_length <- read.csv(inputFiles[1],stringsAsFactors = F)
nonempty <- research_res_l3_removed_length[nchar(research_res_l3_removed_length$intersect)>0,c("intersect","common_path")]
nonempty  <- cbind("Group"=paste0("Group",1:nrow(nonempty)),nonempty)

bnames <- strsplit(nonempty$intersect,",")
pnames <- strsplit(nonempty$common_path,",")
long.table <- data.frame()
for(i in 1:nrow(nonempty)) {
  x <- data.frame(cbind("Group"=as.character(nonempty$Group[i]), "bacteria" = bnames[[i]]))
  y <- data.frame(cbind("Group"=as.character(nonempty$Group[i]), "pathway" = pnames[[i]]))
  z <- inner_join(x,y)
  long.table <- rbind(long.table, z)
}

long.table$genus <- gsub(" .*","",long.table$bacteria)
write.csv(long.table, file ="shiny_app/groupID_bacteria_genus_pathway.csv", row.names =F)

############## "Start from Pathways" Tab - Bacteria level
research_res_l3_removed_genus_species_shiny  <- read.csv(inputFiles[2],stringsAsFactors = F)

bnames_species <- strsplit(research_res_l3_removed_genus_species_shiny$intersect,",")
pnames_species <- strsplit(research_res_l3_removed_genus_species_shiny$common_path,",")
long.table.species <- data.frame()
for(j in 1:nrow(nonempty)) {
  q <- data.frame(cbind("Group"=as.character(research_res_l3_removed_genus_species_shiny$Group[j]), "bacteria" = bnames_species[[j]]))
  w <- data.frame(cbind("Group"=as.character(research_res_l3_removed_genus_species_shiny$Group[j]), "pathway" = pnames_species[[j]]))
  s <- inner_join(q,w)
  long.table.species <- rbind(long.table.species, s)
}
write.csv(long.table.species, file ="shiny_app/groupID_bacteria_pathway.csv", row.names =F)











