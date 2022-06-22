#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read more info
lineageEX_moreinfo <- read.csv("lineageEX_xml_moreinfo.csv",header = T,sep = '|',stringsAsFactors = F)
# unique phylum
table(lineageEX_moreinfo$phylum)
table(lineageEX_moreinfo$class)
table(lineageEX_moreinfo$order)
table(lineageEX_moreinfo$family)
table(lineageEX_moreinfo$genus)
table(lineageEX_moreinfo$species.groups)
table(lineageEX_moreinfo$species)
table(lineageEX_moreinfo$subspecies)

# My work (Create a tree) - Run from here
####################### Prepare data from Literature for creating a tree in Cytoscape
library(igraph)
lineageEX_tree <- read.csv("lineageEX_xml_moreinfo.csv",header = T,sep = '|',stringsAsFactors = F)
lineageEX_tree$organism = "k_Bacteria"
lineageEX_tree$phylum <- paste("p",lineageEX_tree$phylum,sep="_")
lineageEX_tree$class <- paste("c",lineageEX_tree$class,sep="_")
lineageEX_tree$order <- paste("o",lineageEX_tree$order,sep="_")
lineageEX_tree$family <- paste("f",lineageEX_tree$family,sep="_")
lineageEX_tree$genus <- paste("g",lineageEX_tree$genus,sep="_")

kp <- lineageEX_tree[,c("organism","phylum")]
colnames(kp) <- c("source","target")
pc <- lineageEX_tree[,c("phylum","class")]
colnames(pc) <- c("source","target")
co <- lineageEX_tree[,c("class","order")]
colnames(co) <- c("source","target")
of <- lineageEX_tree[,c("order","family")]
colnames(of) <- c("source","target")
fg <- lineageEX_tree[,c("family","genus")]
colnames(fg) <- c("source","target")

tree_df <- rbind(kp,pc,co,of)
tree_df2 <- rbind(kp,pc,co,of,fg)
tree_df <- tree_df[!duplicated(tree_df),]
tree_df2 <- tree_df2[!duplicated(tree_df2),]
write.csv(tree_df, file ="tree_df.csv", row.names =F, quote = F)
write.csv(tree_df2, file ="tree_df2.csv", row.names =F, quote = F)

# quote = F

g <- graph.data.frame(tree_df, directed = T)

summary(g)
myformat <- function(g){
  layout.reingold.tilford(g, root = 1, flip.y = FALSE, circular = FALSE)
}


#lineageEX_tree_format_unique$parent == lineageEX_tree_format_unique$id

# Count the size
library("dplyr")
#tree_df_size <- tree_df
tree_df_size <- tree_df2
count_size <- tree_df_size %>%
  group_by(source) %>%
  summarise(distinct_target = n_distinct(target))
taxon <- names(V(g))
taxon_size <- data.frame(taxon)
taxon_size2 <- taxon_size %>% left_join(count_size, by=c("taxon"="source"))
taxon_size2[is.na(taxon_size2)] <- 0
write.csv(taxon_size2, file ="taxon_size3.csv", row.names =F, quote = F)

plot(g, layout = myformat, vertex.size = taxon_size2$distinct_target)

####################### Prepare data for Thesis work
# Create a tree for Thesis organisms
library(igraph)
lineageEX_tree_thesis <- read.csv("Thesis_lineageEX_xml_moreinfo.csv",header = T,sep = '|',stringsAsFactors = F)
lineageEX_tree_thesis$organism = "k_Bacteria"
lineageEX_tree_thesis$phylum <- paste("p",lineageEX_tree_thesis$phylum,sep="_")
lineageEX_tree_thesis$class <- paste("c",lineageEX_tree_thesis$class,sep="_")
lineageEX_tree_thesis$order <- paste("o",lineageEX_tree_thesis$order,sep="_")
lineageEX_tree_thesis$family <- paste("f",lineageEX_tree_thesis$family,sep="_")
lineageEX_tree_thesis$genus <- paste("g",lineageEX_tree_thesis$genus,sep="_")

kp_thesis <- lineageEX_tree_thesis[,c("organism","phylum")]
colnames(kp_thesis) <- c("source","target")
pc_thesis <- lineageEX_tree_thesis[,c("phylum","class")]
colnames(pc_thesis) <- c("source","target")
co_thesis <- lineageEX_tree_thesis[,c("class","order")]
colnames(co_thesis) <- c("source","target")
of_thesis <- lineageEX_tree_thesis[,c("order","family")]
colnames(of_thesis) <- c("source","target")
fg_thesis <- lineageEX_tree_thesis[,c("family","genus")]
colnames(fg_thesis) <- c("source","target")

tree_df_thesis <- rbind(kp_thesis,pc_thesis,co_thesis,of_thesis)
tree_df2_thesis <- rbind(kp_thesis,pc_thesis,co_thesis,of_thesis,fg_thesis)
tree_df_thesis <- tree_df_thesis[!duplicated(tree_df_thesis),]
tree_df2_thesis <- tree_df2_thesis[!duplicated(tree_df2_thesis),]
write.csv(tree_df_thesis, file ="tree_df_thesis.csv", row.names =F, quote = F)
write.csv(tree_df2_thesis, file ="tree_df2_thesis.csv", row.names =F, quote = F)

g_thesis <- graph.data.frame(tree_df_thesis, directed = T)

summary(g_thesis)
myformat_thesis <- function(g_thesis){
  layout.reingold.tilford(g_thesis, root = 1, flip.y = FALSE, circular = FALSE)
}

# Count the size
library("dplyr")
tree_df_size_thesis <- tree_df2_thesis
count_size_thesis <- tree_df_size_thesis %>%
  group_by(source) %>%
  summarise(distinct_target_thesis = n_distinct(target))

taxon_thesis <- names(V(g_thesis))
taxon_size_thesis <- data.frame(taxon_thesis)
taxon_size2_thesis <- taxon_size_thesis %>% left_join(count_size_thesis, by=c("taxon_thesis"="source"))
taxon_size2_thesis[is.na(taxon_size2_thesis)] <- 0
write.csv(taxon_size2_thesis, file ="taxon_size3_thesis.csv", row.names =F, quote = F)


####################### Generate Source table for Literature and Thesis work
taxon_size3_source <- read.csv("taxon_size3.csv",header = T,stringsAsFactors = F)
taxon_size3_thesis_source <- read.csv("taxon_size3_thesis.csv",header = T,stringsAsFactors = F)
common <- as.data.frame(intersect(taxon_size3_source$taxon, taxon_size3_thesis_source$taxon_thesis))
colnames(common) <- c("taxon")
common$source <- "Both"
# Only exist in Literature Review
uniqueLR <- as.data.frame(setdiff(taxon_size3_source$taxon, taxon_size3_thesis_source$taxon_thesis))
colnames(uniqueLR) <- c("taxon")
uniqueLR$source <- "LR"
# Only exist in Thesis
uniquePG <- as.data.frame(setdiff(taxon_size3_thesis_source$taxon_thesis,taxon_size3_source$taxon))
colnames(uniquePG) <- c("taxon")
uniquePG$source <- "PG"
# Merge
sourceTable <- rbind(common,uniqueLR,uniquePG)
write.csv(sourceTable, file ="sourceTable.csv", row.names =F, quote = F)







