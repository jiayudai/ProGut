path_res_class_normalized <- read.csv("intersect_union_genus_pathway.csv",stringsAsFactors = F)
path_Genus_species_normalized_sameGenus <- read.csv("intersect_union_sameGenus_species_pathway.csv",stringsAsFactors = F)


# 1. Create overlapping density plot
# 1.1 Input
# 1.1.1 Genus
path_res_class_normalized$level <- "Genus"
path_res_class_normalized_sub <- path_res_class_normalized[,c(7,8)]

# 1.1.2 Species
path_Genus_species_normalized_sameGenus$level <- "Species"
path_Genus_species_normalized_sameGenus_sub <- path_Genus_species_normalized_sameGenus[,c(7,8)]

# 1.1.3 Combine
path_res_class_normalized_genus_species_density <- rbind(path_res_class_normalized_sub,path_Genus_species_normalized_sameGenus_sub)

# 1.2 Density plot
library(ggplot2)
ggplot(path_res_class_normalized_genus_species_density,aes(x=jaccard_index_percentage)) + 
  geom_density(data=subset(path_res_class_normalized_genus_species_density,level == 'Genus'),aes(fill = "Genus"), alpha = 0.2) +
  geom_density(data=subset(path_res_class_normalized_genus_species_density,level == 'Species'),aes(fill = "Species"),alpha = 0.2)

pdf("Density_Plot.pdf",height=9, width=18)
ggplot(path_res_class_normalized_genus_species_density,aes(x=jaccard_index_percentage)) + 
  geom_density(data=subset(path_res_class_normalized_genus_species_density,level == 'Genus'),aes(fill = "Genus"), alpha = 0.2) +
  geom_density(data=subset(path_res_class_normalized_genus_species_density,level == 'Species'),aes(fill = "Species"),alpha = 0.2)

dev.off()

# 2. Chi-Square test among percentages on Genus and Strain levels
h1_genus <- hist(path_res_class_normalized_sub$jaccard_index_percentage)
h1_genus_breaks <- h1_genus$breaks
h1_genus_counts <- h1_genus$counts

h2_species <- hist(path_Genus_species_normalized_sameGenus_sub$jaccard_index_percentage)
h2_species_breaks <- h2_species$breaks
h2_species_counts <- h2_species$counts

h1_genus_counts_df <- as.data.frame(h1_genus_counts)
h2_species_counts_df <- as.data.frame(h2_species_counts)
h1_h2_counts <- cbind(h1_genus_counts_df,h2_species_counts_df)
colnames(h1_h2_counts) <- c("Genus","Species")
rownames(h1_h2_counts) <- c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")
chi <- chisq.test(h1_h2_counts) # This is the one for showing the chi-test
chi.df <-data.frame(unlist(chi))
chi.df <- cbind("parameters"=rownames(chi.df),chi.df)
write.csv(chi.df[1:4,],file = "chi_squared.csv",row.names = F)

