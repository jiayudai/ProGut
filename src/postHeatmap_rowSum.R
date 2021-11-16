load("all_path_fuzzy_pathnames_sorted_wide_removed.rfile")

library(pheatmap)
# 1.  Create heatmap2 (After removing big pathways)
rownames(all_path_fuzzy_pathnames_sorted_wide_removed) <- all_path_fuzzy_pathnames_sorted_wide_removed$Bacterium
all_path_fuzzy_pathnames_sorted_wide_removed <- all_path_fuzzy_pathnames_sorted_wide_removed[,2:ncol(all_path_fuzzy_pathnames_sorted_wide_removed)]
all_path_fuzzy_pathnames_sorted_wide_removed_matrix <- as.matrix(all_path_fuzzy_pathnames_sorted_wide_removed)
pheatmap(all_path_fuzzy_pathnames_sorted_wide_removed)
#Make the figures readable
pheatmap(all_path_fuzzy_pathnames_sorted_wide_removed,fontsize=3, height=9, width=9,filename="heatmaps/heatmap_removed.pdf")
pheatmap(all_path_fuzzy_pathnames_sorted_wide_removed,fontsize=3, height=9, width=9,filename="heatmaps/heatmap_removed.jpg")

# 2. Data exploration - Rowsum (after removing 3 big pathways)
all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum <- as.data.frame(all_path_fuzzy_pathnames_sorted_wide_removed_matrix)
all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum$rowsum <- rowSums(all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum[,1:ncol(all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum)])
all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum <- all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum[,c(ncol(all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum),1:(ncol(all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum)-1))]
write.csv(all_path_fuzzy_pathnames_sorted_wide_removed_matrix_sum,"data_exploration_rowsum.csv", row.names = T)










