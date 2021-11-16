# Do the intersect and union work based on the clusters from the genomic enzyme tree and functional tree
levels=c(3,4,5,10,20,30,40,50)
for(i in levels){
  gene <-paste0("trees/geneTree_l",i,".rfile")
  pathway <- paste0("trees/pathwayTree_l",i,".rfile")
  load(gene)
  dfGene <- dfBacteria
  load(pathway)
  dfPath <- dfBacteria
  
  # 3. With dfBacteria and dfBacteria_pathway_removed: do the intersect
  # dfBacteria = dfGene
  # dfBacteria_pathway_removed = dfPath
  intersect_all <- c()
  clust_intersect_ID <- c()
  intersect_matrix <- matrix(0, nrow = nrow(dfGene), ncol=nrow((dfPath)))
  res_removed <- data.frame("e","f", "intersect", "union", "union_length", stringsAsFactors = F)
  
  for (e in 1:nrow(dfGene)){
    
    bact_comp <- dfGene[e,2]
    bact_comp_vector <- c(bact_comp)
    bact_comp_split <- unlist(strsplit(bact_comp_vector, ","))
    
    for (f in 1:nrow(dfPath)){
      #print(f)
      bact_comp_pathway <- dfPath[f,2]
      bact_comp_vector_pathway <- c(bact_comp_pathway)
      bact_comp_pathway_split <- unlist(strsplit(bact_comp_vector_pathway, ","))
      
      intersect1 <- intersect(bact_comp_split, bact_comp_pathway_split)
      intersect_length <- length(intersect1)
      
      
      union1 <- union(bact_comp_split, bact_comp_pathway_split)
      union_length <- length(union1)
      
      ji <- intersect_length / union_length
      
      #print(length(bact_comp_split))
      #print(length(bact_comp_pathway_split))
      
      r <- c(as.character(e),as.character(f), paste(intersect1,collapse = ","), paste(union1, collapse = ","), union_length)
      #print(r)
      res_removed <- rbind(res_removed, r)
      
      intersect_matrix[e,f] <- ji
    }
  }
  intersect_matrix_removed_df <- as.data.frame(intersect_matrix)
  
  colnames(intersect_matrix_removed_df) <- c(1:ncol(intersect_matrix_removed_df))
  rownames(intersect_matrix_removed_df) <- c(1:nrow(intersect_matrix_removed_df))
  write.csv(intersect_matrix_removed_df, file =paste0("cut_results/intersect_matrix_df_l",i,"_removed.csv"), row.names =T)
  
  colnames(res_removed) <- c("gene_clusterID", "pathway_clusterID", "intersect","union","union_length")
  res_removed <- res_removed[-1,]
  rownames(res_removed) <- c(1:nrow(res_removed))
  
  write.csv(res_removed, file =paste0("cut_results/res_l",i,"_removed.csv"), row.names =F)
  
  # 4. Do the anaylsis based on the intersect matrix
  freq_bind_removed <- data.frame("Value","freq", stringsAsFactors = F)
  colnames(freq_bind_removed) <- c("Value", "freq")
  freq_bind_removed <- freq_bind_removed[-1,]
  for (u in 1:nrow(intersect_matrix_removed_df)){
    freq <- plyr::count(intersect_matrix_removed_df, u)
    colnames(freq) <- c("Value", "freq")
    freq_bind_removed <- rbind(freq_bind_removed, freq)
    
    
  }
  freq_final_removed <- plyr::count(freq_bind_removed, 'Value')
  write.csv(freq_final_removed, file =paste0("cut_results/freq_final_l",i,"_removed.csv"), row.names =F)

}























