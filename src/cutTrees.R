# Cut the trees at different levels
load("hc.rfile")
load("hc_pathway_removed.rfile")
# Two inputs: level, tree
# Output: bacteria list with cluster IDs
# Levels
levels=c(3,4,5,10,20,30,40,50)
trees=list("geneTree"=hc,"pathwayTree"=hc_pathway_removed)
# trees[[1]] = hc # tree is a list, so we use [[]]
# names(trees) is a vector, so we use []
# i=10
# i=50
# i=30
# i=20
# i=40
# i=5
# i=3
# i=4
library("ape")
for (tr in 1:length(trees)){
  tree = trees[[tr]]
  print(names(trees)[tr])
  for(i in levels){
    # 1. Cut trees
    clus <- cutree(tree,i)
    #colors = c("red","blue","green","black")
    colors = colors(distinct = T)[2:(i+1)]
    plot.phylo(as.phylo(tree), type = "cladogram", tip.color = colors[clus])
    clus_df <- as.data.frame(clus)
    
    # 2. Create the bacteria list based on the cluster IDs
    clus_df$Bacteria <- rownames(clus_df)
    
    rownames(clus_df) <- c(1:nrow(clus_df))
    bacteria <- c()
    clusterID <- c()
    for (h in unique(clus_df$clus)){
      
      #tempBacteria <- ""
      bacterialist <- "" # a list of bacteria. Each cluster ID will have one bacterlist. So we need to initialize that
      
      commonClustID <- clus_df[clus_df$clus == h,]
      #ID <- clus_l10_df_copy$clus10[h]
      for (g in 1:nrow(commonClustID)){
        tempBacteria <- commonClustID$Bacteria[g]
        bacterialist <- paste0(bacterialist,tempBacteria,",")
        
      }
      clusterID <- c(clusterID, as.character(h))
      bacteria <- c(bacteria,bacterialist)
    }
    
    dfBacteria <- as.data.frame(clusterID)
    dfBacteria$Bacteria <- bacteria
    save(dfBacteria,file=paste0("trees/",names(trees)[tr],"_l",i,".rfile"))
    
  }
}


