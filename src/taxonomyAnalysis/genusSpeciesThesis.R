#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####################### Extract unique genus species for 159 bacteria
my_organisms <- read.csv("CleanShiny_Original_New_completed_JR_correct.csv", stringsAsFactors = F)
my_organisms <- as.data.frame(unique(my_organisms$All_Real_Name))
my_organisms <- as.data.frame(my_organisms[!apply(my_organisms == "", 1, all),])
colnames(my_organisms) <- c("organism")
my_organisms$organism <- as.character(my_organisms$organism)


my_organisms2 <- strsplit(my_organisms$organism," ",fixed=TRUE)
my_organisms_df2 <- unlist(lapply(my_organisms2, function(x) x[1]))
my_organisms_df3 <- unlist(lapply(my_organisms2, function(x) x[2]))
my_organisms_df_Final2<- cbind(my_organisms,my_organisms_df2,my_organisms_df3)
my_organisms_df_Final2$GenusSpecies <- paste(my_organisms_df_Final2$my_organisms_df2,my_organisms_df_Final2$my_organisms_df3,sep=" ")
my_organisms_gs <- as.data.frame(my_organisms_df_Final2$GenusSpecies)
colnames(my_organisms_gs) <- c("GenusSpecies")
my_organisms_gs_unique <- as.data.frame(unique(my_organisms_gs$GenusSpecies))
colnames(my_organisms_gs_unique) <- c("GenusSpecies")
write.table(my_organisms_gs_unique, file ="my_organisms_gs_unique.csv", row.names =F, col.names=F,quote = F)
