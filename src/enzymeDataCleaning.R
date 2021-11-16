library("dplyr")
############## Data cleaning for Enzymes - Seperate file
# Combine three groups together
primary_ec <- read.csv("primaryECtable.csv", stringsAsFactors = F, header = F)
secondary_ec <- read.csv("secondaryECtable.csv", stringsAsFactors = F, header = F)
tnumber_ec <- read.csv("tnumberECtable.csv", stringsAsFactors = F, header = F)
all_ec<- rbind(primary_ec,secondary_ec,tnumber_ec)
all_ec <- all_ec[,2:3]
colnames(all_ec) <- c("EC_number","Bacteria")

# which(grepl(".* .* .* .*", backup_all_ec$EC_number)): row numbers
# length(which(grepl(".* .*", backup_all_ec$EC_number))): one space

#####################
# For the rows that contain more than one enzyme
# backup_all_ec <- all_ec
#all_ec$EC_number <- as.character(all_ec$EC_number)

while(T %in% grepl(" ", all_ec$EC_number)){ # While there exists a space in our list of EC numbers...
  firstSpace <- which(grepl(" ", all_ec$EC_number ) == T)[[1]] # Locate the first instance of a space
  
  oldSplitMin <- all_ec[c(1:(firstSpace - 1)),] # Extract all rows of "all_ec" before the space
  oldSplitMax <- all_ec[c((firstSpace + 1):nrow(all_ec)),] # Extract all rows of "all_ec" after the space
  
  splitEcs <- unlist(strsplit(all_ec$EC_number[[firstSpace]], " ")) # Retrieve a list of desired EC values split by spaces.
  
  # This section create a list of all the EC numbers pulled from the origin string with spaces
  expandedRow <- splitEcs[1]
  for (i in 2:length(splitEcs)){
    expandedRow <- c(expandedRow, paste0("ec:", splitEcs[i]))
  }
  
  expandedDF <- as.data.frame(expandedRow)
  colnames(expandedDF) <- "EC_number"
  expandedDF$Bacteria <- rep(all_ec$Bacteria[[firstSpace]], nrow(expandedDF)) # Attach the name of the bacteria
  # to the new rows of the dataframe we created.
  
  # Rewrite all_ec, where we include the expanded section of the dataframe into the MIDDLE
  # of the old dataframe.
  all_ec <- rbind(oldSplitMin, expandedDF)
  all_ec <- rbind(all_ec, oldSplitMax)
}

# Remove the duplicated bacteria(Bifidobacterium longum subsp. Infantis ATCC 15697)
all_ec_159 <- all_ec[-which(all_ec$Bacteria == "Bifidobacterium longum subsp. Infantis ATCC 15697"),]

# Make each row unique
all_ec_159_unique <- all_ec_159 %>% distinct()
# Save the files all_pathway_159_map_unique as a csv file - combined_pathway.csv
write.csv(all_ec_159_unique, file ="combined_ec.csv", row.names =F)

