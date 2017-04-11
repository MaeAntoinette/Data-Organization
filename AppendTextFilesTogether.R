#Script to mend text files together as one joint file. 
#Original purpose: to create a point shp for ET/NDVI data of Imperial Valley.

#Identify the  folder of text files 
text.fold <- "F:/EEFlux/P39R37/2015/ET/TextFiles/"
text.list <- list.files(text.fold,pattern = ".txt", full.names = TRUE, recursive = FALSE)
text.list 

#Read one file from text.list 
dataset.test <- read.table(text.list[1], header = TRUE, sep=" ")

for (i in (1:length(text.list))){
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(text.list[i], header=TRUE, sep=" ")
  }

  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <- read.table(text.list[i], header=TRUE, sep=" ")
    dataset <- rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}


#Calculate seasonal (may - october) and annual sums for ET. change the year of your column headers.
dataset[is.na(dataset)] <- 0
dataset$Ann.2015 <- rowSums(dataset[,3:length(dataset)])
dataset$Seas.2015 <- rowSums(dataset[,7:12])

#Write your appended df as a text file.
write.table(dataset,file = paste0(text.fold,"2015_Completed.txt"), sep =",", row.names = FALSE, col.names = TRUE)
