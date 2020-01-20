library(tidyr)
library(zoo)

rm(list=ls())
profile.raw <- read.csv("S://Main Campus/Collection Development/Coutts Profiles/R processing project/TEscg Science General 2019.csv",stringsAsFactors = FALSE)

prof.name <- as.character(profile.raw[which(grepl("Profile: ",profile.raw[,1])==TRUE),1])
split.prof.name <- unlist(strsplit(prof.name,":"))
profile <- sub("^\\s+","",(split.prof.name[[3]]))

#Add the headers, remove empty columns, truncate table at the first blank row.
header.row <- which(profile.raw == "Description")
#profile.raw[header.row,]  #This lists the row that has the headers

publishers <- grep("^AP Publishers",profile.raw[,1])

publishers.w.headers <- data.frame(profile.raw[publishers[1]+1:nrow(profile.raw),],stringsAsFactors = FALSE)
colnames(publishers.w.headers) <- profile.raw[header.row,]

#does a new parameter start after?
new.param <- grep("^[A-Za-z]",publishers.w.headers[,1])

if (length(new.param)>0) {
  publishers.w.headers <- publishers.w.headers[1:new.param[[1]]-1,]
}

#Remove any blank rows at end
blank.row <- which(publishers.w.headers$Description=="")
if (length(blank.row)>0) {
  publishers.w.headers <- publishers.w.headers[1:blank.row[[1]]-1,] # This only works if there is a blank row
}

#clean up columns
to.keep <- c(which(names(publishers.w.headers)=="Description"),which(names(publishers.w.headers)=="Books"),which(names(publishers.w.headers)=="Slips"),which(names(publishers.w.headers)=="Exclude"))
publishers.cleaner <- publishers.w.headers[,to.keep]
publishers.cleaner$Profile <- profile

#trim whitespace
publishers.cleaner$Description <- sub("^\\s+","",publishers.cleaner$Description)

#merge Books/Slips/X

publishers.cleaner$Instruction <- paste(publishers.cleaner$Books,publishers.cleaner$Slips,publishers.cleaner$Exclude)
publishers.cleaner$Instruction <- sub("\\s+","",publishers.cleaner$Instruction)
remove <- c(which(names(publishers.cleaner)=="Books"),which(names(publishers.cleaner)=="Slips"),which(names(publishers.cleaner)=="Exclude"))
publishers.done <- publishers.cleaner[,-remove]

# Add a DDA Y/N column
DDA <- grep("PDA",publishers.done$Profile)
if (length(DDA) >0) {
  publishers.done$Format <- "DDA"
  
} else {
  publishers.done$Format <- "Print"
}

allpubs <- read.csv(file="S://Main Campus/Collection Development/Coutts Profiles/R processing project/Publishers table.csv")
allpubs <- allpubs[,-1]

allpubs <-rbind(publishers.done,allpubs)

write.csv(file="S://Main Campus/Collection Development/Coutts Profiles/R processing project/Publishers table.csv",allpubs)

profiles.so.far <- sort(unique(allpubs$Profile))
View(profiles.so.far)
