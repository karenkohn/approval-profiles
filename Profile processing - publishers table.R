library(tidyr)
library(zoo)

rm(list=ls()) # The script is run over & over to import and merge each profile, so it begins with removing the objects created the previous time.
profile.raw <- read.csv("[profile filename].csv",stringsAsFactors = FALSE)

#Identify the line that contains the profile name and make this its own object. It will become a value in a new column later.
prof.name <- as.character(profile.raw[which(grepl("Profile: ",profile.raw[,1])==TRUE),1])
split.prof.name <- unlist(strsplit(prof.name,":"))
profile <- sub("^\\s+","",(split.prof.name[[3]]))

#Add the headers, remove empty columns, truncate table at the first blank row.
header.row <- which(profile.raw == "Description")
#profile.raw[header.row,]  #This lists the row that has the headers

publishers <- grep("^AP Publishers",profile.raw[,1]) #This line identifies that the publishers table is starting.

publishers.w.headers <- data.frame(profile.raw[publishers[1]+1:nrow(profile.raw),],stringsAsFactors = FALSE)
colnames(publishers.w.headers) <- profile.raw[header.row,]

#Checks if a new parameter starts after the publishers table. If so, it will be a line with no space at the beginning.
new.param <- grep("^[A-Za-z]",publishers.w.headers[,1])

if (length(new.param)>0) {
  publishers.w.headers <- publishers.w.headers[1:new.param[[1]]-1,]
}

#Remove any blank rows at end
blank.row <- which(publishers.w.headers$Description=="")
if (length(blank.row)>0) {
  publishers.w.headers <- publishers.w.headers[1:blank.row[[1]]-1,] 
}

#clean up columns
to.keep <- c(which(names(publishers.w.headers)=="Description"),which(names(publishers.w.headers)=="Books"),which(names(publishers.w.headers)=="Slips"),which(names(publishers.w.headers)=="Exclude"))
publishers.cleaner <- publishers.w.headers[,to.keep]
publishers.cleaner$Profile <- profile #Add the profile name into this newly created column.

#trim whitespace
publishers.cleaner$Description <- sub("^\\s+","",publishers.cleaner$Description)

#merge Books/Slips/Exclude into one column
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

# read in the CSV file that holds all the profiles merged so far
allpubs <- read.csv(file="[filename].csv")
allpubs <- allpubs[,-1]

# append the new profile
allpubs <-rbind(publishers.done,allpubs)

write.csv(file="[same filename we read in].csv",allpubs)

# Optional check which profiles we've read in so far. Helps you keep track of where you are.
profiles.so.far <- sort(unique(allpubs$Profile))
View(profiles.so.far)
