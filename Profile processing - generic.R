library(tidyr)
library(zoo)

rm(list=ls())
profile.raw <- read.csv("S://Main Campus/Collection Development/Coutts Profiles/R processing project/TEbcp Biology Chemistry Physics.csv", stringsAsFactors = FALSE)

prof.name <- profile.raw[which(grepl("Profile: ",profile.raw[,1])==TRUE),1]
split.prof.name <- unlist(strsplit(prof.name,":"))
profile <- sub("^\\s+","",(split.prof.name[[3]]))

#Add the headers, remove empty columns, truncate table at the first blank row.
header.row <- which(profile.raw == "Description")
profile.raw[header.row,]  #This lists the row that has the headers
classes <- grep("^CLASS",profile.raw[,1])

profile.w.headers <- profile.raw[classes[[1]]:nrow(profile.raw),]
colnames(profile.w.headers) <- profile.raw[header.row,]

#Subject parameters may end with a blank row
blank.row <- which(profile.w.headers$Description=="")
if (length(blank.row)>0) {
profile.w.headers <- profile.w.headers[1:blank.row[[1]]-1,] # This only works if there is a blank row
}

#Identifies rows that just say Class Q, etc.
classes <- grep("^CLASS",profile.w.headers[,1])
profile.w.headers <- profile.w.headers[-classes,]

#May introduce a new non-subject parameter after
non.subj <- grep("^[A-Za-z]",profile.w.headers[,1])

   if (length(non.subj)>0) {
      profile.w.headers <- profile.w.headers[1:non.subj[[1]]-1,]
      }

#clean up columns
to.keep <- c(which(names(profile.w.headers)=="Description"),which(names(profile.w.headers)=="Books"),which(names(profile.w.headers)=="Slips"),which(names(profile.w.headers)=="Exclude"),which(names(profile.w.headers)=="Routing"))
profile.cleaner <- profile.w.headers[,to.keep]
profile.cleaner$Profile <- profile

#Start to divide the hierarchy of sub-classes
level1 <- grep("^    [A-Z]",profile.cleaner[,1])

level2 <- grep("^       [A-Z]",profile.cleaner[,1])

level3 <- grep("^          [A-Z]",profile.cleaner[,1])

level4 <- grep("^             [A-Z]",profile.cleaner[,1])

level5 <- grep("^                [A-Z]",profile.cleaner[,1])

level6 <- grep("^                   [A-Z]",profile.cleaner[,1])

level7 <- grep("^                      [A-Z]",profile.cleaner[,1])

level8 <- grep("^                         [A-Z]",profile.cleaner[,1])

level9 <- grep("^                            [A-Z]",profile.cleaner[,1])

level10 <- grep("^                               [A-Z]",profile.cleaner[,1])

level11 <- grep("^                                  [A-Z]",profile.cleaner[,1])

level12 <- grep("^                                     [A-Z]",profile.cleaner[,1])

level13 <- grep("^                                        [A-Z]",profile.cleaner[,1])

sofar <- sort(c(level1,level2,level3,level4,level5,level6,level7,level8,level9,level10,level11,level12,level13))  #This is for testing that you've got all the levels.
missing <- profile.cleaner[-(sofar),1]

profile.levels <- profile.cleaner
profile.levels$Level <-""
colnames(profile.levels) <- c(colnames(profile.cleaner),"Levels")

profile.levels[level1,]$Levels <- "1"
profile.levels[level2,]$Levels <- "2"
profile.levels[level3,]$Levels <- "3"
profile.levels[level4,]$Levels <- "4"
profile.levels[level5,]$Levels <- "5"
profile.levels[level6,]$Levels <- "6"
profile.levels[level7,]$Levels <- "7"
profile.levels[level8,]$Levels <- "8"
profile.levels[level9,]$Levels <- "9"
profile.levels[level10,]$Levels <- "10"
profile.levels[level11,]$Levels <- "11"
profile.levels[level12,]$Levels <- "12"
profile.levels[level13,]$Levels <- "13"

profile.levels$Description.1 <- ""
profile.levels$Description.2 <- ""
profile.levels$Description.3 <- ""
profile.levels$Description.4 <- ""
profile.levels$Description.5 <- ""
profile.levels$Description.6 <- ""
profile.levels$Description.7 <- ""
profile.levels$Description.8 <- ""
profile.levels$Description.9 <- ""
profile.levels$Description.10 <- ""
profile.levels$Description.11 <- ""
profile.levels$Description.12 <- ""
profile.levels$Description.13 <- ""

#colnames(profile.levels)[8:20] <- c("Description.1","Description.2","Description.3","Description.4","Description.5","Description.6","Description.7","Description.8","Description.9","Description.10","Description.11","Description.12","Description.13")

#trim the Description column

profile.levels$Description <- sub("^\\s+","",profile.levels$Description)

#spread out the descriptions into columns
profile.levels[which(profile.levels$Levels=="1"),'Description.1'] <- profile.levels[which(profile.levels$Levels=="1"),'Description']
profile.levels[which(profile.levels$Levels!="1"),'Description.1'] <- NA
profile.levels[which(profile.levels$Levels=="2"),'Description.2'] <- profile.levels[which(profile.levels$Levels=="2"),'Description']
profile.levels[which(profile.levels$Levels!="2"),'Description.2'] <- NA
profile.levels[which(profile.levels$Levels=="3"),'Description.3'] <- profile.levels[which(profile.levels$Levels=="3"),'Description']
profile.levels[which(profile.levels$Levels!="3"),'Description.3'] <- NA
profile.levels[which(profile.levels$Levels=="4"),'Description.4'] <- profile.levels[which(profile.levels$Levels=="4"),'Description']
profile.levels[which(profile.levels$Levels!="4"),'Description.4'] <- NA
profile.levels[which(profile.levels$Levels=="5"),'Description.5'] <- profile.levels[which(profile.levels$Levels=="5"),'Description']
profile.levels[which(profile.levels$Levels!="5"),'Description.5'] <- NA
profile.levels[which(profile.levels$Levels=="6"),'Description.6'] <- profile.levels[which(profile.levels$Levels=="6"),'Description']
profile.levels[which(profile.levels$Levels!="6"),'Description.6'] <- NA
profile.levels[which(profile.levels$Levels=="7"),'Description.7'] <- profile.levels[which(profile.levels$Levels=="7"),'Description']
profile.levels[which(profile.levels$Levels!="7"),'Description.7'] <- NA
profile.levels[which(profile.levels$Levels=="8"),'Description.8'] <- profile.levels[which(profile.levels$Levels=="8"),'Description']
profile.levels[which(profile.levels$Levels!="8"),'Description.8'] <- NA
profile.levels[which(profile.levels$Levels=="9"),'Description.9'] <- profile.levels[which(profile.levels$Levels=="9"),'Description']
profile.levels[which(profile.levels$Levels!="9"),'Description.9'] <- NA
profile.levels[which(profile.levels$Levels=="10"),'Description.10'] <- profile.levels[which(profile.levels$Levels=="10"),'Description']
profile.levels[which(profile.levels$Levels!="10"),'Description.10'] <- NA
profile.levels[which(profile.levels$Levels=="11"),'Description.11'] <- profile.levels[which(profile.levels$Levels=="11"),'Description']
profile.levels[which(profile.levels$Levels!="11"),'Description.11'] <- NA
profile.levels[which(profile.levels$Levels=="12"),'Description.12'] <- profile.levels[which(profile.levels$Levels=="12"),'Description']
profile.levels[which(profile.levels$Levels!="12"),'Description.12'] <- NA
profile.levels[which(profile.levels$Levels=="13"),'Description.13'] <- profile.levels[which(profile.levels$Levels=="13"),'Description']
profile.levels[which(profile.levels$Levels!="13"),'Description.13'] <- NA
#View(profile.levels)

#Fill down
profile.filled <- profile.levels
profile.filled$`Description.1` <- na.locf(profile.levels$`Description.1`)
profile.filled$`Description.2` <- na.locf(profile.levels$`Description.2`,na.rm=FALSE)
profile.filled$`Description.3` <- na.locf(profile.levels$`Description.3`,na.rm=FALSE)
profile.filled$`Description.4` <- na.locf(profile.levels$`Description.4`,na.rm=FALSE)
profile.filled$`Description.5` <- na.locf(profile.levels$`Description.5`,na.rm=FALSE)
profile.filled$`Description.6` <- na.locf(profile.levels$`Description.6`,na.rm=FALSE)
profile.filled$`Description.7` <- na.locf(profile.levels$`Description.7`,na.rm=FALSE)
profile.filled$`Description.8` <- na.locf(profile.levels$`Description.8`,na.rm=FALSE)
profile.filled$`Description.9` <- na.locf(profile.levels$`Description.9`,na.rm=FALSE)
profile.filled$`Description.10` <- na.locf(profile.levels$`Description.10`,na.rm=FALSE)
profile.filled$`Description.11` <- na.locf(profile.levels$`Description.11`,na.rm=FALSE)
profile.filled$`Description.12` <- na.locf(profile.levels$`Description.12`,na.rm=FALSE)
profile.filled$`Description.13` <- na.locf(profile.levels$`Description.13`,na.rm=FALSE)

#Level 1 should have cols 2-13 blank, etc.
profile.filled[which(profile.filled$Levels=="1"),9:20] <- ""
profile.filled[which(profile.filled$Levels=="2"),10:20] <- ""
profile.filled[which(profile.filled$Levels=="3"),11:20] <- ""
profile.filled[which(profile.filled$Levels=="4"),12:20] <- ""
profile.filled[which(profile.filled$Levels=="5"),13:20] <- ""
profile.filled[which(profile.filled$Levels=="6"),14:20] <- ""
profile.filled[which(profile.filled$Levels=="7"),15:20] <- ""
profile.filled[which(profile.filled$Levels=="8"),16:20] <- ""
profile.filled[which(profile.filled$Levels=="9"),17:20] <- ""
profile.filled[which(profile.filled$Levels=="10"),18:20] <- ""
profile.filled[which(profile.filled$Levels=="11"),19:20] <- ""
profile.filled[which(profile.filled$Levels=="12"),20:20] <- ""
View(profile.filled)

# Put Books/Slips/Exclude into one column
profile.filled$Instruction <- paste(profile.filled$Books,profile.filled$Slips,profile.filled$Exclude)
profile.filled$Instruction <- sub("\\s+","",profile.filled$Instruction)
remove <- c(which(names(profile.filled)=="Books"),which(names(profile.filled)=="Slips"),which(names(profile.filled)=="Exclude"))
profile.done <- profile.filled[,-remove]

# Add a DDA Y/N column
DDA <- grep("PDA",profile.done$Profile)
if (length(DDA) >0) {
  profile.done$Format <- "DDA"
  
} else {
  profile.done$Format <- "Print"
  }

# read in the CSV file that holds all the profiles merged so far
allprofiles <- read.csv(file="S://Main Campus/Collection Development/Coutts Profiles/R processing project/all subject params.csv")
allprofiles <- allprofiles[,-1] #it imports with a numbered column at the beginning

#append the new profile
allprofiles <- rbind(profile.done,allprofiles)

profiles.so.far <- sort(unique(allprofiles$Profile))
View(profiles.so.far)

write.csv(file="S://Main Campus/Collection Development/Coutts Profiles/R processing project/all subject params.csv",allprofiles)
