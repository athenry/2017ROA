##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsMcG = dir("./McGill", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DMcG <- do.call("readFiles", as.list(filePathsMcG)) 
MMcG <- convert2df(DMcG, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataMcG <- select(MMcG, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataMcG <- cSplit(mydataMcG, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataMcG$C1, ";"))
ifelse(count + nrow(mydataMcG) == nrow(tidy_dataMcG), "No drops", "Warning") 

## Remove non-McGill addresses
McGData <- tidy_dataMcG[grep("MCGILL UNIV", tidy_dataMcG$C1), ]

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=296876605&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(McGData$C1, function(x) abs[str_detect(x, abs)])

McGData<-cbind(McGData,plyr::ldply(dept_test,rbind)[,1])
names(McGData)[6]<-"Abbreviation"
engDeptData <- merge(McGData, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

## Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "McGill.csv", quote = TRUE, row.names = FALSE)