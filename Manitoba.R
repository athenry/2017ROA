##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsMB = dir("./Manitoba", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DMB <- do.call("readFiles", as.list(filePathsMB)) 
MMB <- convert2df(DMB, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataMB <- select(MMB, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataMB <- cSplit(mydataMB, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataMB$C1, ";"))
ifelse(count + nrow(mydataMB) == nrow(tidy_dataMB), "No drops", "Warning") 

## Remove non-MB and non-ENGN addresses
MBData <- tidy_dataMB[grep("UNIV MANITOBA", tidy_dataMB$C1), ]
engDataMB <- MBData[grep("ENGN", MBData$C1), ]

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=1535262908&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(engDataMB$C1, function(x) abs[str_detect(x, abs)])

engDataMB<-cbind(engDataMB,plyr::ldply(dept_test,rbind)[,1])
names(engDataMB)[6]<-"Abbreviation"
engDeptData <- merge(engDataMB, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

## Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Manitoba.csv", quote = TRUE, row.names = FALSE)