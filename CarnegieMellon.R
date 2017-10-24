##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsCM = dir("./CarnegieMellon", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DCM <- do.call("readFiles", as.list(filePathsCM)) 
MCM <- convert2df(DCM, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataCM <- select(MCM, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataCM <- cSplit(mydataCM, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataCM$C1, ";"))
ifelse(count + nrow(mydataCM) == nrow(tidy_dataCM), "No drops", "Warning") 

## Remove non-CarnegieMellon and non-engineering addresses
CMData <- tidy_dataCM[grep("UNIV CarnegieMellon", tidy_dataCM$C1), ]
CMData <- CMData[grep("ENGN", CMData$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=164215798&single=true&output=csv"

depts <- read.csv(deptURL)
abs <- as.character(depts$Abbreviation)

dept_test <- sapply(CMData$C1, function(x) abs[str_detect(x, abs)])
CMData<-cbind(CMData,plyr::ldply(dept_test,rbind)[,1])
names(CMData)[6]<-"Abbreviation"
engDeptData <- merge(CMData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "CarnegieMellon.csv", quote = TRUE, row.names = FALSE)