##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsCUB = dir("./ColoradoBoulder", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DCUB <- do.call("readFiles", as.list(filePathsCUB)) 
MCUB <- convert2df(DCUB, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataCUB <- select(MCUB, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataCUB <- cSplit(mydataCUB, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataCUB$C1, ";"))
ifelse(count + nrow(mydataCUB) == nrow(tidy_dataCUB), "No drops", "Warning") 

## Remove non-CU Boulder addresses
CUBData <- tidy_dataCUB[grep("UNIV COLORADO", tidy_dataCUB$C1), ]
CUBData <- CUBData[grep("BOULDER", CUBData$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=654946467&single=true&output=csv"

depts <- read.csv(deptURL)
abs <- as.character(depts$Abbreviation)

dept_test <- sapply(CUBData$C1, function(x) abs[str_detect(x, abs)])
CUBData<-cbind(CUBData,plyr::ldply(dept_test,rbind)[,1])
names(CUBData)[6]<-"Abbreviation"
engDeptData <- merge(CUBData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)
engMiss<- Other[grep("ENGN", Other$C1), ]
View(engMiss)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "ColoradoBoulder.csv", quote = TRUE, row.names = FALSE)