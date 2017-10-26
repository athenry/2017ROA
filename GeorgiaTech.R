##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsGT = dir("./GeorgiaTech", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DGT <- do.call("readFiles", as.list(filePathsGT)) 
MGT <- convert2df(DGT, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataGT <- select(MGT, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataGT <- cSplit(mydataGT, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataGT$C1, ";"))
ifelse(count + nrow(mydataGT) == nrow(tidy_dataGT), "No drops", "Warning") 

## Remove non-GT and non-engineering addresses
GTData <- tidy_dataGT[grep("GEORGIA INST TECHNOL", tidy_dataGT$C1), ]
GTData <- GTData[grep("ENGN", GTData$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=329265229&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(GTData$C1, function(x) abs[str_detect(x, abs)])

GTData<-cbind(GTData,plyr::ldply(dept_test,rbind)[,1])
names(GTData)[6]<-"Abbreviation"

engDeptData <- merge(GTData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "GeorgiaTech.csv", quote = TRUE, row.names = FALSE)