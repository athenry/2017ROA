##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsQ = dir("./Queens", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DQ <- do.call("readFiles", as.list(filePathsQ)) 
MQ <- convert2df(DQ, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataQ <- select(MQ, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataQ <- cSplit(mydataQ, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataQ$C1, ";"))
ifelse(count + nrow(mydataQ) == nrow(tidy_dataQ), "No drops", "Warning") 

## Remove non-Queen's addresses
QData <- tidy_dataQ[grep("QUEENS UNIV", tidy_dataQ$C1), ]


## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=379080043&single=true&output=csv"

depts <- read.csv(deptURL)
abs <- as.character(depts$Abbreviation)

dept_test <- sapply(QData$C1, function(x) abs[str_detect(x, abs)])
QData<-cbind(QData,plyr::ldply(dept_test,rbind)[,1])
names(QData)[6]<-"Abbreviation"
engDeptData <- merge(QData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Queens.csv", quote = TRUE, row.names = FALSE)