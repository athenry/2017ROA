##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsUF = dir("./Florida", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DUF <- do.call("readFiles", as.list(filePathsUF)) 
MUF <- convert2df(DUF, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataUF <- select(MUF, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataUF <- cSplit(mydataUF, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataUF$C1, ";"))
ifelse(count + nrow(mydataUF) == nrow(tidy_dataUF), "No drops", "Warning") 

## Remove non-UF and non-engineering addresses
UFData <- tidy_dataUF[grep("UNIV FLORIDA", tidy_dataUF$C1), ]
UFData <- UFData[grep("ENGN", UFData$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=1891176148&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(UFData$C1, function(x) abs[str_detect(x, abs)])

UFData<-cbind(UFData,plyr::ldply(dept_test,rbind)[,1])
names(UFData)[6]<-"Abbreviation"

engDeptData <- merge(UFData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Florida.csv", quote = TRUE, row.names = FALSE)