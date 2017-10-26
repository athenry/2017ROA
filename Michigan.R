##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsMich = dir("./Michigan", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DMich <- do.call("readFiles", as.list(filePathsMich)) 
MMich <- convert2df(DMich, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataMich <- select(MMich, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataMich <- cSplit(mydataMich, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataMich$C1, ";"))
ifelse(count + nrow(mydataMich) == nrow(tidy_dataMich), "No drops", "Warning") 

## Remove non-Michigan and non-engineering addresses
MichData <- tidy_dataMich[grep("UNIV MICHIGAN", tidy_dataMich$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=359839112&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(MichData$C1, function(x) abs[str_detect(x, abs)])

MichData<-cbind(MichData,plyr::ldply(dept_test,rbind)[,1])
names(MichData)[6]<-"Abbreviation"

engDeptData <- merge(MichData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))

engMiss <- Other[grep("ENGN", Other$C1), ]
View(engMiss)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Michigan.csv", quote = TRUE, row.names = FALSE)