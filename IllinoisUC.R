##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsIUC = dir("./IllinoisUC", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DIUC <- do.call("readFiles", as.list(filePathsIUC)) 
MIUC <- convert2df(DIUC, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataIUC <- select(MIUC, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataIUC <- cSplit(mydataIUC, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataIUC$C1, ";"))
ifelse(count + nrow(mydataIUC) == nrow(tidy_dataIUC), "No drops", "Warning") 

## Remove non-IUC and non-engineering addresses
IUCData <- tidy_dataIUC[grep("UNIV ILLINOIS", tidy_dataIUC$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=1108083306&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(IUCData$C1, function(x) abs[str_detect(x, abs)])

IUCData<-cbind(IUCData,plyr::ldply(dept_test,rbind)[,1])
names(IUCData)[6]<-"Abbreviation"

engDeptData <- merge(IUCData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))

engMiss <- Other[grep("ENGN", Other$C1), ]
View(engMiss)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "IllinoisUC.csv", quote = TRUE, row.names = FALSE)