##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsUTA = dir("./UTAustin", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DUTA <- do.call("readFiles", as.list(filePathsUTA)) 
MUTA <- convert2df(DUTA, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataUTA <- select(MUTA, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataUTA <- cSplit(mydataUTA, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataUTA$C1, ";"))
ifelse(count + nrow(mydataUTA) == nrow(tidy_dataUTA), "No drops", "Warning") 

## Remove non-UTAustin and non-engineering addresses
UTAData <- tidy_dataUTA[grep("UNIV TEXAS AUSTIN", tidy_dataUTA$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=597067278&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(UTAData$C1, function(x) abs[str_detect(x, abs)])

UTAData<-cbind(UTAData,plyr::ldply(dept_test,rbind)[,1])
names(UTAData)[6]<-"Abbreviation"

engDeptData <- merge(UTAData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))

engMiss <- Other[grep("ENGN", Other$C1), ]
View(engMiss)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "UTAustin.csv", quote = TRUE, row.names = FALSE)