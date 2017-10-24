##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsWat = dir("./Waterloo", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DWat <- do.call("readFiles", as.list(filePathsWat)) 
MWat <- convert2df(DWat, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataWat <- select(MWat, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataWat <- cSplit(mydataWat, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataWat$C1, ";"))
ifelse(count + nrow(mydataWat) == nrow(tidy_dataWat), "No drops", "Warning") 

## Remove non-Waterloo addresses
WatData <- tidy_dataWat[grep("UNIV WATERLOO", tidy_dataWat$C1), ]


## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=985703843&single=true&output=csv"

depts <- read.csv(deptURL)
abs <- as.character(depts$Abbreviation)

dept_test <- sapply(WatData$C1, function(x) abs[str_detect(x, abs)])
WatData<-cbind(WatData,plyr::ldply(dept_test,rbind)[,1])
names(WatData)[6]<-"Abbreviation"
engDeptData <- merge(WatData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)
engTest <- Other[grep("ENGN", Other$C1), ]
View(engTest)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Waterloo.csv", quote = TRUE, row.names = FALSE)