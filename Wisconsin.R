##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsWisc = dir("./Wisconsin", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DWisc <- do.call("readFiles", as.list(filePathsWisc)) 
MWisc <- convert2df(DWisc, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataWisc <- select(MWisc, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataWisc <- cSplit(mydataWisc, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataWisc$C1, ";"))
ifelse(count + nrow(mydataWisc) == nrow(tidy_dataWisc), "No drops", "Warning") 

## Remove non-Wisconsin and non-engineering addresses
WiscData <- tidy_dataWisc[grep("UNIV WISCONSIN", tidy_dataWisc$C1), ]
WiscData <- WiscData[grep("ENGN", WiscData$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=1824697148&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(WiscData$C1, function(x) abs[str_detect(x, abs)])

WiscData<-cbind(WiscData,plyr::ldply(dept_test,rbind)[,1])
names(WiscData)[6]<-"Abbreviation"

engDeptData <- merge(WiscData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Wisconsin.csv", quote = TRUE, row.names = FALSE)