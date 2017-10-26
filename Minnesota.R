##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsMin = dir("./Minnesota", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DMin <- do.call("readFiles", as.list(filePathsMin)) 
MMin <- convert2df(DMin, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataMin <- select(MMin, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataMin <- cSplit(mydataMin, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataMin$C1, ";"))
ifelse(count + nrow(mydataMin) == nrow(tidy_dataMin), "No drops", "Warning") 

## Remove non-Minnesota and non-engineering addresses
MinData <- tidy_dataMin[grep("UNIV MINNESOTA", tidy_dataMin$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=1280150136&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(MinData$C1, function(x) abs[str_detect(x, abs)])

MinData<-cbind(MinData,plyr::ldply(dept_test,rbind)[,1])
names(MinData)[6]<-"Abbreviation"

engDeptData <- merge(MinData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))

engMiss <- Other[grep("ENGN", Other$C1), ]
View(engMiss)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Minnesota.csv", quote = TRUE, row.names = FALSE)