##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsSK = dir("./Saskatchewan", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DSK <- do.call("readFiles", as.list(filePathsSK)) 
MSK <- convert2df(DSK, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataSK <- select(MSK, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataSK <- cSplit(mydataSK, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataSK$C1, ";"))
ifelse(count + nrow(mydataSK) == nrow(tidy_dataSK), "No drops", "Warning") 

## Remove non-Saskatchewan addresses
SKData <- tidy_dataSK[grep("UNIV SASKATCHEWAN", tidy_dataSK$C1), ]


## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=1900927404&single=true&output=csv"

depts <- read.csv(deptURL)
abs <- as.character(depts$Abbreviation)

dept_test <- sapply(SKData$C1, function(x) abs[str_detect(x, abs)])
SKData<-cbind(SKData,plyr::ldply(dept_test,rbind)[,1])
names(SKData)[6]<-"Abbreviation"
engDeptData <- merge(SKData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
engTest <- Other[grep("ENGN", Other$C1), ]
View(Other)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Saskatchewan.csv", quote = TRUE, row.names = FALSE)