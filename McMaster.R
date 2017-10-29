##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsMcM = dir("./McMaster", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DMcM <- do.call("readFiles", as.list(filePathsMcM)) 
MMcM <- convert2df(DMcM, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataMcM <- select(MMcM, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataMcM <- cSplit(mydataMcM, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataMcM$C1, ";"))
ifelse(count + nrow(mydataMcM) == nrow(tidy_dataMcM), "No drops", "Warning") 

## Remove non-McMaster addresses
McMData <- tidy_dataMcM[grep("MCMASTER UNIV", tidy_dataMcM$C1), ]

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=1500842562&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(McMData$C1, function(x) abs[str_detect(x, abs)])

McMData<-cbind(McMData,plyr::ldply(dept_test,rbind)[,1])
names(McMData)[6]<-"Abbreviation"
engDeptData <- merge(McMData, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the NAs for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)
engMiss <- Other[grep("ENGN", Other$C1), ]

## Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "McMaster.csv", quote = TRUE, row.names = FALSE)