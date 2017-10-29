##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsLaval = dir("./Laval", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DLaval <- do.call("readFiles", as.list(filePathsLaval)) 
MLaval <- convert2df(DLaval, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataLaval <- select(MLaval, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataLaval <- cSplit(mydataLaval, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataLaval$C1, ";"))
ifelse(count + nrow(mydataLaval) == nrow(tidy_dataLaval), "No drops", "Warning") 

## Remove non-Laval and non-ENGN addresses
LavalData <- tidy_dataLaval[grep("UNIV LAVAL", tidy_dataLaval$C1), ]
engDataLaval <- LavalData[grep("ENGN", LavalData$C1), ]
genieDataLaval <- LavalData[grep("GENIE", LavalData$C1), ]
fullDataLaval <- rbind(engDataLaval, genieDataLaval)

## Assign departmental affiliation
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=240286264&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(fullDataLaval$C1, function(x) abs[str_detect(x, abs)])

fullDataLaval<-cbind(fullDataLaval, plyr::ldply(dept_test,rbind)[,1])
names(fullDataLaval)[6]<-"Abbreviation"
fullDataLaval <- merge(fullDataLaval, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(fullDataLaval, is.na(Department))
View(Other)

## ##Keep only eng departments and output data to file
finalEngData <- fullDataLaval[complete.cases(fullDataLaval), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Laval.csv", quote = TRUE, row.names = FALSE)
