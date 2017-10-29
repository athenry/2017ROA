##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsMtl = dir("./Montreal", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DMtl <- do.call("readFiles", as.list(filePathsMtl)) 
MMtl <- convert2df(DMtl, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataMtl <- select(MMtl, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataMtl <- cSplit(mydataMtl, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataMtl$C1, ";"))
ifelse(count + nrow(mydataMtl) == nrow(tidy_dataMtl), "No drops", "Warning") 

## Remove non-Mtl and non-ENGN addresses
MtlData <- tidy_dataMtl[grep("POLYTECH", tidy_dataMtl$C1), ]
engDataMtl <- MtlData[grep("ENGN", MtlData$C1), ]
genieDataMtl <- MtlData[grep("GENIE", MtlData$C1), ]
fullDataMtl <- rbind(engDataMtl, genieDataMtl)

## assign departmental affiliation

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=69469653&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(fullDataMtl$C1, function(x) abs[str_detect(x, abs)])

fullDataMtl<-cbind(fullDataMtl,plyr::ldply(dept_test,rbind)[,1])
names(fullDataMtl)[6]<-"Abbreviation"
engDeptData <- merge(fullDataMtl, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

##Remove departmental duplicates (leave institutional duplicates)
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Montreal.csv", quote = TRUE, row.names = FALSE)
