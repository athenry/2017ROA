##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsDal = dir("./Dalhousie", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DDal <- do.call("readFiles", as.list(filePathsDal)) 
MDal <- convert2df(DDal, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataDal <- select(MDal, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataDal <- cSplit(mydataDal, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataDal$C1, ";"))
ifelse(count + nrow(mydataDal) == nrow(tidy_dataDal), "No drops", "Warning") 

## Remove non-Dalhousie addresses
DalData <- tidy_dataDal[grep("DALHOUSIE UNIV", tidy_dataDal$C1), ]
engDataDal <- DalData[grep("ENGN", DalData$C1), ]

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=134374484&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(engDataDal$C1, function(x) abs[str_detect(x, abs)])

engDataDal<-cbind(engDataDal,plyr::ldply(dept_test,rbind)[,1])
names(engDataDal)[6]<-"Abbreviation"
engDeptData <- merge(engDataDal, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

##Keep only eng departments
engDeptData <- filter(engDeptData, Department !="Truro Campus")
finalEngData <- engDeptData[complete.cases(engDeptData), ]

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Dalhousie.csv", quote = TRUE, row.names = FALSE)
