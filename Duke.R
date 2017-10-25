##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsDuke = dir("./Duke", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DDuke <- do.call("readFiles", as.list(filePathsDuke)) 
MDuke <- convert2df(DDuke, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataDuke <- select(MDuke, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataDuke <- cSplit(mydataDuke, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataDuke$C1, ";"))
ifelse(count + nrow(mydataDuke) == nrow(tidy_dataDuke), "No drops", "Warning") 

## Remove non-Duke and non-engineering addresses
DukeData <- tidy_dataDuke[grep("DUKE UNIV", tidy_dataDuke$C1), ]
DukeData <- DukeData[grep("ENGN", DukeData$C1), ]

## Assign Departmental affiliations
deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=106621989&single=true&output=csv"

depts <- read.csv(deptURL)
abs <- as.character(depts$Abbreviation)

dept_test <- sapply(DukeData$C1, function(x) abs[str_detect(x, abs)])
DukeData<-cbind(DukeData,plyr::ldply(dept_test,rbind)[,1])
names(DukeData)[6]<-"Abbreviation"
engDeptData <- merge(DukeData, depts, all.x = TRUE) ##keeps nonmatches and enters NA


## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)
engMiss<- Other[grep("ENGN", Other$C1), ]
View(engMiss)

##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Duke.csv", quote = TRUE, row.names = FALSE)