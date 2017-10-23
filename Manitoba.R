##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsMB = dir("./Manitoba", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DMB <- do.call("readFiles", as.list(filePathsMB)) 
MMB <- convert2df(DMB, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataMB <- select(MMB, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataMB <- cSplit(mydataMB, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataMB$C1, ";"))
ifelse(count + nrow(mydataMB) == nrow(tidy_dataMB), "No drops", "Warning") 

## Remove non-MB and non-ENGN addresses
MBData <- tidy_dataMB[grep("UNIV MANITOBA", tidy_dataMB$C1), ]
engDataMB <- MBData[grep("ENGN", MBData$C1), ]

engDataMB$Department <- ifelse(grepl(" CIVIL ", engDataMB$C1), "Civil Engineering",
                                   ifelse(grepl(" ELECT ", engDataMB$C1), "Electrical and Computer Engineering",
                                          ifelse(grepl(" MECH", engDataMB$C1), "Mechanical  Engineering",
                                                 ifelse(grepl(" BIOSYST", engDataMB$C1), "Biosystems Engineering", 
                                                 "Other"))))

## check the "other"s for articles that should be kept
Other <- filter(engDataMB, Department == "Other")
View(Other)

##Keep only eng departments
engDataMB <- filter(engDataMB, Department !="Other")

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(engDataMB, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Manitoba.csv", quote = TRUE, row.names = FALSE)
