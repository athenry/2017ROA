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

McMData$Department <- ifelse(grepl(" CHEM ", McMData$C1), "Chemical and Petroleum Engineering",
                                ifelse(grepl(" CIVIL ", McMData$C1), "Civil Engineering",
                                       ifelse(grepl(" ELECT ", McMData$C1), "Electrical and Computer Engineering",
                                              ifelse(grepl(" MECH ", McMData$C1), "Mechanical  and Manufacturing Engineering",
                                                     ifelse(grepl(" SOFTWARE ", McMData$C1), "Computing and Software",
                                                            ifelse(grepl(" MAT ", McMData$C1), "Materials Science and Engineering",
                                                                 ifelse(grepl(" ENGN PHYS", McMData$C1), "Engineering Physics",
                                                                        ifelse(grepl(" BIOMED ", McMData$C1), "School of Biomedical Engineering",
                                                                               ifelse(grepl("SCH ENGN TECHNOL", McMData$C1), "School of Engineering Practice and Techology",
                                                                   "Other")))))))))

## check the "other"s for articles that should be kept
Other <- filter(McMData, Department == "Other")
View(Other)

##Keep only eng departments
engDataMcM <- filter(McMData, Department != "Other")

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(engDataMcM, UT, DT, TC, PY, Department))
write.csv(engDataDD, "McMaster.csv", quote = TRUE, row.names = FALSE)
