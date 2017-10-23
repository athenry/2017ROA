##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsUBC = dir("./UBC", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DUBC <- do.call("readFiles", as.list(filePathsUBC)) 
MUBC <- convert2df(DUBC, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataUBC <- select(MUBC, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataUBC <- cSplit(mydataUBC, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataUBC$C1, ";"))
ifelse(count + nrow(mydataUBC) == nrow(tidy_dataUBC), "No drops", "Warning") 

## Remove non-UBC addresses
UBCData <- tidy_dataUBC[grep("UNIV BRITISH COLUMBIA", tidy_dataUBC$C1), ]
engDataUBC <- UBCData[grep("ENGN", UBCData$C1), ]

engDataUBC$Department <- ifelse(grepl(" BIOMED ", engDataUBC$C1), "Biomedical Engineering",
                                ifelse(grepl(" CTR MET ", engDataUBC$C1), "Materials Engineering",
                                       ifelse(grepl(" CHEM ", engDataUBC$C1), "Chemical and Biological Engineering",
                        ifelse(grepl(" BIOL ", engDataUBC$C1), "Chemical and Biological Engineering",
                           ifelse(grepl(" MAT ", engDataUBC$C1), "Materials Engineering",
                                ifelse(grepl(" CIVIL ", engDataUBC$C1), "Civil Engineering",
                                      ifelse(grepl(" MIN ", engDataUBC$C1), "Keevil Institute of Mining Engineering",
                                            ifelse(grepl(" ELECT ", engDataUBC$C1), "Electrical and Computer Engineering",
                                                  ifelse(grepl(" MECH ", engDataUBC$C1), "Mechanical Engineering",
                                                         ifelse(grepl(" OKANAGAN ", engDataUBC$C1), "Okanagan Campus",
                                                                ifelse(grepl(" KELOWNA", engDataUBC$C1), "Okanagan Campus",
                                                                "Other")))))))))))

## check the "other"s for articles that should be kept
Other <- filter(engDataUBC, Department == "Other")
View(Other)

##Keep only eng departments
engDataUBC <- filter(engDataUBC, Department != "Other")

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(engDataUBC, UT, DT, TC, PY, Department))
write.csv(engDataDD, "UBC.csv", quote = TRUE, row.names = FALSE)
