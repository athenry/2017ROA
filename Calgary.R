##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsCal = dir("./Calgary", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DCal <- do.call("readFiles", as.list(filePathsCal)) 
MCal <- convert2df(DCal, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataCal <- select(MCal, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataCal <- cSplit(mydataCal, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataCal$C1, ";"))
ifelse(count + nrow(mydataCal) == nrow(tidy_dataCal), "No drops", "Warning") 

## Remove non-Calgary addresses
CalData <- tidy_dataCal[grep("UNIV CALGARY", tidy_dataCal$C1), ]
engDataCal <- CalData[grep("ENGN", CalData$C1), ]

engDataCal$Department <- ifelse(grepl(" CHEM ", engDataCal$C1), "Chemical and Petroleum Engineering",
                                ifelse(grepl(" CIVIL ", engDataCal$C1), "Civil Engineering",
                                       ifelse(grepl(" ELECT ", engDataCal$C1), "Electrical and Computer Engineering",
                                              ifelse(grepl(" MECH ", engDataCal$C1), "Mechanical  and Manufacturing Engineering",
                                                     ifelse(grepl(" GEOMAT", engDataCal$C1), "Geomatics Engineering",
                                                            ifelse(grepl(" SOFTWARE ", engDataCal$C1), "Electrical and Computer Engineering",
                                                                   ifelse(grepl(" BIOMED ", engDataCal$C1), "Biomedical Engineering",
                                                                          ifelse(grepl(" PIPELINE ", engDataCal$C1), "Mechanical Engineering",
                                                                                 "Other"))))))))

## check the "other"s for articles that should be kept
Other <- filter(engDataCal, Department == "Other")
View(Other)

##Keep only eng departments
engDataCal <- filter(engDataCal, Department != "Other")

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(engDataCal, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Calgary.csv", quote = TRUE, row.names = FALSE)
