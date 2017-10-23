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

fullDataLaval$Department <- ifelse(grepl(" CIVIL ", fullDataLaval$C1), "Civil Engineering and Water  Engineering",
                                ifelse(grepl(" ELECT ", fullDataLaval$C1), "Electrical and Computer Engineering",
                                       ifelse(grepl(" MECH", fullDataLaval$C1), "Mechanical  Engineering",
                                             ifelse(grepl(" CHEM", fullDataLaval$C1), "Chemical Engineering",
                                                    ifelse(grepl("MIN MET ", fullDataLaval$C1), "Mineral, Metallurgical and Materials Engineering",
                                                           ifelse(grepl("COMP SCI", fullDataLaval$C1), "Computer and Software Engineering",
                                                                  ifelse(grepl("GEOL ", fullDataLaval$C1), "Geology and Geological Engineering",
                                                                         ifelse(grepl("PHYS ", fullDataLaval$C1), "Physics, Engineering Physics and Optics",
                                                                               ifelse(grepl("MET ", fullDataLaval$C1), "Mineral, Metallurgical and Materials Engineering",
                                                                  "Other")))))))))

## check the "other"s for articles that should be kept
Other <- filter(fullDataLaval, Department == "Other")
View(Other)

##Keep only eng departments
fullDataLaval <- filter(fullDataLaval, Department !="Other")

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(fullDataLaval, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Laval.csv", quote = TRUE, row.names = FALSE)
