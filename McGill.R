##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsMcG = dir("./McGill", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DMcG <- do.call("readFiles", as.list(filePathsMcG)) 
MMcG <- convert2df(DMcG, dbsource = "isi", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataMcG <- select(MMcG, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataMcG <- cSplit(mydataMcG, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataMcG$C1, ";"))
ifelse(count + nrow(mydataMcG) == nrow(tidy_dataMcG), "No drops", "Warning") 

## Remove non-McG and non-ENGN addresses
McGData <- tidy_dataMcG[grep("MCGILL UNIV", tidy_dataMcG$C1), ]

McGData$Department <- ifelse(grepl(" CIVIL ", McGData$C1), "Civil Engineering and Applied Mechanics",
                               ifelse(grepl(" ELECT ", McGData$C1), "Electrical and Computer Engineering",
                                      ifelse(grepl(" MECH", McGData$C1), "Mechanical  Engineering",
                                             ifelse(grepl(" BIOMED", McGData$C1), "Bioengineering", 
                                                    ifelse(grepl(" MIN ", McGData$C1), "Mining and Materials Engineering",
                                                           ifelse(grepl(" CHEM ", McGData$C1), "Chemical Engineering",
                                                                  ifelse(grepl(" BIORESOURCE ", McGData$C1), "Bioengineering",
                                                                         ifelse(grepl(" MAT ", McGData$C1), "Mining and Materials Engineering",
                                                                                ifelse(grepl(" BIOENGN", McGData$C1), "Bioengineering",
                                                                                       ifelse(grepl(" URBAN PLANNING", McGData$C1), "School of Urban Planning",
                                                    "Other"))))))))))

## check the "other"s for articles that should be kept
Other <- filter(McGData, Department == "Other")
View(Other)

##Keep only eng departments
engDataMcG <- filter(McGData, Department !="Other")

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(McGData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "McGill.csv", quote = TRUE, row.names = FALSE)
