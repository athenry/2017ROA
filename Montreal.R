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

fullDataMtl$Department <- ifelse(grepl(" CIVIL", fullDataMtl$C1), "Civil, Geological and Mining Engineering",
                                 ifelse(grepl(" ELECT", fullDataMtl$C1), "Electrical Engineering",
                                          ifelse(grepl(" MECH", fullDataMtl$C1), "Mechanical  Engineering",
                                                 ifelse(grepl(" MECAN", fullDataMtl$C1), "Mechanical Engineering",
                                                        ifelse(grepl(" CHEM", fullDataMtl$C1), "Chemical Engineering",
                                                               ifelse(grepl(" CHIM", fullDataMtl$C1), "Chemical Engineering",
                                                                      ifelse(grepl(" MATH", fullDataMtl$C1), "Mathematics and Industrial Engineering",
                                                                             ifelse(grepl("COMP SCI", fullDataMtl$C1), "Computer Engineering",
                                                                                    ifelse(grepl(" PHYS", fullDataMtl$C1), "Engineering Physics",
                                                                                           ifelse(grepl(" BIOMED", fullDataMtl$C1), "Institute of Biomedical Engineering",
                                                                                                  ifelse(grepl(" COMP ", fullDataMtl$C1), "Computer Engineering",
                                                                                                         ifelse(grepl(" IND", fullDataMtl$C1), "Mathematics and Industrial Engineering",
                                                                                                                ifelse(grepl(" SOFTWARE ", fullDataMtl$C1), "Computer Engineering",
                                                                                                                       ifelse(grepl(" NUCL", fullDataMtl$C1), "Institute of Nuclear Engineering",
                                                                                                                              ifelse(grepl(" INFORMAT", fullDataMtl$C1), "Computer Engineering",
                                                                                                                                     ifelse(grepl(" SURFACE", fullDataMtl$C1), "Engineering Physics",
                                                                                           "Other"))))))))))))))))

## check the "other"s for articles that should be kept
Other <- filter(fullDataMtl, Department == "Other")
View(Other)

##Keep only eng departments
fullDataMtl <- filter(fullDataMtl, Department !="Other")

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(fullDataMtl, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Montreal.csv", quote = TRUE, row.names = FALSE)
