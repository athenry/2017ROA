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

engDataDal$Department <- ifelse(grepl(" CIVIL ", engDataDal$C1), "Civil and Resource Engineering",
                                       ifelse(grepl(" ELECT ", engDataDal$C1), "Electrical and Computer Engineering",
                                              ifelse(grepl(" MECH", engDataDal$C1), "Mechanical  Engineering",
                                                     ifelse(grepl(" IND ", engDataDal$C1), "Industrial Engineering",
                                                            ifelse(grepl(" BIOMED ", engDataDal$C1), "Biomedical Engineering",
                                                                   ifelse(grepl(" PROC ", engDataDal$C1), "Process Engineering and Applied Science",
                                                                          ifelse(grepl("ENGN MATH", engDataDal$C1), "Engineering Mathematics and Internetworking",
                                                                                 ifelse(grepl(" CHEM", engDataDal$C1), "Process Engineering and Applied Science",
                                                                                        ifelse(grepl("INTERNETWORKING", engDataDal$C1), "Engineering Mathematics and Internetworking",
                                                                                               ifelse(grepl("ENVIRONM", engDataDal$C1), "Process Engineering and Applied Science",
                                                                                                      ifelse(grepl(" TRURO", engDataDal$C1), "Truro Campus",
                                                                          "Other")))))))))))

## check the "other"s for articles that should be kept
Other <- filter(engDataDal, Department == "Other")
View(Other)

##Keep only eng departments
engDataDal <- filter(engDataDal, Department !="Other", Department !="Truro Campus")

##Remove departmental duplicates (leave institutional duplicates)
engDataDD <- unique(select(engDataDal, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Dalhousie.csv", quote = TRUE, row.names = FALSE)
