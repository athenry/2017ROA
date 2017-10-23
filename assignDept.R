## Testing an improved procedure for assigning Departmental affiliations for each articles

AlbertaData <- tidy_data[grep("UNIV ALBERTA", tidy_data$C1), ]
engData <- AlbertaData[grep("ENGN", AlbertaData$C1), ]

deptUrl <- ("https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=0&single=true&output=csv")
depts <- read.csv(deptUrl)
abs <- as.character(depts$Abbreviation)
testData <- engData

dept_test <- sapply(testData$C1, function(x) abs[str_detect(x, abs)])
testData<-cbind(testData,plyr::ldply(dept_test,rbind)[,1])
names(testData)[6]<-"Abbreviation"
finalTestData <- merge(testData, depts, all.x = TRUE) ##keeps nonmatches and enters NA




