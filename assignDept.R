## Testing an improved procedure for assigning Departmental affiliations for each articles
## As-is DOES NOT WORK

deptUrl <- ("https://docs.google.com/spreadsheets/d/e/2PACX-1vTMpIJn2N9pV13zRhYKRdOOAUfvHhKF6dqUzMWhnk3_eaBgPD8XT6UJBuAXfyoWfA0qfvaO4LyQpfJA/pub?gid=0&single=true&output=csv")
depts <- read.csv(deptUrl)
abs <- as.character(depts$Abbreviation)
testData <- tidy_data[grep("UNIV ALBERTA", tidy_data$C1), ]
testData$Department <- ifelse(grepl(paste(abs, collapse = "|"), testData$C1), abs, "Other")
testData$Department <- str_replace(as.character(testData$Department), as.character(depts$Abbreviation), as.character(depts$Name))

