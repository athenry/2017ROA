##Install and load needed packages

##install.packages("splitstackshape")
##install.packages("tidyverse")
##install.packages("viridis")
##install.packages("knitr")

library(splitstackshape)
library(tidyverse)
library(stringr)
library(viridis)
library(knitr)

## read in U15 publication data from previous process, add school name column to each
Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Calgary <- read.csv("Calgary.csv", sep = ",")
Calgary$PY <- as.factor(Calgary$PY)
Calgary$School <- "Calgary"
Calgary$School <- as.factor(Calgary$School)

Dalhousie <- read.csv("Dalhousie.csv", sep = ",")
Dalhousie$PY <- as.factor(Dalhousie$PY)
Dalhousie$School <- "Dalhousie"
Dalhousie$School <- as.factor(Dalhousie$School)

Laval <- read.csv("Laval.csv", sep = ",")
Laval$PY <- as.factor(Laval$PY)
Laval$School <- "Laval"
Laval$School <- as.factor(Laval$School)

Manitoba <- read.csv("Manitoba.csv", sep = ",")
Manitoba$PY <- as.factor(Manitoba$PY)
Manitoba$School <- "Manitoba"
Manitoba$School <- as.factor(Manitoba$School)

McGill <- read.csv("McGill.csv", sep = ",")
McGill$PY <- as.factor(McGill$PY)
McGill$School <- "McGill"
McGill$School <- as.factor(McGill$School)

McMaster <- read.csv("McMaster.csv", sep = ",")
McMaster$PY <- as.factor(McMaster$PY)
McMaster$School <- "McMaster"
McMaster$School <- as.factor(McMaster$School)

Ottawa <- read.csv("Ottawa.csv", sep = ",")
Ottawa$PY <- as.factor(Ottawa$PY)
Ottawa$School <- "Ottawa"
Ottawa$School <- as.factor(Ottawa$School)

Montreal <- read.csv("Montreal.csv", sep = ",")
Montreal$PY <- as.factor(Montreal$PY)
Montreal$School <- "Montreal"
Montreal$School <- as.factor(Montreal$School)

Queens <- read.csv("Queens.csv", sep = ",")
Queens$PY <- as.factor(Queens$PY)
Queens$School <- "Queens"
Queens$School <- as.factor(Queens$School)

Saskatchewan <- read.csv("Saskatchewan.csv", sep = ",")
Saskatchewan$PY <- as.factor(Saskatchewan$PY)
Saskatchewan$School <- "Saskatchewan"
Saskatchewan$School <- as.factor(Saskatchewan$School)

Toronto <- read.csv("Toronto.csv", sep = ",")
Toronto$PY <- as.factor(Toronto$PY)
Toronto$School <- "Toronto"
Toronto$School <- as.factor(Toronto$School)

UBC <- read.csv("UBC.csv", sep = ",")
UBC$PY <- as.factor(UBC$PY)
UBC$School <- "UBC"
UBC$School <- as.factor(UBC$School)

Waterloo <- read.csv("Waterloo.csv", sep = ",")
Waterloo$PY <- as.factor(Waterloo$PY)
Waterloo$School <- "Waterloo"
Waterloo$School <- as.factor(Waterloo$School)

Western <- read.csv("Western.csv", sep = ",")
Western$PY <- as.factor(Western$PY)
Western$School <- "Western"
Western$School <- as.factor(Western$School)

## Group publication data by department 

civil <- rbind(filter(Alberta, Department == "Civil and Environemntal Engineering"), filter(Calgary, Department == "Civil Engineering"| Department == "Geomatics Engineering"), filter(Dalhousie, Department == "Civil and Resource Engineering"), filter(Laval, Department == "Civil Engineering and Water Engineering" | Department == "Geology and Geological Engineering"), filter(Manitoba, Department == "Civil Engineering"), filter (McGill, Department == "Civil Engineering and Applied Mechanics" | Department == "Mining and Materials Engineering"), filter(McMaster, Department == "Civil Engineering"), filter(Montreal, Department == "Civil Geological and Mining Engineering"), filter(Ottawa, Department == "Civil Engineering"), filter(Queens, Department =="Civil Engineering" | Department == "Robert M. Buchan Department of Mining" | Department == "Geological Engineering"), filter(Saskatchewan, Department == "Civil, Geological and Environmental Engineering"), filter(Toronto, Department == "Civil and Mineral Engineering"), filter(UBC, Department == "Civil Engineering" | Department == "Keevil Institute of Mining Engineering"), filter(Waterloo, Department == " Civil & Environmental Engineering"), filter(Western, Department =="Civil and Environmental Engineering"))

chemmat <- rbind(filter(Alberta, Department == "Chemical and Materials Engineering"), filter(Calgary, Department == "Chemical and Petroleum"), filter(Dalhousie, Department == "Process Engineering & Applied Science"), filter(Laval, Department =="Chemical Engineering" | Department == "Mineral, Metallurgical, and Materials Engineering"), filter(McGill, Department == "Chemical Engineering" | Department == "Mining and Materials Engineering"), filter(McMaster, Department == "Chemical Engineering" | Department == "Materials Science and Engineering"), filter(Montreal, Department == "Chemical Engineering"), filter(Ottawa, Department =="Chemical and Biological Engineering"), filter(Queens, Department == "Chemical Engineering" | Department == "Mechanical and Materials Engineering"), filter(Saskatchewan, Department == "Chemical and Biological Engineering"), filter(Toronto, Department == "Chemical Engineering & Applied Chemistry" | Department == "Materials Science & Engineering"), filter(UBC, Department == "Chemical and Biological Engineering" | Department == "Materials Engineering"), filter(Waterloo, Department == "Chemical Engineering"), filter(Western, Department == "Chemical and Biomedical Engineering" | Department == "Mechanical and Materials Engineering"))

mechanical <- rbind(filter(Alberta, Department == "Mechanical Engineering"), filter(Calgary, Department == "Mechanical and Manufacturing Engineering"), filter(Dalhousie, Department == "Mechanical Engineering"), filter(Laval, Department =="Mechanical Engineering"), filter(Manitoba, Department == "Mechanical Engineering"), filter(McGill, Department == "Mechanical Engineering"), filter(McMaster, Department == "Mechanical Engineering"), filter(Montreal, Department == "Mechanical Engineering"), filter(Ottawa, Department =="Mechanical Engineering"), filter(Queens, Department == "Mechanical and Materials Engineering"), filter(Saskatchewan, Department == "Mechanical Engineering"), filter(Toronto, Department == "Mechanical & Industrial Engineering"), filter(UBC, Department == "Mechanical Engineering"), filter(Waterloo, Department == "Mechanical & Mechatronics Engineering"), filter(Western, Department == "Mechanical and Materials Engineering"))

electrical <- rbind(filter(Alberta, Department == "Electrical and Computer Engineering"), filter(Calgary, Department == "Electrical and Computer Engineering"), filter(Dalhousie, Department == "Electrical and Computer Engineering"), filter(Laval, Department =="Electrical and Computer Engineering" | Department == "Computer Science and Software Engineering"), filter(Manitoba, Department == "Computer & Electrical Engineering"), filter(McGill, Department == "Electrical and Computer Engineering"), filter(McMaster, Department == "Computing and Software" | Department == "Electrical and Computer Engineering"), filter(Montreal, Department == "Electrical Engineering" | Department == "Computer Engineering"), filter(Ottawa, Department =="Electrical Engineering and Computer Science"), filter(Queens, Department == "Electrical and Computer Engineering"), filter(Saskatchewan, Department == "Electrical and Computer Engineering"), filter(Toronto, Department == "Electrical & Computer Engineering"), filter(UBC, Department == "Electrical and Computer Engineering"), filter(Waterloo, Department == "Electrical & Computer Engineering"), filter(Western, Department == "Electrical and Computer Engineering"))

biomed <- rbind(filter(Alberta, Department == "Biomedical Engineering"), filter(Dalhousie, Department == "Biomedical Engineering"), filter(Manitoba, Department == "Biosystems Engineering"), filter(McGill, Department == "Bioengineering"), filter(McMaster, Department == "School of Biomedical Engineering"), filter(Montreal, Department == "Institute of Biomedical Engineering"), filter(Ottawa, Department =="Chemical and Biological Engineering"), filter(Saskatchewan, Department == "Biomedical Engineering"), filter(Toronto, Department == "Biomaterials & Biomechanical Engineering"), filter(UBC, Department == "Biomedical Engineering"), filter(Waterloo, Department == "Centre for Bioengineering and Biotechnology"), filter(Western, Department == "Chemical and Biomedical Engineering"))

test <- civil %>%
        group_by(School) %>% 
        summarise('Times Cited' = mean(TC))

ggplot(test) + geom_bar(aes(School)) + labs(title = "Mean number of citations per article", x="School", y="Times Cited") + scale_fill_viridis(discrete = TRUE)