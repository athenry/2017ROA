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
civil <- rbind(filter(Alberta, Department == "Civil and Environmental Engineering"), filter(Calgary, Department == "Civil Engineering"| Department == "Geomatics Engineering"), filter(Dalhousie, Department == "Civil and Resource Engineering"), filter(Laval, Department == "Civil Engineering and Water Engineering" | Department == "Geology and Geological Engineering"), filter(Manitoba, Department == "Civil Engineering"), filter (McGill, Department == "Civil Engineering and Applied Mechanics" | Department == "Mining and Materials Engineering"), filter(McMaster, Department == "Civil Engineering"), filter(Montreal, Department == "Civil Geological and Mining Engineering"), filter(Ottawa, Department == "Civil Engineering"), filter(Queens, Department =="Civil Engineering" | Department == "Robert M. Buchan Department of Mining" | Department == "Geological Engineering"), filter(Saskatchewan, Department == "Civil, Geological and Environmental Engineering"), filter(Toronto, Department == "Civil and Mineral Engineering"), filter(UBC, Department == "Civil Engineering" | Department == "Keevil Institute of Mining Engineering"), filter(Waterloo, Department == " Civil & Environmental Engineering"), filter(Western, Department =="Civil and Environmental Engineering"))

chemmat <- rbind(filter(Alberta, Department == "Chemical and Materials Engineering"), filter(Calgary, Department == "Chemical and Petroleum Engineering"), filter(Dalhousie, Department == "Process Engineering & Applied Science"), filter(Laval, Department =="Chemical Engineering" | Department == "Mineral, Metallurgical, and Materials Engineering"), filter(McGill, Department == "Chemical Engineering" | Department == "Mining and Materials Engineering"), filter(McMaster, Department == "Chemical Engineering" | Department == "Materials Science and Engineering"), filter(Montreal, Department == "Chemical Engineering"), filter(Ottawa, Department =="Chemical and Biological Engineering"), filter(Queens, Department == "Chemical Engineering" | Department == "Mechanical and Materials Engineering"), filter(Saskatchewan, Department == "Chemical and Biological Engineering"), filter(Toronto, Department == "Chemical Engineering & Applied Chemistry" | Department == "Materials Science & Engineering"), filter(UBC, Department == "Chemical and Biological Engineering" | Department == "Materials Engineering"), filter(Waterloo, Department == "Chemical Engineering"), filter(Western, Department == "Chemical and Biomedical Engineering" | Department == "Mechanical and Materials Engineering"))

mechanical <- rbind(filter(Alberta, Department == "Mechanical Engineering"), filter(Calgary, Department == "Mechanical and Manufacturing Engineering"), filter(Dalhousie, Department == "Mechanical Engineering"), filter(Laval, Department =="Mechanical Engineering"), filter(Manitoba, Department == "Mechanical Engineering"), filter(McGill, Department == "Mechanical Engineering"), filter(McMaster, Department == "Mechanical Engineering"), filter(Montreal, Department == "Mechanical Engineering"), filter(Ottawa, Department =="Mechanical Engineering"), filter(Queens, Department == "Mechanical and Materials Engineering"), filter(Saskatchewan, Department == "Mechanical Engineering"), filter(Toronto, Department == "Mechanical & Industrial Engineering"), filter(UBC, Department == "Mechanical Engineering"), filter(Waterloo, Department == "Mechanical & Mechatronics Engineering"), filter(Western, Department == "Mechanical and Materials Engineering"))

electrical <- rbind(filter(Alberta, Department == "Electrical and Computer Engineering"), filter(Calgary, Department == "Electrical and Computer Engineering"), filter(Dalhousie, Department == "Electrical and Computer Engineering"), filter(Laval, Department =="Electrical and Computer Engineering" | Department == "Computer Science and Software Engineering"), filter(Manitoba, Department == "Computer & Electrical Engineering"), filter(McGill, Department == "Electrical and Computer Engineering"), filter(McMaster, Department == "Computing and Software" | Department == "Electrical and Computer Engineering"), filter(Montreal, Department == "Electrical Engineering" | Department == "Computer Engineering"), filter(Ottawa, Department =="Electrical Engineering and Computer Science"), filter(Queens, Department == "Electrical and Computer Engineering"), filter(Saskatchewan, Department == "Electrical and Computer Engineering"), filter(Toronto, Department == "Electrical & Computer Engineering"), filter(UBC, Department == "Electrical and Computer Engineering"), filter(Waterloo, Department == "Electrical & Computer Engineering"), filter(Western, Department == "Electrical and Computer Engineering"))

biomed <- rbind(filter(Alberta, Department == "Biomedical Engineering"), filter(Calgary, Department == "Biomedical Engineering Grad Program"), filter(Dalhousie, Department == "Biomedical Engineering"), filter(Manitoba, Department == "Biosystems Engineering"), filter(McGill, Department == "Bioengineering"), filter(McMaster, Department == "School of Biomedical Engineering"), filter(Montreal, Department == "Institute of Biomedical Engineering"), filter(Ottawa, Department =="Chemical and Biological Engineering"), filter(Saskatchewan, Department == "Biomedical Engineering"), filter(Toronto, Department == "Biomaterials & Biomechanical Engineering"), filter(UBC, Department == "Biomedical Engineering"), filter(Waterloo, Department == "Centre for Bioengineering and Biotechnology"), filter(Western, Department == "Chemical and Biomedical Engineering"))

## Tables for Times Cited
biomedTC <- biomed %>%
        group_by(School) %>% 
        summarise('Times Cited' = mean(TC))

chemmatTC <- chemmat %>%
        group_by(School) %>% 
        summarise('Times Cited' = mean(TC))
        
civTC <- civil %>%
        group_by(School) %>% 
        summarise('Times Cited' = mean(TC))

elecTC <- electrical %>%
        group_by(School) %>% 
        summarise('Times Cited' = mean(TC))

mechTC <- mechanical %>%
        group_by(School) %>% 
        summarise('Times Cited' = mean(TC))

arrange(biomedTC, desc(biomedTC$'Times Cited'))
arrange(chemmatTC, desc(chemmatTC$'Times Cited'))
arrange(civTC, desc(civTC$'Times Cited'))
arrange(elecTC, desc(elecTC$'Times Cited'))
arrange(mechTC, desc(mechTC$'Times Cited'))

## Articles per year per school

biomedCt <- count(biomed, PY, School)
biomedAY <- spread(biomedCt, PY, n)

chemmatCt <- count(chemmat, PY, School)
chemmatAY <- spread(chemmatCt, PY, n)

civilCt <- count(civil, PY, School)
civilAY <- spread(civilCt, PY, n)

elecCt <- count(electrical, PY, School)
elecAY <- spread(elecCt, PY, n)

mechCt <- count(mechanical, PY, School)
mechAY <- spread(mechCt, PY, n)

## Articles per year per faculty
AlbertaFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1250905174&single=true&output=csv")
names(AlbertaFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
AlbertaFac$School <- "Alberta"

CalgaryFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1767152235&single=true&output=csv")
names(CalgaryFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
CalgaryFac$School <- "Calgary"

McMasterFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=377980857&single=true&output=csv")
names(McMasterFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
McMasterFac$School <- "McMaster"

MontrealFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1362062123&single=true&output=csv")
names(MontrealFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
MontrealFac$School <- "Montreal"

OttawaFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=391352894&single=true&output=csv")
names(OttawaFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
OttawaFac$School <- "Ottawa"

TorontoFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=636662368&single=true&output=csv")
names(TorontoFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
TorontoFac$School <- "Toronto"

WaterlooFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1597376299&single=true&output=csv")
names(WaterlooFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
WaterlooFac$School <- "Waterloo"

WesternFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=757635134&single=true&output=csv")
names(WesternFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
WesternFac$School <- "Western" 

chemmatFac <- rbind(filter(AlbertaFac, Department == "Chemical and Materials Engineering"), filter(CalgaryFac, Department == "Chemical and Materials Engineering"), filter(McMasterFac, Department == "Chemical Engineering" | Department == "Materials Science and Engineering"), filter(MontrealFac, Department == "Chemical engineering"), filter(OttawaFac, Department == "Chemical & Biological"), filter(TorontoFac, Department == "Department of Chemical Engineering and Applied Chemistry" | Department == "Department of Materials Science and Engineering"), filter(WaterlooFac, Department == "Chemical Engineering"), filter(WesternFac, Department == "Chemical and Biochemical Engineering"))

civilFac <- rbind(filter(AlbertaFac, Department == "Civil Engineering"), filter(CalgaryFac, Department == "Civil Engineering" | Department == "Geomatics Engineering"), filter(McMasterFac, Department == "Civil Engineering"), filter(MontrealFac, Department == "Civil, geological and mining engineering"), filter(OttawaFac, Department == "Civil" | Department == "Environmental Engineering"), filter(TorontoFac, Department == "Department of Civil Engineering" | Department == "Lassonde Institute of Mining"), filter(Waterloo, Department == "Civil and Environmental Engineering"), filter(WesternFac, Department == "Civil & Environmental Engineering"))

elecFac <- rbind(filter(AlbertaFac, Department == "Electrical and Computer Engineering"), filter(CalgaryFac, Department == "Electrical & Computer Engineering"), filter(McMasterFac, Department == "Electrical and Computer Engineering" | Department == "Computing and Software"), filter(MontrealFac, Department == "Electrical engineering" | Department == "Computer and Software Engineering"), filter(OttawaFac, Department == "Electrical Engineering and Computer Science"), filter(TorontoFac, Department == "Edward S. Rogers Sr. Department of Electrical and Computer Engineering"), filter(WaterlooFac, Department == "Electrical and Computer Engineering"), filter(WesternFac, Department == "Electrical and Computer Engineering"))

mechFac <- rbind(filter(AlbertaFac, Department == "Mechanical Engineering"), filter(CalgaryFac, Department == "Mechanical & Manufacturing Engineering"), filter(McMasterFac, Department == "Mechanical Engineering"), filter(MontrealFac, Department == "Mechanical engineering"), filter(OttawaFac, Department == "Mechanical"), filter(TorontoFac, Department == "Department of Mechanical and Industrial Engineering"), filter(Waterloo, Department == "Mechanical and Mechatronics Engineering"), filter(WesternFac, Department == "Mechanical and Materials Engineering"))

chemmatFacCt <- gather(chemmatFac, key = PY, value = Fac, -Department, -School) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac))

CivilFacCt <- gather(civilFac, key = PY, value = Fac, -Department, -School) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac))

elecFacCt <- gather(elecFac, key = PY, value = Fac, -Department, -School) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac))

mechFacCt <- gather(mechFac, key = PY, value = Fac, -Department, -School) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac))

chemmatFac <- spread(chemmatFacCt, PY, Fac)
civilFac <- spread(chemmatFacCt, PY, Fac)
elecFac <- spread(elecFacCt, PY, Fac)
mechFac <- spread(mechFacCt, PY, Fac)

test <- merge(chemmatAY, chemmatFac, by="School")
