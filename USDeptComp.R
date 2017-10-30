##Install and load needed packages

##install.packages("splitstackshape")
##install.packages("tidyverse")
##install.packages("viridis")

library(splitstackshape)
library(tidyverse)
library(stringr)
library(viridis)


## read in publication data from previous process, add school name column to each
Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

CarnegieMellon <- read.csv("CarnegieMellon.csv", sep = ",")
CarnegieMellon$PY <- as.factor(CarnegieMellon$PY)
CarnegieMellon$School <- "CarnegieMellon"
CarnegieMellon$School <- as.factor(CarnegieMellon$School)

ColoradoBoulder <- read.csv("ColoradoBoulder.csv", sep = ",")
ColoradoBoulder$PY <- as.factor(ColoradoBoulder$PY)
ColoradoBoulder$School <- "ColoradoBoulder"
ColoradoBoulder$School <- as.factor(ColoradoBoulder$School)

Duke <- read.csv("Duke.csv", sep = ",")
Duke$PY <- as.factor(Duke$PY)
Duke$School <- "Duke"
Duke$School <- as.factor(Duke$School)

Florida <- read.csv("Florida.csv", sep = ",")
Florida$PY <- as.factor(Florida$PY)
Florida$School <- "Florida"
Florida$School <- as.factor(Florida$School)

GeorgiaTech <- read.csv("GeorgiaTech.csv", sep = ",")
GeorgiaTech$PY <- as.factor(GeorgiaTech$PY)
GeorgiaTech$School <- "GeorgiaTech"
GeorgiaTech$School <- as.factor(GeorgiaTech$School)

IllinoisUC <- read.csv("IllinoisUC.csv", sep = ",")
IllinoisUC$PY <- as.factor(IllinoisUC$PY)
IllinoisUC$School <- "IllinoisUC"
IllinoisUC$School <- as.factor(IllinoisUC$School)

Michigan <- read.csv("Michigan.csv", sep = ",")
Michigan$PY <- as.factor(Michigan$PY)
Michigan$School <- "Michigan"
Michigan$School <- as.factor(Michigan$School)

Minnesota <- read.csv("Minnesota.csv", sep = ",")
Minnesota$PY <- as.factor(Minnesota$PY)
Minnesota$School <- "Minnesota"
Minnesota$School <- as.factor(Minnesota$School)

UTAustin <- read.csv("UTAustin.csv", sep = ",")
UTAustin$PY <- as.factor(UTAustin$PY)
UTAustin$School <- "UTAustin"
UTAustin$School <- as.factor(UTAustin$School)

Wisconsin <- read.csv("Wisconsin.csv", sep = ",")
Wisconsin$PY <- as.factor(Wisconsin$PY)
Wisconsin$School <- "Wisconsin"
Wisconsin$School <- as.factor(Wisconsin$School)

## Group publication data by department 
biomedUS <- rbind(filter(Alberta, Department == "Biomedical Engineering"), filter(CarnegieMellon, Department == "Biomedical Engineering"), filter(ColoradoBoulder, Department == "Chemical & Biomedical Engineering"), filter(Duke, Department == "Biomedical Engineering"), filter(Florida, Department == "Biomedical Engineering"), filter (GeorgiaTech, Department == "Biomedical Engineering"), filter(IllinoisUC, Department == "Bioengineering"), filter(Michigan, Department == "Biomedical Engineering"), filter(Minnesota, Department == "Biomedical Engineering"), filter(UTAustin, Department =="Biomedical Engineering"), filter(Wisconsin, Department == "Biomedical Engineering"))

chemmatUS <- rbind(filter(Alberta, Department == "Chemical and Materials Engineering"), filter(CarnegieMellon, Department == "Chemical Engineering" | Department == "Materials Science and Engineering"), filter(ColoradoBoulder, Department == "Chemical & Biomedical Engineering"), filter(Duke, Department == "Mechanical Engineering & Materials Science"), filter(Florida, Department == "Chemical Engineering" | Department == "Materials Science & Engineering"), filter (GeorgiaTech, Department == "Chemical and Biomolecular Engineering" | Department == "Materials Science and Engineering"), filter(IllinoisUC, Department == "Chemical & Biomolecular Engineering" | Department == "Materials Science & Engineering"), filter(Michigan, Department == "Chemical Engineering" | Department == "Materials Science and Engineering"), filter(Minnesota, Department == "Chemcial Engineering and Materials Science"), filter(UTAustin, Department =="Chemical Engineering" | Department == "Materials Science and Engineering"), filter(Wisconsin, Department == "Chemical & Biological Engineering" | Department == "Materials Science & Engineering"))

civilUS <- rbind(filter(Alberta, Department == "Civil and Environmental Engineering"), filter(CarnegieMellon, Department == "Civil & Environmental Engineering"), filter(ColoradoBoulder, Department == "Civil, Envionmental & Architectural Engineering"), filter(Duke, Department == "Civil & Environmental Engineering"), filter(Florida, Department == "Engineering School of Sustainable Infrastucuture & Environment"), filter (GeorgiaTech, Department == "Civil and Environmental Engineering"), filter(IllinoisUC, Department == "Civil & Environmental Engineering"), filter(Michigan, Department == "Civil and Environmental Engineering"), filter(Minnesota, Department == "Civil, Environmental, and Geo-Engineering"), filter(UTAustin, Department =="Civil, Architectural and Environmental Engineering" | Department == "Petroleum and Geosystems Engineering"), filter(Wisconsin, Department == "Civil & Environmental Engineering"))

elecUS <- rbind(filter(Alberta, Department == "Electrical and Computer Engineering"), filter(CarnegieMellon, Department == "Electrical & Computer Engineering"), filter(ColoradoBoulder, Department == "Electrical, Computer & Engergy Engineering" | Department == "Computer Science"), filter(Duke, Department == "Electrical & Computer Engineering"), filter(Florida, Department == "Electrical & Computer Engineering"), filter (GeorgiaTech, Department == "Electrical and Computer Engineering"), filter(IllinoisUC, Department == "Electrical & Computer Engineering"), filter(Michigan, Department == "Electrical Engineering and Computer Science"), filter(Minnesota, Department == "Electrical and Computer Engineering"), filter(UTAustin, Department =="Electrical and Computer Engineering"), filter(Wisconsin, Department == "Electrical & Computer Engineering"))

mechUS <- rbind(filter(Alberta, Department == "Mechanical Engineering"), filter(CarnegieMellon, Department == "Mechanical Engineering"), filter(ColoradoBoulder, Department == "Mechanical Engineering"), filter(Duke, Department == "Mechanical Engineering & Materials Science"), filter(Florida, Department == "Mechanical & Aerospace Engineering"), filter (GeorgiaTech, Department == "Mechanical Engineering"), filter(IllinoisUC, Department == "Mechanical Science and Engineering "), filter(Michigan, Department == "Mechanical Engineering"), filter(Minnesota, Department == "Mechanical Engineering"), filter(UTAustin, Department =="Mechanical Engineering"), filter(Wisconsin, Department == "Mechanical Engineering"))

## Article counts
USbiomedCt <- count(biomedUS, PY, School)
USbiomedAY <- spread(USbiomedCt, PY, n)

USchemmatCt <- count(chemmatUS, PY, School)
USchemmatAY <- spread(USchemmatCt, PY, n)

UScivilCt <- count(civilUS, PY, School)
UScivilAY <- spread(UScivilCt, PY, n)

USelecCt <- count(elecUS, PY, School)
USelecAY <- spread(USelecCt, PY, n)

USmechCt <- count(mechUS, PY, School)
USmechAY <- spread(USmechCt, PY, n)

##kable(arrange(USbiomedAY, desc(USbiomedAY$`2016`)), caption = "Articles per year - Biomedical Engineering")

##kable(arrange(USchemmatAY, desc(USchemmatAY$`2016`)), caption = "Articles per year - Chemical and Materials Engineering")

##kable(arrange(UScivilAY, desc(UScivilAY$`2016`)), caption = "Articles per year - Civil Engineering")

##kable(arrange(USelecAY, desc(USelecAY$`2016`)), caption = "Articles per year - Electrical and Computer Engineering")

##kable(arrange(USmechAY, desc(USmechAY$`2016`)), caption = "Articles per year - Mechanical Engineering")

##Read in faculty numbers
AlbertaFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1250905174&single=true&output=csv")
names(AlbertaFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
AlbertaFac$School <- "Alberta"

CarnegieMellonFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1316833998&single=true&output=csv")
names(CarnegieMellonFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
CarnegieMellonFac$School <- "CarnegieMellon"

ColoradoBoulderFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1698168139&single=true&output=csv")
names(ColoradoBoulderFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
ColoradoBoulderFac$School <- "ColoradoBoulder"

DukeFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=31425859&single=true&output=csv")
names(DukeFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
DukeFac$School <- "Duke"

FloridaFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=924312962&single=true&output=csv")
names(FloridaFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
FloridaFac$School <- "Florida"

GeorgiaTechFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=637797385&single=true&output=csv")
names(GeorgiaTechFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
GeorgiaTechFac$School <- "GeorgiaTech"

IllinoisUCFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1942492814&single=true&output=csv")
names(IllinoisUCFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
IllinoisUCFac$School <- "IllinoisUC"

MichiganFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1225116447&single=true&output=csv")
names(MichiganFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
MichiganFac$School <- "Michigan"

MinnesotaFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=55246258&single=true&output=csv")
names(MinnesotaFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
MinnesotaFac$School <- "Minnesota"

UTAustinFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=575756157&single=true&output=csv")
names(UTAustinFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
UTAustinFac$School <- "UTAustin"

WisconsinFac <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT04KVZbmDhkrE43WSPjYgx2kKEgUdApH_z4UawFtmDKhu4sYhOaiMdZH0fdo3zo6rSSimAg7HXa7xc/pub?gid=1122732635&single=true&output=csv")
names(WisconsinFac) <- c("Department", "2012", "2013", "2014", "2015", "2016")
WisconsinFac$School <- "Wisconsin"

##Create departmental groupings
biomedFacUS <- rbind(filter(AlbertaFac, Department == "Biomedical Engineering"), filter(CarnegieMellonFac, Department == "Biomedical Engineering"), filter(ColoradoBoulderFac, Department == "Chemical and Biological Engineering"), filter(DukeFac, Department == "Biomedical Engineering"), filter(FloridaFac, Department == "J. Crayton Pruitt Family Department of Biomedical Engineering"), filter (GeorgiaTechFac, Department == "Biomedical Engineering"), filter(IllinoisUCFac, Department == "Bioengineering"), filter(MichiganFac, Department == "Biomedical Engineering"), filter(MinnesotaFac, Department == "Biomedical Engineering"), filter(UTAustinFac, Department =="Biomedical Engineering"), filter(WisconsinFac, Department == "Biomedical Engineering"))

chemmatFacUS <- rbind(filter(AlbertaFac, Department == "Chemical and Materials Engineering"), filter(CarnegieMellonFac, Department == "Chemical Engineering" | Department == "Materials Science and Engineering"), filter(ColoradoBoulderFac, Department == "Chemical and Biological Engineering" | Department == "Materials Science and Engineering"), filter(DukeFac, Department == "Mechanical Engineering and Materials Science"), filter(FloridaFac, Department == "Chemical Engineering" | Department == "Materials Science & Engineering"), filter (GeorgiaTechFac, Department == "Chemical & Biomolecular Engineering" | Department == "Materials Science & Engineering"), filter(IllinoisUCFac, Department == "Chemical and Biomolecular Engineering" | Department == "Materials Science and Engineering"), filter(MichiganFac, Department == "Chemical Engineering" | Department == "Materials Science and Engineering"), filter(MinnesotaFac, Department == "Chemical Engineering & Materials Science"), filter(UTAustinFac, Department =="Chemical Engineering"), filter(WisconsinFac, Department == "Chemical & Biological Engineering" | Department == "Materials Science & Engineering"))

civilFacUS <- rbind(filter(AlbertaFac, Department == "Civil and Environmental Engineering"), filter(CarnegieMellonFac, Department == "Civil and Environmental"), filter(ColoradoBoulderFac, Department == "Civil, Envionmental & Architectural Engineering"), filter(DukeFac, Department == "Civil and Environmental Engineering"), filter(FloridaFac, Department == "Engineering School of Sustainable Infrastructure & Environment"), filter (GeorgiaTechFac, Department == "Civil & Environmental Engineering"), filter(IllinoisUCFac, Department == "Civil and Environmental Engineering"), filter(MichiganFac, Department == "Civil and Environmental Engineering"), filter(MinnesotaFac, Department == "Civil, Environmental, and Geo-Engineering"), filter(UTAustinFac, Department =="Civil, Architectural and Environmental Engineering" | Department == "Petroleum and Geosystems Engineering"), filter(WisconsinFac, Department == "Civil and Environmental Engineering"))

elecFacUS <- rbind(filter(AlbertaFac, Department == "Electrical and Computer Engineering"), filter(CarnegieMellonFac, Department == "Electrical and Computer Engineering"), filter(ColoradoBoulderFac, Department == "Electrical, Computer and Engergy Engineering" | Department == "Computer Science"), filter(DukeFac, Department == "Electrical and Computer Engineering"), filter(FloridaFac, Department == "Electrical and Computer Engineering"), filter (GeorgiaTechFac, Department == "Electrical & Computer Engineering"), filter(IllinoisUCFac, Department == "Electrical and Computer Engineering"), filter(MichiganFac, Department == "Electrical Engineering and Computer Engineering"), filter(MinnesotaFac, Department == "Electrical and Computer Engineering"), filter(UTAustinFac, Department =="Electrical and Computer Engineering"), filter(WisconsinFac, Department == "Electrical & Computer Engineering"))

mechFacUS <- rbind(filter(AlbertaFac, Department == "Mechanical Engineering"), filter(CarnegieMellonFac, Department == "Mechanical Engineering"), filter(ColoradoBoulderFac, Department == "Mechanical Engineering"), filter(DukeFac, Department == "Mechanical Engineering and Materials Science"), filter(FloridaFac, Department == "Mechanical & Aerospace Engineering"), filter (GeorgiaTechFac, Department == "Mechanical Engineering"), filter(IllinoisUCFac, Department == "Mechanical Science and Engineering"), filter(MichiganFac, Department == "Mechanical Engineering"), filter(MinnesotaFac, Department == "Mechanical Engineering"), filter(UTAustinFac, Department =="Mechanical Engineering"), filter(WisconsinFac, Department == "Mechanical Engineering"))

USchemmatFacCt <- gather(chemmatFacUS, key = PY, value = Fac, -Department, -School) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac))

USCivilFacCt <- gather(civilFacUS, key = PY, value = Fac, -Department, -School) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac))

USelecFacCt <- gather(elecFacUS, key = PY, value = Fac, -Department, -School) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac))

USmechFacCt <- gather(mechFacUS, key = PY, value = Fac, -Department, -School) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac))

USchemmatFac <- spread(USchemmatFacCt, PY, Fac)
UScivilFac <- spread(USchemmatFacCt, PY, Fac)
USelecFac <- spread(USelecFacCt, PY, Fac)
USmechFac <- spread(USmechFacCt, PY, Fac)

## Divide number of articles by number of faculty
USchemmatAF <- cbind(USchemmatAY[1], round(USchemmatAY[-1]/USchemmatFac[-1],1))
UScivilAF <- cbind(UScivilAY[1], round(UScivilAY[-1]/UScivilFac[-1], 1))
USelecAF <- cbind(USelecAY[1], round(USelecAY[-1]/USelecFac[-1], 1))
USmechAF <- cbind(USmechAY[1], round(USmechAY[-1]/USmechFac[-1], 1))

kable(arrange(USchemmatAF, desc(USchemmatAF$`2016`)), digits = 2, caption = "Articles per faculty member per year - Chemical Engineering")

kable(arrange(UScivilAF, desc(UScivilAF$`2016`)), digits = 2, caption = "Articles per faculty member per year - Civil Engineering")

kable(arrange(USelecAF, desc(USelecAF$`2016`)), digits = 2, caption = "Articles per faculty member per year - Electrical Engineering")

kable(arrange(USmechAF, desc(USmechAF$`2016`)), digits = 2, caption = "Articles per faculty member per year - Mechanical Engineering")