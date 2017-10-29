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

## read in publication data from previous process, add school name column to each
Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

Alberta <- read.csv("Alberta.csv", sep = ",")
Alberta$PY <- as.factor(Alberta$PY)
Alberta$School <- "Alberta"
Alberta$School <- as.factor(Alberta$School)

## Group publication data by department 
civil <- rbind(filter(Alberta, Department == "Civil and Environmental Engineering"), filter(Calgary, Department == "Civil Engineering"| Department == "Geomatics Engineering"), filter(Dalhousie, Department == "Civil and Resource Engineering"), filter(Laval, Department == "Civil Engineering and Water Engineering" | Department == "Geology and Geological Engineering"), filter(Manitoba, Department == "Civil Engineering"), filter (McGill, Department == "Civil Engineering and Applied Mechanics" | Department == "Mining and Materials Engineering"), filter(McMaster, Department == "Civil Engineering"), filter(Montreal, Department == "Civil Geological and Mining Engineering"), filter(Ottawa, Department == "Civil Engineering"), filter(Queens, Department =="Civil Engineering" | Department == "Robert M. Buchan Department of Mining" | Department == "Geological Engineering"), filter(Saskatchewan, Department == "Civil, Geological and Environmental Engineering"), filter(Toronto, Department == "Civil and Mineral Engineering"), filter(UBC, Department == "Civil Engineering" | Department == "Keevil Institute of Mining Engineering"), filter(Waterloo, Department == " Civil & Environmental Engineering"), filter(Western, Department =="Civil and Environmental Engineering"))