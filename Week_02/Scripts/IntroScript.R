### This is my first script, I am learning how to import data ###
### Created by Fabiola Gonzalez Zamora ###
### Created on 2026-09-06 ###
##########################################

### Load Libraries ###
library(tidyverse)
library(here)

### Read in my data ###
weightdata <- read.csv(here("Week_02", "Data", "weightdatacopy.csv"))

### Data analysis ###
head(weightdata)
tail(weightdata)
view(weightdata)

