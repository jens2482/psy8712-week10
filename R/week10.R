# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(foreign)
#library(haven)

# Data Import and Cleaning
data = read.spss("../data/GSS2016.sav", to.data.frame=TRUE) #looks like either the foreign library or haven are the main ways to import SPSS files, but for haven all the values were converted to numbers, so I went with foreign.
#data2 = read_sav("../data/GSS2016.sav") #looks like either the foreign library or haven are the main ways to import SPSS files, but for haven all the values were converted to numbers, so I went with foreign.
gss_tbl <- data %>%
  drop_na(MOSTHRS) %>%
  rename('work hours'= MOSTHRS) %>%
  select (-c(HRS1, HRS2)) %>%
  select(where(~mean(is.na(.)) < 0.75))

# Visualization


# Analysis


# Publication