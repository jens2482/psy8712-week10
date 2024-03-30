# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(foreign)
library(caret)

# Data Import and Cleaning
data = read.spss("../data/GSS2016.sav", to.data.frame=TRUE) #looks like either the foreign library or haven are the main ways to import SPSS files, but for haven all the values were converted to numbers, which I thought was a problem...so I went with foreign.
gss_tbl <- data %>%
  drop_na(MOSTHRS) %>% #get rid rows with na for MOSTHRS column
  mutate(MOSTHRS = as.numeric(MOSTHRS)) %>% #needed to change it to numeric for visualization. Was having a hard time with the column title with a space in it so I just did it before I renamed it.
  rename('work hours'= MOSTHRS) %>% #rename to 'work hours'
  select (-c(HRS1, HRS2)) %>% #get rid of HRS1 and HRS1 two columns
  select(where(~mean(is.na(.)) < 0.75)) #thought the helper 'where' might be the easiest here. Hope I did it right!

# Visualization
ggplot (gss_tbl, aes(`work hours`))+
  stat_density (geom = "line") +
  ylab("Density") +
  xlab("Work Hours")

# Analysis
set.seed(54321) #we always used this number for some reason in my machine learning class

rows <- sample(nrow(gss_tbl)) #shuffle row indices (from data camp)
gss_tbl_random <- gss_tbl [rows, ] #randomly order data (from data camp)
split <- round(nrow(gss_tbl_random) * 0.75) #find row to split on (from data camp)
train_gss_tbl <- gss_tbl_random [1:split, ] #create train (from data camp)
test_gss_tbl <- gss_tbl_random[(split +1):nrow(gss_tbl_random), ] #create test (from data camp)

model_lm <- train(
  `work hours` ~ ., 
  train_gss_tbl,
  method = "lm",
  preProcess = "medianImpute",
  na.action = na.pass,
  trControl = trainControl(
    method = "cv", #cross-validation method
    number = 10, #define number of folds
    verboseIter = TRUE
  )
)



# Publication