# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
data2 = read_sav("../data/GSS2016.sav") #looks like either the foreign library or haven are the main ways to import SPSS files, but for haven all the values were converted to numbers, which I thought was probably better for machine learning.
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


myControl <- trainControl( #trainControl settings for all models
  method = "cv", #cross-validation method
  number = 10, #define number of folds,
  search = "grid",
  verboseIter = TRUE
)

model_lm <- train( 
  `work hours` ~ ., #fit model with outcome of work hours with all other variables as predictors
  train_gss_tbl,
  method = "lm", # OLS regression
  preProcess = "medianImpute", #impute median scores
  na.action = na.pass,
  trControl = myControl, #see myControl settings above
  tuneGrid = expand.grid( #only hyperparameter for lm is intercept
    intercept = c(T, F)
  )
)

model_enet <- train( 
  `work hours` ~ ., #fit model with outcome of work hours with all other variables as predictors
  train_gss_tbl,
  method = "enet", #elastic net
  preProcess = "medianImpute",
  na.action = na.pass,
  trControl = myControl,
  tuneGrid = expand.grid(  #used chatGPT to determine hyperparameter ranges
    fraction = seq(0, 1, by = 0.1),
    lambda = seq(0.0001, 0.1, length = 10) 
  )
)

model_rf <- train( 
  `work hours` ~ ., 
  train_gss_tbl,
  method = "ranger", #random forest
  preProcess = "medianImpute",
  na.action = na.pass,
  trControl = myControl,
  tuneGrid = expand.grid( 
    mtry = seq(1, sqrt(ncol(train_gss_tbl)), by = 1),   #used chatGPT to determine hyperparameter ranges
    splitrule = c("gini", "extratrees", "variance"),
    min.node.size = c(1, 5, 10)
  )
)

model_xgb <- train( 
  `work hours` ~ ., 
  train_gss_tbl,
  method = "xgbLinear", #eXtreme gradient boost
  preProcess = "medianImpute",
  na.action = na.pass,
  trControl = myControl,
  tuneGrid = expand.grid(  #used chatGPT to determine hyperparameter ranges
    nrounds = seq(50, 200, by = 25),   
    lambda = c(0, 0.1, 0.01),   
    alpha = c(0, 0.1, 0.01),    
    eta = seq(0.01, 0.3, by = 0.05)    
  )
)

# Publication