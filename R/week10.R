# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
data = read.spss("../data/GSS2016.sav", to.data.frame=TRUE)
#data2 = read_sav("../data/GSS2016.sav", .name_repair = "minimal") #looks like either the foreign library or haven are the main ways to import SPSS files, but for haven all the values were converted to numbers, which I thought was probably better for machine learning.
gss_tbl <- data %>%
  drop_na(MOSTHRS) %>% #get rid rows with na for MOSTHRS column
  mutate(MOSTHRS = as.numeric(MOSTHRS)) %>% #needed to change it to numeric for visualization. Was having a hard time with the column title with a space in it so I just did it before I renamed it.
  rename('work hours'= MOSTHRS) %>% #rename to 'work hours'
  select (-c(HRS1, HRS2)) %>% #get rid of HRS1 and HRS1 two columns
  select(where(~mean(is.na(.)) < 0.75))  #thought the helper 'where' might be the easiest here. Hope I did it right!
  
# Visualization
ggplot (gss_tbl, aes(`work hours`))+
  stat_density (geom = "line") +
  ylab("Density") +
  xlab("Work Hours")

# Analysis
set.seed(54321) #we always used this number for some reason in my machine learning class

inTraining <- createDataPartition(gss_tbl$`work hours`, p = 0.75, list = FALSE) #set split to be 75-25
train_gss_tbl <- gss_tbl[ inTraining,] #create training set
test_gss_tbl  <- gss_tbl[-inTraining,] #create test set

myControl <- trainControl( #trainControl settings for all models
  method = "cv", #cross-validation method
  number = 10, #define number of folds,
  search = "grid", #use a grid search for hyperparameters
  verboseIter = TRUE #show which processes are happening
)

model_lm <- train( 
  `work hours` ~ ., #fit model with outcome of work hours with all other variables as predictors
  train_gss_tbl, #fit model to training set
  method = "lm", # OLS regression
  preProcess = c("medianImpute", "nzv"), #impute median scores and eliminate near zero values #the model for elastic net was failing and based on the data camp lessons, it appears it might be due to variables with zero sd. I figured all the models needed to be trained on the same data so I eliminated them for all models in pre-processing.
  na.action = na.pass, #don't stop at na values
  trControl = myControl, #see myControl settings above
  tuneLength = 10 #default is 3 so I figured 10 would be sufficient
)

model_enet <- train( 
  `work hours` ~ ., #fit model with outcome of work hours with all other variables as predictors (see model_lm for other choices)
  train_gss_tbl,
  method = "enet", #elastic net
  preProcess = c("medianImpute", "nzv"), 
  na.action = na.pass,
  trControl = myControl,
  tuneLength = 10
)

model_rf <- train( 
  `work hours` ~ .,  #fit model with outcome of work hours with all other variables as predictors (see model_lm for other choices)
  train_gss_tbl,
  method = "ranger", #random forest
  preProcess = c("medianImpute", "nzv"),
  na.action = na.pass,
  trControl = myControl,
  tuneLength = 10
)

model_xgb <- train( 
  `work hours` ~ .,  #fit model with outcome of work hours with all other variables as predictors (see model_lm for other choices)
  train_gss_tbl,
  method = "xgbLinear", #eXtreme gradient boost
  preProcess = c("medianImpute", "nzv"),
  na.action = na.pass,
  trControl = myControl,
  tuneLength = 10
)

p_lm <- predict(model_lm,
                test_gss_tbl,
                type = "raw")

# Publication
table1_tbl <- tibble(
  algo = c("OLS Regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  #cv_rsq = c(algo1_cv_rsq, algo2_cv_rsq, algo3_cv_rsq, algo4_cv_rsq),
  #ho_rsq = c(algo1_ho_rsq, algo2_ho_rsq, algo3_ho_rsq, algo4_ho_rsq)
)
