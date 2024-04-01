# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
data = read_sav("../data/GSS2016.sav") #looks like either the foreign library or haven are the main ways to import SPSS files, but for haven all the values were converted to numbers, which I thought was probably better for machine learning.
gss_tbl <- data %>%
  mutate_all(~ifelse(.==0, NA, .)) %>%  # turns any 0 into NA
  drop_na(MOSTHRS) %>% #get rid rows with na for MOSTHRS column
  rename('work hours'= MOSTHRS) %>% #rename to 'work hours'
  select (-c(HRS1, HRS2)) %>% #get rid of HRS1 and HRS1 two columns
  select(where(~mean(is.na(.)) < 0.75)) %>%  #thought the helper 'where' might be the easiest here. Hope I did it right!
  sapply(as.numeric) #needed this to get my models to run
  
# Visualization
ggplot (gss_tbl, aes(`work hours`))+
  geom_histogram() +
  ylab("Frequency") +
  xlab("Work Hours")

# Analysis
set.seed(54321) #we always used this number for some reason in my machine learning class

rows <- sample(nrow(gss_tbl)) # shuffle row indices
shuffled_data <-  gss_tbl[rows,] #put data in random order
split <- round(nrow(gss_tbl)*0.75) #determine row to split on
train_gss_tbl <- shuffled_data[1:split,] #create train
test_gss_tbl <- shuffled_data[(split+1):nrow(gss_tbl),] #create test

myControl <- trainControl( #trainControl settings for all models
  method = "cv", #cross-validation method
  number = 10, #define number of folds,
  search = "grid", #use a grid search for hyperparameters
  verboseIter = TRUE #show which processes are happening
)

model_lm <- train( 
  `work hours` ~ ., #fit model with outcome of work hours with all other variables as predictors. I feel like whenver I do loops, I mess something up. So for simplicity's sake (at least simplicity for me), I copied and pasted my code for each model. I know this is not best-practice.
  train_gss_tbl, #fit model to training set
  method = "lm", # OLS regression
  preProcess = c("medianImpute", "nzv"), #impute median scores and eliminate near zero values - the model for elastic net was failing and based on the data camp lessons, it appears it might be due to variables with near zero variance. I figured all the models needed to be trained on the same data so I eliminated them for all models in pre-processing.
  na.action = na.pass, #don't stop at na values
  trControl = myControl, #see myControl settings above
  tuneGrid = expand.grid( #only hyperparameter for lm is intercept
    intercept = c(T, F)
  )
)

model_enet <- train( 
  `work hours` ~ ., #fit model with outcome of work hours with all other variables as predictors (see model_lm for other choices)
  train_gss_tbl,
  method = "enet", #elastic net
  preProcess = c("medianImpute", "nzv"), 
  na.action = na.pass,
  trControl = myControl,
  tuneGrid = expand.grid(  #used chatGPT to determine hyperparameter ranges
    fraction = c(0,1),
    lambda = seq(0.0001, 0.001, length = 10)
  )
)

model_rf <- train( 
  `work hours` ~ .,  #fit model with outcome of work hours with all other variables as predictors (see model_lm for other choices)
  train_gss_tbl,
  method = "ranger", #random forest
  preProcess = c("medianImpute", "nzv"),
  na.action = na.pass,
  trControl = myControl,
  tuneGrid = expand.grid(
    mtry = c(2, 3, 4, 5, 10, 20, 50, 100),   #used chatGPT to determine hyperparameter ranges
    splitrule = c("gini", "extratrees", "variance"),
    min.node.size = c(1, 5, 10)
  )
)

model_xgb <- train( 
  `work hours` ~ .,  #fit model with outcome of work hours with all other variables as predictors (see model_lm for other choices)
  train_gss_tbl,
  method = "xgbLinear", #eXtreme gradient boost
  preProcess = c("medianImpute", "nzv"),
  na.action = na.pass,
  trControl = myControl,
  tuneGrid = expand.grid(  #used chatGPT to determine hyperparameter ranges
    nrounds = seq(50, 150, by = 50),  
    lambda = c(0, 0.1, 0.01),  
    alpha = c(0, 0.1, 0.01),    
    eta = seq(0.01, 0.2, by = 0.05)    
  )
)

p_lm <- predict(model_lm, #apply lm model to holdout/test set
                test_gss_tbl, #test data
                na.action = na.pass) #code didn't work without this
p_lm_results <- cor(p_lm, as.data.frame(test_gss_tbl)$`work hours`)^2 #calculate r2 for holdout set

p_enet <- predict(model_enet, #apply elastic net model to holdout (other coding choices same as above)
                test_gss_tbl,
                na.action = na.pass)
p_enet_results <- cor(p_lm, as.data.frame(test_gss_tbl)$`work hours`)^2

p_rf <- predict(model_rf, #apply random forest model to holdout (other coding choices same as above)
                test_gss_tbl,
                na.action = na.pass)
p_rf_results <- cor(p_rf, as.data.frame(test_gss_tbl)$`work hours`)^2

p_xgb <- predict(model_xgb, #apply eXtreme Gradient Boosting model to holdout (other coding choices same as above)
                  test_gss_tbl,
                  na.action = na.pass)
p_xgb_results <- cor(p_xgb, as.data.frame(test_gss_tbl)$`work hours`)^2


# Publication
lm_train_results <- str_replace(formatC(max(model_lm$results$Rsquared, na.rm = TRUE), format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero; used max r2
lm_test_results <- str_replace(formatC(p_lm_results, format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero
enet_train_results <- str_replace(formatC(max(model_enet$results$Rsquared, na.rm = TRUE), format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero; used max r2
enet_test_results <- str_replace(formatC(p_enet_results, format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero
rf_train_results <- str_replace(formatC(max(model_rf$results$Rsquared, na.rm = TRUE), format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero; used max r2
rf_test_results <- str_replace(formatC(p_rf_results, format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero
xgb_train_results <- str_replace(formatC(max(model_xgb$results$Rsquared, na.rm = TRUE), format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero; used max r2
xgb_test_results <- str_replace(formatC(p_xgb_results, format = "f", digits = 2), "^0", "") #2 decimal places and no leading zero

table1_tbl <- tibble( #pull all reformatted numbers into a tibble)
  algo = c("OLS Regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  cv_rsq = c(lm_train_results, enet_train_results, rf_train_results, xgb_train_results),
  ho_rsq = c(lm_test_results, enet_test_results, rf_test_results, xgb_test_results)
)

# 1. How did your results change between models? Why do you think this happened, specifically?
#    Generally my results improved with each model, though my random forest model actually out-performed the eXtreme Gradient Boosting model on both the CV and holdout sets. From watching the outputs, it appeared that each model got more and more complex which resulted in it taking them more time to run, but also appears to have led to better results.
# 2. How did you results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
#    For all models except for xgb, the training set's r-squared is higher than the holdout set's r-squared. This would be the pattern that would be expected because through tuning and training, the cross-validated set should be better explained by the data. On the contrary, since the model has never seen the holdout data, this would result in a lower r-squared because it hasn't been trained on that data. I am not sure why for xgb the r-squared is higher for the holdout versus the training set. This could be an error on my part or could just mean this algorithm is good at not overfitting.
# 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph.
#    Random forest appears to be the best choice for this dataset for me. There are a number of factors that would weigh into this decision, but most obviously it has a higher r-squared for both the train and test data. Additionally, it went a lot faster than the xgb model, though that could be due to the number of hyperparameters I used to tune. I also know that random forest is a commonly used algorithm, and I had never heard of xgb before, though admittedly I am not in the machine learning field. 
