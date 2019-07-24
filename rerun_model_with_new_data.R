##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 23 JULY 2019
# 
# Contact the author with any questions at:
#   isaiah.l.valdez@gmail.com

##### PACKAGES #######################
library(h2o)
library(readr)
library(data.table)
library(magrittr)
library(dplyr)
library(stringr)
Sys.setlocale(locale = "UTF-8")
setwd(dir = "/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")

data = read_csv(file = "rerun_model_actual_data_2019.csv") %>% data.table(stringsAsFactors = T)
data[, tvo := as.factor(as.character(tvo))]
data[, tvo_pres := as.factor(as.character(tvo_pres))]
data[, log_perc_for := log1p(perc_for)]

# need this to split the data later on
data$unique_id = 1:nrow(data)
which(!unique(data$unique_id)) 

## H2O is an R package
library(h2o) 
library(data.table)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "10G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

## convert to H20 object but save a local copy
df = data[which(data$year == 2019),] #there are dupes if a person was previously an MP, oops
predict_df = df # save a copy
df = as.h2o(df)
names(df)
h2o.isfactor(df)

## First, we will create three splits for train/test/valid independent data sets.
## We will train a data set on one set and use the others to test the validity
##  of model by ensuring that it can predict accurately on data the model has not
##  been shown.
## The second set will be used for validation most of the time. The third set will
##  be withheld until the end, to ensure that our validation accuracy is consistent
##  with data we have never seen during the iterative process. 
splits <- h2o.splitFrame(
  df,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234)    ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex") 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex
rm(splits)
rm(data)


## ideas...
# take variable "party_ideology" and "region_ideology", and use the variable
# matching_ideology to show if they match or not?

## take a look at the first few rows of the data set
train[1:5,]   ## rows 1-5, all columns
names(train)
feature_names = c("tvo", "oblast", "proposed", "gender", "age",
                  "job", "party", "pres_candidate", "pres_perc_for",
                  "average_age", "pop_change", "region_ideology", "previous",
                  "ever", "funding", "power_status", "party_ideology",
                  "prop_election_forecast")
feature_names
h2o.isfactor(train[,feature_names])

##### XGBOOST PREDICTING WINNER (BINARY CLASS ) ####################################
x = feature_names
y = 8 #winner column
names(train[,x])
names(train[,y])

# Same model that was used in the predictions
xgb1 <- h2o.xgboost(            ## h2o.randomForest function
  model_id = "xgb1",
  ntrees = 150,
  eta = 0.1,
  min_child_weight = 9,        ## very imbalanced
  max_depth = 11,
  gamma = 0,                   ## should be tuned, depends on logloss function
  max_delta_step = 0,          ## defaults to 0, can be helpful in unbalanced models
  subsample = 0.8,             ## defaults to 1, lowering prevents overfit
  colsample_bytree = 0.8,      ## fraction of columns to samples for each tree
  stopping_rounds = 50,
  stopping_metric = "logloss",
  verbose = T,
  seed = 1234,                  ##
  training_frame = train,      ## the H2O frame for training
  validation_frame = valid,    ## the H2O frame for validation (not required)
  x = x,                       ## the predictor columns, by column index
  y = y  #winner              ## the target index (what we are predicting) "log_perc_win"
)

var_imp = h2o.varimp(xgb1)
h2o.varimp(xgb1)
h2o.performance(xgb1)

prediction_150 = h2o.predict(xgb1, newdata = df) %>% as.data.frame() #auto training the data
predict_df$prediction_150 = prediction_150[,3]

# function that picks the winner by taking the person with the highest probability in each tvo
# can probably do this with a DPLYR chain much more easily...
pick_winners = function(df, tvo_column, vote_column) {
  won = vector()
  for (i in 1:nrow(df)) {
    subset = df[which(df[[tvo_column]] == df[[tvo_column]][i]),]
    max_in_tvo = max(subset[[vote_column]])
    won[i] = ifelse(df[[vote_column]][i] == max_in_tvo, TRUE, FALSE)
  }
  won
}
predict_df$winner_prediction = pick_winners(predict_df, tvo_column = "tvo", vote_column = "prediction_150")

#### XGBOOST PREDICTING LOG_PERC_FOR (LINEAR MODEL) #######################################
hist(predict_df$log_perc_for)
hist(predict_df$perc_for)
x = feature_names
y = 32 #perf_for column
names(train[,x])
names(train[,y])

xgb2 <- h2o.xgboost(            ## h2o.randomForest function
  model_id = "xgb2",
  ntrees = 150,
  eta = 0.1,
  min_child_weight = 9,        ## very imbalanced
  max_depth = 11,
  gamma = 0,                   ## should be tuned, depends on logloss function
  max_delta_step = 0,          ## defaults to 0, can be helpful in unbalanced models
  subsample = 0.8,             ## defaults to 1, lowering prevents overfit
  colsample_bytree = 0.8,      ## fraction of columns to samples for each tree
  stopping_rounds = 50,
  verbose = T,
  seed = 1234,                  ##
  training_frame = train,      ## the H2O frame for training
  validation_frame = valid,    ## the H2O frame for validation (not required)
  x = x,                       ## the predictor columns, by column index
  y = y  #log_perc_win              ## the target index (what we are predicting) "log_perc_win"
)

var_imp_linear = h2o.varimp(xgb2)
var_imp_linear %>% as.data.frame()
h2o.performance(xgb2)

prediction_perc_for = h2o.predict(xgb2, newdata = df) %>% as.data.frame() #auto training the data
predict_df$prediction_perc_for = prediction_perc_for[,1]
# getting the winners based on who has the highest prediction in each tvo
predict_df %<>% group_by(tvo) %>% mutate(winner_prediction_by_perc = prediction_perc_for == max(prediction_perc_for))

####### build the actual confusion matrix (recognizing at each tvo has exactly one winner) ######
library(caret)
# the mask for which data was not trained on at all
test_ids = test$unique_id %>% as.vector()

predict_df$winner %<>% base::as.factor()
predict_df$winner_prediction %<>% base::as.factor()
predict_df$winner_prediction_by_perc %<>% base::as.factor()

#confusion matrix for binary classification model (xgb1)
confusionMatrix(predict_df$winner_prediction[test_ids], predict_df$winner[test_ids], positive = "TRUE")

#confusion matrix for linear regression model (xgb2)
confusionMatrix(predict_df$winner_prediction_by_perc[test_ids], predict_df$winner[test_ids], positive = "TRUE")

# overall confustion matrix (including the train_data which could be influenced by overfitting)
confusionMatrix(predict_df$winner_prediction, predict_df$winner, positive = "TRUE")
confusionMatrix(predict_df$winner_prediction_by_perc, predict_df$winner, positive = "TRUE")

# false positives from binary that were fixed with linear (log_perc_for)
View(predict_df[which(predict_df$winner_prediction == TRUE &
                        predict_df$winner_prediction_by_perc == FALSE &
                        predict_df$winner == FALSE),]) 

