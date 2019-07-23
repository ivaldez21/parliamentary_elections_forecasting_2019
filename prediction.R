##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 9 JULY 2019
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

data = read_csv(file = "combined_results_cleaned.csv") %>% data.table(stringsAsFactors = T)
data[, tvo := as.factor(as.character(tvo))]
data[, tvo_pres := as.factor(as.character(tvo_pres))]

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

## Load a file from disk
# df <- h2o.importFile(path = normalizePath("train_df.csv"))
df = data[which(data$year != 2019),]
train_df = df
df = as.h2o(df)

df_test = data[which(data$year == 2019),]
predict_df = df_test
df_test = as.h2o(df_test)

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
feature_names = c("tvo", "oblast", "year", "proposed", "gender", "age",
                  "job", "party", "pres_candidate", "pres_perc_for",
                  "average_age", "pop_change", "region_ideology", "previous",
                  "ever", "funding", "power_status", "party_ideology",
                  "prop_election_forecast")
feature_names
h2o.isfactor(train[,feature_names])


# ## run our first predictive model
# x = feature_names
# y = 20 #20 or 2 or 29
# rf1 <- h2o.randomForest(         ## h2o.randomForest function
#   training_frame = train,        ## the H2O frame for training
#   validation_frame = valid,      ## the H2O frame for validation (not required)
#   x = x,             ## the predictor columns, by column index
#   y = y,  #winner              ## the target index (what we are predicting) "log_perc_win"
#   nfolds = 3,
#   model_id = "rf_covType_v1",
#   ntrees = 50,
#   sample_rate = 0.7,
#   mtries = 5,
#   stopping_rounds = 5,
#   stopping_metric = "logloss",
#   seed = 1000000,
#   ## balance the classes by oversampling the "1"'s
#   balance_classes = T)
# 
# perf_rf1 = h2o.performance(rf1, newdata = test)
# perf_rf1
# h2o.confusionMatrix(perf_rf1)
# 
# # y = 30 #log_perc_for
# # rf2 <- h2o.randomForest(
# #   training_frame = train,     ## the H2O frame for training
# #   validation_frame = valid,   ## the H2O frame for validation (not required)
# #   x = x,                      ## the predictor columns, by column index
# #   y = y,  #winner             ## the target index (what we are predicting) "log_perc_win"
# #   stopping_rounds = 5,
# #   seed = 1000000,
# #   ntrees = 500,
# #   nfolds = 3
# # )
# # summary(rf2)
# # h2o.performance(rf2, newdata = test)
# 
# # hyperparameter grid
# hyper_grid.h2o <- list(
#   ntrees      = seq(25, 150, by = 25),
#   mtries = c(4,10,15,19),
#   sample_rate = c(0.45, .55, .632, .70, 0.8)
# )
# 
# # build grid search
# grid <- h2o.grid(
#   algorithm = "drf",
#   grid_id = "rf_grid",
#   x = x,
#   y = y,
#   training_frame = train,
#   validation_frame = valid,
#   hyper_params = hyper_grid.h2o,
#   search_criteria = list(strategy = "Cartesian")
# )
# 
# # collect the results and sort by our model performance metric of choice
# grid_perf <- h2o.getGrid(
#   grid_id = "rf_grid",
#   sort_by = "rmse",
#   decreasing = FALSE
# )
# summary(grid_perf)
# 
# #predict on test file
# h2o.predict(rf1, newdata = df_test)

##### XGBOOST ####################################
x = feature_names
y = 20
names(train[,x])
names(train[,y])

# hyper_params <- list(ntrees = seq(100, 500, 100),
#                      learn_rate = seq(0.0001, 0.1, 0.0001),
#                      max_depth = seq(1, 20, 1),
#                      sample_rate = seq(0.5, 1.0, 0.1),
#                      col_sample_rate = seq(0.2, 1.0, 0.1))

# hyper_params <- list(gamma = seq(0,5,0.5)/10)
# search_criteria <- list(strategy = "Cartesian")
# # Train the grid
# xgb_grid <- h2o.grid(algorithm = "xgboost",
#                      grid_id = "xgb_grid1",
#                      max_depth = 11,
#                      min_child_weight = 9,
#                      learn_rate = 0.1,
#                      n_estimators=100,
#                      subsample = 0.8,
#                      colsample_bytree = 0.8,
#                      x = x, y = y,
#                      training_frame = train,
#                      validation_frame = valid,
#                      seed = 1234,
#                      hyper_params = hyper_params,
#                      search_criteria = search_criteria)
# 
# # collect the results and sort by our model performance metric of choice
# grid_perf <- h2o.getGrid(
#   grid_id = "xgb_grid1",
#   sort_by = "recall"
# )
# grid_perf ## xgboost 1
## xgb1 with 150 trees, xgb2 with 100, xgb3 with 50 trees, and xgb4 default plus min child weight = 9

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

xgb2 <- h2o.xgboost(            ## h2o.randomForest function
  model_id = "xgb2",
  ntrees = 100,
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

xgb3 <- h2o.xgboost(            ## h2o.randomForest function
  model_id = "xgb3",
  ntrees = 50,
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

xgb4 <- h2o.xgboost(            ## h2o.randomForest function
  model_id = "xgb4",
  min_child_weight = 9,
  verbose = T,
  seed = 1234,                  ##
  training_frame = train,      ## the H2O frame for training
  validation_frame = valid,    ## the H2O frame for validation (not required)
  x = x,                       ## the predictor columns, by column index
  y = y  #winner              ## the target index (what we are predicting) "log_perc_win"
)

var_imp = h2o.varimp(xgb1)
h2o.varimp(xgb1)
# # not looking at these closely because some tvo's predict no winners, some several
# # because the model does not know that one person wins in each tvo
# h2o.performance(xgb1, newdata = train) 
# h2o.performance(xgb1, newdata = valid)
# h2o.performance(xgb1, newdata = test)

h2o.varimp(xgb2)
# h2o.performance(xgb2, newdata = train)
# h2o.performance(xgb2, newdata = valid)
# h2o.performance(xgb2, newdata = test)

h2o.varimp(xgb3)
# h2o.performance(xgb3, newdata = train)
# h2o.performance(xgb3, newdata = valid)
# h2o.performance(xgb3, newdata = test)

h2o.varimp(xgb4)
# h2o.performance(xgb4, newdata = train)
# h2o.performance(xgb4, newdata = valid)
# h2o.performance(xgb4, newdata = test)

prediction_150 = h2o.predict(xgb1, newdata = df_test) %>% as.data.frame()
prediction_100 = h2o.predict(xgb2, newdata = df_test) %>% as.data.frame()
prediction_50 = h2o.predict(xgb3, newdata = df_test) %>% as.data.frame()
prediction_default_plus_class = h2o.predict(xgb4, newdata = df_test) %>% as.data.frame()

predict_df$prediction_150 = prediction_150[,3]
predict_df$prediction_100 = prediction_100[,3]
predict_df$prediction_50 = prediction_50[,3]
predict_df$prediction_default_plus_class = prediction_default_plus_class[,3]

pick_winners = function(df, tvo_column, vote_column) {
  won = vector()
  for (i in 1:nrow(df)) {
    subset = df[which(df[[tvo_column]] == df[[tvo_column]][i]),]
    max_in_tvo = max(subset[[vote_column]])
    won[i] = ifelse(df[[vote_column]][i] == max_in_tvo, 1, 0)
  }
  won
}

predict_df$winner = pick_winners(predict_df, tvo_column = "tvo", vote_column = "prediction_150")
# View(predict_df[,c("tvo","deputat","winner","prediction_150","proposed")] %>%
#        arrange(tvo,desc(prediction_150)))

# temp_df = predict_df %>% group_by(proposed) %>% summarize(winner_count = sum(winner))
predict_df$tvo %<>% as.character() %>% as.numeric()
predict_df %<>% arrange(tvo,desc(prediction_150))

write.csv(predict_df, file = "predictions.csv", row.names = F)

##### LOOKING AT THE PREDICTIONS FROM THE MODEL TO SEE WHERE IT GUESSES WRONG ###################
# library(caret)
# 
# predictions = h2o.predict(xgb1, newdata = df) %>% as.data.frame()
# predicted_winner = predictions$predict %>% as.logical()
# predicted_prob = predictions$TRUE.
# train_df$prediction_prob = predicted_prob
# names(train_df)
# train_df$tvo %<>% as.numeric()
# 
# #this needs to be done in two steps... 2012 and 2014 seperately
# mask_2012 = which(train_df$year == "2012")
# mask_2014 = which(train_df$year == "2014")
# train_df$prediction[mask_2012] = pick_winners(train_df[mask_2012,], tvo_column = "tvo", vote_column = "prediction_prob") %>%
#   as.logical()
# train_df$prediction[mask_2014] = pick_winners(train_df[mask_2014,], tvo_column = "tvo", vote_column = "prediction_prob") %>%
#   as.logical()
# 
# # the mask for which data was not trained on at all
# test_ids = test$unique_id %>% as.vector()
# validation_ids = valid$unique_id %>% as.vector()
# 
# temp_df = train_df[, c("deputat", "tvo", "year", "winner", "prediction", "prediction_prob", "perc_for") ]
# 
# temp_df$winner %<>% base::as.factor()
# temp_df$prediction %<>% base::as.factor()
# confusionMatrix(temp_df$prediction[test_ids], temp_df$winner[test_ids], positive = "TRUE")
# 
# false_positives = which(temp_df$winner == FALSE & temp_df$prediction == TRUE)
# false_negatives = which(temp_df$winner == TRUE & temp_df$prediction == FALSE)
# correct = which(temp_df$winner == temp_df$prediction)
# temp_df$correct = FALSE
# temp_df$correct[correct] = TRUE
# # true_positives = which(temp_df$winner == TRUE & temp_df$prediction == TRUE)
# # true_negatives = which(temp_df$winner == FALSE & temp_df$prediction == FALSE)
# # all_positive = which(temp_df$winner == TRUE)
# # all_negative = which(temp_df$winner == FALSE)
# # length(false_positives)
# # length(false_negatives)
# # length(true_positives)
# # length(true_negatives)
# # length(all_positive)
# # length(all_negative)
# temp_df = temp_df %>% arrange(year,tvo)
# View(temp_df[which(temp_df$correct == FALSE),])

###### ADD FACTION INFO ###################################
predict_df = read_csv("predictions.csv")
# # Run the below code just to download the results and create the csv once
# MP_urls = paste0("https://data.rada.gov.ua/ogd/mps/skl", 1:8,"/mps0", 1:8,"-data.csv")
# 
# MPs_all_convocation = bind_rows(lapply(MP_urls, read_csv))
# # MPs_all_convocation = MPs_all_convocation[,which(colSums(is.na(MPs_all_convocation)) == 0)]
# # MPs_all_convocation = MPs_all_convocation[,c("convocation", "full_name", "birthday")]
# MPs_all_convocation$birthday = gsub(pattern = "0(\\d+)", replacement = "\\1", x = MPs_all_convocation$birthday)
# MPs_all_convocation$full_name = gsub(pattern = "'", replacement = "’", x = MPs_all_convocation$full_name)
# write.csv(MPs_all_convocation, "MPs_all_convocations_full.csv", row.names = F)
MPs_all_convocation = read_csv("MPs_all_convocations_full.csv")
MPs_all_convocation$birthday %<>% as.Date(format = "%d.%m.%Y")
MPs_all_convocation %<>% arrange(desc(convocation))
MPs_all_convocation = MPs_all_convocation %>%
  select(id, full_name, birthday)

combined_predictions = left_join(x = predict_df, y = MPs_all_convocation, by = c("deputat" = "full_name", "birthday"))
combined_predictions %<>% group_by(deputat,birthday) 
combined_predictions = combined_predictions[-which(duplicated(combined_predictions[,c("deputat", "birthday")])),]

fractions_df = read_csv("mp-posts_full.csv")
fractions_df = fractions_df %>%
  filter(grepl(pattern = "депутатської (фракції|групи)", x = post_name)) %>%
  select(mp_id, full_name, post_title)

fractions_df$post_title %<>% str_extract(pattern = "((?<=фракції )|(?<=групи ))([^.]+)") %>%
  gsub(pattern = "[[:punct:]]", replacement = "") %>% tolower()
names(fractions_df) = c("id", "full_name", "fraction")
combined_predictions = left_join(x = combined_predictions, y = fractions_df, by = c("id"))
names(combined_predictions)

samovy = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?output=csv")
names(samovy)
samovy = samovy[,c("id","deputat", "tvo", "fraction")]
nrow(combined_predictions)
combined_predictions = left_join(x = combined_predictions, y = samovy, by = c("id","tvo", "deputat"))
nrow(combined_predictions)
names(combined_predictions)
# View(combined_predictions[,c("fraction.x", "fraction.y")])

for (i in 1:nrow(combined_predictions)) {
  if (is.na(combined_predictions$fraction.x[i])) {
    if (!is.na(combined_predictions$fraction.y[i])) {
      combined_predictions$fraction.x[i] = combined_predictions$fraction.y[i]
    }
  }
  print(i)
}

head(combined_predictions$fraction.x,150)
head(combined_predictions$fraction.y,100)
# View(combined_predictions[,c("fraction.x","fraction.y")])
combined_predictions$fraction.y <- NULL
combined_predictions %<>% rename(fraction = fraction.x)
# combined_predictions %<>% mutate(
#   proposed = ifelse(
#     test = proposed == "самовисування",
#     yes = fraction,
#     no = proposed)
# )

sort(table(combined_predictions$proposed), decreasing = T)

# write.csv(combined_prediction_2, "new_prediction.csv", row.names = F)

# # names(combined_predictions)
# filtered_df = combined_predictions %>%
#   group_by(tvo) %>%
#   arrange(tvo, desc(prediction_150)) %>%
#   top_n(n=5, wt = prediction_150) %>%
#   select(id, deputat, tvo, winner, factions_text, party_name,
#        anketa_data, proposed, prediction_150, year, convocation,
#        previous, ever, anketa_url, fraction, district_num) %>%
#   mutate(same_tvo_last_year = tvo == district_num) %>%
#   filter(year == 2019) %>%
#   filter(proposed == "самовисування") %>%
#   filter(prediction_150 > 0.099)
# 
# table(filtered_df$same_tvo_last_year, useNA = "always")
# 
# write.csv(filtered_df, "samovysuvantsi.csv", row.names = F)
#   
# colSums(is.na(filtered_df))
# sort(table(filtered_df$fraction), decreasing = T)
# 
# filtered_df_2 = combined_predictions %>%
#   group_by(tvo) %>%
#   arrange(tvo, desc(prediction_150)) %>%
#   top_n(n=1, wt = prediction_150) %>%
#   select(id, deputat, tvo, winner, factions_text, party_name,
#          anketa_data, proposed, prediction_150, year, convocation,
#          previous, ever, anketa_url, fraction, party) %>%
#   filter(year == 2019)
# sort(table(filtered_df_2$proposed), decreasing = T)
# sort(table(filtered_df_2 %>% ungroup %>% filter(proposed == "самовисування") %>% select(fraction)), decreasing = T)
# 
filtered_top_three = combined_predictions %>%
  group_by(tvo) %>%
  arrange(tvo, desc(prediction_150)) %>%
  top_n(n=5, wt = prediction_150) %>%
  mutate(likelihood = prediction_150 / sum(prediction_150, na.rm = T)) %>%
  select(deputat, tvo, winner, likelihood, proposed, fraction, age, gender, fraction) %>%
  top_n(n=3, wt = likelihood) %>%
  mutate(spread = likelihood - median(likelihood))
sort(table(filtered_top_three$proposed), decreasing = T)
# needs to be cleaned by hand...
write.csv(filtered_top_three, "top_3_per_tvo.csv", row.names = F)
# 
# fractions_df_7th = read_csv("https://data.rada.gov.ua/ogd/mps/skl7/mp-posts_full.csv")
# fractions_df_7th = fractions_df_7th %>%
#   filter(grepl(pattern = "депутатської (фракції|групи)", x = post_name)) %>%
#   select(mp_id, full_name, post_title)
# 
# fractions_df_7th$post_title %<>% str_extract(pattern = "((?<=фракції )|(?<=групи ))([^.]+)") %>%
#   gsub(pattern = "[[:punct:]]", replacement = "") %>% tolower()
# names(fractions_df_7th) = c("id", "full_name", "fraction")

# # edited by hand in google sheets
# temp_df = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=1071998123&single=true&output=csv")
# 
# combined_predictions_7th = left_join(x = temp_df, y = fractions_df_7th, by = c("id")) %>%
#   select(deputat, tvo, fraction.x, fraction.y, winner, prediction_150, previous, party_name)

# # getting several errors with the by hand method , moving to web scraping method
# library(rvest)
# for (i in 1:nrow(temp_df)) {
#   start = Sys.time()
#   page_url = temp_df$anketa_url[i]
#   if (!is.na(page_url)) {
#     page = read_html(page_url)
#     info_list = page %>% html_nodes(".simple_info:nth-child(1) td+ td") %>% html_text() %>% strsplit(split = "(\r\n|\\s{2,})") %>% unlist()
#     fraction = grep(pattern = "(фракці|групи)", x = info_list, value = T)
#     if (length(fraction) > 0) {
#       temp_df$fraction[i] = fraction
#     } else {
#       temp_df$fraction[i] = "NA"
#     }
#   }
#   end = Sys.time()
#   left = 2 - (end - start)
#   sleep = ifelse(left > 0, left, 0.0001)
#   Sys.sleep(sleep)
#   print(i)
# }
# 
# sort(table(temp_df$fraction), decreasing = T)
# write.csv(temp_df, "temp_df.csv", row.names = F)
