##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 22 JULY 2019
# 
# Contact the author with any questions at:
#   isaiah.l.valdez@gmail.com

##### PACKAGES #######################
library(readr)
library(rvest)
library(data.table)
library(magrittr)
library(dplyr)
library(stringr)
Sys.setlocale(locale = "UTF-8")
setwd(dir = "/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")

###### Scrape results from CVK ####################################
# landind page for results from every TVO (party and single-member)

get_results = function() {
  base_url = "https://www.cvk.gov.ua/pls/vnd2019/wp306_npt001f01=919.html"
  base_prefix = "https://www.cvk.gov.ua/pls/vnd2019/"
  
  page = read_html(base_url)
  tvo_rel_urls = page %>% html_nodes('table tbody tr td a.a1') %>% html_attr("href")
  tvo_urls = paste(base_prefix, tvo_rel_urls, sep = "")
  
  get_tvo_results = function(tvo_url) {
    
    start_time = Sys.time() #timer to have 2 seconds or more per request
    
    base_prefix = "https://www.cvk.gov.ua/pls/vnd2019/"
    page = read_html(tvo_url)
    
    tvo_name = page %>% html_node("h1") %>% html_text %>%
      str_extract(pattern = "\\d+") # %>% as.numeric (if needed), default as string
    percent_counted = page %>% html_node("tr + tr .b") %>% html_text %>% trimws()
    
    tvo_table = page %>% html_node("div main div table") %>% html_table()
    tvo_table[['tvo']] = tvo_name
    candidate_rel_url = page %>% html_nodes("table a.a2") %>% html_attr("href")
    tvo_table[['candidate_url']] = paste(base_prefix, candidate_rel_url, sep = "")
    tvo_table[['percent_counted']] = percent_counted
    
    tvo_table %<>% mutate_all(as.character)
    
    end_time = Sys.time() #timer to have 2 seconds or more per request
    time_left = 2 - (end_time - start_time)
    if (time_left > 0) {
      Sys.sleep(time_left) #sleeps if less than 2 seconds past
    }
    
    print(tvo_name) # verbose to know where the loop is
    tvo_table
  }
  
  frames_list = lapply(tvo_urls, get_tvo_results)
  frames_list
}

result_raw_list = get_results()
results_raw = bind_rows(result_raw_list)
write.csv(results_raw, "2019_results_raw.csv", row.names = F)
rm(result_raw_list, get_results, result_raw_list)

######## NOT RUNNING THE SCRAPER AGAIN ############################
results_raw = read_csv("2019_results_raw.csv")

names(results_raw) = c("candidate", "graph", "perc_for",
                       "votes_for", "tvo", "candidate_url", "percent_counted")
# rm(result_raw_list) #save memory

##### Clean the raw extracted data #####################
results = results_raw #keeping a copy
sapply(results, mode)
results[['graph']] <- NULL

#fix the comma/deciml/space problem in numeric columns
for (col in c("perc_for", "votes_for", "percent_counted")) {
  results[[col]] %<>%
    gsub(pattern = ",", replacement = ".") %>%
    gsub(pattern = "\\s", replacement = "") %>%
    as.numeric()
}
sapply(results, class)

#split the candidate column into candidate and proposed party
results[["proposed"]] = results[["candidate"]] %>%
  strsplit(split = ",") %>% sapply(FUN = `[[`, 2) %>% trimws()
results[["candidate"]] %<>%
  strsplit(split = ",") %>% sapply(FUN = `[[`, 1) %>% trimws()

# results %<>% mutate_if(is.character,as.factor)
sapply(results, class)

#shorten the party names for graphs
for (i in 1:nrow(results)) {
  results$proposed[i] = switch(results$proposed[i],
                            'ПОЛІТИЧНА ПАРТІЯ "СЛУГА НАРОДУ"' = "Слуга Народу",
                            "самовисування" = "самовисування",
                            'Політична партія "ОПОЗИЦІЙНА ПЛАТФОРМА – ЗА ЖИТТЯ"' = "Опозиційна платформа",
                            'Політична партія "ОПОЗИЦІЙНИЙ БЛОК"' = "Опозиційний блок",
                            'політична партія Всеукраїнське об’єднання "Батьківщина"' = "Батьківщина",
                            'Політична Партія "ГОЛОС"' = 'Голос',
                            'Політична партія "Європейська Солідарність"' = "Європейська Солідарність",
                            "Other")
  results$candidate[i] = gsub(pattern = "[[:punct:]]", replacement = "", x = results$candidate[i])
}

#group by tvo and take only the top one people
winners_df = results %>% group_by(tvo) %>% top_n(n = 2, wt = perc_for) %>%
  mutate(spread = perc_for - min(perc_for)) %>% top_n(n = 1, wt = perc_for)
  # %>% filter(percent_counted < 50)

# predicted_table = table(winners_df$proposed)
# bins = seq(0,100,2)
# hist(winners_df$percent_counted, breaks = bins)

# Plot for number of winners per party
# Fitting Labels 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,12,4,2)) # increase y-axis margin.

# # number of winners per party
# winners_df$proposed %<>% as.factor
# counts = sort(table(winners_df$proposed))
# barplot(counts, main="Distribution of Winners", horiz=TRUE)

par(mar=c(2,2,2,2)) # increase y-axis margin.

# # small df with descriptive stats per party
# parties_df = winners_df %>% group_by(proposed) %>%
#   summarise(winners = n(),
#             spread = mean(spread),
#             percent_counted = mean(percent_counted))

# compare winners to predictions
predictions_df = read_csv("predictions.csv")
clean_predictions_df = read_csv("combined_results_cleaned.csv")
predictions_df_top = read_csv("top_2_per_tvo.csv") #original predictions file
predictions_updated = read_csv("predictions_updated.csv")

# this is taking the top using perc_for linear method
predictions_updated %<>% group_by(tvo) %>% top_n(1, wt = prediction_50)
# predictions_updated = predictions_updated[-which(duplicated(predictions_updated$tvo)),]

# set up the confusion matrix. Can change predicted to see performance of different models
comparison_df = data.frame(
  predicted = predictions_updated$deputat, #predictions_updated$deputat,
  atual = winners_df$candidate,
  tvo = winners_df$tvo,
  party = winners_df$proposed,
  spread = winners_df$spread,
  stringsAsFactors = F)
# View(comparison_df)

correct = comparison_df$predicted == comparison_df$atual
table(correct)
# View(comparison_df[!correct,])

# combine results with candidate info
names(winners_df)
names(predictions_df)
sapply(winners_df, class)
sapply(predictions_df, class)
winners_df$tvo %<>% as.numeric()

combined_df_1 = left_join(winners_df, predictions_df, by = c("candidate" = "deputat", "tvo"))
combined_df = left_join(combined_df_1, predictions_df_top[,c("deputat", "tvo", "winner")], by = c("candidate" = "deputat", "tvo"))
names(combined_df)
combined_df_sub = combined_df %>% select(candidate, perc_for.x, votes_for.x, tvo, candidate_url,
                                         proposed.x, spread, oblast, gender, age, birthday, job, previous, ever, winner.y)
temp_names = names(combined_df_sub)
temp_names[c(2:3,6,15)] = c("perc_for", 'votes_for', "nominated", "predicted_winner")
names(combined_df_sub) = temp_names
rm(temp_names)
write.csv(x = combined_df_sub, "election_results.csv", row.names = F, fileEncoding = "UTF-8")
# rm(list = ls())

#add the results to the predition dataframe and rerun the model
names(results)
names(clean_predictions_df)

#add winner column to results
results %<>% group_by(tvo) %>%
  mutate(winner = perc_for == max(perc_for))
head(results$winner, 25)
# results is shorter because some people left the race
clean_predictions_df$tvo %<>% as.numeric()
data_2019 = left_join(results, clean_predictions_df[,-c(2,3,8,20)],
                      by = c("candidate" = "deputat", "tvo"))
names(data_2019)
write.csv(data_2019, "rerun_model_actual_data_2019.csv", row.names = F)
# ###### Add new results to GEOJSON for plotting using leaflet ################################
# results_df = read_csv("election_results.csv")

######### Check the results using the new updated data ########################################


