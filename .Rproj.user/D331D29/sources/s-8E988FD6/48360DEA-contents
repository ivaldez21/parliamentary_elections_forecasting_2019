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
results= read_csv("2019_results_raw.csv")
names(results) = c("candidate", "graph", "perc_for",
                       "votes_for", "tvo", "candidate_url", "percent_counted")

##### Clean the raw extracted data #####################
results[['graph']] <- NULL
results[["proposed"]] = ""
results[1,]

#fix the comma/deciml/space problem in numeric columns
for (col in c("perc_for", "votes_for", "percent_counted")) {
  results[[col]] %<>%
    gsub(pattern = ",", replacement = ".") %>%
    gsub(pattern = "\\s", replacement = "") %>%
    as.numeric()
}
results[1,]

#split the candidate column into candidate and proposed party
results[["proposed"]] = results[["candidate"]] %>%
  strsplit(split = ",") %>% sapply(FUN = `[[`, 2) %>% trimws()
results[["candidate"]] %<>%
  strsplit(split = ",") %>% sapply(FUN = `[[`, 1) %>% trimws()

#shorten the party names for graphs
for (i in 1:nrow(results)) {
  # results$proposed[i] = switch(results$proposed[i],
  #                           'ПОЛІТИЧНА ПАРТІЯ "СЛУГА НАРОДУ"' = "Слуга Народу",
  #                           "самовисування" = "самовисування",
  #                           'Політична партія "ОПОЗИЦІЙНА ПЛАТФОРМА – ЗА ЖИТТЯ"' = "Опозиційна платформа",
  #                           'Політична партія "ОПОЗИЦІЙНИЙ БЛОК"' = "Опозиційний блок",
  #                           'політична партія Всеукраїнське об’єднання "Батьківщина"' = "Батьківщина",
  #                           'Політична Партія "ГОЛОС"' = 'Голос',
  #                           'Політична партія "Європейська Солідарність"' = "Європейська Солідарність",
  #                           "Other")
  results$proposed[i] %<>% tolower() %>% gsub(pattern = "[[:punct:]]", replacement = "", x = .)
  results$candidate[i] = gsub(pattern = "[[:punct:]]", replacement = "", x = results$candidate[i])
}

#group by tvo and take only the top one people
winners_df = results %>% group_by(tvo) %>% top_n(n = 2, wt = perc_for) %>%
  mutate(spread = perc_for - min(perc_for)) %>% top_n(n = 1, wt = perc_for)

# compare winners to predictions
clean_predictions_df = read_csv("combined_results_cleaned.csv")
predictions_df_top = read_csv("top_2_per_tvo.csv") #original predictions file

# this is taking the top using perc_for linear method
predictions_df_top %<>% group_by(tvo) %>% top_n(1, wt = likelihood)

# our model predicted properly in 90 of 199 TVO's
correct = predictions_df_top$deputat == winners_df$candidate
table(correct)

# add winner column (boolean) to results
results %<>%
  group_by(tvo) %>%
  mutate(winner = perc_for == max(perc_for))

# combine results with candidate info
results$year = 2019
sapply(results, class)
sapply(clean_predictions_df, class)

mask_2019 = which(clean_predictions_df$year == 2019)
temp_2019 = clean_predictions_df[mask_2019,]
temp_else = clean_predictions_df[-mask_2019,]

# need to use inner joins (only return rows from LHS with matches in RHS because some candidates left)
# this is because our data for the candidates hasn't been updated since end of june and before the 
# elections additional candidates (not winners, supposedly) joined the race late. Don't need them
temp_2019 = inner_join(temp_2019[,-c(2,3,20)], results[,-c(5,6)], by = c("deputat" = "candidate", "tvo", "year", "proposed"))
colSums(is.na(temp_2019))
names(temp_2019) %in% names(temp_else)
sapply(temp_2019, class)
sapply(temp_else, class)

data_all = bind_rows(temp_else, temp_2019)

colSums(is.na(data_all))

write.csv(data_all, "actual_results_all_years.csv", row.names = F)
###### Add new results to GEOJSON for plotting using leaflet ################################
# results_df = read_csv("election_results.csv")

######### Check the results using the new updated data ########################################


