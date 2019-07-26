##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 10 JULY 2019
# 
# Contact the author with any questions at:
#   isaiah.l.valdez@gmail.com

##### PACKAGES #######################
library(readr)
library(magrittr)
library(dplyr)
Sys.setlocale(locale = "UTF-8")
setwd(dir = "/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")

df = read.csv(file = "combined_results.csv")
copy_df = df #why do we need this copy?
names(df)

## Ensuring column types are set properly (no fake integers, such as tvo)
factor_columns = c("party", "tvo", "oblast", "tvo_pres", "pres_candidate", "proposed", "gender",
                   "job", "city_living", "region_ideology", "power_status", "party_ideology")
number_columns = c("perc_for", "votes_for", "year", "pres_perc_for", "pres_votes_for", "age", 
                   "average_age", "pop_change", "funding", "prop_election_forecast", "count")
logical_columns = c("winner", "previous", "ever")
date_columns = c("registration", "birthday") #both are written as DD.MM.YYYY
char_columns = c("info", "deputat")
setdiff(names(df), logical_columns) %>% setdiff(number_columns) %>% setdiff(factor_columns) %>%
  setdiff(date_columns) %>% setdiff(char_columns) #should be character(0)

fix_columns = function(df) {
  for (col in factor_columns) {df[[col]] = as.factor(df[[col]])}
  for (col in number_columns) {df[[col]] = as.numeric(df[[col]])}
  for (col in date_columns) {df[[col]] = as.Date(df[[col]], format = "%d.%m.%Y")}
  for (col in logical_columns) {df[[col]] = as.logical(df[[col]])}
  for (col in char_columns) {df[[col]] = as.character(df[[col]])}
  df
}

df = fix_columns(df)

## Cleaning the factor columns by reducing the number of levels
for (col in factor_columns) {
  print(col)
  print(length(levels(df[[col]])))
  }

# oblast has 28 levels.. should only have 27
# party has 147, maybe some are same party with name change?
# tvo has 225, which is fine
# pres_candidate has 29, also fine
# proposed, like party, has tons of levels.. maybe can be reduced
# gender and power_stats have 2, (fine, they are dummy variables)
# job has 1801, this can be reduced through regex
# city_lving, can also be reduced through regex
# region_ and party_ideology 3 categories

# 50 most frequent job's
head(table(df$job) %>% sort(decreasing = T), 50)
# 50 most frequent job's among those who won elections
a = table(df$job[which(df$winner == T)]) %>% sort(decreasing = T) %>% data.frame()
# View(df[which(df$winner == T),])

df$job <- as.character(df$job) #because need to create new levels
df$job[grepl(pattern = "риємець", x = df$job)] = "підприємець"
df$job[grepl(pattern = "директор\\b", x = df$job)] = "директор"
df$job[grepl(pattern = "(міський|селищний|міський) голова\\b", x = df$job)] = "city_village head"
df$job[grepl(pattern = "голова\\b", x = df$job)] = "голова"
df$job[grepl(pattern = "заступник голови\\b", x = df$job)] = "заступник голови"
df$job[grepl(pattern = "заступник [^голови]", x = df$job)] = "заступник"
df$job[grepl(pattern = "начальник\\b", x = df$job)] = "начальник"
df$job[grepl(pattern = "\\bвійсь", x = df$job)] = "військовий"
df$job[grepl(pattern = "\\b(війсь|командир|рядовий)", x = df$job)] = "військовий"
df$job[grepl(pattern = "керівник\\b", x = df$job)] = "керівник"
df$job[grepl(pattern = "Міністр\\b", x = df$job)] = "Міністр"
df$job[grepl(pattern = "президент\\b", x = df$job)] = "президент"


for (column in c("party", "proposed")) {
  df[[column]] = tolower(df[[column]]) %>%
    gsub(pattern = "[[:punct:]]", replacement = "", x = .)
}

jobs = df %>% group_by(job) %>%
  summarise(n = n(), 
            freq = n/nrow(df),
            winner_n = sum(winner, na.rm = T),
            winner = mean(winner, na.rm = T),
            above_thresh = (n > 7)) %>% filter(above_thresh == TRUE) %>%
  arrange(desc(freq))


df$job[-which(df$job %in% jobs$job)] = "Other"
length(levels(as.factor(as.character(df$job)))) #reduced to 57 levels + other
df$winner <- as.numeric(df$winner)

parties = levels(as.factor(df$party))
parties = df %>% group_by(party) %>%
  summarise(n = n(), 
            freq = n/nrow(df),
            winner_n = sum(winner, na.rm = T),
            winner = mean(winner, na.rm = T),
            above_thresh = (n > 10)) %>% filter(above_thresh == TRUE) %>%
  arrange(desc(freq))
# write.csv(parties, file = "compare_parties.csv", row.names = F) #did this once 
# did some by hand editing to combine parties that had name changes or mergers 
parties = read_csv("compare_parties.csv")
names(parties) = c("party", "unique", "keep")

for (i in 1:nrow(df)) {
  match = which(parties$party == df$party[i])
  if (length(match) > 0) {
    
    if (parties$unique[match] == 1) {
      df$party[i] = parties$party[match]
    } else {
      df$party[i] = parties$keep[match]
    }
    
  } else {
    df$party[i] = "Other"
  }
}

length(levels(as.factor(as.character(df$party)))) #reduced/combined to 43 levels

df = fix_columns(df)
copy_df = fix_columns(copy_df)

sapply(df, class) == sapply(copy_df, class)

write.csv(df, file = "combined_results_cleaned.csv", row.names = F)
