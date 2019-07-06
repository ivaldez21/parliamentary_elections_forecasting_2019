##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 18 JUNE 2019
# 
# Contact the author with any questions at:
#   isaiah.l.valdez@gmail.com

##### PACKAGES #######################
require(rvest)
require(magrittr)
require(stringr)
require(dplyr)
require(readr)
require(stringi)
setwd("/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")
Sys.setlocale(category = "LC_ALL", "ukrainian")

##### COLLECTION #####################
# TVO's for 2010 are for the pres. election, 2012 is for parl.
TVO_comp_2010_url = 'https://www.cvk.gov.ua/pls/vp2010/wp030f0d8.html?PT001F01=700'
TVO_comp_2012_url = 'https://www.cvk.gov.ua/pls/vnd2012/wp030ad94.html?PT001F01=900'
TVO_comp_2014_pres_url = 'https://www.cvk.gov.ua/pls/vp2014/wp030pt001f01=702.html'
TVO_comp_2014_parl_url = 'https://www.cvk.gov.ua/pls/vnd2014/wp03052e6.html?PT001F01=910'

# all needed information is available one level deep, i.e. do not need to open
# the link to the each TVO.

scrape_tvo = function(url, encoding) {
  
  page = read_html(url, encoding = encoding)
  base_url = str_extract(string = url, pattern = "^(.*[\\/])")
  regions_rel_hyper = page %>% html_nodes('a.a1') %>% html_attr('href')
  regions = paste(base_url, regions_rel_hyper, sep = '') #links to each region
  
  all_frames = lapply(regions, scrape_tvo_table)
  df = bind_rows(all_frames)
  df
}

scrape_tvo_table = function(url) {
  page = read_html(url)
  tables = page %>% html_nodes('table.t2')
  table = tables[[2]] %>% html_table
  table = table[-1,] #the table 'headers' are coded as the first row
  colnames(table) = c('tvo', 'num_voters', 'num_stations', 'description')
  print(table[1,'tvo'])
  table 
}

# Scrape the data once, save to .csv, load from local environment if needed again
TVO_2010 = scrape_tvo(TVO_comp_2010_url, encoding = "windows-1251")
TVO_2012 = scrape_tvo(TVO_comp_2012_url, encoding = "windows-1251")
TVO_2014_pres = scrape_tvo(TVO_comp_2014_pres_url, encoding = "windows-1251")
TVO_2014_parl = scrape_tvo(TVO_comp_2014_parl_url, encoding = "windows-1251")

write.csv(TVO_2010, "TVO_2010.csv", row.names = F)
write.csv(TVO_2012, "TVO_2012.csv", row.names = F)

##### LOAD DATA ######################################
TVO_2010 = read_csv("TVO_2010.csv")
TVO_2012 = read_csv("TVO_2012.csv")

##### CLEANING & FEATURES ############################

# cleaning the text by removing extra spaces and having consistent abbreviations
clean_TVO = function(df) {
  df$tvo = str_extract(string = df$tvo, pattern = "[[:digit:]]+")
  df$description = gsub(pattern = "[[:space:]]+", replacement = " ", x = df$description)
  df$description = gsub(pattern = " селище міського типу ", replacement = " смт ", x = df$description)
  df$description = gsub(pattern = ";", replacement = ",", x = df$description)
  df
}

column_split = function(df, delimiter){
  regex_pat = sprintf("(?<=: ).*(?=%s)",delimiter)
  df$city_neighborhood = str_extract(df$description, pattern = regex_pat) %>% trimws()
  df$includes = str_extract(df$description, pattern = "(?<=включає: ).*") %>% trimws()
  df
}

TVO_2010 = clean_TVO(TVO_2010)
TVO_2012 = clean_TVO(TVO_2012)
TVO_2010 = column_split(df = TVO_2010, delimiter = "ТВО")
TVO_2012 = column_split(df = TVO_2012, delimiter = "ОВО")

# # compare_cities = cbind(TVO_2010$city_neighborhood,TVO_2012$city_neighborhood) %>% data.frame(stringsAsFactors = F)
# # compare_includes = cbind(TVO_2010$includes,TVO_2012$includes) %>% data.frame(stringsAsFactors = F)
# 
# cities_same = TVO_2010$city_neighborhood == TVO_2012$city_neighborhood
# details_same = TVO_2010$includes == TVO_2012$includes
# 
# # function to compare the two lists
# same = list()
# for (i in 1:nrow(TVO_2010)){
#   print(TVO_2010$includes[i])
#   print(TVO_2012$includes[i])
#   same[i] = readline(prompt = "same? (y/n/m)")
# }
# 
# same_logic = ifelse(same == "y" | same == "m", TRUE, FALSE)
# details_same_int = details_same | same_logic

#export to excel to check
export_df = data.frame(TVO_2010$tvo, TVO_2010$includes, TVO_2012$tvo, TVO_2012$includes, details_same_int)
write.csv(export_df, "TVO_comparison.csv", fileEncoding = "UTF-8")
