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
Sys.setlocale(locale = "UTF-8")

extract_cyr = function(string, pattern) {
  extraction = regmatches(x = string, regexpr(pattern = pattern, text = string, perl = T))
  extraction
}

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
  regions_rel_hyper = page %>% html_nodes('table.t2 a.a1') %>% html_attr('href')
  regions_names = page %>% html_nodes('table.t2 a.a1') %>% html_text
  regions = paste(base_url, regions_rel_hyper, sep = '') #links to each region
  
  all_frames = mapply(scrape_tvo_table, url = regions, encoding = encoding, region = regions_names, SIMPLIFY = F)
  df = bind_rows(all_frames)
  df
}

scrape_tvo_table = function(url, encoding, region) {
  page = read_html(url, encoding = encoding)
  tables = page %>% html_nodes('table.t2')
  table = tables[[2]] %>% html_table
  table = table[-1,] #the table 'headers' are coded as the first row
  table$district = region
  colnames(table) = c('tvo', 'num_voters', 'num_stations', 'description', 'district')
  print(table[1,'tvo'])
  table 
}

# Scrape the data once, save to .csv, load from local environment if needed again
TVO_2010 = scrape_tvo(TVO_comp_2010_url, encoding = "windows-1251")
TVO_2012 = scrape_tvo(TVO_comp_2012_url, encoding = "windows-1251")
TVO_2014_pres = scrape_tvo(TVO_comp_2014_pres_url, encoding = "windows-1251")
TVO_2014_parl = scrape_tvo(TVO_comp_2014_parl_url, encoding = "windows-1251")

write.csv(TVO_2010, "TVO_2010.csv", row.names = F, fileEncoding = "UTF-8")
write.csv(TVO_2012, "TVO_2012.csv", row.names = F, fileEncoding = "UTF-8")
write.csv(TVO_2014_parl, "TVO_2014_parl.csv", row.names = F, fileEncoding = "UTF-8")
write.csv(TVO_2014_pres, "TVO_2014_pres.csv", row.names = F, fileEncoding = "UTF-8")

##### LOAD DATA ######################################
TVO_2010 = read_csv("TVO_2010.csv")
TVO_2012 = read_csv("TVO_2012.csv")
TVO_2014_parl = read_csv("TVO_2014_parl.csv")
TVO_2014_pres = read_csv("TVO_2014_pres.csv")

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

TVO_2010      = clean_TVO(TVO_2010)
TVO_2012      = clean_TVO(TVO_2012)
TVO_2014_pres = clean_TVO(TVO_2014_pres)
TVO_2014_parl = clean_TVO(TVO_2014_parl)

TVO_2010      = column_split(df = TVO_2010, delimiter = "ТВО")
TVO_2012      = column_split(df = TVO_2012, delimiter = "ОВО")
TVO_2014_pres = column_split(df = TVO_2014_pres, delimiter = "ТВО")
TVO_2014_parl = column_split(df = TVO_2014_parl, delimiter = "ОВО")
head(TVO_2010)
head(TVO_2012)
head(TVO_2014_pres)
head(TVO_2014_parl)

# export_df = data.frame(TVO_2010$tvo, TVO_2010$includes, TVO_2012$tvo, TVO_2012$includes, details_same_int)
# write.csv(export_df, "TVO_comparison.csv", fileEncoding = "UTF-8")

get_mista_and_rajony = function(tvo_df, column_name) {
  
  includes = vector()
  for (i in 1:nrow(tvo_df)) {
    
    regex = "([^ ,\n]+ий)(?=.*район[и]? [ву] місті)" #gets name of city (if its part of a city)
    misto_rajony_temp = str_extract_all(string = tvo_df$includes[i], pattern = regex)
    regex = "([^ ,\n]+ий)(?!.*район[и]? (в|у) місті)" #gets the rajony
    rajony_temp = str_extract_all(string = tvo_df$includes[i], pattern = regex)
    regex = "(?<=місто )([^ ,]+)" #gets the one city
    misto_temp = str_extract(string = tvo_df$includes[i], pattern = regex)
    regex = "(?<=міста )([^ ,]+[^й,])(, ([^ ,]+[^й,]))" #gets several cities
    mista_temp = str_extract(string = tvo_df$includes[i], pattern = regex) %>% 
    gsub(pattern = "[[:punct:]]", replacement = "") %>% strsplit(split = " ")
    # print("---------------")
    # print(misto_rajony_temp)
    # print(rajony_temp)
    # print(misto_temp)
    # print(mista_temp)
    
    description = ""
    if (!is.na(misto_temp)) {
      misto_temp = paste("м.", misto_temp)
      description = misto_temp
      if (length(rajony_temp[[1]]) > 0) {
        rajony_temp = paste(rajony_temp[[1]], "район", collapse = ";")
        description = paste(description, rajony_temp, collapse = "", sep = ";")
      }
    } else if (length(mista_temp[[1]]) > 1) {
      mista_temp = paste("м.", mista_temp[[1]], collapse = ";")
      description = mista_temp
      if (length(rajony_temp[[1]]) > 0) {
        rajony_temp = paste(rajony_temp[[1]], "район", collapse = ";")
        description = paste(description, rajony_temp, collapse = "", sep = ";")
      }
    } else if (length(rajony_temp[[1]]) > 0) {
      rajony_temp = paste(rajony_temp[[1]], "район", collapse = ";")
      description = rajony_temp
    }
    
    
    
    # I will check these by hand instead...
    # if (length(misto_rajony_temp[[1]]) > 0) {
    #   misto_rajony_temp = paste(misto_rajony_temp[[1]], "район", collapse = ";")
    #   description = paste(description, misto_rajony_temp, collapse = "", sep = ";")
    # }
    
    includes[i] = description
    
  }
  
  includes
  
}

new = get_mista_and_rajony(tvo_df = TVO_2010, column_name = "includes")
new2 = get_mista_and_rajony(tvo_df = TVO_2012, column_name = "includes")
new3 = get_mista_and_rajony(tvo_df = TVO_2014_parl, column_name = "includes")
new4 = get_mista_and_rajony(tvo_df = TVO_2014_pres, column_name = "includes")

TVO_2010$sections      = new
TVO_2012$sections      = new2
TVO_2014_parl$sections = new3
TVO_2014_pres$sections = new4

population_2012 = read_csv('population_by_region_2010-2012.csv')
population_2014 = read_csv('population_by_region_2012-2014.csv')

compare_tvo_lists = function(tvo_parl, tvo_pres, population_df) {
  a = tvo_pres$sections %>% strsplit(split = ";")
  b = tvo_pres$district
  x = tvo_pres$tvo
  c = tvo_parl$sections %>% strsplit(split = ";") #parliament
  d = tvo_parl$district #parliament
  y = tvo_parl$tvo
  
  corresponding_parl_tvo = tvo_parl$tvo
  corresponding_pres_tvo = vector()
  # for (i in 1:length(c)) {
  for (i in 1:nrow(tvo_parl)) {
    print(i)
    sections_parl = c[[i]]
    if (length(sections_parl) >0) {
      tvo_parl = y[i]
      current_district = d[i]
      
      sections_pres = a[which(b == current_district)]
      tvos_pres = y[which(b == current_district)]
      
      sub_populations = population_df[which(population_df$district == current_district),]
      populations_parl = vector()
      for (j in 1:length(sections_parl)) {
        temp = sub_populations$pop_2012[which(sub_populations$place == sections_parl[j])]
        populations_parl[j] = ifelse(length(temp) == 0, yes = 0, no = temp)
      }
      
      matches = FALSE
      correspondance_tvo = list()
      correspondance_amount = list()
      for (ii in 1:length(sections_parl)) {
        for (jj in 1:length(sections_pres)) {
          if (sections_parl[ii] %in% sections_pres[[jj]]) {
            correspondance_tvo[ii] = tvos_pres[[jj]]
            correspondance_amount[ii] = populations_parl[[ii]]
            matches = TRUE
          }
        }
      }
      
      if (matches == TRUE) {
        temp_tibble = tibble(tvo = unlist(correspondance_tvo), pop = unlist(correspondance_amount))
        temp_tibble = temp_tibble %>% group_by(tvo) %>% summarise(sum = sum(pop))
        corresponding_pres_tvo[i] = temp_tibble$tvo[which(temp_tibble$sum == max(temp_tibble$sum))]
      } else {
        corresponding_pres_tvo[[i]] = NA
      }
      
    } else {
      corresponding_pres_tvo[[i]] = NA
    }
  }
  
  tibble(tvo_parl = correspondance_parl_tvo, tvo_pres = corresponding_pres_tvo)
}

tvo_parl_pres_2012_2010 = compare_tvo_lists(tvo_parl = TVO_2012, tvo_pres = TVO_2010, population_df = population_2012)
tvo_parl_pres_2014_2014 = compare_tvo_lists(tvo_parl = TVO_2014_parl, tvo_pres = TVO_2014_pres, population_df = population_2014)

write_csv2(x = tvo_parl_pres_2012_2010, path = "tvo_parl_pres_2012_2010.csv")
write_csv2(x = tvo_parl_pres_2014_2014, path = "tvo_parl_pres_2014_2014.csv")
