##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 04 JULY 2019
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
require(tidyr)
# setwd("/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")
setwd("C:/Users/Isaiah Valdez/texty/isaiah/parliamentary_elections_forecasting_2019")
Sys.setlocale(locale = "ukrainian")

##### COLLECTION #####################
url_2014 = "https://www.cvk.gov.ua/pls/vnd2014/wp03252e6.html?PT001F01=910"
url_2012 = "https://www.cvk.gov.ua/pls/vnd2012/wp032ad94.html?PT001F01=900"

extract_cyr = function(string, pattern) {
  extraction = regmatches(x = string, regexpr(pattern = pattern, text = string, perl = T))
  extraction
}

scrape_tvo_results = function(url) {
  
  page = read_html(url)
  base_url = str_extract(string = url, pattern = "^(.*[\\/])")
  tvos_rel_hyper = page %>% html_nodes('.td2 .a1') %>% html_attr('href')
  tvos = paste(base_url, tvos_rel_hyper, sep = '') #links to each region
  
  all_frames = lapply(tvos, scrape_tvo_table)
  df = bind_rows(all_frames)
  df
}

scrape_tvo_table = function(url) {
  page = read_html(url)
  tables = page %>% html_nodes('table.t2')
  table = tables[[2]] %>% html_table
  colnames(table) = table[1,]
  table = table[-1,] #the table 'headers' are coded as the first row
  table[,'tvo'] = page %>% html_nodes('p > font') %>% html_text %>% str_extract(pattern = "\\d+")
  print(table[1,'tvo'])
  Sys.sleep(2.0)
  table 
}

candidates_2014_raw = scrape_tvo_results(url_2014)
candidates_2012_raw = scrape_tvo_results(url_2012)

clean_candidate_df = function(df) {
  for (i in 1:nrow(df)) {
    # fixes the spacing in the names
    df[i,1] =  gsub(pattern = "([а-яґєії])([А-ЯҐЄІЇ])", x = df[i,1], replacement = "\\1 \\2")
    # splits the info column
    df[i,"gender"] = ifelse(test = grepl(pattern = "народився", x = df[i,2]), yes = "M", "F")
    df[i,"age"] = as.integer(format(Sys.Date(), "%Y")) - as.integer(regmatches(x = df[i,2], regexpr("\\d{4}", df[i,2])))
    df[i,"job"] = extract_cyr(string = df[i,2], pattern = "(освіта.*,).(?=проживає)") %>%
      strsplit(split = ",") %>%
      extract2(1) %>% magrittr::extract(2) %>% trimws()
    df[i,"city"] = extract_cyr(string = df[i,2], pattern = "проживає (в|на) ((?!територ)[^,])+")
    print(i)
  }
  df
}

# write.csv(candidates_2014_raw, "candidates_2014_raw.csv", row.names = F, fileEncoding = "UTF-8")
# candidates_2014_raw_a = read_csv(file = "candidates_2014_raw.csv")

copy_2014 = clean_candidate_df(candidates_2014_raw)
copy_2012 = clean_candidate_df(candidates_2012_raw)
head(copy$age)
head(copy$gender)
head(copy$job)
head(copy$city)

write.csv(copy_2014, "candidates_info_2014.csv", row.names = F, fileEncoding = "UTF-8")
write.csv(copy_2012, "candidates_info_2012.csv", row.names = F, fileEncoding = "UTF-8")
