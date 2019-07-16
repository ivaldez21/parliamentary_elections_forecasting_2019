##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 28 JUNE 2019
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
setwd("/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")


##### COLLECTION #####################
url = "https://www.cvk.gov.ua/pls/vp2010/wp335f0d8.html?PT001F01=700"

# all needed information is available one level deep, i.e. do not need to open
# the link to the each TVO.

scrape_tvo_results = function(url) {
  
  page = read_html(url)
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
  table = tables[[1]] %>% html_table
  colnames(table) = table[1,]
  table = table[-1,] #the table 'headers' are coded as the first row
  table[,'tvo'] = page %>% html_nodes('td.td0') %>% html_text %>% str_extract(pattern = "\\d+")
  print(table[1,'tvo'])
  table 
}

presidential_2010_full = scrape_tvo_results(url)

##### SAVE DATA ######################################
write.csv(presidential_2010_full, "presidential_2010_full.csv", row.names = F)
presidential_2010_full = read_csv("presidential_2010_full.csv")
# our data is grouped by TVO, so group by TVO all columns
presidential_2010_full[,1] = as.character(presidential_2010_full[,1])
presidential_2010 = presidential_2010_full %>%
  group_by(tvo) %>% 
  summarise_if(is.numeric, sum)
presidential_2010[226,1] = "Zakordony"
write.csv(presidential_2010, "presidential_2010.csv", row.names = F)

##### LOAD DATA ######################################
presidential_2010 = read_csv("presidential_2010.csv")
presidential_2010_full = read_csv("presidential_2010_full.csv")

##### CONVERSION #####################################
# the data from the table is in wide format
# the data we need, like the parliamentary results,
# are in long format with extra data.
presidential_2010_long = gather(data = presidential_2010, key = "candidate", value = "votes_for", 5:22)
# create a copy of the table with percentages instead of total num votes
presidential_2010[,4:22] = sapply(presidential_2010[,4:22], function(x) {
  x*100 / presidential_2010[,3]
})
presidential_2010_long_perc = gather(data = presidential_2010, key = "candidate", value = "votes_for", 5:22)

presidential_2010_long_perc = presidential_2010_long_perc[,c('tvo', 'candidate', 'votes_for')]
presidential_2010_long = presidential_2010_long[,c('tvo', 'candidate', 'votes_for')]
presidential_2010 = inner_join(x = presidential_2010_long_perc, y = presidential_2010_long, by = c('tvo', 'candidate'))
names(presidential_2010) = c("tvo", "candidate", "perc_for", "votes_for")
write.csv(presidential_2010, "presidential_2010_long.csv", row.names = F)

####### REPEAT FOR 2014 #############################
presidential_2014_full = read_csv("https://github.com/OPORA/electiondata/raw/master/election_results/president_2014_all_results.csv")
presidential_2014_full = presidential_2014_full[,-c(1,2,4,5,6,7,8,9,10,11,12,14)]
# our data is grouped by TVO, so group by TVO all columns
presidential_2014 = presidential_2014_full %>%
  group_by(District) %>% 
  summarise_if(is.numeric, sum)
# the list is shorter than 225 because it excludes non-government controlled territories 
presidential_2019_full = read_csv("https://github.com/OPORA/electiondata/raw/master/election_results/President_2019_1_round.csv")
presidential_2019_full = presidential_2019_full[,-c(1,2,4:19,21,61)]
# our data is grouped by TVO, so group by TVO all columns
presidential_2019 = presidential_2019_full %>%
  group_by(okrug) %>% 
  summarise_if(is.numeric, sum)

write.csv(presidential_2014, "presidential_2014.csv", row.names = F)
write.csv(presidential_2014_full, "presidential_2014_full.csv", row.names = F)
write.csv(presidential_2019, "presidential_2019.csv", row.names = F)
write.csv(presidential_2019_full, "presidential_2019_full.csv", row.names = F)
##### LOAD DATA ######################################
presidential_2014 = read_csv("presidential_2014.csv")
presidential_2014_full = read_csv("presidential_2014_full.csv")
presidential_2019 = read_csv("presidential_2019.csv")
presidential_2019_full = read_csv("presidential_2019_full.csv")

##### CONVERSION #####################################
# the data from the table is in wide format
# the data we need, like the parliamentary results,
# are in long format with extra data.
presidential_2014_long = gather(data = presidential_2014, key = "candidate", value = "votes_for", 3:23)
names(presidential_2014_long) = c("tvo", "total_votes", "candidate", "votes_for")
# create a copy of the table with percentages instead of total num votes
presidential_2014[,3:23] = sapply(presidential_2014[,3:23], function(x) {
  x*100 / presidential_2014[,2]
})
presidential_2014_long_perc = gather(data = presidential_2014, key = "candidate", value = "votes_for", 3:23)
names(presidential_2014_long_perc) = c("tvo", "total_votes", "candidate", "votes_for")

presidential_2014_long_perc = presidential_2014_long_perc[,c('tvo', 'candidate', 'votes_for')]
presidential_2014_long = presidential_2014_long[,c('tvo', 'candidate', 'votes_for')]
presidential_2014 = inner_join(x = presidential_2014_long_perc, y = presidential_2014_long, by = c('tvo', 'candidate'))
names(presidential_2014) = c("tvo", "candidate", "perc_for", "votes_for")
write.csv(presidential_2014, "presidential_2014_long.csv", row.names = F)

# presidential 2019
presidential_2019_long = gather(data = presidential_2019, key = "candidate", value = "votes_for", 3:41)
names(presidential_2019_long) = c("tvo", "total_votes", "candidate", "votes_for")
# create a copy of the table with percentages instead of total num votes
presidential_2019[,3:41] = sapply(presidential_2019[,3:41], function(x) {
  x*100 / presidential_2019[,2]
})
presidential_2019_long_perc = gather(data = presidential_2019, key = "candidate", value = "votes_for", 3:41)
names(presidential_2019_long_perc) = c("tvo", "total_votes", "candidate", "votes_for")

presidential_2019_long_perc = presidential_2019_long_perc[,c('tvo', 'candidate', 'votes_for')]
presidential_2019_long = presidential_2019_long[,c('tvo', 'candidate', 'votes_for')]
presidential_2019 = inner_join(x = presidential_2019_long_perc, y = presidential_2019_long, by = c('tvo', 'candidate'))
names(presidential_2019) = c("tvo", "candidate", "perc_for", "votes_for")

# The names are messed up, so split them
presidential_2019$candidate = gsub(pattern = "([а-яґєії])([А-ЯҐЄІЇ])", x = presidential_2019$candidate, replacement = "\\1 \\2")

write.csv(presidential_2019, "presidential_2019_long.csv", row.names = F)

