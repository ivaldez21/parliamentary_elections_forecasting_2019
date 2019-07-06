##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 6 JULY 2019
# 
# Contact the author with any questions at:
#   isaiah.l.valdez@gmail.com

##### PACKAGES #######################
require(magrittr)
require(stringr)
require(dplyr)
require(readr)
require(stringi)
require(tidyr)
Sys.setlocale(locale = "ukrainian")

# setwd("/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")
setwd("C:/Users/Isaiah Valdez/texty/isaiah/parliamentary_elections_forecasting_2019/")
Sys.setlocale(locale = "ukrainian")

population_12_14 = read_csv('population_by_region_2012-2014.csv')
tvo_2012 = read_csv('TVO_2012.csv')
tvo_2010 = read_csv('TVO_2010.csv')
tvo_2014_parl = read.csv('TVO_2014_parl.csv', sep = ";", stringsAsFactors = F, encoding = "UTF-8")
tvo_2014_pres = read.csv('TVO_2014_pres.csv', sep = ";", stringsAsFactors = F, encoding = "UTF-8")
