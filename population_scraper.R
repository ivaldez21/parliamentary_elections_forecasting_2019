##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 5 JULY 2019
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

path = "C:\\Users\\Isaiah Valdez\\Downloads\\population\\zb_chnas_2011\\zb_chnas_2011.pdf"
path_2 = "population_2012.csv"
regions = "Автономна Республіка Крим\nВінницька область\nВолинська область\nДніпропетровська область\nДонецька область\nЖитомирська область\nЗакарпатська область\nЗапорізька область\nІвано-Франківська область\nКиївська область\nКіровоградська область\nЛуганська область\nЛьвівська область\nМиколаївська область\nОдеська область\nПолтавська область\nРівненська область\nСумська область\nТернопільська область\nХарківська область\nХерсонська область\nХмельницька область\nЧеркаська область\nЧернівецька область\nЧернігівська область\nм. Київ\nСевастополь (міськрада)"
regions = regions %>% strsplit(split = '\\n') %>% unlist()

population = read_csv(file = "population_2012.csv", col_names = "data")
population2 = read_csv(file = file.choose(), col_names = "data")

##### SEPERATE COLUMNS ################################
# eliminate columns that don't matter
indices_keep = grep(pattern = '\\d{2,} \\d{2,} \\d{2,}', x = population$data)
population = population[indices_keep,]
indices_drop = grep(pattern = 'населення', x = population$data, fixed = T)
population = population[-indices_drop,]

#extract the place column
place = str_extract(string = population$data, pattern = "[^\\d]+") %>% trimws()
pop = str_extract_all(string = population$data, pattern = "\\d+")
pop_2010 = sapply(pop, `[`, 1)
pop_2011 = sapply(pop, `[`, 2)
pop_2012 = sapply(pop, `[`, 3)

#check to make sure all districts are here
regions %in% place

#combine the data into a dataframe
df_pop = data.frame(place = place, pop_2010 = pop_2010, pop_2011 = pop_2011, pop_2012 = pop_2012, stringsAsFactors = F)

#clean names of places
df_pop$place = gsub(x = df_pop$place, pattern = "(м\\.)([^ ]+)", replacement = "\\1 \\2")


##### create new table which only has misto, rajon, and celo (district in another column)
total_population = df_pop[1,]
df_pop = df_pop[-1,]
place = list()
district = list()
df_pop$district = ""
for (i in 1:nrow(df_pop)) {
    if (df_pop$place[i] %in% regions) {
        current_district = df_pop$place[i]
        df_pop$distrct[i] = current_district
    } else {
      df_pop$distrct[i] = current_district
    }
}

write.csv(df_pop, "population_by_region_2010-2012.csv", row.names = F)

population2 = as.vector(population2)
population2 = population2[-c(1,2,3,4),]
population2 = population2[['data']]



new_list = list()
new_list_2012 = list()
new_list_2013 = list()
new_list_2014 = list()
j = 1
i = 1
while (i < length(population2)) {
  header = FALSE
  temp_row = population2[i]
  
  if (grepl(pattern = 'Міське населення', x = temp_row)) {
    # don't want this data
    i = i + 3
    header = FALSE
  } else if (grepl(pattern = 'Сільське населення', x = temp_row)) {
    # don't want this data
    i = i + 3
    header = FALSE
  } else if (grepl(pattern = 'район\\b', x = temp_row, perl = T)) {
    header = TRUE
  } else if (grepl(pattern = 'м[.]', x = temp_row)) {
    header = TRUE
  } else if (grepl(pattern = 'смт ', x = temp_row)) {
    header = TRUE
  } else if (grepl(pattern = 'міськрада ', x = temp_row)) {
    header = TRUE
  } else if (grepl(pattern = 'область ', x = temp_row)) {
    header = TRUE
  } else if (grepl(pattern = 'Автономна ', x = temp_row)) {
    header = TRUE
  }
  
  if (header == TRUE) {
    new_list[[j]] = temp_row
    new_list_2012[[j]] = population2[i+1]
    new_list_2013[[j]] = population2[i+2]
    new_list_2014[[j]] = population2[i+3]
    
    j = j + 1
    i = i + 4
    print(temp_row)
  } else {
    i = i + 1
  }
}
new_list      = unlist(new_list     )
new_list_2012 = unlist(new_list_2012)
new_list_2013 = unlist(new_list_2013)
new_list_2014 = unlist(new_list_2014)

combined_pop2 = data.frame(place = new_list, pop_2012 = new_list_2012, pop_2013 = new_list_2013, pop_2014 = new_list_2014)

for (i in 2:4) {
  combined_pop2[,i] = gsub(x = combined_pop2[,i], pattern = "\\s", replacement = "")
}

write.csv(combined_pop2, "population_by_region_2012-2014.csv", row.names = F)
