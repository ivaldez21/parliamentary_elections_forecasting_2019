##### METADATA #######################
# Author: Isaiah Valdez
# Project: Election forecasting with TEXTY.org.ua
# Date: created on 29 JUNE 2019
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
setwd("/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")
# setwd("C:/Users/Isaiah Valdez/texty/isaiah/parliamentary_elections_forecasting_2019/")
Sys.setlocale(locale = "UTF-8")

parl_2002 = read_csv(file = "parliament_elections_2002.csv")
parl_2012 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=741387941&single=true&output=csv")
parl_2014 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=30704990&single=true&output=csv")
parl_2019 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=359006821&single=true&output=csv")
pres_2010 = read_csv(file = "presidential_2010_long.csv")
pres_2014 = read_csv(file = "presidential_2014_long.csv")
pres_2019 = read_csv(file = "presidential_2019_long.csv")
cand_parties_2010 = read_csv(file = "president-parties-2010.csv")
cand_parties_2014 = read_csv(file = "president-parties-2014.csv")
cand_parties_2019 = read_csv(file = "president-parties-2019.csv")
cand_parties_2019$pres = gsub(pattern = "([^ ]+) ([^ ]+) [^ ]+", replacement = "\\1 \\2", x = cand_parties_2019$pres)

clean_cand_parties = function(df) {
  for (i in 1:nrow(df)) {
    df[i,2] = gsub(pattern = "[[:punct:]]", replacement = "", x = df[i,2])
    df[i,2] = tolower(df[i,2])
  }
  df
}
cand_parties_2010 %<>% clean_cand_parties()
cand_parties_2014 %<>% clean_cand_parties()
cand_parties_2019 %<>% clean_cand_parties()

# deputy_parties_2012 = levels(as.factor(parl_2012$partia))
# deputy_parties_2014 = levels(as.factor(parl_2014$partia))
# deputy_parties_2019 = levels(as.factor(parl_2019$partia))
# dp2012 = data.frame(party = deputy_parties_2012, year = 2012)
# dp2014 = data.frame(party = deputy_parties_2014, year = 2014)
# dp2019 = data.frame(party = deputy_parties_2019, year = 2019)
# deputy_parties = rbind(dp2012, dp2014)
# write.csv(deputy_parties, file = "deputy_candidate_parties.csv", fileEncoding = "UTF-8", row.names = F)

##### ADDING CANDIDATE INFO TO PARL ################################
candidate_info_2019 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=1504125909&single=true&output=csv")
candidate_info_2014 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=1830288841&single=true&output=csv")
candidate_info_2012 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=51255785&single=true&output=csv")
candidate_info_2019 = candidate_info_2019[,-c(3, 5, 7)]
candidate_info_2014 = candidate_info_2014[,-c(4, 6)]
candidate_info_2012 = candidate_info_2012[,-c(4, 5, 7)]
names(candidate_info_2014) = c("deputat", "info", "proposed", "registration", "tvo", "gender", "age", "birthday", "job", "city_living", "party")
names(candidate_info_2012) = c("deputat", "info", "proposed", "registration", "tvo", "gender", "age", "birthday", "job", "city_living", "party")
names(candidate_info_2019) = c("deputat", "info", "proposed", "registration", "tvo", "gender", "age", "birthday", "job", "city_living", "party")

parl_2014 = left_join(x = parl_2014, y = candidate_info_2014, by = c("deputat", "tvo"))
parl_2012 = left_join(x = parl_2012, y = candidate_info_2012, by = c("deputat", "tvo"))
parl_2019 = left_join(x = parl_2019, y = candidate_info_2019, by = c("deputat", "tvo"))

# # check dupes 
# get_dupes = function(parl_df) {
#   dupes = parl_df %>% group_by(deputat, tvo) %>% summarize(n = n()) %>% filter(n > 1)
#   dupes
#   for (i in 1:nrow(dupes)) {
#     print(grep(pattern = dupes$deputat[i], x = parl_df$deputat, value = T))
#     print("________________________________________________________________")
#   }
# }
# dupes_2019 = get_dupes(parl_2019)
# dupes_2014 = get_dupes(parl_2014)
# dupes_2012 = get_dupes(parl_2012)
table(duplicated(parl_2019))
table(duplicated(parl_2014))
table(duplicated(parl_2012))

##### ADDING PARTIES TO PRES. ELECTION ###############################
# take parties from cand_parties_2014 and _2010 and add them to the results
pres_2010$party = ""
for (i in 1:nrow(pres_2010)) {
  pres_2010$party[i] = cand_parties_2010$Party[which(cand_parties_2010$Last == pres_2010$candidate[i])]
  # print(i)
}
# repeat for 2014
pres_2014$party = ""
for (i in 1:nrow(pres_2014)) {
  pres_2014$party[i] = cand_parties_2014$Party[which(cand_parties_2014$Last == pres_2014$candidate[i])]
  # print(i)
}
# repeat for 2019
pres_2019$party = ""
for (i in 1:nrow(pres_2019)) {
  pres_2019$party[i] = cand_parties_2019$partia[which(cand_parties_2019$pres == pres_2019$candidate[i])]
  # print(i)
}

##### COMPARING PARTIES PARL/PRES 2010/2012/2014 ##########################
# 2010 presidential with 2012 parliamentary
parties_pres_2010 = levels(as.factor(pres_2010$party))
parties_parl_2012 = levels(as.factor(parl_2012$party))
# 2014 presidential with 2014 parliamentary
parties_pres_2014 = levels(as.factor(pres_2014$party))
parties_parl_2014 = levels(as.factor(parl_2014$party))
# 2019 presidential with 2019 parliamentary
parties_pres_2019 = levels(as.factor(pres_2019$party))
parties_parl_2019 = levels(as.factor(parl_2019$party))

# shows which pres parties were not in the parliament elections
parties_pres_2010[-which(parties_pres_2010 %in% parties_parl_2012)]
parties_pres_2014[-which(parties_pres_2014 %in% parties_parl_2014)]
parties_pres_2019[-which(parties_pres_2019 %in% parties_parl_2019)]
# missing only four super small parties that never got 1% in a single tvo

##### ADDING 2010 PRES to 2012 PARL #####################################
TVO_corresponding_2010_2012 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=0&single=true&output=csv")

# add presidential 2010 TVO's to parliament 2012 TVO's
parl_2012$tvo_pres = 0
for (i in 1:nrow(parl_2012)) {
  parl_2012$tvo_pres[i] = TVO_corresponding_2010_2012$TVO_pres[which(TVO_corresponding_2010_2012$TVO_parl == parl_2012$tvo[i])]
  # print(i)
}

# add presidential 2010 results to parliament 2012 candidates with same party
parl_2012$pres_candidate = ""
parl_2012$pres_perc_for = 0
parl_2012$pres_votes_for = 0
for (i in 1:nrow(parl_2012)) {
  deputy_party = parl_2012$proposed[i]
  if (deputy_party != "самовисування" & deputy_party %in% parties_pres_2010) {
    #get the matching president
    matching_pres = cand_parties_2010$Last[which(cand_parties_2010$Party == deputy_party)]
    parl_2012$pres_candidate[i] = matching_pres
    matching_pres_rows = pres_2010[which(pres_2010$candidate == matching_pres),c(1,3,4)]
    matching_pres_row = which(matching_pres_rows$tvo == parl_2012$tvo_pres[i])
    if (length(matching_pres_row) > 0) {
      parl_2012$pres_perc_for[i] = matching_pres_rows$perc_for[matching_pres_row]
      parl_2012$pres_votes_for[i] = matching_pres_rows$votes_for[matching_pres_row]
    } else {
      parl_2012$pres_candidate[i] = NA
    }
  } else {
    parl_2012$pres_candidate[i] = NA
  }
  # print(i)
}
# write.csv(parl_2012, "parl_pres_2012_2010.csv")
 
##### ADDING 2014 PRES to 2014 PARL #####################################
TVO_corresponding_2014 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=180283976&single=true&output=csv")

# add presidential 2010 TVO's to parliament 2012 TVO's
parl_2014$tvo_pres = 0
for (i in 1:nrow(parl_2014)) {
  parl_2014$tvo_pres[i] = TVO_corresponding_2014$TVO_pres[which(TVO_corresponding_2014$TVO_parl == parl_2014$tvo[i])]
  # print(i)
}

# add presidential 2014 results to parliament 2014 candidates with same party
parl_2014$pres_candidate = ""
parl_2014$pres_perc_for = 0
parl_2014$pres_votes_for = 0
for (i in 1:nrow(parl_2014)) {
  deputy_party = parl_2014$proposed[i]
  if (deputy_party != "самовисування" & deputy_party %in% parties_pres_2014) {
    #get the matching president
    matching_pres = cand_parties_2014$Last[which(cand_parties_2014$Party == deputy_party)]
    parl_2014$pres_candidate[i] = matching_pres
    matching_pres_rows = pres_2014[which(pres_2014$candidate == matching_pres),c(1,3,4)]
    matching_pres_row = which(matching_pres_rows$tvo == parl_2014$tvo_pres[i])
    if (length(matching_pres_row) != 0) {
      parl_2014$pres_perc_for[i] = matching_pres_rows$perc_for[matching_pres_row]
      parl_2014$pres_votes_for[i] = matching_pres_rows$votes_for[matching_pres_row]
    } else {
      parl_2012$pres_candidate[i] = NA
    }
  } else {
    parl_2014$pres_candidate[i] = "NA"
  }
  # print(i)
}
# write.csv(parl_2014, "parl_pres_2014_2014.csv")

##### 2019 TVO's #####################################################
# the parliamentary and presidential tvo's are identical for 2019

# add presidential 2010 TVO's to parliament 2012 TVO's
parl_2019$tvo_pres = parl_2019$tvo #they are identical

# add presidential 2019 results to parliament 2019 candidates with same party
parl_2019$pres_candidate = ""
parl_2019$pres_perc_for = 0
parl_2019$pres_votes_for = 0
for (i in 1:nrow(parl_2019)) {
  deputy_party = parl_2019$proposed[i]
  if (deputy_party != "самовисування" & deputy_party %in% parties_pres_2019) {
    #get the matching president
    matching_pres = cand_parties_2019$pres[which(cand_parties_2019$partia == deputy_party)]
    parl_2019$pres_candidate[i] = matching_pres
    matching_pres_rows = pres_2019[which(pres_2019$candidate == matching_pres),c(1,3,4)]
    matching_pres_row = which(matching_pres_rows$tvo == parl_2019$tvo_pres[i])
    if (length(matching_pres_row) != 0) {
      parl_2019$pres_perc_for[i] = matching_pres_rows$perc_for[matching_pres_row]
      parl_2019$pres_votes_for[i] = matching_pres_rows$votes_for[matching_pres_row]
    } else {
      parl_2012$pres_candidate[i] = NA
    }
  } else {
    parl_2019$pres_candidate[i] = NA
  }
  # print(i)
}

##### ADDING WINNERS ###################

find_winners = function(df, vote_column, tvo_column) {
  won = vector()
  for (i in 1:nrow(df)) {
    subset = df[which(df[[tvo_column]] == df[[tvo_column]][i]),]
    max_in_tvo = max(subset[[vote_column]])
    won[i] = ifelse(df[[vote_column]][i] == max_in_tvo, 1, 0)
  }
  won
}

parl_2014$winner = find_winners(df = parl_2014, vote_column = "perc_for", "tvo") == 1
parl_2012$winner = find_winners(df = parl_2012, vote_column = "perc_for", "tvo") == 1

table(parl_2012$tvo[which(parl_2012$winner == 1)])
table(parl_2014$tvo[which(parl_2014$winner == 1)])

##### ADDING DEMOGRAPHICS TO TVOS ############################################################
demo = read.csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=34744934&single=true&output=csv",
                encoding = "UTF-8", dec = ",", stringsAsFactors = F)
demo_2012 = demo[,c(1,3,5,6)]
demo_2014 = demo[,c(1,2,4,6)]

data_2010_2012 = inner_join(x = TVO_corresponding_2010_2012, y = demo_2012, by = "oblast")
data_2014_2014 = inner_join(x = TVO_corresponding_2014, y = demo_2014, by = "oblast")
demo_2012_tvo = data_2010_2012[,c(1,2,6,7,8)]
demo_2014_tvo = data_2014_2014[,c(1,2,6,7,8)]
demo_2019_tvo = demo_2014_tvo #just reusing this information....?
names(demo_2012_tvo) = c("oblast", "tvo", "average_age", "pop_change", "region_ideology")
names(demo_2014_tvo) = c("oblast", "tvo", "average_age", "pop_change", "region_ideology")
names(demo_2019_tvo) = c("oblast", "tvo", "average_age", "pop_change", "region_ideology")
write.csv(data_2010_2012, "data_2010_2012.csv", row.names = F)
write.csv(data_2014_2014, "data_2014_2014.csv", row.names = F)
# levels(as.factor(data_2010_2012$oblast))
# levels(as.factor(data_2014_2014$oblast))

##### ADDING DEMOGRAPHICS BY TVO TO PARL

parl_2014_copy = left_join(x = parl_2014, y = demo_2014_tvo, by = c("tvo", "oblast"))
parl_2012_copy = left_join(x = parl_2012, y = demo_2012_tvo, by = c("tvo", "oblast"))
parl_2019_copy = left_join(x = parl_2019, y = demo_2019_tvo, by = c("tvo", "oblast"))

# getting 92 duplicates for 2012...STILL! why? I have no idea...
table(duplicated(parl_2012_copy))
table(duplicated(parl_2014_copy))
table(duplicated(parl_2019_copy))
parl_2012_copy = parl_2012_copy[-which(duplicated(parl_2012_copy)),]
# parl_2019_copy = parl_2019_copy[-which(duplicated(parl_2019_copy)),] #no dupes
# parl_2014_copy = parl_2014_copy[-which(duplicated(parl_2014_copy)),] #no dupes

###### ADD COLUMN "PREVIOUSLY AN MP" ###################################
# # Run the below code just to download the results and create the csv once
# MP_urls = paste0("https://data.rada.gov.ua/ogd/mps/skl", 1:8,"/mps0", 1:8,"-data.csv")
# 
# MPs_all_convocation = bind_rows(lapply(MP_urls, read_csv))
# # MPs_all_convocation = MPs_all_convocation[,which(colSums(is.na(MPs_all_convocation)) == 0)]
# MPs_all_convocation = MPs_all_convocation[,c("convocation", "full_name", "birthday")]
# MPs_all_convocation$birthday = gsub(pattern = "0(\\d+)", replacement = "\\1", x = MPs_all_convocation$birthday)
# MPs_all_convocation$full_name = gsub(pattern = "'", replacement = "’", x = MPs_all_convocation$full_name)
# write.csv(MPs_all_convocation, "MPs_all_convocations.csv", row.names = F)
MPs_all_convocation = read_csv("MPs_all_convocations.csv")
# we will subset the mps dataset by convocation then search for match by birthdate/gender/name?

winner_2012 = parl_2012_copy[which(parl_2012_copy$winner == 1),]
winner_2014 = parl_2014_copy[which(parl_2014_copy$winner == 1),]

# in_both_dfs = levels(as.factor(winner_2012$deputat)) %in% levels(as.factor(MPs_all_convocation$full_name))
# in_both_dfs_2014 = levels(as.factor(winner_2014$deputat)) %in% levels(as.factor(MPs_all_convocation$full_name))
# table(in_both_dfs)
# table(in_both_dfs_2014)
# levels(as.factor(winner_2012$deputat))[!in_both_dfs] #these two results were "cancelled'
# levels(as.factor(winner_2014$deputat))[!in_both_dfs_2014]

#first need to clean the numerals from deputy names (were added for dupes)
#(and sometimes quotes are different in different ukrainian sources)
parl_2012_copy$deputat = gsub(pattern = "[[:punct:][:digit:]]", replacement = "", x = parl_2012_copy$deputat) %>% trimws()
parl_2014_copy$deputat = gsub(pattern = "[[:punct:][:digit:]]", replacement = "", x = parl_2014_copy$deputat) %>% trimws()
parl_2019_copy$deputat = gsub(pattern = "[[:punct:][:digit:]]", replacement = "", x = parl_2019_copy$deputat) %>% trimws()
MPs_all_convocation$full_name = gsub(pattern = "[[:punct:][:digit:]]", replacement = "", x = MPs_all_convocation$full_name) %>% trimws()
names(MPs_all_convocation)
names(parl_2012_copy)

# shows if they were a deputy in the last convocation (incumbent)
parl_2012_copy_previous = semi_join(x = parl_2012_copy,
                                    y = MPs_all_convocation[which(MPs_all_convocation$convocation == 6),],
                                    by = c("deputat" = "full_name", "birthday"))
parl_2014_copy_previous = semi_join(x = parl_2014_copy,
                                    y = MPs_all_convocation[which(MPs_all_convocation$convocation == 7),],
                                    by = c("deputat" = "full_name", "birthday"))
parl_2019_copy_previous = semi_join(x = parl_2019_copy,
                                    y = MPs_all_convocation[which(MPs_all_convocation$convocation == 8),],
                                    by = c("deputat" = "full_name", "birthday"))
# shows if they were EVER a deputy
parl_2012_copy_ever = semi_join(x = parl_2012_copy,
                                y = MPs_all_convocation[which(MPs_all_convocation$convocation %in% 1:6),],
                                by = c("deputat" = "full_name", "birthday"))
parl_2014_copy_ever = semi_join(x = parl_2014_copy,
                                y = MPs_all_convocation[which(MPs_all_convocation$convocation %in% 1:7),],
                                by = c("deputat" = "full_name", "birthday"))
parl_2019_copy_ever = semi_join(x = parl_2019_copy,
                                y = MPs_all_convocation[which(MPs_all_convocation$convocation %in% 1:8),],
                                by = c("deputat" = "full_name", "birthday"))

parl_2012_copy_previous = parl_2012_copy_previous[,c('deputat','birthday')]
parl_2014_copy_previous = parl_2014_copy_previous[,c('deputat','birthday')]
parl_2019_copy_previous = parl_2019_copy_previous[,c('deputat','birthday')]
parl_2012_copy_ever = parl_2012_copy_ever[,c('deputat','birthday')]
parl_2014_copy_ever = parl_2014_copy_ever[,c('deputat','birthday')]
parl_2019_copy_ever = parl_2019_copy_ever[,c('deputat','birthday')]

parl_2012_copy_previous$previous = 1
parl_2014_copy_previous$previous = 1
parl_2019_copy_previous$previous = 1
parl_2012_copy_ever$ever = 1
parl_2014_copy_ever$ever = 1
parl_2019_copy_ever$ever = 1

parl_2012_copy = left_join(x = parl_2012_copy, y = parl_2012_copy_previous, by = c("deputat", "birthday"))
parl_2014_copy = left_join(x = parl_2014_copy, y = parl_2014_copy_previous, by = c("deputat", "birthday"))
parl_2019_copy = left_join(x = parl_2019_copy, y = parl_2019_copy_previous, by = c("deputat", "birthday"))
parl_2012_copy = left_join(x = parl_2012_copy, y = parl_2012_copy_ever, by = c("deputat", "birthday"))
parl_2014_copy = left_join(x = parl_2014_copy, y = parl_2014_copy_ever, by = c("deputat", "birthday"))
parl_2019_copy = left_join(x = parl_2019_copy, y = parl_2019_copy_ever, by = c("deputat", "birthday"))

parl_2012_copy$previous = replace_na(parl_2012_copy$previous, replace = 0)
parl_2014_copy$previous = replace_na(parl_2014_copy$previous, replace = 0)
parl_2019_copy$previous = replace_na(parl_2019_copy$previous, replace = 0)
parl_2012_copy$ever = replace_na(parl_2012_copy$ever, replace = 0)
parl_2014_copy$ever = replace_na(parl_2014_copy$ever, replace = 0)
parl_2019_copy$ever = replace_na(parl_2019_copy$ever, replace = 0)

table(parl_2012_copy$previous) #around 152 are incumbent (convocation 6)
table(parl_2014_copy$previous) #around 157 are incumbent (convocation 7)
table(parl_2019_copy$previous) #around 197 are incumbent (convocation 8)
table(parl_2012_copy$ever) #around 245 were previously MP's (convocations 1-6)
table(parl_2014_copy$ever) #around 227 were previously MP's (convocations 1-7)
table(parl_2019_copy$ever) #around 247 were previously MP's (convocations 1-8)
# less in 2014(less because of lustration/outing after Yanukovich)
# for (i in nrow) 
#there were problems here because of "dupe" names in 2014, two candidates in same tvo, same name

##### COMBINING FUNDING ##############################################

funding_2013 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=357252681&single=true&output=csv")
funding_2012 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=1119820321&single=true&output=csv")

combine_funding = function(funding_df) {
  names(funding_df) = c("code", "region", "amount", "tvo")
  # drop the first column because we don't need it
  funding_df = funding_df[,-1]
  
  funding_df_grouped = funding_df %>%
    group_by(tvo) %>% 
    summarise_if(is.numeric, sum) %>%
    arrange(tvo)
  
  funding_df_grouped
}

funding_2013 = combine_funding(funding_2013)
funding_2012 = combine_funding(funding_2012)
funding_2019 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=2084930143&single=true&output=csv")
funding_2019 = funding_2019[,c("tvo", "amount")]
funding_2019$amount = funding_2019$amount*1000
hist(funding_2019$amount)
hist(funding_2013$amount)
hist(funding_2012$amount)

##### ADDING FUNDING TO PARL ########################################
# function takes inputs of parl_df, funding_df(grouped), column to group by, column to append
add_funding = function(parl, parl_previous = NULL, funding_df, group_column, value_column, criteria_column, year) {
  parl$funding = 0
  for (i in 1:nrow(parl)) {
    
    funding_amt = funding_df[which(funding_df[,group_column] == parl$tvo[i]),value_column][[1]]

    if (year != 2012) {
      current_tvo = parl$tvo[i]
      previous_winner = parl_previous$deputat[which(parl_previous$tvo == current_tvo & parl_previous$winner == 1)]
      previous_winner = ifelse(length(previous_winner) > 0, yes = previous_winner, no = NA)
      if (!is.na(previous_winner)) {
        # the funding only counts if the candidate was MP in this district "last year"
        if (parl[[criteria_column]][i] == 1 & parl$deputat[i] == previous_winner) {
          parl$funding[i] = ifelse(length(funding_amt) != 0, funding_amt, 0)
        } else {
          parl$funding[i] = 0
        }
      }
    } else if (year == 2012) {
      if (parl$party[i] == "партія регіонів") {
        parl$funding[i] = ifelse(length(funding_amt) != 0, funding_amt, 0)
      } else {
        parl$funding[i] = 0
      }
    }
  }
  parl
}

parl_2014_copy = add_funding(parl = parl_2014_copy, parl_previous = parl_2012_copy, funding_df = funding_2013, group_column = "tvo", value_column = "amount", criteria_column = "previous", year = 2014)
parl_2012_copy = add_funding(parl = parl_2012_copy, parl_previous = NULL,           funding_df = funding_2012, group_column = "tvo", value_column = "amount", criteria_column = "previous", year = 2012)
parl_2019_copy = add_funding(parl = parl_2019_copy, parl_previous = parl_2014_copy, funding_df = funding_2019, group_column = "tvo", value_column = "amount", criteria_column = "previous", year = 2019)

###### COMBINE THE TWO TESTS SETS ######################################
parl_2019_copy$winner = NA
names(parl_2012_copy) %in% names(parl_2014_copy)
names(parl_2019_copy) %in% names(parl_2012_copy)
names(parl_2014_copy) %in% names(parl_2019_copy)
length(names(parl_2012_copy)) == length(names(parl_2014_copy)) 
length(names(parl_2012_copy)) == length(names(parl_2019_copy))
combined_df = bind_rows(parl_2012_copy, parl_2014_copy, parl_2019_copy)

party_ideologies = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=1018797553&single=true&output=csv")

combined_df_copy = left_join(x = combined_df, y = party_ideologies, by = c("partia" = "party", "year"))
combined_df_copy$partia = NULL

write_csv(x = combined_df_copy, path = "combined_results.csv")
train_df = combined_df_copy[which(combined_df_copy$year %in% c(2012,2014)),]
predict_df = combined_df_copy[which(combined_df_copy$year == 2019),]

write_csv(x = train_df, path = "train_df.csv")
write_csv(x = predict_df, path = "predict_df.csv")

