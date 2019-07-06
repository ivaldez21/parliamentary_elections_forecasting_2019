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
# require(googlesheets)
# setwd("/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")
setwd("C:/Users/Isaiah Valdez/texty/isaiah/parliamentary_elections_forecasting_2019/")
Sys.setlocale(locale = "Russian")

parl_2012 = read_csv(file = "parliament_elections_2012.csv")
parl_2014 = read_csv(file = "parliament_elections_2014.csv")
pres_2010 = read_csv(file = "presidential_2010_long.csv")
pres_2014 = read_csv(file = "presidential_2014_long.csv")
cand_parties_2010 = read_csv(file = "president-parties-2010.csv")
cand_parties_2014 = read_csv(file = "president-parties-2014.csv")

deputy_parties_2012 = levels(as.factor(parl_2012$partia))
deputy_parties_2014 = levels(as.factor(parl_2014$partia))
dp2012 = data.frame(party = deputy_parties_2012, year = 2012)
dp2014 = data.frame(party = deputy_parties_2014, year = 2014)
deputy_parties = rbind(dp2012, dp2014)
write.csv(deputy_parties, file = "deputy_candidate_parties.csv", fileEncoding = "UTF-8", row.names = F)

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

##### COMPARING PARTIES PARL/PRES 2010/2012/2014 ##########################
# 2010 presidential with 2012 parliamentary
parties_pres_2010 = levels(as.factor(pres_2010$party))
parties_parl_2012 = levels(as.factor(parl_2012$partia))
# 2014 presidential with 2014 parliamentary
parties_pres_2014 = levels(as.factor(pres_2014$party))
parties_parl_2014 = levels(as.factor(parl_2014$partia))

parties_pres_2010 %in% parties_parl_2012
parties_pres_2010

parties_pres_2014 %in% parties_parl_2014
parties_pres_2014

##### ADDING 2010 PRES to 2012 PARL #####################################
TVO_corresponding_2010_2012 = read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=0&single=true&output=csv")

# add presidential 2010 TVO's to parliament 2012 TVO's
parl_2012$tvo_pres = 0
for (i in 1:nrow(parl_2012)) {
  parl_2012$tvo_pres[i] = TVO_corresponding_2010_2012$TVO_pres[which(TVO_corresponding_2010_2012$TVO_parl == parl_2012$tvo[i])]
  print(i)
}

# add presidential 2010 results to parliament 2012 candidates with same party
parl_2012$pres_candidate = ""
parl_2012$pres_perc_for = 0
parl_2012$pres_votes_for = 0
for (i in 1:nrow(parl_2012)) {
  deputy_party = parl_2012$partia[i]
  if (deputy_party != "самовисування" & deputy_party %in% parties_pres_2010) {
    #get the matching president
    matching_pres = cand_parties_2010$Last[which(cand_parties_2010$Party == deputy_party)]
    parl_2012$pres_candidate[i] = matching_pres
    matching_pres_rows = pres_2010[which(pres_2010$candidate == matching_pres),c(1,3,4)]
    matching_pres_row = which(matching_pres_rows$tvo == parl_2012$tvo_pres[i])
    parl_2012$pres_perc_for[i] = matching_pres_rows$perc_for[matching_pres_row]
    parl_2012$pres_votes_for[i] = matching_pres_rows$votes_for[matching_pres_row]
  } else {
    parl_2012$pres_candidate[i] = "NA"
  }
  # print(i)
}

write.csv(parl_2012, "parl_pres_2012_2010.csv")
 
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
  deputy_party = parl_2014$partia[i]
  if (deputy_party != "самовисування" & deputy_party %in% parties_pres_2014) {
    #get the matching president
    matching_pres = cand_parties_2014$Last[which(cand_parties_2014$Party == deputy_party)]
    parl_2014$pres_candidate[i] = matching_pres
    matching_pres_rows = pres_2014[which(pres_2014$candidate == matching_pres),c(1,3,4)]
    matching_pres_row = which(matching_pres_rows$tvo == parl_2014$tvo_pres[i])
    if (length(matching_pres_row) != 0) {
      parl_2014$pres_perc_for[i] = matching_pres_rows$perc_for[matching_pres_row]
      parl_2014$pres_votes_for[i] = matching_pres_rows$votes_for[matching_pres_row]
      }
  } else {
    parl_2014$pres_candidate[i] = "NA"
  }
  # print(i)
}

write.csv(parl_2014, "parl_pres_2014_2014.csv")

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

funding_2013_combined = combine_funding(funding_2013)
funding_2012_combined = combine_funding(funding_2012)

##### ADDING FUNDING TO PARL ########################################
# function takes inputs of parl_df, funding_df(grouped), column to group by, column to append
add_funding = function(parl, funding_df, group_column, value_column) {
  parl$funding = 0
  for (i in 1:nrow(parl)) {
    funding_amt = funding_df[which(funding_df[,group_column] == parl$tvo[i]),value_column][[1]]
    parl$funding[i] = ifelse(length(funding_amt[[1]]) != 0, funding_amt, 0)
  
  }
  return(parl)
}

parl_2014 = add_funding(parl = parl_2014, funding_df = funding_2013_combined, group_column = "tvo", value_column = "amount")
parl_2012 = add_funding(parl = parl_2012, funding_df = funding_2012_combined, group_column = "tvo", value_column = "amount")

##### ADDING CANDIDATE INFO TO PARL ################################
candidate_info_2014 = read_csv(file = "candidates_info_2014.csv")
candidate_info_2012 = read_csv(file = "candidates_info_2012.csv")

parl_2014 = inner_join(x = parl_2014, y = candidate_info_2014, by = c("deputat" = "Прізвище, ім'я та по батькові кандидата в депутати", "tvo"))
parl_2012 = inner_join(x = parl_2012, y = candidate_info_2012, by = c("deputat" = "Прізвище, ім'я та по батькові кандидата в депутати", "tvo"))

##### ADDING DEMOGRAPHICS TO TVOS ############################################################
demo = read.csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSNxiVppRf_pyTzKIXI-NA-5ncgsmjr8EmKSNUlarYzpdVC12ToK80zDjDXRkWvw_lmF6ng-yoTRMvP/pub?gid=34744934&single=true&output=csv",
                encoding = "UTF-8", dec = ",", stringsAsFactors = F)
demo_2012 = demo[,c(1,3,5,6)]
demo_2014 = demo[,c(1,2,4,6)]

data_2010_2012 = inner_join(x = TVO_corresponding_2010_2012, y = demo_2012, by = "oblast")
data_2014_2014 = inner_join(x = TVO_corresponding_2014, y = demo_2014, by = "oblast")
demo_2012_tvo = data_2010_2012[,c(1,4,6,7,8)]
demo_2014_tvo = data_2014_2014[,c(1,4,6,7,8)]
write.csv(data_2010_2012, "data_2010_2012.csv", row.names = F)
write.csv(data_2014_2014, "data_2014_2014.csv", row.names = F)

levels(as.factor(data_2010_2012$oblast))
levels(as.factor(data_2014_2014$oblast))

##### ADDING DEMOGRAPHICS BY TVO TO PARL

parl_2014_copy = full_join(x = parl_2014, y = demo_2014_tvo, by = c("tvo" = "TVO_parl", "oblast"))
parl_2012_copy = full_join(x = parl_2012, y = demo_2012_tvo, by = c("tvo" = "TVO_parl", "oblast"))

typeof(parl_2014$funding)
