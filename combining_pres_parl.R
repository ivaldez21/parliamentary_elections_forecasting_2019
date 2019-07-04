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

##### ADDING PARTIES TO PRES. ELECTION ###############################
# take parties from cand_parties_2014 and _2010 and add them to the results
pres_2010$party = ""
for (i in 1:nrow(pres_2010)) {
  pres_2010$party[i] = cand_parties_2010$Party[which(cand_parties_2010$Last == pres_2010$candidate[i])]
  print(i)
}
# repeat for 2014
pres_2014$party = ""
for (i in 1:nrow(pres_2014)) {
  pres_2014$party[i] = cand_parties_2014$Party[which(cand_parties_2014$Last == pres_2014$candidate[i])]
  print(i)
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
TVO_corresponding = read_csv(file = "TVO_corresponding.csv")
TVO_corresponding = TVO_corresponding[,c(1,2,3,4)]
names(TVO_corresponding) = c("TVO_2010", "desc_2010", "TVO_2012","desc_2012")

# add presidential 2010 TVO's to parliament 2012 TVO's
parl_2012$tvo_pres = 0
for (i in 1:nrow(parl_2012)) {
  parl_2012$tvo_pres[i] = TVO_corresponding$TVO_2010[which(TVO_corresponding$TVO_2012 == parl_2012$tvo[i])]
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
  print(i)
}

# write.csv(parl_2012, "parl_pres_2012_2010.csv")
# 
##### ADDING 2014 PRES to 2014 PARL #####################################
TVO_corresponding_2014 = read_csv(file = "TVO_corresponding_2014.csv")
TVO_corresponding_2014 = TVO_corresponding_2014[,c(6,7,4,5)]
names(TVO_corresponding_2014) = c("TVO_2014_pres", "desc_2014_pres", "TVO_2014_parl","desc_2014_parl")

# add presidential 2010 TVO's to parliament 2012 TVO's
parl_2014$tvo_pres = 0
for (i in 1:nrow(parl_2014)) {
  parl_2014$tvo_pres[i] = TVO_corresponding_2014$TVO_2014_pres[which(TVO_corresponding_2014$TVO_2014_parl == parl_2014$tvo[i])]
  print(i)
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
  print(i)
}

write.csv(parl_2014, "parl_pres_2014_2014.csv")
