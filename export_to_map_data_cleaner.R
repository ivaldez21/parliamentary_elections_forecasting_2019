# cleaner for export
library(readr)
library(dplyr)
library(magrittr)
Sys.setlocale(locale = "UTF-8")
setwd(dir = "/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")


data = read_csv("top_3_per_tvo.csv")

table(data$tvo)
table(data$gender)
sort(table(data$proposed))

data$proposed %<>% gsub(pattern = "\\s+", replacement = " ")
data$fraction %<>% gsub(pattern = "\\s+", replacement = " ")

parties = c("самовисування", "політична партія слуга народу",
            "політична партія європейська солідарність",
            "політична партія опозиційна платформа за життя",
            "політична партія голос",
            "політична партія всеукраїнське обєднання батьківщина",
            "політична партія всеукраїнське обєднання свобода",
            "політична партія опозиційний блок")

data$proposed_full = data$proposed

for (i in 1:nrow(data)) {
  if (data$proposed[i] %in% parties) {
    party_clean = switch(data$proposed[i],
                         "самовисування" = "самовисування",
                         "політична партія слуга народу" = "СН",
                         "політична партія європейська солідарність" = "ЄС",
                         "політична партія опозиційна платформа за життя" = "ОПЗЖ",
                         "політична партія голос" = "Голос",
                         "політична партія всеукраїнське обєднання батьківщина" = "ВОБ",
                         "політична партія всеукраїнське обєднання свобода" = "Свобода",
                         "політична партія опозиційний блок" = "ОПОБЛОК")
    data$proposed[i] = party_clean
  } else {
    data$proposed[i] = "Other"
  }
}
table(data$proposed)

write_csv(data, "top_3_per_tvo.csv")

