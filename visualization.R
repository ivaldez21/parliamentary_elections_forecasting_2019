### Visualiztion with leaflet #################
library(readr)
library(jsonlite)
library(leaflet)
library(geojsonio)
library(rgdal)
library(geojsonlint)
library(magrittr)
setwd("/Users/isaiahlawrencevaldez/Documents/GitHub/parliamentary_elections_forecasting_2019/isaiah")

data = read_csv("top_2_per_tvo.csv")

mymap = leaflet() %>%
  addTiles() %>% setView(lat = 49.272021, lng = 31.437523, zoom = 5)
mymap

tvos <- geojson_read(x = "polygons.geojson", method = "local", what = "sp")

tvos <- jsonlite::read_json("polygons.json")

tvos_copy = tvos %>% extract(1) %>% extract2(1) %>% extract(2) %>% use_series(features) %>% extract2(10)
tvos_copy$features[1]

geojson_write(input = tvos_copy, geometry = "polygon", file = "crimea.geojson")
