## Packages

library(XML)
library(RCurl)
library(Rspotify)
library(spotifyr)
library(stringr)
library(magrittr)
library(jsonlite)
library(networkD3)
library(rvest)
library(compare)
library(plyr)
library(dplyr)
library(ggplot2)
library(igraph)
library(data.table)

## Getting data ####################################################################

dir <- '../Data/'

top200 <- readRDS(paste(dir,'top200clean_01072017.Rda'))
artist <- readRDS(paste(dir,'artists_info_01072017.Rda'))
colnames(artist) <- c('artist', 'id', 'popularity', 'followers', 'genres', 'type')
top200complete <- merge(x = top200, y = artist, by = "artist", all.x = TRUE)
genres <- readRDS(paste(dir,'artists_genre_01072017.Rda'))
origin <- readRDS(paste(dir,'artists_area_mbid_01072017.Rda'))
countries <- read.csv(paste0(dir, "pais_sigla.csv"))
market.genre <- readRDS(paste(dir, 'market_genre_01072017.Rda'))

colnames(top200) <- c('mk', 'position', 'track', 'artist', 'streams', 'url')
colnames(artist) <- c('name', 'id', 'popularity', 'followers', 'genres', 'type')
colnames(genres) <- c('genre', 'artist')
colnames(origin) <- c('track', 'artist', 'url', 'id', 'area')
colnames(countries) <- c('mk', 'market'); countries$mk <- tolower(countries$mk)

colors <- c("#2B303B", "#848993", "#DEE3B5", "#BCD430", "#FF4E3A")
