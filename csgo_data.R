# CSGO Data Analysis

# Supervised machine learning to predict rating for CSGO fantasy
library(tidyverse)

setwd("C:/Users/Hunter/OneDrive/Documents/Datasets/CSGO")
player <- read.csv("players.csv")

colnames(player)
player <- player %>% 
  select("player_name", "team", "opponent", "match_id", "best_of", "kills", 
         "assists", "deaths", "hs", "flash_assists", "kast", "kddiff", "adr", 
         "fkdiff")

results <- read.csv("results.csv")
str(results)

player_results <- player %>% 
  left_join(results)

head(player)
