library(tidyverse)
library(DescTools)

setwd("C:/Users/Hunter/OneDrive/Documents/Datasets/CSGO")

player_match <- read.csv("online_LAN_major_Oct-Feb-2021-2022.csv")

fantasy <- player_match %>% 
  mutate(win = ifelse(Map.Wins > Opponent.Map.Wins, 1, 0),
         player_points = floor(0.5*(Rating2.0 - 1) * 100),
         fantasy_points = ifelse(win == 1, 6 + player_points, player_points - 3),
         log_hltv_rank = 1 + log(as.integer(HLTV.Rank)),
         log_opp_hltv_rank = 1 + log(as.integer(Opponent.HLTV.Rank)),
         rank_diff = as.integer(HLTV.Rank) - as.integer(Opponent.HLTV.Rank),
         hltv_rank = as.integer(HLTV.Rank),
         opp_hltv_rank = as.integer(Opponent.HLTV.Rank),
         kast = as.numeric(sub("%", "", KAST))
  ) %>% 
  select(-KAST, -HLTV.Rank, -Opponent.HLTV.Rank, -Date,-Opponent,
         -Map.Wins, -Opponent.Map.Wins, -KD)

player <- fantasy %>% 
  group_by(Player) %>% 
  summarise(avg_player = mean(player_points),
            avg_fantasy = mean(fantasy_points),
            win_percentage = mean(win),
            median_opp_rank = median(opp_hltv_rank),
            matches_played = n()) %>% 
  filter(matches_played > 3) %>% 
  arrange(desc(avg_fantasy))

## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##

players <- c("karrigan", "rain", "Twistzz", "ropz","broky",
             "apEX", "dupreeh", "Magisk", "ZywOo", "misutaaa",
             "buster", "FL1T", "Qikert", "Jame", "YEKINDAR",
             "arT", "yuurih", "KSCERATO", "saffee", "drop",
             "Snappi", "Maden", "dycha", "hades", "Spinx",
             "raalz", "Spiidi", "slaxz-", "Marix", "Staehr")

prices <- c(173000, 201000, 211000, 217000, 217000, 
            193000, 202000, 201000, 240000, 187000,
            188000, 204000, 195000, 216000, 220000,
            192000, 206000, 217000, 227000, 177000,
            176000, 187000, 196000, 190000, 206000,
            174000, 185000, 204000, 192000, 191000)

teams <- c("FaZe", "FaZe", "FaZe", "FaZe", "FaZe",
           "Vitality","Vitality", "Vitality", "Vitality", "Vitality",
           "Virtus.pro", "Virtus.pro", "Virtus.pro", "Virtus.pro", "Virtus.pro",
           "FURIA", "FURIA", "FURIA", "FURIA", "FURIA",
           "ENCE", "ENCE", "ENCE", "ENCE", "ENCE",
           "Sprout", "Sprout", "Sprout", "Sprout", "Sprout")

pl_groupb <- data.frame(players, prices, n = 1:30)

pl_groupb <- pl_groupb %>% 
  left_join(player, by = c("players" = "Player"))

pl_groupb[, 9] <- teams

combo <- as.data.frame(CombSet(players, 5, repl=FALSE, ord=FALSE))
combo[, 6:14] <- rep(0, nrow(combo))

names(combo) <- c("p1", "p2", "p3", "p4", "p5", 
                  "tot_score", "tot_cost", "opp_rank", "win_percent",
                  "team1", "team2", "team3", "team4", "team5")

for (i in 1:nrow(combo)) {
  
  if (i %% 1400 == 0) {
    progress = i / nrow(combo)
    print(str_glue("{progress}"))
  }
  
  score <- 0
  cost <- 0
  opp_rank <- 0
  win_percent <- 0
  for (j in 1:5) {
    index = which(pl_groupb$players == combo[i, j])
    score <- score + pl_groupb[index, 5]
    cost <- cost + pl_groupb[index, 2]
    opp_rank <- opp_rank + pl_groupb[index, 6]
    win_percent <- win_percent + pl_groupb[index, 7]
    combo[i, 9+j] <- pl_groupb[index, 9]
  }
  combo[i, 6] <- score
  combo[i, 7] <- cost
  combo[i, 8] <- (opp_rank / 5)
  combo[i, 9] <- (win_percent / 5)
}

combo_temp <- combo %>% 
  mutate(adjusted_score = tot_score - (sqrt(opp_rank)) + (sqrt(win_percent))) %>% 
  filter(tot_cost < 1000000)

invalid_teams <- list()

for (i in 1:nrow(combo_temp)) {
  vec <- c(combo_temp[i, 10], combo_temp[i, 11], combo_temp[i, 12], combo_temp[i, 13], combo_temp[i, 14])
  t <- table(vec)
  for (j in 1:length(t)) {
    if (t[j] > 2) {
      invalid_teams[length(invalid_teams)+1] <- i
    }
  }
}

vec <- unlist(invalid_teams)
combo_temp <- combo_temp[-vec, ]

best_combo <- combo_temp %>% 
  arrange(desc(adjusted_score))

## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##

players_a <- c("es3tag", "REZ", "hampus", "Plopski", "phzy",
               "JACKZ", "NiKo", "huNter-", "Aleksib", "m0NESY",
               "NickelBack", "Krad", "Lack1", "El1an", "Forester",
               "KRIMZ", "ALEX", "poizon", "Peppzor", "mezii",
               "NBK-", "dexter", "frozen", "torzsi", "Bymas",
               "ap0c", "sterling", "HaZR", "Liki", "SaVage")

teams_a <- c("NIP", "NIP", "NIP", "NIP", "NIP",
           "G2","G2", "G2", "G2", "G2",
           "Entropiq", "Entropiq", "Entropiq", "Entropiq", "Entropiq",
           "fnatic", "fnatic", "fnatic", "fnatic", "fnatic",
           "MOUZ", "MOUZ", "MOUZ", "MOUZ", "MOUZ",
           "LookingForOrg", "LookingForOrg", "LookingForOrg", "LookingForOrg", "LookingForOrg")

pl_groupa <- data.frame(players_a, n = 1:30)

pl_groupa <- pl_groupa %>% 
  left_join(player, by = c("players_a" = "Player"))

pl_groupa[, 8] <- teams_a

combo_a <- as.data.frame(CombSet(players_a, 5, repl=FALSE, ord=FALSE))
combo_a[, 6:13] <- rep(0, nrow(combo_a))

names(combo_a) <- c("p1", "p2", "p3", "p4", "p5", 
                  "tot_score","opp_rank", "win_percent",
                  "team1", "team2", "team3", "team4", "team5")

for (i in 1:nrow(combo_a)) {
  
  if (i %% 1400 == 0) {
    progress = i / nrow(combo_a)
    print(str_glue("{progress}"))
  }
  
  score <- 0
  opp_rank <- 0
  win_percent <- 0
  for (j in 1:5) {
    index = which(pl_groupa$players == combo_a[i, j])
    score <- score + pl_groupa[index, 4]
    opp_rank <- opp_rank + pl_groupa[index, 6]
    win_percent <- win_percent + pl_groupa[index, 5]
    combo[i, 8+j] <- pl_groupa[index, 8]
  }
  combo_a[i, 6] <- score
  combo_a[i, 7] <- (opp_rank / 5)
  combo_a[i, 8] <- (win_percent / 5)
}

combo_temp_a <- combo %>% 
  mutate(adjusted_score = tot_score - (sqrt(opp_rank)) + (sqrt(win_percent))) %>% 
  filter(tot_cost < 1000000)

invalid_teams <- list()

for (i in 1:nrow(combo_temp_a)) {
  vec <- c(combo_temp_a[i, 10], combo_temp_a[i, 11], combo_temp_a[i, 12], combo_temp_a[i, 13], combo_temp_a[i, 14])
  t <- table(vec)
  for (j in 1:length(t)) {
    if (t[j] > 2) {
      invalid_teams[length(invalid_teams)+1] <- i
    }
  }
}

vec <- unlist(invalid_teams)
combo_temp <- combo_temp[-vec, ]

best_combo <- combo_temp %>% 
  arrange(desc(adjusted_score))

## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##

players <- c("tabseN", "tiziaN", "syrsoN", "faveN","Krimbo",
             "HEN1", "TACO", "dumau", "b4rtin", "latto",
             "alex", "mopoz", "DeathZz", "SunPayus", "dav1g",
             "shox", "nitr0", "NAF", "EliGE", "oSee",
             "ptr", "djay", "Jonji", "ben1337", "PwnAlone",
             "HObbit", "interz", "Ax1Le", "sh1ro", "nafany")

prices <- c(207000, 176000, 208000, 223000, 223000, 
            196000, 169000, 208000, 187000, 196000,
            192000, 190000, 181000, 205000, 159000,
            180000, 171000, 208000, 203000, 225000,
            196000, 199000, 208000, 182000, 219000,
            219000, 189000, 224000, 247000, 196000)

teams <- c("BIG", "BIG", "BIG", "BIG", "BIG",
           "GODSENT","GODSENT", "GODSENT", "GODSENT", "GODSENT",
           "Movistar Riders", "Movistar Riders", "Movistar Riders", "Movistar Riders", "Movistar Riders",
           "Liquid", "Liquid", "Liquid", "Liquid", "Liquid",
           "Party Astronauts", "Party Astronauts", "Party Astronauts", "Party Astronauts", "Party Astronauts",
           "Gambit", "Gambit", "Gambit", "Gambit", "Gambit")

pl_groupc <- data.frame(players, prices, n = 1:30)

pl_groupc <- pl_groupc %>% 
  left_join(player, by = c("players" = "Player"))

pl_groupc[, 9] <- teams

combo_c <- as.data.frame(CombSet(players, 5, repl=FALSE, ord=FALSE))
combo_c[, 6:14] <- rep(0, nrow(combo_c))

names(combo_c) <- c("p1", "p2", "p3", "p4", "p5", 
                  "tot_score", "tot_cost", "opp_rank", "win_percent",
                  "team1", "team2", "team3", "team4", "team5")

for (i in 1:nrow(combo_c)) {
  
  if (i %% 1400 == 0) {
    progress = i / nrow(combo_c)
    print(str_glue("{progress}"))
  }
  
  score <- 0
  cost <- 0
  opp_rank <- 0
  win_percent <- 0
  for (j in 1:5) {
    index = which(pl_groupc$players == combo_c[i, j])
    score <- score + pl_groupc[index, 5]
    cost <- cost + pl_groupc[index, 2]
    opp_rank <- opp_rank + pl_groupc[index, 6]
    win_percent <- win_percent + pl_groupc[index, 7]
    combo_c[i, 9+j] <- pl_groupc[index, 9]
  }
  combo_c[i, 6] <- score
  combo_c[i, 7] <- cost
  combo_c[i, 8] <- (opp_rank / 5)
  combo_c[i, 9] <- (win_percent / 5)
}

combo_temp_c <- combo_c %>% 
  mutate(adjusted_score = tot_score - (sqrt(opp_rank)) + (sqrt(win_percent))) %>% 
  filter(tot_cost < 1000000)

invalid_teams_c <- list()

for (i in 1:nrow(combo_temp_c)) {
  vec <- c(combo_temp_c[i, 10], combo_temp_c[i, 11], combo_temp_c[i, 12], combo_temp_c[i, 13], combo_temp_c[i, 14])
  t <- table(vec)
  for (j in 1:length(t)) {
    if (t[j] > 2) {
      invalid_teams_c[length(invalid_teams_c)+1] <- i
    }
  }
}

vec <- unlist(invalid_teams_c)
combo_temp_c <- combo_temp_c[-vec, ]

best_combo_c <- combo_temp_c %>% 
  arrange(desc(adjusted_score))

