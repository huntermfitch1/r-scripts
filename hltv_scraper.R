library(rvest)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(DescTools)

scrape_match <- function(s, href) {
  tourney_url <- str_glue("{base_url}{href}")
  tourney_page <- s %>% 
    jump_to(tourney_url) # navigates to finished event page
  
  title <- tourney_page %>% 
    html_element("body > div.bgPadding > div > div.colCon > div.contentCol > div.event-page > div.event-hub > a > h1") %>% 
    html_attr("h1")
  
  results_page <- tourney_page %>% 
    html_element("body > div.bgPadding > div > div.colCon > div.leftCol > div.event-sidebar > div > div:nth-child(2) > div.sidebar-second-level > div > a:nth-child(1)")

  href2 <- html_attr(results_page, "href")
  tourney_url <- str_glue("{base_url}{href2}")
  tourney_page <- s %>%
    jump_to(tourney_url) # navigates to finished event page
  
  matches <- tourney_page %>% 
    html_element("body > div.bgPadding > div > div.colCon > div.contentCol > div.results > div.results-holder > div") %>% 
    html_elements(".result-con")
  
  df <- data.frame(date=character(),
                   player_name=character(),
                   team=character(),
                   opponent=character(),
                   hltv_rank=character(),
                   opponent_hltv_rank=character(),
                   mapwins=character(),
                   maplosses=character(),
                   kd=character(),
                   adr=double(),
                   kast=character(),
                   rating=double())
  
  for(node in matches) {
    match <- node %>% 
      html_element(".a-reset")
    href3 <- html_attr(match, "href")
    match_url <- str_glue("{base_url}{href3}")
    match <- s %>% 
      jump_to(match_url)
    
    default <- match %>% 
      html_element("body > div.bgPadding > div > div.colCon > div.contentCol > div.match-page > div.g-grid.maps > div.col-6.col-7-small > div.flexbox-column") %>% 
      html_elements(".mapname") %>% 
      html_text()
    
    if(default[1] == "Default") {
      next
    }
    
    date <- match %>% 
      html_element("body > div.bgPadding > div > div.colCon > div.contentCol > div.match-page > div.standard-box.teamsBox > div.timeAndEvent > div.date") %>% 
      html_text()
    
    teams <- match %>% 
      html_elements(".teamName") %>% 
      html_text()
    
    team1 <- teams[1]
    team2 <- teams[2]
    
    team1_map_wins <- match %>%
      html_element("body > div.bgPadding > div > div.colCon > div.contentCol > div.match-page > div.standard-box.teamsBox > div:nth-child(1) > div > div") %>% 
      html_text()
    
    team2_map_wins <- match %>%
      html_element("body > div.bgPadding > div > div.colCon > div.contentCol > div.match-page > div.standard-box.teamsBox > div:nth-child(3) > div > div") %>% 
      html_text()
    
    team1_hltv_rank <- match %>% 
      html_element("body > div.bgPadding > div > div.colCon > div.contentCol > div.match-page > div.lineups > div > div:nth-child(1) > div.box-headline.flex-align-center > div.teamRanking > a") %>% 
      html_text()
    
    team2_hltv_rank <- match %>% 
      html_element("body > div.bgPadding > div > div.colCon > div.contentCol > div.match-page > div.lineups > div > div:nth-child(3) > div.box-headline.flex-align-center > div.teamRanking > a") %>% 
      html_text()
    
    stats <- match %>% 
      html_element(".matchstats") %>% 
      html_element("#all-content") %>% 
      html_children()
    
    print(str_glue("{date} --"))
    print(str_glue("{team1} ({team1_hltv_rank}) vs {team2} ({team2_hltv_rank})\n"))
    
    stats_team1 <- as.data.frame(stats[1] %>% html_table(header = TRUE))
    for (row in 1:nrow(stats_team1)) {
      temp <- data.frame(date=date,
                         player_name=stats_team1[row, 1],
                         team=team1,
                         opponent=team2,
                         hltv_rank=team1_hltv_rank,
                         opponent_hltv_rank=team2_hltv_rank,
                         mapwins=team1_map_wins,
                         maplosses=team2_map_wins,
                         kd=stats_team1[row, 2],
                         adr=stats_team1[row, 4],
                         kast=stats_team1[row, 5],
                         rating=stats_team1[row, 6])
      df <- rbind(df, temp)
    }
    
    stats_team2 <- as.data.frame(stats[4] %>% html_table(header = TRUE))
    for (row in 1:nrow(stats_team2)) {
      temp <- data.frame(date=date,
                         player_name=stats_team2[row, 1],
                         team=team2,
                         opponent=team1,
                         hltv_rank=team2_hltv_rank,
                         opponent_hltv_rank=team1_hltv_rank,
                         mapwins=team2_map_wins,
                         maplosses=team1_map_wins,
                         kd=stats_team2[row, 2],
                         adr=stats_team2[row, 4],
                         kast=stats_team2[row, 5],
                         rating=stats_team2[row, 6])
      df <- rbind(df, temp)
    }
    
    Sys.sleep(0.5)
    
  }
  
  return(df)
  
}

# forming session to leaf through HLTV pages
base_url <- 'https://www.hltv.org'
s <- session(str_glue("{base_url}/events/archive?eventType=MAJOR&eventType=INTLLAN&eventType=ONLINE&prizeMin=20000&prizeMax=2000000"))

months <- s %>% 
  html_elements("body > div.bgPadding > div > div.colCon > div.contentCol > div.events-page > div.events-month")

df2 <- data.frame(date=character(),
                 player_name=character(),
                 team=character(),
                 opponent=character(),
                 hltv_rank=character(),
                 opponent_hltv_rank=character(),
                 mapwins=character(),
                 maplosses=character(),
                 kd=character(),
                 adr=double(),
                 kast=character(),
                 rating=double())


for (i in 1:length(months)) {

  events <- months[i] %>% 
    html_elements('a')
  
  print(length(events))
  
  for (i in 3:length(events)) {
    print(i)
    res <- scrape_match(s, html_attr(events[i], "href"))
    df2 <- rbind(df2, res)
  }
}

df2 <- df2 %>% 
  na.omit()

fixed_names <- rep(NA, nrow(df2))
for (i in 1:nrow(df2)) {
  fixed_names[i] <- str_split(df2[i,2], "'")[[1]][2]
}

df2 <- cbind(df2, fixed_names)

df2 <- df2 %>% 
  mutate(mapwins = as.integer(mapwins),
         opp_mapwins = as.integer(maplosses))

fixed_map_wins <- rep(NA, nrow(df2))
fixed_map_losses <- rep(NA, nrow(df2))
for (i in 1:nrow(df2)) {
  if (df2[i, 7] >= 16 & df2[i, 7] > df2[i, 14]) { # team1 wins best of one
    fixed_map_wins[i] <- 1
    fixed_map_losses[i] <- 0
  }
  else if (df2[i, 14] >= 16 & df2[i, 14] > df2[i, 7]) {
    fixed_map_wins[i] <- 0
    fixed_map_losses[i] <- 1
  }
  else {
    fixed_map_wins[i] <- df2[i, 7]
    fixed_map_losses[i] <- df2[i, 14]
  }
}

df2 <- cbind(df2, fixed_map_wins)
df2 <- cbind(df2, fixed_map_losses)

vec <- df2[, "hltv_rank"]
vec <- str_split(vec, "#", simplify = TRUE)

vec2 <- df2[, "opponent_hltv_rank"]
vec2 <- str_split(vec2, "#", simplify = TRUE)

df2 <- cbind(df2, vec[, 2])
df2 <- cbind(df2, vec2[, 2])

df2 <- df2 %>% 
  select("date", "fixed_names", "team", "opponent", "vec[, 2]", "vec2[, 2]",
         "fixed_map_wins", "fixed_map_losses", "kd", "adr", "kast", "rating")

names(df2) <- c("Date", "Player", "Team", "Opponent", "HLTV Rank",
                "Opponent HLTV Rank", "Map Wins", "Opponent Map Wins",
                "KD", "ADR", "KAST", "Rating2.0")

fantasy <- df2 %>% 
  mutate(win = ifelse(`Map Wins` > `Opponent Map Wins`, 1, 0),
         player_points = floor(0.5*(Rating2.0 - 1) * 100),
         fantasy_points = ifelse(win == 1, 6 + player_points, player_points - 3),
         log_hltv_rank = 1 + log(as.integer(`HLTV Rank`)),
         log_opp_hltv_rank = 1 + log(as.integer(`Opponent HLTV Rank`)),
         rank_diff = as.integer(`HLTV Rank`) - as.integer(`Opponent HLTV Rank`),
         hltv_rank = as.integer(`HLTV Rank`),
         opp_hltv_rank = as.integer(`Opponent HLTV Rank`),
         kast = as.numeric(sub("%", "", KAST))
         ) %>% 
  select(-KAST, -`HLTV Rank`, -`Opponent HLTV Rank`, -Date, -Team, -Opponent,
         -`Map Wins`, -`Opponent Map Wins`, -KD)

player <- fantasy %>% 
  group_by(Player) %>% 
  summarise(avg_player = mean(player_points),
            avg_fantasy = mean(fantasy_points),
            win_percentage = mean(win),
            matches_played = n()) %>% 
  filter(matches_played > 3) %>% 
  arrange(desc(avg_fantasy))

ggplot(fantasy) +
  geom_density(aes(fantasy_points), lwd=1.4) +
  theme_minimal()

ggplot(fantasy, aes(x=log_hltv_rank, y=fantasy_points)) +
  geom_point() +
  geom_jitter(width = 0.05) +
  geom_smooth()

ggplot(fantasy, aes(x=kast, y=fantasy_points)) +
  geom_point() +
  geom_jitter(width = 0.05) +
  geom_smooth()

fit <- lm(fantasy_points ~ rank_diff, data=fantasy[-c(1035, 972, 1480, 645, 1377, 956),])
a <- autoplot(fit)
a
summary(fit)

library(tidyverse)

players <- c("karrigan",
             "rain",
             "Twistzz",
             "ropz",
             "broky",
             "apEX",
             "dupreeh",
             "Magisk",
             "ZywOo",
             "misutaaa",
             "buster",
             "FL1T",
             "Qikert",
             "Jame",
             "YEKINDAR",
             "arT",
             "yuurih",
             "KSCERATO",
             "saffee",
             "drop",
             "Snappi",
             "Maden",
             "dycha",
             "hades",
             "Spinx",
             "raalz",
             "Spiidi",
             "slaxz-",
             "Marix",
             "Staehr")

prices <- c(173000,
            201000,
            211000,
            217000,
            217000,
            193000,
            202000,
            201000,
            240000,
            187000,
            188000,
            204000,
            195000,
            216000,
            220000,
            192000,
            206000,
            217000,
            227000,
            177000,
            176000,
            187000,
            196000,
            190000,
            206000,
            174000,
            185000,
            204000,
            192000,
            191000)

pl_groupb <- data.frame(players, prices, n = 1:30)

pl_groupb <- pl_groupb %>% 
  left_join(player, by = c("players" = "Player"))

combo <- as.data.frame(CombSet(players, 5, repl=FALSE, ord=FALSE))
combo[, 6] <- rep(0, nrow(combo))
combo[, 7] <- rep(0, nrow(combo))

names(combo) <- c("p1", "p2", "p3", "p4", "p5", "tot_score", "tot_cost")

for (i in 1:nrow(combo)) {
  score <- 0
  cost <- 0
  for (j in 1:5) {
    index = which(pl_groupb$players == combo[i, j])
    score <- score + pl_groupb[index, 5]
    cost <- cost + pl_groupb[index, 2]
  }
  combo[i, 6] <- score
  combo[i, 7] <- cost
}

valid_combos <- combo %>% 
  filter(tot_cost < 1000000)

best_combo <- valid_combos %>% 
  arrange(desc(tot_score))
