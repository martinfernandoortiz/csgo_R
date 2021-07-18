#https://adsoncostanzifilho.github.io/blog/2021/03/25/csgo-package/
library(tidyverse)
#install.packages("CSGo")
library(CSGo)
library(dplyr)
library(stringr)
library(highcharter)
#76561199036069511
csgo_Stats1 <- get_stats_friends(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561198018191299') #Dani
csgo_Stats <- get_stats_friends(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561199036069511') #yo

#dani_stats <- get_stats_user(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561198018191299')

#ortiX_stats <- get_stats_user(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561199036069511')
#ortiX <-  csgo_api_stats(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561199036069511')

#vignette("auth", package = "CSGo")
#vignette("usecase", package = "CSGo") 


df <- as.data.frame(csgo_Stats$friends_stats)
df1 <- as.data.frame(csgo_Stats1$friends_stats) %>% filter(player_name=='0rT1x')
df <- rbind(df,df1)

#df %>% group_by(category, type) %>% count()

df[is.na(df)] <- 0
df <- df %>% mutate(value=as.integer(value))
#PERFORMANCE WINS POR MAPA


maps <- df %>%  filter(type=='maps') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
weapon <- df %>%  filter(type=='weapon info') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
stat <- df %>%  filter(type=='stat' & category=='stats') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
performance <- df %>%  filter(category=='performance') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
last <- df %>%  filter(category=='last match') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)


df %>% group_by(category,type) %>% count()


#TIEMPO JUGADO
tiempoJugado <- hchart(stat, "column", hcaes(x = player_name, y = total_time_played))

#SHOTS/KILLS
performance <- performance %>% mutate(efectividadShots= as.integer(total_kills)/as.integer(total_shots_fired)) %>% arrange(-efectividadShots)
efectividad <- hchart(performance, "column", hcaes(x = player_name, y = efectividadShots))
#hits/shots
performance <- performance %>% mutate(efectividadhits= as.integer(total_shots_hit)/as.integer(total_shots_fired)) %>% arrange(-efectividadhits)
hits <- hchart(performance, "column", hcaes(x = player_name, y = efectividadhits))

#GANADOS/JUGADoS
stat <- stat %>% mutate(efectividadPartidos= as.integer(total_matches_won)/as.integer(total_matches_played)) %>% arrange(-efectividadPartidos)

matchsWon <- hchart(stat, "column", hcaes(x = player_name, y = efectividadPartidos))
matchsWon
# MVPS/ MATCH
stat <- stat %>% left_join(performance[,c(1:7,38:40,71,72)], by="player_name") %>% 
  mutate(mvp= as.integer(total_mvps)/as.integer(total_rounds_played)) %>% arrange(-mvp)

mvp <- hchart(stat, "column", hcaes(x = player_name, y = mvp))

# CUCHI/KILLS
tiempoJugado <- hchart(stat, "column", hcaes(x = player_name, y = total_time_played))

#DEADS/MATCHS
tiempoJugado <- hchart(stat, "column", hcaes(x = player_name, y = total_time_played))

#DAMAGE
stat <- stat %>% mutate(damage= as.integer(total_damage_done)/as.integer(total_rounds_played)) %>% arrange(-damage)
damage <- hchart(stat, "column", hcaes(x = player_name, y = damage))

#Score
stat <- stat %>% mutate(score= as.integer(total_contribution_score)/as.integer(total_matches_played)) %>% arrange(-score)

total_contribution_score total_rounds_played
