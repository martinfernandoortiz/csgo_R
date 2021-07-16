#https://adsoncostanzifilho.github.io/blog/2021/03/25/csgo-package/
library(tidyverse)
install.packages("CSGo")
library(CSGo)
library(dplyr)
library(stringr)

#76561199036069511
csgo_Stats <- get_stats_friends(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561199036069511') #Dani

#dani_stats <- get_stats_user(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561198018191299')

ortiX_stats <- get_stats_user(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561199036069511')
ortiX <-  csgo_api_stats(api_key = '8FEEC193DD806E291A104B62C8337B3B', user_id = '76561199036069511')

#vignette("auth", package = "CSGo")
#vignette("usecase", package = "CSGo") 


df <- as.data.frame(csgo_Stats$friends_stats)
df <- rbind(df,ortiX_stats)

df %>% group_by(category, type) %>% count()


#PERFORMANCE WINS POR MAPA


maps <- df %>%  filter(type=='maps') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
weapon <- df %>%  filter(type=='weapon info') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
stat <- df %>%  filter(type=='stat' & category=='stats') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
performance <- df %>%  filter(category=='performance') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
last <- df %>%  filter(category=='last match') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)

last <- df %>% filter(str_sub(name,1,4) =='last')
last
df %>% group_by(category,type) %>% count()
