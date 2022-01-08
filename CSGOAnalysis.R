##Looking at insights in the CS:GO Round Winner Classification dataset from Kaggle.com
##Dataset created by user Christian Lillelund
#Analysis by Bailey Marlow
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(tidyr)

csgo <- read.csv("data/csgo_round_snapshots.csv")

##Data Prep::
#remove NA values:
csgo <- csgo %>% 
  drop_na()

#Looking at dataset, see I need to add some variables
# want to assign a game id column to know when the game is different as there is no marker currently

csgo<- csgo %>%
  mutate(match_id=ifelse(lag(map)==map,'NoID',row_number())) %>%
  mutate(match_id=ifelse(is.na(match_id),row_number(),match_id)) %>%
  mutate(match_id=ifelse(match_id=='NoID',NA,match_id)) %>%
  fill(match_id) %>% # foward fill
  mutate(match_id=paste('ID:',match_id))

#Make a round value to denote what round each game is in 

csgo <- csgo %>% 
  mutate(round = ct_score + t_score + 1)

#Make the times more distinct for future time series analysis
csgo<- csgo %>% 
  mutate(time_left = round(time_left))

##Begin Analysis
##Had some questions picked out, will begin with those and add to it if I find something else to look at:

###How likely to win the round if the bomb is planted 
csgoBP<-
  csgo %>% 
  filter(bomb_planted == "True") %>% 
  distinct(match_id, round, .keep_all = TRUE) %>% 
  summarise(prop_T_win = (sum(round_winner == "T")/sum(!is.na(round_winner)))) %>% 
  as_tibble()

csgoBP

##The percentage of games won by the T side when they planted the bomb was 73.8%. Conversely, the percentage of games 
##won by the CT side when they planted the bomb was 26.2%. This makes sense as having the bomb planted allows additional pressure
#for the T side while reaching the starting condition for a T side win.

#Looking at the prior proportion, want to look into the likelihood that the bomb gets planted in a round given what map the round is played on
csgoBPbM <-
  csgo %>% 
  group_by(map) %>% 
  filter(bomb_planted == "True") %>% 
  distinct(round, match_id, .keep_all = TRUE) %>% 
  summarise(rounds_planted = sum(!is.na(map)))

csgoRPbyM <-
  csgo %>% 
  group_by(map) %>% 
  distinct(round, match_id, .keep_all = TRUE) %>% 
  summarise(rounds_played = sum(!is.na(map)))

csgoBPbMjoin <- merge.data.frame(csgoBPbM, csgoRPbyM, by.x = "map", by.y = "map")

##graph of proportions
ggplot(data = csgoBPbMjoin,
       aes(x = map, y = (rounds_planted/rounds_played))) +
  geom_col() + 
  xlab("Map") +
  ylab("Proportion") +
  ggtitle("Proportions of Likelihood of the Bomb being planted given the Map")

##This graph helps to show the proportions per map that the bomb is planted. Seeing this raises some questions on the difficulty of planting the bomb on each map.

### Looking at the bomb planted proportions leads to me wanting to look at the round winners per map to see if those line up with the bomb planted proportions.
csgo %>%
  filter(map != "de_cache") %>% 
  group_by(map) %>% 
  distinct(round, match_id, .keep_all = TRUE) %>% 
  ggplot(., aes(x = map, fill = round_winner)) +
  geom_bar(position = "dodge") + 
  ggtitle("Terrorist vs Counter-Terrorist Round Winner by Map") +
  scale_fill_manual(values = c("blue", "gold"))+
  ylab("Rounds Won")

#There seems to be a slight T side advantage  on inferno, vertigo and dust2. Similarly, there seems to be a slight CT side advantage on mirage and overpass
# Interested in the significance of the CT side advantage on nuke and train as the advantage seems to be more prevalent than on the other maps.


  
