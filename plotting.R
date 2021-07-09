library(tidyverse)
library(ggdark)
library(RColorBrewer)

players_as_coaches=read_csv("Coach-Player Data.csv")

coach_prev_player_by_yr=players_as_coaches %>% group_by(seas) %>% 
  summarize(num_players=sum(!is.na(player_id)),tot_coaches=n(),
            percent_former_player=num_players/tot_coaches*100) %>% ungroup()

coach_prev_player_by_yr %>% ggplot(aes(x=seas,y=num_players)) + 
  geom_bar(stat="identity",aes(fill = (as.numeric(seas) %% 2 == 0))) +
  dark_theme_gray() + ylab("Number of Player-Turned-Coaches") +
  scale_x_discrete("Season",limits=seq(1950,2021,by=5)) +
  theme(legend.position = "none")

ggsave("Number of Players Turned Coaches By Year.png")

coach_prev_player_by_yr %>% ggplot(aes(x=seas,y=percent_former_player)) + 
  geom_bar(stat="identity",aes(fill = (as.numeric(seas) %% 2 == 0))) +
  dark_theme_gray() + ylab("Percent of Coaches Being Former Players") +
  scale_x_discrete("Season",limits=seq(1950,2021,by=5)) +
  theme(legend.position = "none")

ggsave("Percent of Coaches Being Former Players By Year.png")

coach_prev_player_by_team=players_as_coaches %>% group_by(tm) %>% 
  distinct(coach,birth_year,player_id) %>%
  summarize(num_players=sum(!is.na(player_id)),tot_coaches=n(),
            percent_former_player=num_players/tot_coaches*100) %>% ungroup()

coach_prev_player_by_team %>% 
  ggplot(aes(x=reorder(tm,-num_players),y=num_players,fill=reorder(tm,-num_players))) + 
  geom_bar(stat="identity") +
  dark_theme_gray() + xlab("Team") +ylab("Number of Player-Turned-Coaches") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave("Number of Players Turned Coaches By Team.png",width=12,height = 6,units="in")

coach_prev_player_by_team %>% 
  ggplot(aes(x=reorder(tm,-percent_former_player),y=percent_former_player,
             fill=reorder(tm,-percent_former_player))) + 
  geom_bar(stat="identity") +
  dark_theme_gray() + xlab("Team") +ylab("Percent of Coaches Being Former Players") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave("Percent of Coaches Being Former Players By Team.png",width=12,height = 6,units="in")

coach_prev_player_by_team %>% 
  filter(tm %in% (players_as_coaches %>% filter(seas==2021) %>% pull(tm))) %>%
  ggplot(aes(x=reorder(tm,-percent_former_player),y=percent_former_player,
             fill=reorder(tm,-percent_former_player))) + 
  geom_bar(stat="identity") +
  dark_theme_gray() + xlab("Team") +ylab("Percent of Coaches Being Former Players") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave("Percent of Coaches Being Former Players By Team, Current Franchises Only.png")
