library(tidyverse)
library(rvest)
library(janitor)
library(polite)

bbref_bow=bow("https://www.basketball-reference.com/",user_agent = "Sumitro Datta",
              force=TRUE,delay=10)
print(bbref_bow)

session=nod(bbref_bow,path=paste0("coaches/"))
coach_register=scrape(session) %>% html_node("table") %>% html_table() %>% 
  row_to_names(1) %>% clean_names() %>%
  #get rid of column name repeats & letter rows
  filter(nchar(coach)>5) %>% 
  #extract asterisk out of name
  mutate(hof=str_detect(coach,"\\*")) %>%
  mutate(coach=ifelse(hof,str_sub(coach,end=-2),coach)) %>% 
  #birth years to distinguish between father-son
  mutate(birth_year=as.numeric(str_sub(birth_date,start=-4,end=-1)))

get_coaches<-function(season=2021){
  session=nod(bbref_bow,path=paste0("leagues/NBA_", season, "_coaches.html"))
  coaches_for_season=scrape(session) %>% 
    html_node("table") %>% 
    html_table() %>% 
    row_to_names(2) %>% 
    clean_names() %>% 
    select(1:2) %>% mutate(seas=season)
  return(coaches_for_season)
}

coach_tibble=get_coaches(2021)

sapply(2020:1950,function(x){
  new_seas=get_coaches(x)
  coach_tibble<<-coach_tibble %>% bind_rows(new_seas)
  print(x)
})

player_totals=read_csv("https://raw.githubusercontent.com/sumitrodatta/bball-reference-datasets/master/Player%20Totals.csv")
list_of_players=player_totals %>% distinct(player_id,player,birth_year)

players_as_coaches=left_join(coach_tibble,coach_register %>% select(coach,birth_year)) %>%
  left_join(.,list_of_players,by=c("coach"="player")) %>% 
  filter(is.na(birth_year.y) | birth_year.x ==birth_year.y) %>% select(-birth_year.y) %>%
  rename(birth_year=birth_year.x)

write_csv(coach_tibble,"Coaches by Season.csv")
write_csv(players_as_coaches,"Coach-Player Data.csv")
