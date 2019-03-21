library(pacman)
p_load(dplyr,readr,tidylog,tibble,purrr,survival,survminer)

ncaa_tourney = read_csv('data/DataFiles/NCAATourneyCompactResults.csv') 
ncaa_tourney_04 = ncaa_tourney %>% filter(Season>=2004)

graph_kp = read_csv('data/Final/final_graph_kp.csv')


ncaa_wins_04 = ncaa_tourney_04 %>% group_by(Season,WTeamID) %>% 
  summarise(wins = n())


final_df = graph_kp %>% left_join(ncaa_wins_04,by=c('year'='Season','TeamID'='WTeamID')) %>% 
  mutate_all(funs(replace_na(.,0))) %>% 
  mutate(status = ifelse(wins == 6,1,2))

train_df = final_df %>% filter(year<2019)
test_df = final_df %>% filter(year==2019) %>% select(-wins,-status)

res.cox <- coxph(Surv(wins, status) ~ out_degree +  ADJOE + ADJDE + Luck, data =  train_df)

test_df$pred <- predict(res.cox,test_df)

final_res <- test_df %>% select(TeamID,pred)

submission_file = read_csv("data/SampleSubmissionStage2.csv") 

submission_filev2 = submission_file %>% separate(ID,int=c("year","low_id","high_id")) %>% 
  mutate_all(as.numeric)

submission_filev2 <- submission_filev2 %>% left_join(final_res,by=c('low_id'='TeamID')) %>%
  left_join(final_res,by=c('high_id'='TeamID'),suffix=c('.l','.w')) %>% 
  mutate_all(funs(replace_na(.,0)))


final_file <- submission_filev2 %>% mutate(
  pred = 1 - ((pred.l)/(pred.l+pred.w))
)  
