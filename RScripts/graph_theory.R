library(pacman)
p_load(dplyr,igraph,readr,tidylog,tibble,purrr)

raw_szn_dat <- read_csv("data/Final/network_adj_szn.csv")

teams_played_tourney = raw_szn_dat %>% filter(WTID_in_tourney == T & LTID_in_tourney == T)

create_graph <- function(season,dat){
  yr_szn = dat %>% filter(Season == season)
  yr_szn = yr_szn %>% select(WTeamID,LTeamID) %>% rename(from = 'WTeamID',to = 'LTeamID')
  graph_szn = graph_from_data_frame(yr_szn)
  out_degree <- degree(graph_szn,mode='out')
  in_degree <- degree(graph_szn,mode='in')
  final_df = data.frame(out_degree,in_degree) %>% rownames_to_column()
}

raw_yrs = teams_played_tourney %>% pull(Season) %>% unique()
full_network_dat = data.frame(raw_yrs) %>% mutate(
  graph_df = map(raw_yrs,create_graph,dat=teams_played_tourney)
) %>% unnest() %>% rename(TeamID = 'rowname',year = 'raw_yrs') %>% 
  mutate(TeamID = as.numeric(TeamID))

kp_dat <- read_csv('data/Final/final_kp_dat.csv') 


full_dat <- full_network_dat %>% inner_join(kp_dat)
write_csv(full_dat,'data/Final/final_graph_kp.csv')
