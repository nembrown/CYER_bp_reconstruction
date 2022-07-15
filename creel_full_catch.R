options(scipen=999)


creel_full_catch <- read.csv(here::here("data/Creel_full_catch_starting_2009.csv")) %>% as_tibble()


creel_full_catch_chinook<- creel_full_catch %>% filter(SPECIES_TXT2 == "Chinook", YEAR < 2022) 

creel_full_catch_chinook

#### MAX ID
creel_full_catch_chinook_max_id<- creel_full_catch_chinook %>% group_by(YEAR, MONTH, AREA, SUBAREA, TYPE, VALTYPE) %>% 
                                                               summarize(max_ID = max(ID))
creel_full_catch_chinook_with_max<- merge(creel_full_catch_chinook, creel_full_catch_chinook_max_id, all=TRUE) %>% as_tibble()
creel_full_catch_chinook_with_max<- creel_full_catch_chinook_with_max %>% filter(max_ID == ID)

### Is this already filtered by MAX id? yes? 
