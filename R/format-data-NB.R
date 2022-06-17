library(dplyr)
library(stringr)

 format_data_NB <- function() {
  creel <- read.csv(here::here("data/creel_filter.csv"))
  irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))
  arealu <- read.csv(here::here("data/areaLU.csv"))

  #useful function
  "%notin%" <- Negate("%in%")
  
  #Edit creel data
  #Sum the eastern and western portions of 23, 19, 2 - before April 2014 and Area 20 - before April 2020
  #Area 20 occasionally only had W or E, not both
  #The list below is only combining the estimates for months which contain BOTH east and West
  
  creelcc <- creel %>%
    rename(AREA = PFMA) %>%
    filter(YEAR > 2011) %>% 
    filter(ESTIMATE_SOURCE == "Creel") %>%
    mutate(SURVEY = case_when(
      Include..20. == "Y" ~ "creel20",
      Include..15. == "Y" ~ "creel15",
      TRUE ~ "creel"
    )) %>%
    mutate(AREA = case_when(
      YEAR == 2013 & MONTH > 4 & str_detect(AREA, "Area 20") ~ "Area 20", 
      YEAR == 2014 & MONTH %in% c(3,6:9) & str_detect(AREA, "Area 20") ~ "Area 20", 
      YEAR %in% c(2015, 2016, 2018) & MONTH %in% c(6:9) & str_detect(AREA, "Area 20") ~ "Area 20", 
      YEAR %in% c(2017, 2019) & MONTH %in% c(5:9) & str_detect(AREA, "Area 20") ~ "Area 20", 
      YEAR == 2020 & MONTH < 4 & str_detect(AREA, "Area 20") ~ "Area 20",
      YEAR <2014 & str_detect(AREA, "Area 23") ~ "Area 23", 
      YEAR == 2014  & MONTH < 4 & str_detect(AREA, "Area 23") ~ "Area 23", 
      YEAR <2014 & str_detect(AREA, "Area 19") ~ "Area 19", 
      YEAR == 2014  & MONTH < 4 & str_detect(AREA, "Area 19") ~ "Area 19", 
      YEAR <2014 & str_detect(AREA, "2E|2W") ~ "Area 2", 
      YEAR == 2014 & MONTH < 4 & str_detect(AREA, "2E|2W") ~ "Area 2",
            TRUE ~ as.character(AREA)
      )) %>%
    select(AREA, YEAR, MONTH, TYPE, ESTIMATE, STANDARD_ERROR,SURVEY ) %>% 
    group_by(AREA, YEAR, MONTH, TYPE, SURVEY) %>% 
    summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),STANDARD_ERROR = sum(STANDARD_ERROR, na.rm = TRUE)) %>%  
    filter(AREA %notin% c("Area 20 (West)", "Area 20 (East)") ) %>% 
    left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
    rename(SDCREEL = STANDARD_ERROR, DISPOSITION = TYPE, CREEL = ESTIMATE)
  
  
  #Edit IREC data
  #Area 29 (marine) is the same as Area 29 in the CREEL data, Remove in river fisheries
  irec <- irec %>% 
    filter(METHOD == "Angling from boat") %>% 
    mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
    filter(AREA != "Area 29 (In River)", YEAR > 2011)
  
  
  
  # Expand Irec data
  # input 0s for missing observations on irec
  # create df for all possible observations - using variables of interest only
  # if other variables are considered, need to include them here
  allobs <- expand.grid(list(
    AREA = unique(irec$AREA),
    YEAR = unique(irec$YEAR),
    MONTH = unique(irec$MONTH),
    DISPOSITION = unique(irec$DISPOSITION)
   ))

  #create zero observations, with 0 variance
  irecall <- left_join(allobs, irec)
  irecall$ESTIMATE[is.na(irecall$ESTIMATE)] <- 0
  irecall$VARIANCE[is.na(irecall$VARIANCE)]<-0
  irecall <- irecall %>% left_join(arealu[, c("AREA", "LU_GROUPING3")])
  
  ireccc <- irecall %>%
    select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, VARIANCE, LU_GROUPING3)) %>%
    group_by(AREA, YEAR, MONTH, DISPOSITION, LU_GROUPING3) %>%
    summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE), VARIANCE = sum(VARIANCE, na.rm = TRUE)) %>%
    mutate(SD = sqrt(VARIANCE)) %>%
    select(c(!VARIANCE)) %>%
    rename(IREC = ESTIMATE, SDIREC = SD)
  
  #should we change this to June since there are 91% missing data in May for creel? Changed to 6 here
  datxy <- left_join(ireccc, creelcc) %>%
    mutate(SEASON = if_else(MONTH < 6 | MONTH > 9, "offseason", "peakseason"))
  
  names(datxy) <- tolower(names(datxy))
  datxy <- rename(datxy, region = lu_grouping3)
  datxy
  
  datnna<-datxy[!is.na(datxy$creel),]
  datnna
 }
