#=======================================================================
# This script is for recreating exactly the 2021 CNR file but using my data not what Katie used
# #Norah Brown
# July 2022
#=======================================================================

# load packages -----------------------------------------------------------

library(ggplot2)
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr)
library(ggplot2)
library(odbc)
library(tagFisheryMapping)
library(dbplyr)
library(janitor)
library(xlsx)
library(purrr)
library(writexl)
library(stringr)

#useful function
"%notin%" <- Negate("%in%")

# load irec and creel data -------------------------------------------------------------------------

#bcf is the calibration factor
bcf<-read.csv(here::here("data/bcf.csv"))
bcf<- bcf %>% filter(Species=="Chinook") %>% as_tibble()
names(bcf) <- tolower(names(bcf))
bcf_short<-bcf %>% select(licence.year, disposition, bcf)

# ### Load in the creel and irec data, this is the exact data that Katie used
# creel_katie<- read_excel(here::here("data/2021 Creel Catch.xlsx"), sheet="Export Worksheet")
# irec_2021<- read_excel(here::here("data/2021 iREC Catch.xlsx"), sheet="Angling from boat") 

## OR: Use this data to validate the other data is the same: 
irec_2021<- read_excel(here::here("data/2021 iREC Catch.xlsx"), sheet="Angling from boat") %>% as_tibble()  %>%  mutate(YEAR = as.numeric(YEAR), MONTH = as.numeric(MONTH))
irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv")) %>% as_tibble()
irec <- irec %>%  select(YEAR, MONTH, AREA, METHOD, ITEM, ADIPOSE_MODIFIER, DISPOSITION, RETAINABLE, ITEM_GROUP, ESTIMATE)
head(irec)
#need to do distinct here b/c the previous irec dataset has some 2021 data
irec_combined<- bind_rows(irec, irec_2021) %>%  as_tibble()  %>% distinct()

  
creel_full_catch <- read.csv(here::here("data/Creel_full_catch_starting_2009.csv")) %>% as_tibble()
creel_full_catch_chinook<- creel_full_catch %>% filter(SPECIES_TXT2 == "Chinook") 
creel_full_catch_chinook<-creel_full_catch_chinook %>% rename(AREA_NUM=AREA, AREA=AREA_GROUP, DISPOSITION=TYPE, RETAINABLE=SUB_TYPE) %>% 
                                                       select(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE, VAL, SOURCE, MARKS_DESC, FISH_SIZE, DATESINC) %>%  
                                                       distinct()

#creel_full_catch_chinook %>% get_dupes(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE, VAL, SOURCE, MARKS_DESC) %>% head()

#add in this historic data here - for now just call it all legal 
creel_kris <- read.csv(here::here("data/SC Rec Chinook 2008 2021 ISBM AABM V2.csv")) %>% as_tibble()
creel_historic<- creel_kris %>% filter(ESTIMATE_SOURCE == "Historic") %>% rename(AREA = PFMA, VAL = ESTIMATE, SOURCE=ESTIMATE_SOURCE, DISPOSITION=TYPE)
creel_historic$RETAINABLE<- "LEGAL"
creel_historic$FISH_SIZE<- "N/A"
creel_historic$DATESINC<- 130

creel_historic<- creel_historic %>% select(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE, VAL, SOURCE, MARKS_DESC, FISH_SIZE)

creel_full_catch_chinook_historic<- bind_rows(creel_full_catch_chinook, creel_historic)
creel_full_catch_chinook_historic %>% get_dupes(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE,SOURCE, MARKS_DESC, FISH_SIZE, DATESINC)


# filter creel data ---------------------------------------------------------
#Split by datasource just to see if that's where inconsistencies come in 
#need to have retainable as a factor 
creel_full_catch_chinook_wide<- creel_full_catch_chinook_historic %>% pivot_wider(names_from = SOURCE, values_from = VAL) %>% 
                                   rename(creel_in_fill_sc =`In Fill`, 
                                              logbook_estimate =`Log Estimate`,
                                              creel =`Creel Estimate`,
                                              creel_estimate_2 =`Creel Estimate 2`,
                                              lodge_log =`Lodge Log`, 
                                              lodge_manifest = `Lodge Manifest`,
                                              lodge_estimate = `Lodge Estimate`,
                                              lodge_elog = `Lodge eLog`,
                                              historic = `Historic` )%>% 
                                   rowwise() %>% 
                                   mutate(creel_log_total = sum(logbook_estimate, creel, creel_in_fill_sc, creel_estimate_2, lodge_log, lodge_manifest, lodge_estimate,lodge_elog,historic, na.rm=TRUE), 
                                   log_total = sum(logbook_estimate, lodge_log, lodge_manifest, lodge_estimate,lodge_elog,historic, na.rm=TRUE))
names(creel_full_catch_chinook_wide)<- tolower(names(creel_full_catch_chinook_wide))

#Identify era fisheries as areas, a few different options based on what I think is in the treaty vs what is used
treaty_cbc<-c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107")
chelsea_cbc<-c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 108", "Area 109", "Area 107")
treaty_nbc_aabm<-c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W",  "Area 130")
chelsea_nbc_aabm<-c("Area 1", "Area 101", "Area 102",  "Area 2W")
treaty_nbc_isbm<-c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")
chelsea_nbc_isbm<-c("Area 3", "Area 4", "Area 5")
gst<-c("Area 13", "Area 14", "Area 15", "Area 16", "Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") 
jst<-c("Area 11", "Area 111", "Area 12")
jdf<-c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)")

#Apply areas to erafisheries
#management column and creel subarea are needed to properly identify WCVI AABM and ISBM
creel_full_catch_chinook_wide<- creel_full_catch_chinook_wide %>%  mutate(erafishery = case_when(
                                         management == "AABM" & area%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") ~ "WCVI AABM S",#this is in line with creel
                                         management == "ISBM" & area%in%c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") ~ "WCVI ISBM S",#this is in line with creel
                                         area%in%c("Area 21", "Area 24") & month%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",#this is correct, line in with creel
                                         grepl("Area 23", area) & month%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S", #this is correct, line in with creel
                                         area%in%c("Area 21", "Area 24") & month%in%c(8,9) ~ "WCVI ISBM S", #this is correct, line in with creel
                                         grepl("Area 23", area) & month%in%c(8,9) ~ "WCVI ISBM S", #this is correct, line in with creel
                                         area%in%c("Area 25", "Area 26", "Area 27") & month%in%c(10,11,12,1,2,3,4,5,6) ~ "WCVI AABM S", #this is correct, in line in with creel
                                         area%in%c("Area 25", "Area 26", "Area 27") & month%in%c(7,8,9) ~ "WCVI ISBM S", #this is correct, line in with creel
                                         area %in% chelsea_cbc~ "CBC S", 
                                         area %in% treaty_nbc_aabm~ "NBC AABM S", 
                                         area %in% treaty_nbc_isbm~ "NBC ISBM S", 
                                         area %in% gst~ "GEO ST S",
                                         area %in% jst~ "JNST S",
                                         area %in% jdf~ "BC JF S"))


#summarize by era fishery by month
creel_full_catch_chinook_wide_erafishery<- creel_full_catch_chinook_wide %>%  group_by(erafishery, year, month, disposition, retainable) %>% 
                                            summarise(historic = sum(historic, na.rm=TRUE),
                                                      creel_in_fill_sc = sum(creel_in_fill_sc, na.rm=TRUE),
                                                      creel_log_total = sum(creel_log_total, na.rm=TRUE),
                                                      log_total = sum(log_total, na.rm=TRUE))  
                                          
#summarize by era fishery by year
creel_full_catch_chinook_wide_erafishery_year<- creel_full_catch_chinook_wide %>%  group_by(erafishery, year, disposition, retainable) %>% 
                                                         summarise(historic = sum(historic, na.rm=TRUE),
                                                         creel_in_fill_sc = sum(creel_in_fill_sc, na.rm=TRUE),
                                                         creel_log_total = sum(creel_log_total, na.rm=TRUE),
                                                         log_total = sum(log_total, na.rm=TRUE))  
                                             
#summarize by area AND erafishery by month 
creelcc_full_catch_chinook_wide<- creel_full_catch_chinook_wide %>%  
                                  mutate(area = case_when(
                                    year <2020 & str_detect(area, "Area 20") ~ "Area 20", 
                                    year == 2020 & month < 4 & str_detect(area, "Area 20") ~ "Area 20",
                                    year <2014 & str_detect(area, "Area 23") ~ "Area 23", 
                                    year == 2014  & month < 4 & str_detect(area, "Area 23") ~ "Area 23", 
                                    year <2014 & str_detect(area, "Area 19") ~ "Area 19", 
                                    year == 2014  & month < 4 & str_detect(area, "Area 19") ~ "Area 19", 
                                    year <2014 & str_detect(area, "2E|2W") ~ "Area 2", 
                                    year == 2014 & month < 4 & str_detect(area, "2E|2W") ~ "Area 2",
                                    TRUE ~ as.character(area)
                                  )) %>%
                                  mutate(area = case_when(
                                    area == "Area 20" & month %in% c(1) & year%in% c(2008,2009,2012) ~ "Area 20 (East)", 
                                    area == "Area 20" & month %in% c(2) & year%in% c(2008,2009,2011,2012,2014,2015,2018) ~ "Area 20 (East)", 
                                    area == "Area 20" & month %in% c(3) & year%in% c(2008,2009,2010,2011,2012,2013,2015,2016,2017,2018) ~ "Area 20 (East)", 
                                    area == "Area 20" & month %in% c(4) & year%in% c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019) ~ "Area 20 (East)", 
                                    area == "Area 20" & month %in% c(5) & year%in% c(2009,2010,2011,2012,2014,2015,2016,2018) ~ "Area 20 (East)", 
                                    area == "Area 20" & month %in% c(10) &year %in% c(2015,2017,2018, 2019) ~ "Area 20 (East)", 
                                    area == "Area 20" & month %in% c(11) &year %in% c(2008,2009,2011) ~ "Area 20 (East)", 
                                    area == "Area 20" & month %in% c(12) &year %in% c(2008,2009,2011) ~ "Area 20 (East)", 
                                    TRUE ~ as.character(area)
                                  )) %>% 
                                  group_by(area, erafishery, year, month, disposition, retainable) %>% 
                                  summarise(historic = sum(historic, na.rm=TRUE),
                                  creel_in_fill_sc = sum(creel_in_fill_sc, na.rm=TRUE),
                                  creel_log_total = sum(creel_log_total, na.rm=TRUE),
                                  log_total = sum(log_total, na.rm=TRUE))

# filter irec data --------------------------------------------------------
#use the same data as katie for 2021 and the rest use Nick's data

#include retainable
irec <- irec_combined %>% filter(METHOD == "Angling from boat") %>% 
                      mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
                      filter(AREA != "Area 29 (In River)") %>% 
                      mutate(RETAINABLE = case_when(
                             grepl("Legal", RETAINABLE)  ~ "LEGAL", 
                             grepl("Sublegal", RETAINABLE)  ~ "SUB-LEGAL"))
                     
# Expand Irec data
allobs <- expand.grid(list(
  AREA = unique(irec$AREA),
  YEAR = 2012:2021,
  MONTH = c(1:12),
  DISPOSITION = unique(irec$DISPOSITION), 
  RETAINABLE = unique(irec$RETAINABLE)
))

#create zero observations, with 0 variance
irecall <- left_join(allobs, irec)
irecall$ESTIMATE[is.na(irecall$ESTIMATE)] <- 0

ireccc <- irecall %>%
  select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, RETAINABLE)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION, RETAINABLE) %>%
  summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE)) %>%
  rename(IREC = ESTIMATE)

#assign irec pfmas to era fishery
#although can't quite line up with creel b/c of no creel subarea
ireccc<- ireccc %>% mutate(erafishery = case_when(
  AREA%in%c("Area 121", "Area 123", "Area 124") & MONTH%in% c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",#this is not quite right or in line with creel
  AREA%in%c("Area 125", "Area 126", "Area 127") & MONTH%in% c(10,11,12,1,2,3,4,5,6) ~ "WCVI AABM S",#this is not quite right or in line with creel
  AREA%in%c("Area 121", "Area 123", "Area 124") & MONTH%in% c(8,9) ~ "WCVI ISBM S",#this is not quite right or in line with creel
  AREA%in%c("Area 125", "Area 126", "Area 127") & MONTH%in% c(7,8,9) ~ "WCVI ISBM S",#this is not quite right or in line with creel
  AREA%in%c("Area 21", "Area 24") & MONTH%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",#this is correct, line in with creel
  grepl("Area 23", AREA) & MONTH%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S", #this is correct, line in with creel
  AREA%in%c("Area 21", "Area 24") & MONTH%in%c(8,9) ~ "WCVI ISBM S", #this is correct, line in with creel
  grepl("Area 23", AREA) & MONTH%in%c(8,9) ~ "WCVI ISBM S", #this is correct, line in with creel
  AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(10,11,12,1,2,3,4,5,6) ~ "WCVI AABM S", #this is correct, in line in with creel
  AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(7,8,9) ~ "WCVI ISBM S", #this is correct, line in with creel
  AREA %in% chelsea_cbc~ "CBC S", 
  AREA %in% treaty_nbc_aabm~ "NBC AABM S", 
  AREA %in% treaty_nbc_isbm~ "NBC ISBM S", 
  AREA %in% gst~ "GEO ST S",
  AREA %in% jst~ "JNST S",
  AREA %in% jdf~ "BC JF S")) %>% 
  filter(AREA != "Area 22")

names(ireccc) <- tolower(names(ireccc))

#group by area, era fishery by month
ireccc1<- ireccc %>%  group_by(area, erafishery, year, month, disposition, retainable) %>% 
                      summarise(irec = sum(irec, na.rm=TRUE)) 

#group by era fishery and month
ireccc_erafishery<- ireccc %>%  group_by(erafishery, year, month, disposition, retainable) %>% 
                                summarise(irec = sum(irec, na.rm=TRUE)) 
# Merge creel and irec option 1 ----------------------------------------------------

#Merge creel and irec (by era fishery AND area)
irec_creel_merged_new_data <- merge(creelcc_full_catch_chinook_wide, ireccc1, all=TRUE) %>% as_tibble() 

#add in calibration factor to option 1
irec_creel_merged1_new_data<- irec_creel_merged_new_data %>% mutate(licence.year= case_when(
                                       month > 3 ~ as.numeric(year),
                                       month < 4 ~ as.numeric(year - 1 )))
irec_creel_merged1_new_data<-merge(irec_creel_merged1_new_data, bcf_short, all=TRUE)%>% as_tibble()

#create a pseudocreel_version1 - do this on an area by area basis
#this is how I would calculate this
irec_creel_merged_pseudo_new_data<-irec_creel_merged1_new_data %>%  mutate(pseudocreel = case_when(
                                                  year > 2012 & month %in% c(5:9) & (is.na(creel_log_total)| creel_log_total ==0) ~ as.numeric(irec/bcf),
                                                  year > 2012 & month %in% c(1:4,10:12) & (irec == 0 | is.na(irec)) & !is.na(creel_log_total) ~ as.numeric(creel_log_total),
                                                  year > 2012 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                  year < 2013 ~  as.numeric(creel_log_total),
                                                  TRUE ~ as.numeric(creel_log_total)))

#### Summarize across year:
irec_creel_merged_pseudo_sum_erafishery_new_data<- irec_creel_merged_pseudo_new_data %>% group_by(erafishery, year, disposition, retainable) %>% 
                                                                       summarise(historic_sum=ifelse(all(is.na(historic)), NA, sum(historic, na.rm=TRUE)),
                                                                       log_total_sum=ifelse(all(is.na(log_total)), NA, sum(log_total, na.rm=TRUE)),
                                                                       creel_log_total_sum=ifelse(all(is.na(creel_log_total)), NA, sum(creel_log_total, na.rm=TRUE)),
                                                                       creel_infill_sum=ifelse(all(is.na(creel_in_fill_sc)), NA, sum(creel_in_fill_sc, na.rm=TRUE)),
                                                                       irec_sum=ifelse(all(is.na(irec)), NA, sum(irec, na.rm=TRUE)),
                                                                       pseudocreel_sum=ifelse(all(is.na(pseudocreel)), NA, sum(pseudocreel, na.rm=TRUE))) 



# Merge creel and irec option 2 -------------------------------------------

#second option is merge creel and irec just by era fishery 
irec_creel_merged2_new_data <- merge(creel_full_catch_chinook_wide_erafishery, ireccc_erafishery, all=TRUE) %>% as_tibble() 
irec_creel_merged2_new_data$creel_log_total[is.na(irec_creel_merged2_new_data$creel_log_total)] <- 0
irec_creel_merged2_new_data$irec[is.na(irec_creel_merged2_new_data$irec)]<-0


#add in calibration factor to option 2
irec_creel_merged2_new_data <-  irec_creel_merged2_new_data %>% rename(irec2 = irec, creel_log_total2=creel_log_total) %>% 
  mutate(licence.year= case_when(
    month > 3 ~ as.numeric(year),
    month < 4 ~ as.numeric(year - 1 )))
irec_creel_merged2_new_data <- merge(irec_creel_merged2_new_data, bcf_short, all=TRUE)%>% as_tibble()

#This is Katie's method
irec_creel_merged_pseudo2_new_data<-irec_creel_merged2_new_data %>%  mutate(pseudocreel2_irec_portion = case_when(
                                                  month %in% c(5:9) & creel_log_total2 == 0 ~ as.numeric(irec2/bcf),
                                                  month %in% c(1:4,10:12) ~ as.numeric(irec2/bcf),
                                                   TRUE ~ 0))
#### Summarize across year:
irec_creel_merged_pseudo_sum_erafishery2_new_data<- irec_creel_merged_pseudo2_new_data %>% group_by(erafishery, year, disposition, retainable) %>% 
                                                                         summarise(creel_log_total_sum2=ifelse(all(is.na(creel_log_total2)), NA, sum(creel_log_total2, na.rm=TRUE)),
                                                                         irec_sum2=ifelse(all(is.na(irec2)), NA, sum(irec2, na.rm=TRUE)),
                                                                         pseudocreel_irec_portion_sum2=ifelse(all(is.na(pseudocreel2_irec_portion)), NA, sum(pseudocreel2_irec_portion, na.rm=TRUE))) 

# Join option 1 and 2 -----------------------------------------------------

irec_creel_merged_pseudo_sum_erafishery_3_new_data<- merge(irec_creel_merged_pseudo_sum_erafishery_new_data, irec_creel_merged_pseudo_sum_erafishery2_new_data, all=TRUE) %>% as_tibble()

irec_creel_merged_pseudo_sum_erafishery_3_new_data<- irec_creel_merged_pseudo_sum_erafishery_3_new_data %>%  mutate(cnr_recreated = creel_log_total_sum2 + pseudocreel_irec_portion_sum2)


# Download CNR from CAMP ---------------------------------------------------------------------

#open connection to CAMP
fileName <- 'camp.config'
conn_string<-readChar(fileName, file.info(fileName)$size)
campdb <- dbConnect(odbc::odbc(),
                    .connection_string = conn_string)

cnr<-tbl(campdb, "REAMCNRData") %>% as_tibble()

fishery.era<-tbl(campdb, "CAMPFisheryERA") %>% as_tibble()
fishery.era<- fishery.era %>% rename(ERAFishery = FisheryERAID)
fishery.era

#cwdb<-tbl(campdb, "cwdbrecovery")%>% as_tibble()

cnr_fishery<- merge(cnr, fishery.era) %>% as_tibble() %>% rename(erafishery=Name, year = ERAYear)
cnr_canada_sport<- cnr_fishery %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S", "WCVI AABM S", "WCVI ISBM S", "BC JF S", "GEO ST S", "JNST S"))   
cnr_canada_sport<- cnr_canada_sport %>% rename(Kept = CNRValue1, Released= CNRValue2) %>% select(erafishery, year, Kept, Released) 
cnr_canada_sport<- cnr_canada_sport %>% pivot_longer(cols=c("Kept", "Released"), names_to = "disposition", values_to = "cnr")
cnr_canada_sport$retainable<- "LEGAL"


# Merge CNR with irec and creel -------------------------------------------

irec_creel_cnr_new_data<-merge(irec_creel_merged_pseudo_sum_erafishery_3_new_data, cnr_canada_sport, all=TRUE) %>% as_tibble() %>% filter(year>2007)                        

irec_creel_cnr_new_data<- irec_creel_cnr_new_data %>% mutate(cnr_diff = cnr - cnr_recreated)

irec_creel_cnr_plot_new_data<- irec_creel_cnr_new_data %>% pivot_longer(cols=contains(c("sum", "cnr")), names_to = "source", values_to = "values")  


# CNR Creel only NEW METHOD
irec_creel_cnr_new_data_legal <- irec_creel_cnr_new_data %>%  filter(retainable == "LEGAL")
creel_only_cnr<- irec_creel_cnr_new_data_legal %>%   filter(erafishery %in% c("WCVI AABM S", "WCVI ISBM S", "NBC AABM S", "NBC ISBM S", "CBC S"), year > 2008, year<2022) %>%  select(erafishery, year, disposition, creel_log_total_sum) 
creel_only_cnr<- creel_only_cnr %>%  pivot_wider(names_from = disposition, values_from = creel_log_total_sum)  %>% rename(CNRValue1_creel_only=Kept, CNRValue2_creel_only = Released)
creel_only_cnr  



creel_cnr<- merge(creel_only_cnr, cnr_fishery, all=TRUE) %>%  as_tibble() %>%  
                                            mutate(CNRValue1 = case_when(
                                                               year %in% c(2009:2021) & erafishery %in% c("WCVI AABM S", "WCVI ISBM S",  "CBC S") & !is.na(CNRValue1_creel_only) ~ CNRValue1_creel_only, 
                                                               TRUE ~ CNRValue1), 
                                                   CNRValue2 = case_when(
                                                               year %in% c(2009:2021) & erafishery %in% c("WCVI AABM S", "WCVI ISBM S",  "CBC S") & !is.na(CNRValue2_creel_only) ~ CNRValue2_creel_only, 
                                                               TRUE ~ CNRValue2)) %>%
                                            rename(ERAYear = year) %>% 
                                            select(ERAFishery, ERAYear, CNRType, CNRValue1, CNRValue2, CNRValue3, CNRValue4, Remarks)

### need monthly NBC data - could take out NBC from above just to get a working "creel only" - 
# took out NBC (both aabm and isbm from the above code - add back in when I get monthly estimates - until then cnr = the "creel only" estimate)

anti_join(creel_cnr, cnr) %>% head()
anti_join(cnr, creel_cnr) %>% head()


### IREC plus creel = pesuedocreel
pseudocreel_cnr<- irec_creel_cnr_new_data_legal %>%   filter(erafishery %in% c("WCVI AABM S", "WCVI ISBM S", "NBC AABM S", "NBC ISBM S", "CBC S"), year > 2008, year<2022) %>%  select(erafishery, year, disposition, pseudocreel_sum) 
pseudocreel_cnr<- pseudocreel_cnr %>%  pivot_wider(names_from = disposition, values_from = pseudocreel_sum)  %>% rename(CNRValue1_pseudocreel=Kept, CNRValue2_pseudocreel = Released)
pseudocreel_cnr 

creel_with_irec_cnr<- merge(pseudocreel_cnr, cnr_fishery, all=TRUE) %>%  as_tibble() %>%  
                                                          mutate(CNRValue1 = case_when(
                                                            year %in% c(2009:2021) & erafishery %in% c("WCVI AABM S", "WCVI ISBM S", "NBC AABM S", "NBC ISBM S", "CBC S") & !is.na(CNRValue1_pseudocreel) ~ CNRValue1_pseudocreel, 
                                                            TRUE ~ CNRValue1), 
                                                            CNRValue2 = case_when(
                                                              year %in% c(2009:2021) & erafishery %in% c("WCVI AABM S", "WCVI ISBM S", "NBC AABM S", "NBC ISBM S", "CBC S") & !is.na(CNRValue2_pseudocreel) ~ CNRValue2_pseudocreel, 
                                                              TRUE ~ CNRValue2)) %>%
                                                          rename(ERAYear = year) %>% 
                                                          select(ERAFishery, ERAYear, CNRType, CNRValue1, CNRValue2, CNRValue3, CNRValue4, Remarks)

anti_join(creel_with_irec_cnr, cnr)


#CNR IREC and creel CURRENT METHOD
irec_creel_cnr_new_data_legal <- irec_creel_cnr_new_data %>%  filter(retainable == "LEGAL")
creel_with_irec_cnr_current<- irec_creel_cnr_new_data_legal %>%   filter(erafishery %in% c("WCVI AABM S", "WCVI ISBM S", "NBC AABM S", "NBC ISBM S", "CBC S"), year > 2008, year<2022) %>%  select(erafishery, year, disposition, cnr_recreated) 
creel_with_irec_cnr_current<- creel_with_irec_cnr_current %>%  pivot_wider(names_from = disposition, values_from = cnr_recreated)  %>% rename(CNRValue1_creel_only=Kept, CNRValue2_creel_only = Released)
creel_with_irec_cnr_current  



creel_with_irec_cnr_current <- merge(creel_with_irec_cnr_current, cnr_fishery, all=TRUE) %>%  as_tibble() %>%  
                               mutate(CNRValue1 = case_when(
                                 year %in% c(2009:2021) & erafishery %in% c("WCVI AABM S", "WCVI ISBM S",  "CBC S") & !is.na(CNRValue1_creel_only) ~ CNRValue1_creel_only, 
                                 TRUE ~ CNRValue1), 
                                 CNRValue2 = case_when(
                                   year %in% c(2009:2021) & erafishery %in% c("WCVI AABM S", "WCVI ISBM S",  "CBC S") & !is.na(CNRValue2_creel_only) ~ CNRValue2_creel_only, 
                                   TRUE ~ CNRValue2)) %>%
                               rename(ERAYear = year) %>% 
                               select(ERAFishery, ERAYear, CNRType, CNRValue1, CNRValue2, CNRValue3, CNRValue4, Remarks)


write.csv(creel_cnr, "creel_cnr.csv")
write.csv(creel_with_irec_cnr_current, "creel_with_irec_cnr_current.csv")
write.csv(creel_with_irec_cnr, "creel_with_irec_cnr.csv")


# Plotting ----------------------------------------------------------------
theme_set(theme_bw())
pkept <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year < 2022, erafishery %in% c("WCVI AABM S", "WCVI ISBM S"), disposition=="Kept", retainable=="LEGAL", source %in% c("cnr_recreated", "cnr", "creel_log_total_sum2")) ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept


pkept <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year < 2022, erafishery %in% c("WCVI AABM S", "WCVI ISBM S"), disposition=="Kept", retainable=="LEGAL", source %in% c("cnr_recreated", "cnr", "pseudocreel_sum")) ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept

#don't have historic data for 2021 so need to just fill in 
pkept <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year < 2022, erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S"), disposition=="Kept", retainable=="LEGAL",  source %in% c("cnr_recreated", "cnr", "pseudocreel_sum")),aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept



preleases <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year > 2008, year< 2022, erafishery %in% c("WCVI AABM S", "WCVI ISBM S"), disposition=="Released", retainable=="LEGAL", source %in% c("cnr_recreated", "cnr")) ,aes(x=as.factor(year), y=values, color=source, group=source))
preleases <- preleases + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
preleases

preleases <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year > 2008, year< 2022, erafishery %in% c("WCVI AABM S", "WCVI ISBM S"), disposition=="Released", retainable=="LEGAL", source %in% c("cnr_recreated", "cnr", "pseudocreel_sum")) ,aes(x=as.factor(year), y=values, color=source, group=source))
preleases <- preleases + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
preleases


preleases <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year < 2022, erafishery %in% c("WCVI AABM S", "WCVI ISBM S"), disposition=="Released", retainable=="LEGAL", source %in% c("cnr_recreated", "cnr")) ,aes(x=as.factor(year), y=values, color=source, group=source))
preleases <- preleases + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
preleases

preleases <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year < 2022, erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S"), disposition=="Released", retainable=="LEGAL",  source %in% c("cnr_recreated", "cnr", "pseudocreel_sum")),aes(x=as.factor(year), y=values, color=source, group=source))
preleases<- preleases + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
preleases



pkept <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year < 2022, erafishery %in% c("NBC AABM S", "NBC ISBM S"), disposition=="Kept", retainable=="LEGAL") ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept

pkept <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year < 2022, erafishery %in% c("WCVI AABM S", "WCVI ISBM S", "NBC AABM S", "NBC ISBM S", "CBC S"), disposition=="Kept", retainable=="LEGAL", source %in% c("pseudocreel_sum", "cnr", "creel_log_total_sum2")) ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept

#all regions
preleases <- ggplot(irec_creel_cnr_plot_new_data %>% filter(year < 2022, erafishery %in% c("WCVI AABM S", "WCVI ISBM S", "NBC AABM S", "NBC ISBM S", "CBC S"), disposition=="Released", retainable=="LEGAL", source %in% c("pseudocreel_sum", "cnr", "creel_log_total_sum2")) ,aes(x=as.factor(year), y=values, color=source, group=source))
preleases <- preleases + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
preleases



