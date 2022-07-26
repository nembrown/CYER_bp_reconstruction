#=======================================================================
# Base period reconstruction for CYER 
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

# load irec and creel data -------------------------------------------------------------------------


#bcf is the calibration factor

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

#need to do distinct here b/c the previous irec dataset has some 2021 data
irec_combined<- bind_rows(irec, irec_2021) %>%  as_tibble()  %>% distinct()


creel_full_catch <- read.csv(here::here("data/Creel_full_catch_starting_2009.csv")) %>% as_tibble()
creel_full_catch_chinook<- creel_full_catch %>% filter(SPECIES_TXT2 == "Chinook") 
creel_full_catch_chinook<-creel_full_catch_chinook %>% rename(AREA_NUM=AREA, AREA=AREA_GROUP, DISPOSITION=TYPE, RETAINABLE=SUB_TYPE) %>% 
  select(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE, VAL, SOURCE, MARKS_DESC, FISH_SIZE, DATESINC) %>%  
  distinct()

#creel_full_catch_chinook %>% get_dupes(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE, VAL, SOURCE, MARKS_DESC) %>% View()

#add in this historic data here - for now just call it all legal 
creel_kris <- read.csv(here::here("data/SC Rec Chinook 2008 2021 ISBM AABM V2.csv")) %>% as_tibble()
creel_historic<- creel_kris %>% filter(ESTIMATE_SOURCE == "Historic") %>% rename(AREA = PFMA, VAL = ESTIMATE, SOURCE=ESTIMATE_SOURCE, DISPOSITION=TYPE)
creel_historic$RETAINABLE<- "LEGAL"
creel_historic$FISH_SIZE<- "N/A"
creel_historic$DATESINC<- 130

creel_historic<- creel_historic %>% select(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE, VAL, SOURCE, MARKS_DESC, FISH_SIZE)

creel_full_catch_chinook_historic<- bind_rows(creel_full_catch_chinook, creel_historic)
creel_full_catch_chinook_historic %>% get_dupes(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE,SOURCE, MARKS_DESC, FISH_SIZE, DATESINC)

#useful function
"%notin%" <- Negate("%in%")
options(scipen=999)



# C files -----------------------------------------------------------------
mrp_rec_recoveries<- getDfoRecRecoveries(2009:2022)

creel_full_catch <- read.csv(here::here("data/Creel_full_catch_starting_2009.csv"))


### Load in the creel and irec data:
creel_nick <- read.csv(here::here("data/2009 to 2021 Creel Data.csv"))
irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))
arealu <- read.csv(here::here("data/areaLU.csv"))

#Problem: Nick's data doesn't have variance
#Problem: Nick doesn't have released
### proportion of unchecked mark

unique(creel_nick$DATASOURCE)
View(creel_nick)

#try with Kris' data instead:
creel_adipose_kris1 <- creel_kris %>%
  rename(AREA = PFMA) %>%
  filter(TYPE == "Kept", YEAR>2008) %>%
  select(AREA, YEAR, MONTH, TYPE, MARKS_DESC, ESTIMATE) %>% 
  group_by(AREA, YEAR, MONTH, TYPE, MARKS_DESC) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)
names(creel_adipose_kris1) <- tolower(names(creel_adipose_kris1))

creel_kept_marked_prop_kris1<- creel_adipose_kris1 %>% mutate(marks_desc = case_when(
  marks_desc=="Adipose Marked" ~ "Adipose_Marked", 
  marks_desc=="Not Adipose Checked" ~ "Not_Adipose_Checked", 
  marks_desc=="Not Adipose Marked" ~ "Not_Adipose_Marked", 
  marks_desc=="Not Applicable" ~ "Not_Applicable")) %>% 
  pivot_wider(names_from=marks_desc, values_from=creel) 
creel_kept_marked_prop_kris1$Adipose_Marked[is.na(creel_kept_marked_prop_kris1$Adipose_Marked)] <- 0
creel_kept_marked_prop_kris1$Not_Adipose_Marked[is.na(creel_kept_marked_prop_kris1$Not_Adipose_Marked)] <- 0
creel_kept_marked_prop_kris1$Not_Adipose_Checked[is.na(creel_kept_marked_prop_kris1$Not_Adipose_Checked)] <- 0

# creel_kept_marked_prop_year<-creel_kept_marked_prop1 %>% group_by(area_num, year) %>% summarise(across(where(is.numeric), sum)) %>%
#   mutate(marked_prop_year = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked)) %>% select(area_num, year, marked_prop_year)
# 
# creel_kept_marked_prop<- merge(creel_kept_marked_prop1, creel_kept_marked_prop_year, all=TRUE) %>% as_tibble() %>%
#                          mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked),
#                                 marked_prop_combined =case_when( marked_prop == 0 ~ marked_prop_year,
#                                                                  TRUE ~ marked_prop),
#                                 Not_Adipose_Checked_marked = marked_prop_combined*Not_Adipose_Checked,
#                                 creel = Adipose_Marked + Not_Adipose_Checked_marked)

creel_kept_marked_prop_kris<- creel_kept_marked_prop_kris1 %>% mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked),
                                                            Not_Adipose_Checked_marked = marked_prop*Not_Adipose_Checked,
                                                            creel = Adipose_Marked + Not_Adipose_Checked_marked)

creel_adipose_kris<- creel_kept_marked_prop_kris %>% select(area, year, month, disposition, creel) %>% rename(creel_kris = creel) %>% filter(creel_kris!=0)

unique(creel_nick$DATASOURCE)
unique(creel_kris$PFMA)

#Edit creel data
creel_adipose1 <- creel_nick %>%
  rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
  filter(TYPE == "Kept") %>%
  select(AREA, YEAR, MONTH, TYPE, MARKS_DESC, ESTIMATE) %>% 
  group_by(AREA, YEAR, MONTH, TYPE, MARKS_DESC) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)
names(creel_adipose1) <- tolower(names(creel_adipose1))

creel_kept_marked_prop1<- creel_adipose1 %>% mutate(marks_desc = case_when(
  marks_desc=="Adipose Marked" ~ "Adipose_Marked", 
  marks_desc=="Not Adipose Checked" ~ "Not_Adipose_Checked", 
  marks_desc=="Not Adipose Marked" ~ "Not_Adipose_Marked", 
  marks_desc=="Not Applicable" ~ "Not_Applicable")) %>% 
  pivot_wider(names_from=marks_desc, values_from=creel) 
creel_kept_marked_prop1$Adipose_Marked[is.na(creel_kept_marked_prop1$Adipose_Marked)] <- 0
creel_kept_marked_prop1$Not_Adipose_Marked[is.na(creel_kept_marked_prop1$Not_Adipose_Marked)] <- 0
creel_kept_marked_prop1$Not_Adipose_Checked[is.na(creel_kept_marked_prop1$Not_Adipose_Checked)] <- 0

# creel_kept_marked_prop_year<-creel_kept_marked_prop1 %>% group_by(area_num, year) %>% summarise(across(where(is.numeric), sum)) %>%
#   mutate(marked_prop_year = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked)) %>% select(area_num, year, marked_prop_year)
# 
# creel_kept_marked_prop<- merge(creel_kept_marked_prop1, creel_kept_marked_prop_year, all=TRUE) %>% as_tibble() %>%
#                          mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked),
#                                 marked_prop_combined =case_when( marked_prop == 0 ~ marked_prop_year,
#                                                                  TRUE ~ marked_prop),
#                                 Not_Adipose_Checked_marked = marked_prop_combined*Not_Adipose_Checked,
#                                 creel = Adipose_Marked + Not_Adipose_Checked_marked)

creel_kept_marked_prop<- creel_kept_marked_prop1 %>% mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked),
         Not_Adipose_Checked_marked = marked_prop*Not_Adipose_Checked,
         creel = Adipose_Marked + Not_Adipose_Checked_marked)

creel_adipose<- creel_kept_marked_prop %>% select(area, year, month, disposition, creel) %>% filter(creel!=0)

### Different grouping for matching with IREC 
creel_adipose_for_irec1 <- creel_nick %>%
    rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
    filter(TYPE == "Kept") %>%
    mutate(AREA = case_when(
    YEAR <2020 & str_detect(AREA, "Area 20") ~ "Area 20", 
    YEAR == 2020 & MONTH < 4 & str_detect(AREA, "Area 20") ~ "Area 20",
    YEAR <2014 & str_detect(AREA, "Area 23") ~ "Area 23", 
    YEAR == 2014  & MONTH < 4 & str_detect(AREA, "Area 23") ~ "Area 23", 
    YEAR <2014 & str_detect(AREA, "Area 19") ~ "Area 19", 
    YEAR == 2014  & MONTH < 4 & str_detect(AREA, "Area 19") ~ "Area 19", 
    YEAR <2014 & str_detect(AREA, "2E|2W") ~ "Area 2", 
    YEAR == 2014 & MONTH < 4 & str_detect(AREA, "2E|2W") ~ "Area 2",
    TRUE ~ as.character(AREA)
  )) %>%
  mutate(AREA = case_when(
    AREA == "Area 20" & MONTH %in% c(1) & YEAR %in% c(2008,2009,2012) ~ "Area 20 (East)", 
    AREA == "Area 20" & MONTH %in% c(2) & YEAR %in% c(2008,2009,2011,2012,2014,2015,2018) ~ "Area 20 (East)", 
    AREA == "Area 20" & MONTH %in% c(3) & YEAR %in% c(2008,2009,2010,2011,2012,2013,2015,2016,2017,2018) ~ "Area 20 (East)", 
    AREA == "Area 20" & MONTH %in% c(4) & YEAR %in% c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019) ~ "Area 20 (East)", 
    AREA == "Area 20" & MONTH %in% c(5) & YEAR %in% c(2009,2010,2011,2012,2014,2015,2016,2018) ~ "Area 20 (East)", 
    AREA == "Area 20" & MONTH %in% c(10) & YEAR %in% c(2015,2017,2018, 2019) ~ "Area 20 (East)", 
    AREA == "Area 20" & MONTH %in% c(11) & YEAR %in% c(2008,2009,2011) ~ "Area 20 (East)", 
    AREA == "Area 20" & MONTH %in% c(12) & YEAR %in% c(2008,2009,2011) ~ "Area 20 (East)", 
    TRUE ~ as.character(AREA)
  )) %>%
    select(AREA, YEAR, MONTH, TYPE, MARKS_DESC, ESTIMATE) %>% 
    group_by(AREA,YEAR, MONTH, TYPE, MARKS_DESC) %>% 
    summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
    rename(DISPOSITION = TYPE, CREEL = ESTIMATE)
names(creel_adipose_for_irec1) <- tolower(names(creel_adipose_for_irec1))


creel_kept_marked_prop_for_irec1<- creel_adipose_for_irec1 %>% mutate(marks_desc = case_when(
  marks_desc=="Adipose Marked" ~ "Adipose_Marked", 
  marks_desc=="Not Adipose Checked" ~ "Not_Adipose_Checked", 
  marks_desc=="Not Adipose Marked" ~ "Not_Adipose_Marked", 
  marks_desc=="Not Applicable" ~ "Not_Applicable")) %>% 
  pivot_wider(names_from=marks_desc, values_from=creel)

creel_kept_marked_prop_for_irec1$Adipose_Marked[is.na(creel_kept_marked_prop_for_irec1$Adipose_Marked)] <- 0
creel_kept_marked_prop_for_irec1$Not_Adipose_Marked[is.na(creel_kept_marked_prop_for_irec1$Not_Adipose_Marked)] <- 0
creel_kept_marked_prop_for_irec1$Not_Adipose_Checked[is.na(creel_kept_marked_prop_for_irec1$Not_Adipose_Checked)] <- 0

# creel_kept_marked_prop_for_irec_year<-creel_kept_marked_prop_for_irec1 %>% group_by(area, year) %>% summarise(across(where(is.numeric), sum)) %>% 
#   mutate(marked_prop_year = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked)) %>% select(area, year, marked_prop_year)
# 
# creel_kept_marked_prop_for_irec<- merge(creel_kept_marked_prop_for_irec1, creel_kept_marked_prop_for_irec_year, all=TRUE) %>% as_tibble() %>% 
#   mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked), 
#          marked_prop_combined =case_when( marked_prop == 0 ~ marked_prop_year, 
#                                           TRUE ~ marked_prop),
#          Not_Adipose_Checked_marked = marked_prop_combined*Not_Adipose_Checked, 
#          creel = Adipose_Marked + Not_Adipose_Checked_marked)
# 

creel_kept_marked_prop_for_irec<- creel_kept_marked_prop_for_irec1 %>% mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked),
                                                            Not_Adipose_Checked_marked = marked_prop*Not_Adipose_Checked,
                                                            creel = Adipose_Marked + Not_Adipose_Checked_marked)

creel_adipose_for_irec<- creel_kept_marked_prop_for_irec %>% select(area, year, month, disposition, creel) %>% filter(creel!=0)



irec <- irec_combined %>% filter(METHOD == "Angling from boat") %>% 
  mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
  filter(AREA != "Area 29 (In River)") %>% 
  mutate(RETAINABLE = case_when(
    grepl("Legal", RETAINABLE)  ~ "LEGAL", 
    grepl("Sublegal", RETAINABLE)  ~ "SUB-LEGAL"))


#Edit IREC data
irec_adipose <- irec_combined %>% 
  filter(METHOD == "Angling from boat") %>% 
  mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
  filter(AREA != "Area 29 (In River)") %>% 
  filter(DISPOSITION == "Kept")%>% 
  select(-RETAINABLE)


# Expand Irec data
# input 0s for missing observations on irec
# create df for all possible observations - using variables of interest only
# if other variables are considered, need to include them here
allobs_adipose <- expand.grid(list(
  AREA = unique(irec_adipose$AREA),
  YEAR = unique(irec_adipose$YEAR),
  MONTH = unique(irec_adipose$MONTH),
  DISPOSITION = unique(irec_adipose$DISPOSITION), 
  ADIPOSE_MODIFIER = unique(irec_adipose$ADIPOSE_MODIFIER)
))


#create zero observations, with 0 variance
irecall_adipose <- left_join(allobs_adipose, irec_adipose)
irecall_adipose$ESTIMATE[is.na(irecall_adipose$ESTIMATE)] <- 0

ireccc_adipose <- irecall_adipose %>%
  select(c(AREA, YEAR, MONTH, DISPOSITION, ADIPOSE_MODIFIER, ESTIMATE)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION, ADIPOSE_MODIFIER) %>%
  summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE)) %>%
  rename(IREC = ESTIMATE, MARKS_DESC = ADIPOSE_MODIFIER)
names(ireccc_adipose) <- tolower(names(ireccc_adipose ))

irec_kept_marked_prop1<- ireccc_adipose %>% mutate(marks_desc = case_when(
  marks_desc=="Adipose Marked" ~ "Adipose_Marked", 
  marks_desc=="Not Checked" ~ "Not_Adipose_Checked", 
  marks_desc=="Not Adipose Marked" ~ "Not_Adipose_Marked")) %>% 
  pivot_wider(names_from=marks_desc, values_from=irec)

irec_kept_marked_prop1$Adipose_Marked[is.na(irec_kept_marked_prop1$Adipose_Marked)] <- 0
irec_kept_marked_prop1$Not_Adipose_Marked[is.na(irec_kept_marked_prop1$Not_Adipose_Marked)] <- 0
irec_kept_marked_prop1$Not_Adipose_Checked[is.na(irec_kept_marked_prop1$Not_Adipose_Checked)] <- 0


# irec_kept_marked_prop_year<-irec_kept_marked_prop1 %>% group_by(area, year) %>% summarise(across(where(is.numeric), sum)) %>% 
#   mutate(marked_prop_year = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked)) %>% select(area, year, marked_prop_year)
# 
# irec_kept_marked_prop<- merge(irec_kept_marked_prop1, irec_kept_marked_prop_year, all=TRUE) %>% as_tibble() %>% 
#   mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked), 
#          marked_prop_combined =case_when( marked_prop == 0 ~ marked_prop_year, 
#                                           TRUE ~ marked_prop),
#          Not_Adipose_Checked_marked = marked_prop_combined*Not_Adipose_Checked, 
#          irec = Adipose_Marked + Not_Adipose_Checked_marked)

irec_kept_marked_prop <- irec_kept_marked_prop1 %>% mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked),
                                                                              Not_Adipose_Checked_marked = marked_prop*Not_Adipose_Checked,
                                                                              irec = Adipose_Marked + Not_Adipose_Checked_marked)

ireccc_adipose<- irec_kept_marked_prop %>% select(area, year, month, disposition, irec)

ireccc_adipose$irec[is.na(ireccc_adipose$irec)] <- 0


### merge irec and creel adipose
irec_creel_merged_adipose <- merge(creel_adipose_for_irec, ireccc_adipose, all=TRUE) %>% as_tibble() 

irec_creel_merged_adipose_1<-  irec_creel_merged_adipose %>% mutate(licence.year= case_when(
  month > 3 ~ as.numeric(year),
  month < 4 ~ as.numeric(year - 1 )))
irec_creel_merged_adipose_1<-merge( irec_creel_merged_adipose_1, bcf_short, all=TRUE)%>% as_tibble()




#create a pseudocreel_version1 - do this on an area by area basis
#no variation - took out those parts
irec_creel_merged_adipose_pseudo<-irec_creel_merged_adipose_1 %>%  mutate(
                                                         pseudocreel = case_when(
                                                           year > 2011 & month %in% c(5:9) & (is.na(creel) | creel ==0) ~ as.numeric(irec/bcf),
                                                           year > 2011 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                           year < 2013 ~  as.numeric(creel),
                                                           TRUE ~ as.numeric(creel)), 
                                                         irec_calibrated = irec/bcf)


### take out creel here and add it in from before 
irec_creel_merged_adipose_pseudo<- irec_creel_merged_adipose_pseudo %>% select(-creel)
irec_creel_merged_adipose_pseudo<- merge(irec_creel_merged_adipose_pseudo, creel_adipose, all=TRUE) %>% as_tibble()
irec_creel_merged_adipose_pseudo<- merge(irec_creel_merged_adipose_pseudo, creel_adipose_kris, all=TRUE) %>% as_tibble()
 
### add in regions here: 
fishery_simple<- irec_creel_merged_adipose_pseudo %>% select(area) %>% distinct()
fishery_simple<- fishery_simple %>%  mutate(region = case_when(
                                       area %in% c("Area 13", "Area 14", "Area 15", "Area 16", "Area 13 SOG") ~ "22", 
                                       area %in% c("Area 28", "Area 29",  "Area 17", "Area 18", "Area 19 (GS)") ~ "23", 
                                       area %in% c("Area 19 (JDF)", "Area 19", "Area 20", "Area 20 (East)", "Area 20 (West)") ~ "24", 
                                       area %in% c("Area 2","Area 1", "Area 101", "Area 102", "Area 142", "Area 2E", "Area 2W", "Area 3", "Area 4", "Area 104", "Area 103", "Area 5") ~ "25", 
                                       area %in% c("Area 10", "Area 11", "Area 111", "Area 12" , "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 108", "Area 109", "Area 107") ~ "26",  
                                       area %in% c("Area 23", "Area 123", "Area 24", "Area 124", "Area 25", "Area 125", "Area 26", "Area 126", "Area 27", "Area 127", "Area 23 (Barkley)", "Area 21", "Area 22", "Area 121") ~ "27",
                                       area == "Area 23 (Alberni Canal)" ~ "28")) %>% 
                                       add_row(area = "Area 13", region="61") %>% 
                                       add_row(area = "Area 13 SOG", region="61") %>% 
                                       add_row(area = "Area 14", region="62") %>% 
                                       add_row(area = "Area 15", region="62") %>% 
                                       add_row(area = "Area 16", region="62")


irec_creel_merged_adipose_pseudo_region<- merge(irec_creel_merged_adipose_pseudo, fishery_simple, all=TRUE) %>% as_tibble()
irec_creel_merged_adipose_pseudo_region<- irec_creel_merged_adipose_pseudo_region %>% group_by(year, month, region) %>% 
                                          summarise(sum_creel = ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
                                                    sum_irec = ifelse(all(is.na(irec)), NA, sum(irec, na.rm=TRUE)), 
                                                    sum_pseudocreel= ifelse(all(is.na(pseudocreel)), NA, sum(pseudocreel, na.rm=TRUE)),
                                                    sum_irec_calibrated= ifelse(all(is.na(irec_calibrated)), NA, sum(irec_calibrated, na.rm=TRUE)),
                                                    sum_creel_kris= ifelse(all(is.na(creel_kris)), NA, sum(creel_kris, na.rm=TRUE)))

View(irec_creel_merged_adipose_pseudo_region)
#### load in mrp data
#mrp_recoveries<-getDfoTagRecoveries(2009:2022)
fishery_lookup_simple<-mrp_rec_recoveries %>% select(region, recovery_year, area, psc_fishery_id, area_name) %>% distinct()



#heads
mrp_rec_recoveries_heads<- mrp_rec_recoveries %>% group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)
mrp_rec_recoveries_heads_filter1<- mrp_rec_recoveries %>% filter(tag_code != "Not Readable") %>%  group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)
mrp_rec_recoveries_heads_filter2<- mrp_rec_recoveries %>% filter(cwt_estimate>2) %>%  group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)

mrp_rec_recoveries_heads_not_readable<- mrp_rec_recoveries %>% filter(tag_code == "Not Readable") %>%  group_by(recovery_year, region, rec_month) %>% summarise(not_readable=n() ) %>% rename(month=rec_month, year=recovery_year)


mrp_rec_direct_heads<- mrp_rec_recoveries %>% filter(tag_code != "Not Readable") %>% 
                                                      mutate(direct_heads = case_when(
                                                      cwt_estimate < 2 ~ "direct", 
                                                      TRUE ~ "not_direct")) %>%  group_by(recovery_year, region, rec_month, direct_heads) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year) %>% 
                                              pivot_wider(names_from = direct_heads, values_from = heads)


mrp_rec_direct_heads$direct[is.na(mrp_rec_direct_heads$direct)] <- 0
mrp_rec_direct_heads$not_direct[is.na(mrp_rec_direct_heads$not_direct)] <- 0

mrp_rec_direct_heads<- merge(mrp_rec_direct_heads, mrp_rec_recoveries_heads_not_readable, all=TRUE) %>% as_tibble()
mrp_rec_direct_heads$not_readable[is.na(mrp_rec_direct_heads$not_readable)] <- 0

View(mrp_tag_recoveries)

mrp_tag_recoveries<- mrp_rec_recoveries %>% filter( ! is.na(cwt_estimate))
mrp_tag_recoveries_simple<- mrp_tag_recoveries %>% select(recovery_id, tag_code, recovery_year, rec_month, region, cwt_estimate) %>% 
  rename(year = recovery_year, month=rec_month) %>% mutate(direct_heads = case_when(
                                                           cwt_estimate < 2 ~ "direct", 
                                                           TRUE ~ "not_direct")) 
                                                       
mrp_tag_recoveries_simple_check<- mrp_tag_recoveries_simple %>% select(region, year, month, cwt_estimate) %>% distinct() %>% arrange(region, year, month)  
mrp_tag_recoveries_simple_check_mean_year<- mrp_tag_recoveries_simple_check %>% filter(cwt_estimate %notin% c(1,2,3,4,5,8, 2.5), cwt_estimate >2) %>%  group_by(region, year) %>%  summarise(cwt_estimate_mean= mean(cwt_estimate, na.rm=TRUE))  
mrp_tag_recoveries_simple_check_mean_year_include_artificial<-mrp_tag_recoveries_simple_check %>%  group_by(region, year) %>%  summarise(cwt_estimate_mean_artificial= mean(cwt_estimate, na.rm=TRUE))  
mrp_tag_recoveries_simple_check_mean_month_all_years<-mrp_tag_recoveries_simple_check %>%  group_by(region, month) %>%  summarise(cwt_estimate_mean_month= mean(cwt_estimate, na.rm=TRUE))  


mrp_irec_creel<-merge(irec_creel_merged_adipose_pseudo_region, mrp_rec_direct_heads, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_year, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_year_include_artificial, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_month_all_years, all=TRUE) %>% as_tibble()


mrp_irec_creel_tags<-merge(mrp_irec_creel, mrp_tag_recoveries_simple, all.y =TRUE) %>% as_tibble()
mrp_irec_creel_tags

#View(mrp_tag_recoveries)x > 2 & x < 5
View(mrp_irec_creel_tags)
mrp_irec_creel_tags<- mrp_irec_creel_tags %>% mutate(submission_rate = 1/cwt_estimate, 
                                                     accatch = not_direct/submission_rate, 
                                                     flag= case_when(submission_rate %in% c(0.5, 1, 0.25) | cwt_estimate %in% c(1, 1.01, 1.02, 2.5, 3, 5, 8) | cwt_estimate <2 ~ "artificial sub_rate", 
                                                                     
                                                                     cwt_estimate > (round(cwt_estimate_mean_artificial, 2) - 0.02) & cwt_estimate < (round(cwt_estimate_mean_artificial, 2) + 0.02) ~ "average_sub_rate_w_artificial",
                                                                    
                                                                     cwt_estimate > (round(cwt_estimate_mean, 2) - 0.02) & cwt_estimate < (round(cwt_estimate_mean, 2) + 0.02) ~ "average_sub_rate",
                                                                     
                                                                     cwt_estimate > (round(cwt_estimate_mean_month, 2) - 0.02) & cwt_estimate < (round(cwt_estimate_mean_month, 2) + 0.02) ~ "average_sub_rate_month",
                                                                     
                                                                     TRUE ~ "calculated sub_rate"), 
                                                     #used_heads = submission_rate*sum_creel, 
                                                     creel_only_sub_rate = not_direct/sum_creel, 
                                                     creel_with_irec_sub_rate = not_direct/sum_pseudocreel, 
                                                     irec_only_sub_rate = not_direct/sum_irec_calibrated, 
                                                     prop_not_read = not_readable/not_direct,
                                                     cwt_creel_only = (1/creel_only_sub_rate)*(1+prop_not_read), 
                                                     cwt_creel_with_irec = (1/creel_with_irec_sub_rate)*(1+prop_not_read), 
                                                     cwt_irec_only = (1/irec_only_sub_rate)*(1+prop_not_read),
                                                     cwt_recreated = case_when(
                                                                     flag == "average_sub_rate_w_artificial" ~ cwt_estimate_mean,
                                                                     flag == "average_sub_rate_month" ~ cwt_estimate_mean,
                                                                     flag == "average_sub_rate" ~ cwt_estimate_mean,
                                                                     flag == "artificial sub_rate" ~ cwt_estimate,
                                                                     flag == "calculated sub_rate" & year <2017 ~ cwt_creel_only,
                                                                     flag == "calculated sub_rate" & year >2016 & region %notin% c(25,26) ~ cwt_creel_with_irec,
                                                                     flag == "calculated sub_rate" & year >2019 & region %in% c(25,26) ~ cwt_irec_only,
                                                                     TRUE ~ cwt_creel_only ))


#mrp_irec_creel_tags$region[mrp_irec_creel_tags$region=="22"]<-"62"
View(mrp_irec_creel_tags)


#### to-do - take out where creel and psuedocreel = 0 call that NA, take out artifical sub rate

mrp_irec_creel_tags_plotting<- mrp_irec_creel_tags %>% select(-cwt_estimate, -submission_rate, -not_direct, -direct, -sum_irec, -recovery_id, -tag_code) %>% pivot_longer(cols=c(sum_creel, sum_pseudocreel, accatch), names_to = "source", values_to = "values")
mrp_irec_creel_tags_plotting<- mrp_irec_creel_tags_plotting %>%  mutate(year_month = lubridate::make_date(year, month)) %>% distinct()


mrp_irec_creel_tags_simple<- mrp_irec_creel_tags %>% select(-recovery_id, -tag_code) %>% distinct()

mrp_irec_creel_tags_simple_diff<-mrp_irec_creel_tags_simple %>% mutate(diff_creel=accatch - sum_creel,
                                                                       diff_irec =accatch - sum_irec, 
                                                                       diff_pseudo = accatch - sum_pseudocreel,
                                                                       diff_cwt = cwt_recreated - cwt_estimate)


View(mrp_irec_creel_tags_simple_diff)
View(mrp_irec_creel_tags_simple)
#ggplot(mrp_irec_creel_tags_plotting,aes(x=year_month, y=values, color=source, group=source))+ geom_point(size=3, alpha=.5) +  
 # facet_wrap(~region, scales="free") + geom_line()+theme(legend.position = "bottom")


ggplot(mrp_irec_creel_tags_simple %>% filter(region ==28), aes(x=cwt_estimate, y= cwt_creel_only, fill=as.factor(year), col=as.factor(year)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


#years
ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", region %in% c(27,28)), aes(x=accatch, y= sum_creel, fill=as.factor(year), col=as.factor(year)))+geom_point()+geom_abline(slope=1)+
  facet_wrap(~region, scales="free")

#region
ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", region %in% c(27,28) ), aes(x=accatch, y= sum_pseudocreel2, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")

#this one
ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", year<2017, direct_heads=="not_direct"), aes(x=accatch, y= sum_creel, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags_simple %>% filter( direct_heads=="not_direct"), aes(x=cwt_estimate, y= cwt_recreated, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", year>2017, direct_heads=="not_direct"), aes(x=accatch, y= sum_pseudocreel, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")

ggplot(mrp_irec_creel_tags_simple %>% filter(direct_heads=="not_direct",  region %in% c(27,28)), aes(x=accatch, y= sum_creel, fill=as.factor(flag), col=as.factor(flag)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


#years
ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", region %in% c(27,28)), aes(x=cwt_estimate, y= calculated_cwt_norah, fill=as.factor(year), col=as.factor(year)))+geom_point()+geom_abline(slope=1)+
  facet_wrap(~region, scales="free")

#region
ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", region %in% c(27,28) ), aes(x=accatch, y= sum_pseudocreel2, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")



ggplot(mrp_irec_creel_tags_simple, aes(x=accatch, y= sum_creel_kris, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")

ggplot(mrp_irec_creel_tags_simple, aes(x=accatch, y= sum_pseudocreel, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", year<2018), aes(x=accatch, y= sum_creel_kris, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags_simple %>% filter( year<2018), aes(x=accatch, y= sum_creel, fill=as.factor(flag), col=as.factor(flag)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


#
ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", region==23), aes(x=accatch, y= sum_creel, fill=as.factor(year), col=as.factor(year)))+geom_point()+geom_abline(slope=1)+facet_wrap(~region, scales="free")

#region
ggplot(mrp_irec_creel_tags_unchecked_simple %>% filter(flag=="calculated sub_rate", year<2018), aes(x=accatch, y= sum_creel, fill=as.factor(year), col=as.factor(year)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


#### next steps - use Kris' creel datat o do the same thing, might be better


ggplot(mrp_irec_creel_tags_simple, aes(x=accatch, y= sum_creel, col=region, shape=flag))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm", aes(linetype=flag))+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags_unchecked %>% filter(year<2018, sum_creel !=0), aes(x=accatch, y= sum_creel, col=region, shape=flag))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm", aes(linetype=flag))+facet_wrap(~region, scales="free")



ggplot(mrp_irec_creel_tags_unchecked %>% filter(year<2018, sum_creel !=0), aes(x=accatch, y= sum_creel, col=region))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")

ggplot(mrp_irec_creel_tags , aes(x=accatch, y= sum_creel, col=region))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags, aes(x=accatch, y= sum_creel, col=region))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~year, scales="free")




#### Summarise across year:
irec_creel_merged_adipose_pseudo_sum_erafishery<- irec_creel_merged_adipose_pseudo %>% group_by(erafishery, year, disposition) %>% 
  summarise(creel_sum=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
           # creel_var_sum=ifelse(all(is.na(creel_var)), NA, sum(creel_var, na.rm=TRUE)),
          #  creel_sd_sum = sqrt(creel_var_sum),
            pseudocreel_sum=sum(pseudocreel, na.rm=TRUE)
          #, 
           # pseudocreel_var_sum=sum(pseudocreel_var, na.rm=TRUE), 
          #  pseudocreel_sd_sum=sqrt(pseudocreel_var_sum)
          ) 
#%>% 
#  select(-pseudocreel_var_sum, -creel_var_sum)


irec_creel_merged_adipose_and_unchecked_pseudo_sum_erafishery<- irec_creel_merged_adipose_and_unchecked_pseudo %>% group_by(erafishery, year, disposition) %>% 
  summarise(creel_sum=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
            # creel_var_sum=ifelse(all(is.na(creel_var)), NA, sum(creel_var, na.rm=TRUE)),
            #  creel_sd_sum = sqrt(creel_var_sum),
            pseudocreel_sum=sum(pseudocreel, na.rm=TRUE)
            #, 
            # pseudocreel_var_sum=sum(pseudocreel_var, na.rm=TRUE), 
            #  pseudocreel_sd_sum=sqrt(pseudocreel_var_sum)
  ) 
#%>% 
#  select(-pseudocreel_var_sum, -creel_var_sum)

adipose_long<- irec_creel_merged_adipose_pseudo_sum_erafishery %>% pivot_longer(cols=c("creel_sum", "pseudocreel_sum"), names_to = "source", values_to = "values")
adipose_and_unchecked_long<- irec_creel_merged_adipose_and_unchecked_pseudo_sum_erafishery %>% pivot_longer(cols=c("creel_sum", "pseudocreel_sum"), names_to = "source", values_to = "values")




## plots
adipose_kept <- ggplot(adipose_long %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, color=source, group=source))
adipose_kept <- adipose_kept + geom_point(size=3, alpha=.5)+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
adipose_kept  

adipose_and_unchecked_kept <- ggplot(adipose_and_unchecked_long %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, color=source, group=source))
adipose_and_unchecked_kept <-   adipose_and_unchecked_kept + geom_point(size=3, alpha=.5)+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
adipose_and_unchecked_kept

