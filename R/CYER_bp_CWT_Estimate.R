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
library(slider)

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

#creel_full_catch_chinook %>% get_dupes(YEAR, MONTH, AREA, SUBAREA, MANAGEMENT, DISPOSITION, RETAINABLE, VAL, SOURCE, MARKS_DESC) %>% head()
#dupes for fish size


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
head(creel_nick)

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
head(creel_nick)
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
    AREA == "Area 13 SoG" ~ "Area 13", 
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

 creel_kept_marked_prop_for_irec_year<-creel_kept_marked_prop_for_irec1 %>% group_by(area, year) %>% summarise(across(where(is.numeric), sum)) %>% 
   mutate(marked_prop_year = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked)) %>% select(area, year, marked_prop_year)

 creel_kept_marked_prop_for_irec_area<-creel_kept_marked_prop_for_irec1 %>% group_by(area) %>% summarise(across(where(is.numeric), sum)) %>% 
   mutate(marked_prop_area = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked)) %>% select(area, marked_prop_area)
 

# 
creel_kept_marked_prop_for_irec1<- merge(creel_kept_marked_prop_for_irec1, creel_kept_marked_prop_for_irec_year, all=TRUE) %>% as_tibble()
creel_kept_marked_prop_for_irec1<- merge(creel_kept_marked_prop_for_irec1, creel_kept_marked_prop_for_irec_area, all=TRUE) %>% as_tibble()


# 
 creel_kept_marked_prop_for_irec<-creel_kept_marked_prop_for_irec1 %>% as_tibble() %>% 
   mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked), 
          marked_prop_combined =case_when( is.nan( marked_prop_year) | marked_prop_year ==0 ~ marked_prop_area, 
                                           is.nan( marked_prop) | marked_prop ==0 ~ marked_prop_year, 
                                           TRUE ~ marked_prop),
          Not_Adipose_Checked_marked = marked_prop_combined*Not_Adipose_Checked, 
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


irec_kept_marked_prop1_area<-irec_kept_marked_prop1 %>% group_by(area) %>% summarise(across(where(is.numeric), sum)) %>% 
  mutate(marked_prop_area = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked)) %>% select(area,  marked_prop_area)

irec_kept_marked_prop1_yearly<- irec_kept_marked_prop1 %>% group_by(area, year, disposition) %>% summarise_if(is.numeric, sum, na.rm=TRUE)  %>% 
                                                           mutate(marked_prop_year = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked)) %>% select(-month, -Adipose_Marked, -Not_Adipose_Marked, -Not_Adipose_Checked)

irec_kept_marked_prop1 <- merge(irec_kept_marked_prop1, irec_kept_marked_prop1_yearly, all=TRUE) %>% as_tibble()
irec_kept_marked_prop1 <- merge(irec_kept_marked_prop1, irec_kept_marked_prop1_area, all=TRUE) %>% as_tibble()


irec_kept_marked_prop<-irec_kept_marked_prop1 %>% as_tibble() %>% 
                    mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked), 
                           marked_prop_combined =case_when( is.nan( marked_prop_year) | marked_prop_year ==0 ~ marked_prop_area, 
                                                            is.nan( marked_prop) | marked_prop ==0 ~ marked_prop_year, 
                                                            TRUE ~ marked_prop),
                           Not_Adipose_Checked_marked = marked_prop_combined*Not_Adipose_Checked, 
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
                                                           year > 2012 & month %in% c(5:9) & (is.na(creel) | creel ==0) ~ as.numeric(irec/bcf),
                                                           year > 2012 & month %in% c(1:4,10:12) & (irec == 0 | is.na(irec))  & !is.na(creel) ~ as.numeric(creel),
                                                           year > 2012 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                           year < 2013  ~  as.numeric(creel),
                                                           TRUE ~ as.numeric(creel)), 
                                                         irec_calibrated = irec/bcf)


### take out creel here and add it in from before 
irec_creel_merged_adipose_pseudo<- irec_creel_merged_adipose_pseudo %>% select(-creel)
irec_creel_merged_adipose_pseudo<- merge(irec_creel_merged_adipose_pseudo, creel_adipose, all=TRUE) %>% as_tibble()
irec_creel_merged_adipose_pseudo<- merge(irec_creel_merged_adipose_pseudo, creel_adipose_kris, all=TRUE) %>% as_tibble()
 
### add in regions here: 
fishery_simple<- irec_creel_merged_adipose_pseudo %>% select(area) %>% distinct()
fishery_simple<- fishery_simple %>%   mutate(CWT_area = case_when(
                                       area %in% c("Area 13", "Area 13 SOG") ~ "P013",
                                       area %in% c("Area 14") ~ "P014",
                                       area %in% c("Area 15") ~ "P015",
                                       area %in% c("Area 16") ~ "P016",
                                       area %in% c("Area 17") ~ "P017", 
                                       area %in% c("Area 18") ~ "P018", 
                                       area %in% c("Area 19 (GS)") ~ "M19A", 
                                       area %in% c("Area 28") ~ "P028", 
                                       area %in% c("Area 29") ~ "P29M", 
                                       area %in% c("Area 19 (JDF)", "Area 19") ~ "M19B", 
                                       area %in% c("Area 20", "Area 20 (East)", "Area 20 (West)") ~ "P020", 
                                       area %in% c("Area 2E") ~ "P2E", 
                                       area %in% c("Area 2W") ~ "P2W", 
                                       area %in% c("Area 1") ~ "P001", 
                                       area %in% c("Area 101") ~ "P101", 
                                       area %in% c("Area 104") ~ "P104", 
                                       area %in% c("Area 102") ~ "P102", 
                                       area %in% c("Area 103") ~ "P103",
                                       area %in% c("Area 105") ~ "P105",
                                       area %in% c("Area 106") ~ "P106",
                                       area %in% c("Area 107") ~ "P107",
                                       area %in% c("Area 108") ~ "P108",
                                       area %in% c("Area 109") ~ "P109",
                                       area %in% c("Area 110") ~ "P110",
                                       area %in% c("Area 2") ~ "P002",
                                       area %in% c("Area 142") ~ "P142", 
                                       area %in% c("Area 3") ~ "P003",
                                       area %in% c("Area 4") ~ "P004",
                                       area %in% c("Area 5") ~ "P005", 
                                       area %in% c("Area 10") ~ "P010",  
                                       area %in% c("Area 11") ~ "P011",  
                                       area %in% c("Area 111") ~ "P111",
                                       area %in% c("Area 12") ~ "P012",  
                                       area %in% c("Area 6") ~ "P006", 
                                       area %in% c("Area 7") ~ "P007",
                                       area %in% c("Area 8") ~ "P008",
                                       area %in% c("Area 9") ~ "P009",
                                       area %in% c("Area 121") ~ "P121",
                                       area %in% c("Area 123") ~ "P123",
                                       area %in% c("Area 124") ~ "P124",
                                       area %in% c("Area 125") ~ "P125",
                                       area %in% c("Area 126") ~ "P126",
                                       area %in% c("Area 127") ~ "P127",
                                       area %in% c("Area 21") ~ "P021",
                                       area %in% c("Area 22") ~ "P022",
                                       area %in% c("Area 23") ~ "P023",
                                       area %in% c("Area 24") ~ "P024",
                                       area %in% c("Area 25") ~ "P025",
                                       area %in% c("Area 26") ~ "P026",
                                       area %in% c("Area 27") ~ "P027",
                                       area %in% c("Area 130") ~ "P130",
                                       area %in% c("Area 23 (Barkley)") ~ "M23B",
                                       area %in% c("Area 23 (Alberni Canal)") ~ "M23A")) %>% 
                                       add_row(area = "Area 17", CWT_area="GSPS") %>% 
                                       add_row(area = "Area 18", CWT_area="GSPS") %>% 
                                       add_row(area = "Area 19 (GS)", CWT_area="GSPS") %>% 
                                       add_row(area = "Area 28", CWT_area="GSPS") %>% 
                                       add_row(area = "Area 29", CWT_area="GSPS") %>%
                                       add_row(area = "Area 19 (JDF)", CWT_area="M153") %>% 
                                       add_row(area = "Area 19", CWT_area="M153") %>% 
                                       add_row(area = "Area 20", CWT_area="M153") %>%
                                       add_row(area = "Area 20 (East)", CWT_area="M153") %>%
                                       add_row(area = "Area 20 (West)", CWT_area="M153") %>%
                                       add_row(area = "Area 2E", CWT_area="P002") %>%
                                       add_row(area = "Area 2W", CWT_area="P002") %>%
                                       add_row(area = "Area 2", CWT_area="P002") %>%
                                       add_row(area = "Area 1", CWT_area="H001") %>%
                                       add_row(area = "Area 101", CWT_area="H001") %>%
                                       add_row(area = "Area 102", CWT_area="H02E") %>%
                                       add_row(area = "Area 2E", CWT_area="H02E") %>%
                                       add_row(area = "Area 142", CWT_area="H02W") %>%
                                       add_row(area = "Area 2W", CWT_area="H02W") %>%
                                       add_row(area = "Area 3", CWT_area="H003") %>%
                                       add_row(area = "Area 103", CWT_area="H003") %>%
                                       add_row(area = "Area 3", CWT_area="M068") %>%
                                       add_row(area = "Area 103", CWT_area="M068") %>%
                                       add_row(area = "Area 4", CWT_area="M068") %>%
                                       add_row(area = "Area 104", CWT_area="M068") %>%
                                       add_row(area = "Area 4", CWT_area="H004") %>%
                                       add_row(area = "Area 104", CWT_area="H004") %>%
                                       add_row(area = "Area 10", CWT_area="M079") %>%
                                       add_row(area = "Area 11", CWT_area="M079") %>%
                                       add_row(area = "Area 12", CWT_area="M079") %>%
                                       add_row(area = "Area 110", CWT_area="M079") %>%
                                       add_row(area = "Area 111", CWT_area="M079") %>%
                                       add_row(area = "Area 10", CWT_area="H010") %>%
                                       add_row(area = "Area 110", CWT_area="H010") %>%
                                       add_row(area = "Area 10", CWT_area="M092") %>%
                                       add_row(area = "Area 11", CWT_area="M092") %>%
                                       add_row(area = "Area 110", CWT_area="M092") %>%
                                       add_row(area = "Area 111", CWT_area="M092") %>% 
                                       add_row(area = "Area 11", CWT_area="H011") %>%
                                       add_row(area = "Area 111", CWT_area="H011") %>%
                                       add_row(area = "Area 11", CWT_area="M094") %>%
                                       add_row(area = "Area 111", CWT_area="M094") %>%
                                       add_row(area = "Area 12", CWT_area="M094") %>%
                                       add_row(area = "Area 6", CWT_area="H006") %>%
                                       add_row(area = "Area 106", CWT_area="H006") %>%
                                       add_row(area = "Area 7", CWT_area="H007") %>%
                                       add_row(area = "Area 107", CWT_area="H007") %>%
                                       add_row(area = "Area 8", CWT_area="H008") %>%
                                       add_row(area = "Area 108", CWT_area="H008") %>%
                                       add_row(area = "Area 9", CWT_area="H009") %>%
                                       add_row(area = "Area 109", CWT_area="H009") %>%
                                       add_row(area = "Area 8", CWT_area="M086") %>%
                                       add_row(area = "Area 108", CWT_area="M086") %>%
                                       add_row(area = "Area 9", CWT_area="M086") %>%
                                       add_row(area = "Area 109", CWT_area="M086") %>%
                                       add_row(area = "Area 123", CWT_area="M401") %>%
                                       add_row(area = "Area 124", CWT_area="M401") %>%
                                       add_row(area = "Area 125", CWT_area="M403") %>%
                                       add_row(area = "Area 126", CWT_area="M403") %>%
                                       add_row(area = "Area 23 (Barkley)", CWT_area="M402") %>%
                                       add_row(area = "Area 123", CWT_area="M402") %>%
                                       add_row(area = "Area 124", CWT_area="M402") %>%
                                       add_row(area = "Area 24", CWT_area="M402") %>%
                                       add_row(area = "Area 21", CWT_area="H021") %>%
                                       add_row(area = "Area 121", CWT_area="H021") %>%
                                       add_row(area = "Area 23 (Barkley)", CWT_area="H23B") %>%
                                       add_row(area = "Area 123", CWT_area="H23B") %>%
                                       add_row(area = "Area 24", CWT_area="H024") %>%
                                       add_row(area = "Area 124", CWT_area="H024") %>%
                                       add_row(area = "Area 24", CWT_area="M118") %>%
                                       add_row(area = "Area 124", CWT_area="M118") %>%
                                       add_row(area = "Area 25", CWT_area="M118") %>%
                                       add_row(area = "Area 125", CWT_area="M118") %>%
                                       add_row(area = "Area 25", CWT_area="H025") %>%
                                       add_row(area = "Area 125", CWT_area="H025") %>%
                                       add_row(area = "Area 26", CWT_area="M120") %>%
                                       add_row(area = "Area 126", CWT_area="M120") %>%
                                       add_row(area = "Area 25", CWT_area="M120") %>%
                                       add_row(area = "Area 125", CWT_area="M120") %>%
                                       add_row(area = "Area 26", CWT_area="H026") %>%
                                       add_row(area = "Area 126", CWT_area="H026") %>%
                                       add_row(area = "Area 27", CWT_area="H027") %>%
                                       add_row(area = "Area 127", CWT_area="H027") %>%
                                       add_row(area = "Area 3", CWT_area="M124") %>%
                                       add_row(area = "Area 103", CWT_area="M124") %>%
                                       add_row(area = "Area 4", CWT_area="M124") %>%
                                       add_row(area = "Area 104", CWT_area="M124") %>%
                                       add_row(area = "Area 5", CWT_area="M124") %>%
                                       add_row(area = "Area 105", CWT_area="M124") %>%
                                       add_row(area = "Area 6", CWT_area="M124") %>%
                                       add_row(area = "Area 106", CWT_area="M124") %>%
                                       add_row(area = "Area 17", CWT_area="M039") %>% 
                                       add_row(area = "Area 16", CWT_area="M039") %>% 
                                       mutate(region = case_when(
                                         area %in% c("Area 13", "Area 14", "Area 15", "Area 16", "Area 13 SoG") ~ "22", 
                                         area %in% c("Area 28", "Area 29",  "Area 17", "Area 18", "Area 19 (GS)") ~ "23", 
                                         area %in% c("Area 19 (JDF)", "Area 19", "Area 20", "Area 20 (East)", "Area 20 (West)") ~ "24", 
                                         area %in% c("Area 2","Area 1", "Area 101", "Area 102", "Area 142", "Area 2E", "Area 2W", "Area 3", "Area 4", "Area 104", "Area 103", "Area 5", "Area 105") ~ "25", 
                                         area %in% c("Area 10", "Area 11", "Area 111", "Area 130", "Area 12" , "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 108", "Area 109", "Area 107") ~ "26",  
                                         area %in% c("Area 23", "Area 123", "Area 24", "Area 124", "Area 25", "Area 125", "Area 26", "Area 126", "Area 27", "Area 127", "Area 23 (Barkley)", "Area 21", "Area 22", "Area 121") ~ "27",
                                         area == "Area 23 (Alberni Canal)" ~ "28")) %>% 
                                       add_row(area = "Area 13", region="61", CWT_area= "P013") %>% 
                                       add_row(area = "Area 13 SoG", region="61", CWT_area= "P013") %>% 
                                       add_row(area = "Area 14", region="62", CWT_area= "P014") %>% 
                                       add_row(area = "Area 15", region="62", CWT_area= "P015") %>% 
                                       add_row(area = "Area 16", region="62", CWT_area= "P016") %>% 
                                       add_row(area = "Area 14", region="62", CWT_area= "M034") %>% 
                                       add_row(area = "Area 15", region="62", CWT_area= "M034") %>% 
                                       drop_na()

fishery_simple_cwt_area<- fishery_simple %>% select(area, CWT_area) %>% distinct()
fishery_simple_region_only<- fishery_simple %>% select(area, region) %>% distinct() 
  
irec_creel_merged_adipose_pseudo_region<- merge(irec_creel_merged_adipose_pseudo, fishery_simple_region_only, all=TRUE) %>% as_tibble()
irec_creel_merged_adipose_pseudo_region<- irec_creel_merged_adipose_pseudo_region %>% group_by(year, month, region) %>% 
                                          summarise(sum_creel = ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
                                                    sum_irec = ifelse(all(is.na(irec)), NA, sum(irec, na.rm=TRUE)), 
                                                    sum_pseudocreel= ifelse(all(is.na(pseudocreel)), NA, sum(pseudocreel, na.rm=TRUE)),
                                                    sum_irec_calibrated= ifelse(all(is.na(irec_calibrated)), NA, sum(irec_calibrated, na.rm=TRUE)),
                                                    sum_creel_kris= ifelse(all(is.na(creel_kris)), NA, sum(creel_kris, na.rm=TRUE)))


### adding in a version with cwt_area
irec_creel_merged_adipose_pseudo_cwt_area<- merge(irec_creel_merged_adipose_pseudo, fishery_simple_cwt_area, all=TRUE) %>% as_tibble()
irec_creel_merged_adipose_pseudo_cwt_area<- irec_creel_merged_adipose_pseudo_cwt_area %>% group_by(year, month, CWT_area) %>% 
  summarise(sum_creel = ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
            sum_irec = ifelse(all(is.na(irec)), NA, sum(irec, na.rm=TRUE)), 
            sum_pseudocreel= ifelse(all(is.na(pseudocreel)), NA, sum(pseudocreel, na.rm=TRUE)),
            sum_irec_calibrated= ifelse(all(is.na(irec_calibrated)), NA, sum(irec_calibrated, na.rm=TRUE)),
            sum_creel_kris= ifelse(all(is.na(creel_kris)), NA, sum(creel_kris, na.rm=TRUE)))





#### load in mrp data
#mrp_recoveries<-getDfoTagRecoveries(2009:2022)
fishery_lookup_simple<-mrp_rec_recoveries %>% select(region, area, psc_fishery_id, area_name) %>% distinct()
head(fishery_lookup_simple)

#heads - a few different ways to do this
mrp_rec_recoveries_heads<- mrp_rec_recoveries %>% group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)
mrp_rec_recoveries_heads_filter1<- mrp_rec_recoveries %>% filter(tag_code != "Not Readable") %>%  group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)
mrp_rec_recoveries_heads_filter2<- mrp_rec_recoveries %>% filter(cwt_estimate>2) %>%  group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)

mrp_rec_recoveries_heads_not_readable<- mrp_rec_recoveries %>% filter(tag_code == "Not Readable") %>%  group_by(recovery_year, region, rec_month) %>% summarise(not_readable=n() ) %>% rename(month=rec_month, year=recovery_year)

## use this method:
#need a #direct heads 
mrp_rec_direct_heads<- mrp_rec_recoveries %>% filter(tag_code != "Not Readable") %>% 
                                                      mutate(direct_heads = case_when(
                                                      cwt_estimate < 2 ~ "direct", 
                                                      TRUE ~ "not_direct")) %>%  group_by(recovery_year, region, rec_month, direct_heads) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year) %>% 
                                              pivot_wider(names_from = direct_heads, values_from = heads)


mrp_rec_direct_heads$direct[is.na(mrp_rec_direct_heads$direct)] <- 0
mrp_rec_direct_heads$not_direct[is.na(mrp_rec_direct_heads$not_direct)] <- 0

mrp_rec_direct_heads<- merge(mrp_rec_direct_heads, mrp_rec_recoveries_heads_not_readable, all=TRUE) %>% as_tibble()
mrp_rec_direct_heads$not_readable[is.na(mrp_rec_direct_heads$not_readable)] <- 0

#### Add in a heads method by cwt_area:
mrp_rec_recoveries_heads_not_readable_cwt_area<- mrp_rec_recoveries %>% filter(tag_code == "Not Readable") %>%  group_by(recovery_year, area, rec_month) %>% summarise(not_readable=n() ) %>% rename(month=rec_month, year=recovery_year, CWT_area = area)

mrp_rec_direct_heads_cwt_area<- mrp_rec_recoveries %>% filter(tag_code != "Not Readable") %>% 
  mutate(direct_heads = case_when(
    cwt_estimate < 2 ~ "direct", 
    TRUE ~ "not_direct")) %>%  group_by(recovery_year, area, rec_month, direct_heads) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year, CWT_area=area) %>% 
  pivot_wider(names_from = direct_heads, values_from = heads)


mrp_rec_direct_heads_cwt_area$direct[is.na(mrp_rec_direct_heads_cwt_area$direct)] <- 0
mrp_rec_direct_heads_cwt_area$not_direct[is.na(mrp_rec_direct_heads_cwt_area$not_direct)] <- 0

mrp_rec_direct_heads_cwt_area<- merge(mrp_rec_direct_heads_cwt_area, mrp_rec_recoveries_heads_not_readable_cwt_area, all=TRUE) %>% as_tibble()
mrp_rec_direct_heads_cwt_area$not_readable[is.na(mrp_rec_direct_heads_cwt_area$not_readable)] <- 0

########

mrp_tag_recoveries<- mrp_rec_recoveries %>% filter( ! is.na(cwt_estimate))
mrp_tag_recoveries_simple<- mrp_tag_recoveries %>% select(recovery_id, tag_code, recovery_year, rec_month, region, cwt_estimate) %>% 
  rename(year = recovery_year, month=rec_month) %>% mutate(direct_heads = case_when(
                                                           cwt_estimate < 2 ~ "direct", 
                                                           TRUE ~ "not_direct")) 
                                                       
mrp_tag_recoveries_simple_check<- mrp_tag_recoveries_simple %>% select(region, year, month, cwt_estimate, direct_heads) %>% distinct() %>% arrange(region,  year, month)  

#tag recoveries by CWT_area:
mrp_tag_recoveries_cwt_area<- mrp_rec_recoveries %>% filter( ! is.na(cwt_estimate))
mrp_tag_recoveries_simple_cwt_area<- mrp_tag_recoveries_cwt_area %>% select(recovery_id, tag_code, recovery_year, rec_month, region, area, cwt_estimate) %>% 
  rename(year = recovery_year, month=rec_month, CWT_area=area) %>% mutate(direct_heads = case_when(
    cwt_estimate < 2 ~ "direct", 
    TRUE ~ "not_direct")) 

mrp_tag_recoveries_simple_check_cwt_area<- mrp_tag_recoveries_simple_cwt_area %>% select(CWT_area,  region, year, month, cwt_estimate, direct_heads) %>% distinct() %>% arrange(region, CWT_area, year, month)  

head(mrp_tag_recoveries_simple_check_mean_year)
#means by region
mrp_tag_recoveries_simple_check_mean_year<- mrp_tag_recoveries_simple_check %>% filter(cwt_estimate %notin% c(1, 1.01, 2, 2.5, 3, 5, 8,2.01, 2.02, 2.03,2.04, 3.01, 3.02, 3.03, 3.04,4.01, 4.01, 4.03, 4.04 ), direct_heads == "not_direct") %>%  group_by(region, year) %>%  summarise(cwt_estimate_mean= mean(cwt_estimate, na.rm=TRUE), n=n())  
mrp_tag_recoveries_simple_check_mean_year<- mrp_tag_recoveries_simple_check_mean_year %>% mutate(cwt_estimate_mean = case_when(n == 1 ~ NA_real_, TRUE~ cwt_estimate_mean)) %>% select(-n)

mrp_tag_recoveries_simple_check_mean_year_summer_only<- mrp_tag_recoveries_simple_check %>% filter(cwt_estimate %notin% c(1, 1.01, 2, 2.5, 3, 5, 8,2.01, 2.02, 2.03,2.04, 3.01, 3.02, 3.03, 3.04,4.01, 4.01, 4.03, 4.04), cwt_estimate >2, month %in% c(5,6,7,8,9)) %>%  group_by(region, year) %>%  summarise(cwt_estimate_mean_summer= mean(cwt_estimate, na.rm=TRUE), n=n())  
mrp_tag_recoveries_simple_check_mean_year_summer_only<- mrp_tag_recoveries_simple_check_mean_year_summer_only %>% mutate(cwt_estimate_mean_summer = case_when(n == 1 ~ NA_real_, TRUE~ cwt_estimate_mean_summer)) %>% select(-n)

mrp_tag_recoveries_simple_check_mean_year_include_artificial<-mrp_tag_recoveries_simple_check %>% filter(direct_heads=="not_direct") %>%  group_by(region, year) %>%  summarise(cwt_estimate_mean_artificial= mean(cwt_estimate, na.rm=TRUE), n=n())  
mrp_tag_recoveries_simple_check_mean_year_include_artificial<- mrp_tag_recoveries_simple_check_mean_year_include_artificial %>% mutate(cwt_estimate_mean_artificial = case_when(n == 1 ~ NA_real_, TRUE~ cwt_estimate_mean_artificial)) %>% select(-n)

#this one doesn't make sense since it didn't just start in 2009 - there are earlier years... take out
#mrp_tag_recoveries_simple_check_mean_month_all_years<-mrp_tag_recoveries_simple_check %>%  group_by(region, month) %>%  mutate(cwt_estimate_mean_month= slide_mean(cwt_estimate,na_rm=TRUE, before=Inf))  

#means by cwt_area and not region
mrp_tag_recoveries_simple_check_cwt_area_mean_year<- mrp_tag_recoveries_simple_check_cwt_area %>% filter(cwt_estimate %notin% c(1, 1.01, 2, 2.5, 3, 5, 8,2.01, 2.02, 2.03,2.04, 3.01, 3.02, 3.03, 3.04,4.01, 4.01, 4.03, 4.04 ), direct_heads == "not_direct") %>%  group_by(CWT_area, year) %>%  summarise(cwt_estimate_mean= mean(cwt_estimate, na.rm=TRUE), n=n())  
mrp_tag_recoveries_simple_check_cwt_area_mean_year<- mrp_tag_recoveries_simple_check_cwt_area_mean_year %>% mutate(cwt_estimate_mean = case_when(n == 1 ~ NA_real_, TRUE~ cwt_estimate_mean)) %>% select(-n)

mrp_tag_recoveries_simple_check_cwt_area_mean_year_summer_only<- mrp_tag_recoveries_simple_check_cwt_area %>% filter(cwt_estimate %notin% c(1,2,3,4,5,8, 2.5), cwt_estimate >2, month %in% c(5,6,7,8,9)) %>%  group_by(CWT_area, year) %>%  summarise(cwt_estimate_mean_summer= mean(cwt_estimate, na.rm=TRUE), n=n())  
mrp_tag_recoveries_simple_check_cwt_area_mean_year_summer_only<- mrp_tag_recoveries_simple_check_cwt_area_mean_year_summer_only %>% mutate(cwt_estimate_mean_summer = case_when(n == 1 ~ NA_real_, TRUE~ cwt_estimate_mean_summer)) %>% select(-n)

mrp_tag_recoveries_simple_check_cwt_area_mean_year_include_artificial<-mrp_tag_recoveries_simple_check_cwt_area %>% filter(direct_heads=="not_direct") %>%  group_by(CWT_area, year) %>%  summarise(cwt_estimate_mean_artificial= mean(cwt_estimate, na.rm=TRUE), n=n())  
mrp_tag_recoveries_simple_check_cwt_area_mean_year_include_artificial<- mrp_tag_recoveries_simple_check_cwt_area_mean_year_include_artificial %>% mutate(cwt_estimate_mean_artificial = case_when(n == 1 ~ NA_real_, TRUE~ cwt_estimate_mean_artificial)) %>% select(-n)


#mrp_tag_recoveries_simple_check_cwt_area_mean_month_all_years<-mrp_tag_recoveries_simple_check_cwt_area %>%  group_by(CWT_area, month) %>%  summarise(cwt_estimate_mean_month= mean(cwt_estimate, na.rm=TRUE))  

mrp_irec_creel<-merge(irec_creel_merged_adipose_pseudo_region, mrp_rec_direct_heads, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_year, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_year_summer_only, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_year_include_artificial, all=TRUE) %>% as_tibble()
#mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_month_all_years, all=TRUE) %>% as_tibble()

## combine pesudo with mrp_rec_direct heads #CWT area
mrp_irec_creel_cwt_area<-merge(irec_creel_merged_adipose_pseudo_cwt_area, mrp_rec_direct_heads_cwt_area, all=TRUE) %>% as_tibble()
mrp_irec_creel_cwt_area<-merge(mrp_irec_creel_cwt_area,mrp_tag_recoveries_simple_check_cwt_area_mean_year, all=TRUE) %>% as_tibble()
mrp_irec_creel_cwt_area<-merge(mrp_irec_creel_cwt_area,mrp_tag_recoveries_simple_check_cwt_area_mean_year_summer_only, all=TRUE) %>% as_tibble()
mrp_irec_creel_cwt_area<-merge(mrp_irec_creel_cwt_area,mrp_tag_recoveries_simple_check_cwt_area_mean_year_include_artificial, all=TRUE) %>% as_tibble()
#mrp_irec_creel_cwt_area<-merge(mrp_irec_creel_cwt_area,mrp_tag_recoveries_simple_check_cwt_area_mean_month_all_years, all=TRUE) %>% as_tibble()



mrp_irec_creel_tags<-merge(mrp_irec_creel, mrp_tag_recoveries_simple, all.y =TRUE) %>% as_tibble()
mrp_irec_creel_tags

#add in another way to do the average - take out direct heads and then do average of where there IS creel
mrp_irec_creel_tags_average<- mrp_irec_creel_tags %>% filter(direct_heads=="not_direct", !is.na(sum_creel)) %>%  
                                                      select(region, year, month, sum_creel, cwt_estimate) %>% 
                                                      distinct() %>% group_by(region, year) %>%  
                                                      summarise(cwt_estimate_mean_creel_pres= mean(cwt_estimate, na.rm=TRUE)) 

mrp_irec_creel_tags<- merge(mrp_irec_creel_tags,mrp_irec_creel_tags_average, all=TRUE ) %>% as_tibble()
mrp_irec_creel_tags_simple1<- mrp_irec_creel_tags %>% select(-recovery_id, -tag_code) %>% distinct()

mrp_irec_creel_tags_dupes<- mrp_irec_creel_tags_simple1 %>% group_by(year, region) %>%  
                                                            get_dupes(year, region, cwt_estimate) %>% 
                                                            select(year, region, month, cwt_estimate) %>% 
                                                            filter(cwt_estimate!= 1) %>% 
                                                            mutate(dupe_flag = "yes")


mrp_irec_creel_tags<- merge(mrp_irec_creel_tags, mrp_irec_creel_tags_dupes, all=TRUE)
mrp_irec_creel_tags$dupe_flag[is.na(mrp_irec_creel_tags$dupe_flag)]<- "no"


mrp_irec_creel_tags<- mrp_irec_creel_tags %>% filter(region != 29) %>% 
                                                mutate(submission_rate = 1/cwt_estimate, 
                                                     accatch = not_direct/submission_rate, 
                                                     flag= case_when(submission_rate %in% c(0.5, 1, 0.25) | cwt_estimate %in% c(1, 1.01, 2, 2.5, 3, 5, 8) | cwt_estimate <2  | cwt_estimate %in% c(2.01, 2.02, 2.03,2.04, 3.01, 3.02, 3.03, 3.04,4.01, 4.01, 4.03, 4.04) & is.na(sum_creel) ~ "artificial sub_rate", 

                                                                     cwt_estimate > (round(cwt_estimate_mean_artificial, 2) - 0.03) & cwt_estimate < (round(cwt_estimate_mean_artificial, 2) + 0.03) ~ "average_sub_rate_w_artificial",
                                                                     
                                                                     cwt_estimate > (round(cwt_estimate_mean, 2) - 0.03) & cwt_estimate < (round(cwt_estimate_mean, 2) + 0.03) ~ "average_sub_rate",
                                                                     
                                                                     cwt_estimate > (round(cwt_estimate_mean_summer, 2) - 0.03) & cwt_estimate < (round(cwt_estimate_mean_summer, 2) + 0.03) ~ "average_sub_rate_summer",
                                                                     
                                                                     cwt_estimate > (round(cwt_estimate_mean_creel_pres, 2) - 0.03) & cwt_estimate < (round(cwt_estimate_mean_creel_pres, 2) + 0.03) ~ "average_sub_rate_creel_pres",
                                                                     
                                                                     dupe_flag == "yes" ~ "unknown_average", 
                                                                     
                                                                     is.na(sum_creel)   ~ "unknown_average_no_creel",
                                                                     
                                                                     
                                                                     TRUE ~ "calculated sub_rate"), 
                                                     #used_heads = submission_rate*sum_creel, 
                                                     creel_only_sub_rate = not_direct/sum_creel, 
                                                     creel_with_irec_sub_rate = not_direct/sum_pseudocreel, 
                                                     irec_only_sub_rate = not_direct/sum_irec_calibrated, 
                                                     prop_not_read = not_readable/not_direct,
                                                     cwt_creel_only = (1/creel_only_sub_rate)*(1+prop_not_read), 
                                                     cwt_creel_with_irec = (1/creel_with_irec_sub_rate)*(1+prop_not_read), 
                                                     cwt_irec_only = (1/irec_only_sub_rate)*(1+prop_not_read),
                                                     cwt_creel_only = case_when(
                                                       is.na(sum_creel) ~ cwt_estimate_mean_creel_pres,
                                                       TRUE ~ cwt_creel_only), 
                                                     cwt_creel_with_irec = case_when(
                                                       is.na(sum_creel) & is.na(sum_pseudocreel) ~ cwt_estimate_mean_creel_pres,
                                                       TRUE ~ cwt_creel_with_irec), 
                                                     cwt_recreated = case_when(
                                                                     is.na(sum_creel) & month %in% c(1,2,3,4,10,11,12) ~ cwt_estimate_mean_creel_pres,
                                                                     flag == "average_sub_rate_w_artificial" ~ cwt_estimate_mean_creel_pres,
                                                                     flag == "average_sub_rate" ~ cwt_estimate_mean_creel_pres,
                                                                     flag == "average_sub_rate_creel_pres" ~ cwt_estimate_mean_creel_pres,
                                                                     flag == "average_sub_rate_summer" ~ cwt_estimate_mean_creel_pres,
                                                                     flag == "unknown_average" ~ cwt_estimate_mean_creel_pres,
                                                                     flag == "unknown_average_no_creel" ~ cwt_estimate,
                                                                     flag == "artificial sub_rate" ~ cwt_estimate,
                                                                     flag == "calculated sub_rate" & year <2019 ~ cwt_creel_only,
                                                                     flag == "calculated sub_rate" & year >2018 & region %notin% c(25,26) ~ cwt_creel_with_irec,
                                                                     flag == "calculated sub_rate" & year >2019 & region %in% c(25,26) ~ cwt_irec_only,
                                                                     TRUE ~ cwt_creel_only ), 
                                                     flag_summary = case_when(
                                                       flag == "average_sub_rate_w_artificial" ~ "average",
                                                       flag == "average_sub_rate_month" ~ "average",
                                                       flag == "average_sub_rate" ~ "average",
                                                       flag == "average_sub_rate_creel_pres" ~ "average",
                                                       flag == "average_sub_rate_summer" ~ "average",
                                                       flag == "unknown_average_no_creel"~ "average_unknown",
                                                       flag == "unknown_average"~ "average_unknown",
                                                       flag == "artificial sub_rate" ~ "artificial",
                                                       flag == "calculated sub_rate" ~"calculated"))

### cwt_area version:
mrp_irec_creel_tags_cwt_area<-merge(mrp_irec_creel_cwt_area, mrp_tag_recoveries_simple_cwt_area, all.y =TRUE) %>% as_tibble()
mrp_irec_creel_tags_cwt_area


### cwt_area version:
mrp_irec_creel_tags_average_cwt_area<- mrp_irec_creel_tags_cwt_area %>% filter(direct_heads=="not_direct", !is.na(sum_creel)) %>%  
  select(CWT_area, year, month, sum_creel, cwt_estimate) %>% 
  distinct() %>% group_by(CWT_area, year) %>%  
  summarise(cwt_estimate_mean_creel_pres= mean(cwt_estimate, na.rm=TRUE)) 

mrp_irec_creel_tags_cwt_area<- merge(mrp_irec_creel_tags_cwt_area,mrp_irec_creel_tags_average_cwt_area, all=TRUE ) %>% as_tibble()
mrp_irec_creel_tags_cwt_area_simple1<- mrp_irec_creel_tags_cwt_area %>% select(-recovery_id, -tag_code) %>% distinct()


mrp_irec_creel_tags_cwt_area_dupes<- mrp_irec_creel_tags_cwt_area_simple1 %>% group_by(year, CWT_area) %>%  
  get_dupes(year, CWT_area, cwt_estimate) %>% 
  select(year, CWT_area, month, cwt_estimate) %>% 
  filter(cwt_estimate!= 1) %>% 
  mutate(dupe_flag = "yes")


mrp_irec_creel_tags_cwt_area<- merge(mrp_irec_creel_tags_cwt_area, mrp_irec_creel_tags_cwt_area_dupes, all=TRUE)
mrp_irec_creel_tags_cwt_area$dupe_flag[is.na(mrp_irec_creel_tags_cwt_area$dupe_flag)]<- "no"

####


### cwt_area_version
mrp_irec_creel_tags_cwt_area<- mrp_irec_creel_tags_cwt_area %>% filter(region != 29) %>% 
  mutate(submission_rate = 1/cwt_estimate, 
         accatch = not_direct/submission_rate, 
         flag= case_when(submission_rate %in% c(0.5, 1, 0.25) | cwt_estimate %in% c(1, 1.01, 2, 2.5, 3, 5, 8) | cwt_estimate <2  | cwt_estimate %in% c(2.01, 2.02, 2.03,2.04, 3.01, 3.02, 3.03, 3.04,4.01, 4.01, 4.03, 4.04) & is.na(sum_creel) ~ "artificial sub_rate", 

                         
                         cwt_estimate > (round(cwt_estimate_mean_artificial, 2) - 0.03) & cwt_estimate < (round(cwt_estimate_mean_artificial, 2) + 0.03) ~ "average_sub_rate_w_artificial",
                         
                         cwt_estimate > (round(cwt_estimate_mean, 2) - 0.03) & cwt_estimate < (round(cwt_estimate_mean, 2) + 0.03) ~ "average_sub_rate",
                         
                         cwt_estimate > (round(cwt_estimate_mean_summer, 2) - 0.03) & cwt_estimate < (round(cwt_estimate_mean_summer, 2) + 0.03) ~ "average_sub_rate_summer",
                         
                         cwt_estimate > (round(cwt_estimate_mean_creel_pres, 2) - 0.03) & cwt_estimate < (round(cwt_estimate_mean_creel_pres, 2) + 0.03) ~ "average_sub_rate_creel_pres",
                         
                         dupe_flag == "yes" ~ "unknown_average", 
                         
                        is.na(sum_creel)   ~ "unknown_average_no_creel",
                         
                         TRUE ~ "calculated sub_rate"), 
         #used_heads = submission_rate*sum_creel, 
         creel_only_sub_rate = not_direct/sum_creel, 
         creel_with_irec_sub_rate = not_direct/sum_pseudocreel, 
         irec_only_sub_rate = not_direct/sum_irec_calibrated, 
         prop_not_read = not_readable/not_direct,
         cwt_creel_only = (1/creel_only_sub_rate)*(1+prop_not_read), 
         cwt_creel_with_irec = (1/creel_with_irec_sub_rate)*(1+prop_not_read), 
         cwt_irec_only = (1/irec_only_sub_rate)*(1+prop_not_read),
         cwt_creel_only = case_when(
           is.na(sum_creel) ~ cwt_estimate_mean_creel_pres,
           TRUE ~ cwt_creel_only), 
         cwt_creel_with_irec = case_when(
           is.na(sum_creel) & is.na(sum_pseudocreel) ~ cwt_estimate_mean_creel_pres,
           TRUE ~ cwt_creel_with_irec), 
         cwt_recreated = case_when(
           is.na(sum_creel) & month %in% c(1,2,3,4,10,11,12) ~ cwt_estimate_mean_creel_pres,
           flag == "average_sub_rate_w_artificial" ~ cwt_estimate_mean_creel_pres,
           flag == "average_sub_rate" ~ cwt_estimate_mean_creel_pres,
           flag == "average_sub_rate_creel_pres" ~ cwt_estimate_mean_creel_pres,
           flag == "average_sub_rate_summer" ~ cwt_estimate_mean_creel_pres,
           flag == "unknown_average" ~ cwt_estimate_mean_creel_pres,
           flag == "unknown_average_no_creel" ~ cwt_estimate,
           flag == "artificial sub_rate" ~ cwt_estimate,
           flag == "calculated sub_rate" & year <2019 ~ cwt_creel_only,
           flag == "calculated sub_rate" & year >2018 & region %notin% c(25,26) ~ cwt_creel_with_irec,
           flag == "calculated sub_rate" & year >2019 & region %in% c(25,26) ~ cwt_irec_only,
           TRUE ~ cwt_creel_only ), 
         flag_summary = case_when(
           flag == "average_sub_rate_w_artificial" ~ "average",
           flag == "average_sub_rate_month" ~ "average",
           flag == "average_sub_rate" ~ "average",
           flag == "average_sub_rate_creel_pres" ~ "average",
           flag == "average_sub_rate_summer" ~ "average",
           flag == "unknown_average_no_creel"~ "average_unknown",
           flag == "unknown_average"~ "average_unknown",
           flag == "artificial sub_rate" ~ "artificial",
           flag == "calculated sub_rate" ~"calculated"))




#### to-do - take out where creel and psuedocreel = 0 call that NA, take out artifical sub rate

mrp_irec_creel_tags_plotting<- mrp_irec_creel_tags %>% select(-cwt_estimate, -submission_rate, -not_direct, -direct, -sum_irec, -recovery_id, -tag_code) %>% pivot_longer(cols=c(sum_creel, sum_pseudocreel, accatch), names_to = "source", values_to = "values")
mrp_irec_creel_tags_plotting<- mrp_irec_creel_tags_plotting %>%  mutate(year_month = lubridate::make_date(year, month)) %>% distinct()


mrp_irec_creel_tags_simple<- mrp_irec_creel_tags %>% select(-recovery_id, -tag_code) %>% distinct()

#mrp_irec_creel_tags_simple %>% group_by(year, region) %>%  get_dupes(year, region, cwt_estimate) %>% head()



mrp_irec_creel_tags_simple_diff<-mrp_irec_creel_tags_simple %>% mutate(diff_creel=accatch - sum_creel,
                                                                       diff_irec =accatch - sum_irec, 
                                                                       diff_pseudo = accatch - sum_pseudocreel,
                                                                       diff_cwt = cwt_recreated - cwt_estimate)

head( mrp_irec_creel_tags_simple_diff)
##### cwt_area:
mrp_irec_creel_tags_plotting_cwt_area<- mrp_irec_creel_tags_cwt_area %>% select(-cwt_estimate, -submission_rate, -not_direct, -direct, -sum_irec, -recovery_id, -tag_code) %>% pivot_longer(cols=c(sum_creel, sum_pseudocreel, accatch), names_to = "source", values_to = "values")
mrp_irec_creel_tags_plotting_cwt_area<- mrp_irec_creel_tags_plotting_cwt_area %>%  mutate(year_month = lubridate::make_date(year, month)) %>% distinct()


mrp_irec_creel_tags_simple_cwt_area<- mrp_irec_creel_tags_cwt_area %>% select(-recovery_id, -tag_code) %>% distinct()

mrp_irec_creel_tags_simple_diff_cwt_area<-mrp_irec_creel_tags_simple_cwt_area %>% mutate(diff_creel=accatch - sum_creel,
                                                                       diff_irec =accatch - sum_irec, 
                                                                       diff_pseudo = accatch - sum_pseudocreel,
                                                                       diff_cwt = cwt_recreated - cwt_estimate)


head(mrp_irec_creel_tags_simple_diff_cwt_area)

theme_set(theme_bw())

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

ggplot(mrp_irec_creel_tags_simple_cwt_area %>% filter(flag=="calculated sub_rate", year<2017, direct_heads=="not_direct"), aes(x=accatch, y= sum_creel, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~CWT_area, scales="free")

#### here::
#calculated only:
ggplot(mrp_irec_creel_tags_simple %>% filter( direct_heads=="not_direct", flag_summary == "calculated"), aes(x=cwt_estimate, y= cwt_recreated, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags_simple_cwt_area %>% filter( direct_heads=="not_direct", flag_summary == "calculated" ), aes(x=cwt_estimate, y= cwt_recreated, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~CWT_area, scales="free")

#cwt_area
ggplot(mrp_irec_creel_tags_simple_cwt_area %>% filter( direct_heads=="not_direct", flag_summary == "calculated" ), aes(x=cwt_estimate, y= cwt_recreated, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


#### irec vs creel only 
ggplot(mrp_irec_creel_tags_simple %>% filter(year>2012, direct_heads=="not_direct", region != "26"), aes(x=cwt_estimate, y= cwt_creel_with_irec, fill=as.factor(flag_summary), col=as.factor(flag_summary)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~flag_summary, scales="free")


ggplot(mrp_irec_creel_tags_simple %>% filter(year>2012, direct_heads=="not_direct", region != "26"), aes(x=cwt_estimate, y= cwt_creel_with_irec, fill=as.factor(flag_summary), col=as.factor(flag_summary)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")

ggplot(mrp_irec_creel_tags_simple_cwt_area %>% filter(year>2012, direct_heads=="not_direct", region == "26"), aes(x=cwt_estimate, y= cwt_creel_with_irec, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


#### bar chart calculated vs. average
ggplot(mrp_irec_creel_tags_simple %>% filter( direct_heads=="not_direct" ), aes( x= region, fill=as.factor(flag_summary), col=as.factor(flag_summary)))+geom_bar(position="fill")


ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", year>2017, direct_heads=="not_direct"), aes(x=accatch, y= sum_pseudocreel, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")

ggplot(mrp_irec_creel_tags_simple %>% filter(direct_heads=="not_direct",  region %in% c(27,28)), aes(x=accatch, y= sum_creel, fill=as.factor(flag_summary), col=as.factor(flag_summary)))+geom_point()+geom_abline(slope=1)+
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

