#=======================================================================
# Base period reconstruction for CYER 
# #Norah Brown
# May 2022
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


#run  this code to get daxy: 
#source(here::here("R/format-data-NB.R"))
#format_data_NB()

#bcf is the calibration factor
bcf<-read.csv(here::here("data/bcf.csv"))
bcf<- bcf %>% filter(Species=="Chinook") %>% as_tibble()
names(bcf) <- tolower(names(bcf))
bcf_short<-bcf %>% select(licence.year, disposition, bcf) %>% filter(licence.year>2011)

### Load in the creel and irec data:
#creel1 <- read.csv(here::here("data/creel_filter.csv"))
creel1 <- read.csv(here::here("data/creel_filter_2008_2021.csv"))


creel_kris <- read.csv(here::here("data/creel_filter_2008_2021_marked_fixed.csv"))
irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))

#useful function
"%notin%" <- Negate("%in%")

creel_kris_wide<- creel_kris %>% select(-LINK, -STANDARD_ERROR) %>% 
                                 pivot_wider(names_from = ESTIMATE_SOURCE, values_from = ESTIMATE) %>% 
                                 mutate(flag = case_when(
                                   !is.na(Creel) & !is.na(`In Fill`) ~ "yes", 
                                   TRUE ~ "no"
                                 )) %>% 
                                rename(creel_in_fill_SC =`In Fill`, 
                                       logbook_estimate =`Log Estimate`)

#Edit creel data
#Sum the eastern and western portions of 23, 19, 2 - before April 2014 and Area 20 - before April 2020
#Area 20 occasionally only had W or E, not both
#The list below is only combining the estimates for months which contain BOTH east and West
#SURVEY can identify different types of creel ... but need to combine them to get the proper estimates

creelcc <- creel_kris %>%
  rename(AREA = PFMA) %>%
  #filter(ESTIMATE_SOURCE == "Creel") %>%
  # mutate(SURVEY = case_when(
  #   Include..20. == "Y" ~ "creel20",
  #   Include..15. == "Y" ~ "creel15",
  #   TRUE ~ "creel")) %>% 
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
  select(AREA, YEAR, MONTH, TYPE, ESTIMATE_SOURCE, ESTIMATE) %>% 
  group_by(AREA, YEAR, MONTH, TYPE, ESTIMATE_SOURCE) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)


creelcc <- creelcc %>% pivot_wider(names_from = ESTIMATE_SOURCE, values_from = CREEL) %>% rename(creel_in_fill_SC =`In Fill`, 
         logbook_estimate =`Log Estimate`) %>% mutate(creel_total = sum(Historic, Creel, creel_in_fill_SC, logbook_estimate, na.rm=TRUE))


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
#irecall <- irecall %>% left_join(arealu[, c("AREA", "LU_GROUPING3")])

ireccc <- irecall %>%
  select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, VARIANCE)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION) %>%
  summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE), VARIANCE = sum(VARIANCE, na.rm=TRUE)) %>%
  mutate(SD = sqrt(VARIANCE)) %>%
  select(c(!VARIANCE)) %>%
  rename(IREC = ESTIMATE, SDIREC = SD)

#should we change this to June since there are 91% missing data in May for creel? Changed to 6 here
irec_creel_merged <- merge(creelcc, ireccc, all=TRUE) %>% as_tibble() 
 # mutate(SEASON = if_else(MONTH < 6 | MONTH > 9, "offseason", "peakseason"))

#take out SD
irec_creel_merged <- irec_creel_merged  %>% select(-SDIREC)


names(irec_creel_merged) <- tolower(names(irec_creel_merged))
irec_creel_merged

View(irec_creel_merged)

#Just for completeness, I identify these other areas, but they don't get used in psl/cnr 
#treaty
#check that 107, 108, 109 are in treaty cbc
treaty_cbc<-c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130", "Area 108", "Area 109", "Area 107")
treaty_nbc_aabm<-c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")
treaty_nbc_isbm<-c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")
gst<-c("Area 13", "Area 14", "Area 15", "Area 16", "Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") 
jst<-c("Area 11", "Area 111", "Area 12")
jdf<-c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)")


irec_creel_merged<-irec_creel_merged%>% mutate(erafishery = case_when(
                                        area%in%c("Area 121", "Area 122", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") ~ "WCVI AABM S",
                                        area%in%c("Area 21", "Area 22", "Area 24") & month%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",
                                        grepl("Area 23", area) & month%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",
                                        area%in%c("Area 21", "Area 22", "Area 24") & month%in%c(8,9) ~ "WCVI ISBM S",
                                        grepl("Area 23", area) & month%in%c(8,9) ~ "WCVI ISBM S",
                                        area%in%c("Area 25", "Area 26", "Area 27") & month%in%c(10,11,12,1,2,3,4,5,6) ~ "WCVI AABM S",
                                        area%in%c("Area 25", "Area 26", "Area 27") & month%in%c(7,8,9) ~ "WCVI ISBM S",
                                        area %in% treaty_cbc~ "CBC S", 
                                        area %in% treaty_nbc_aabm~ "NBC AABM S", 
                                        area %in% treaty_nbc_isbm~ "NBC ISBM S", 
                                        area %in% gst~ "GEO ST S",
                                        area %in% jst~ "JNST S",
                                        area %in% jdf~ "BC JF S"))

irec_creel_merged1<- irec_creel_merged %>% mutate(licence.year= case_when(
                                       month > 3 ~ as.numeric(year),
                                       month < 4 ~ as.numeric(year - 1 )))

irec_creel_merged1<-merge(irec_creel_merged1, bcf_short, all=TRUE)%>% as_tibble()

#create a pseudocreel_version1 - do this on an area by area basis
irec_creel_merged_pseudo<-irec_creel_merged1 %>%  mutate(pseudocreel = case_when(
                                                                      year > 2011 & month %in% c(5:9) & is.na(creel_total) ~ as.numeric(irec/bcf),
                                                                      year > 2011 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                                      year < 2012 ~  NA_real_,
                                                                      TRUE ~ as.numeric(creel_total)))
#### Summarise across year:
irec_creel_merged_pseudo_sum_erafishery<- irec_creel_merged_pseudo %>% group_by(erafishery, year, disposition) %>% 
                                                                       summarise(creel_sum=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                 historic_sum=ifelse(all(is.na(historic)), NA, sum(historic, na.rm=TRUE)),
                                                                                 creel_total_sum=ifelse(all(is.na(creel_total)), NA, sum(creel_total, na.rm=TRUE)),
                                                                                 logbook_sum=ifelse(all(is.na(logbook_estimate)), NA, sum(logbook_estimate, na.rm=TRUE)),
                                                                                 irec_sum=ifelse(all(is.na(irec)), NA, sum(irec, na.rm=TRUE)),
                                                                               #  creel_var_sum=ifelse(all(is.na(creel_var)), NA, sum(creel_var, na.rm=TRUE)),
                                                                                # creel_sd_sum = sqrt(creel_var_sum),
                                                                                 pseudocreel_sum=ifelse(all(is.na(pseudocreel)), NA, sum(pseudocreel, na.rm=TRUE))) 
                                                                                 #pseudocreel_var_sum=ifelse(all(is.na(pseudocreel_var)), NA, sum(pseudocreel_var, na.rm=TRUE)), 
                                                                                 #pseudocreel_sd_sum=sqrt(pseudocreel_var_sum)) %>% 
                                                                     #  select(-pseudocreel_var_sum, -creel_var_sum)





# CNR / PSL ---------------------------------------------------------------------

#Either read in cnr here
cnr<-read_excel(here::here("data/REAMCNRData.xlsx"))

#or open connection to CAMP
fileName <- 'camp.config'
conn_string<-readChar(fileName, file.info(fileName)$size)

campdb <- dbConnect(odbc::odbc(),
                    .connection_string = conn_string)

cnr<-tbl(campdb, "REAMCNRData")

fishery.era<-tbl(campdb, "CAMPFisheryERA")
fishery.era<- fishery.era %>% rename(ERAFishery = FisheryERAID)
fishery.era

cwdb<-tbl(campdb, "cwdbrecovery")%>% as_tibble()

cnr<- merge(cnr, fishery.era) %>% as_tibble() %>% rename(erafishery=Name, year = ERAYear)
cnr_canada_sport<- cnr %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S", "WCVI AABM S", "WCVI ISBM S"))   
cnr_canada_sport<- cnr_canada_sport %>% rename(Kept = CNRValue1, Released= CNRValue2) %>% select(erafishery, year, Kept, Released) 
cnr_canada_sport<- cnr_canada_sport %>% pivot_longer(cols=c("Kept", "Released"), names_to = "disposition", values_to = "cnr")
cnr_canada_sport


cnr_canada_sport<- cnr_canada_sport %>% mutate(cnr_sd = 0)
irec_creel_cnr<-merge(irec_creel_merged_pseudo_sum_erafishery, cnr_canada_sport, all=TRUE) %>% as_tibble()                                

irec_creel_cnr<- irec_creel_cnr %>% pivot_longer(cols=c("creel_sum", "pseudocreel_sum","cnr"), names_to = "source", values_to = "values") %>%   
                                    pivot_longer(cols=c("creel_sd_sum", "pseudocreel_sd_sum", "cnr_sd"), names_to = "sourcesd", values_to = "sd") %>% 
                                    mutate(source_correct = case_when(
                                          source == "creel_sum" & sourcesd =="creel_sd_sum" ~ "yes", 
                                          source == "pseudocreel_sum" & sourcesd =="pseudocreel_sd_sum" ~ "yes", 
                                          source == "cnr" & sourcesd =="cnr_sd" ~ "yes", 
                                          TRUE ~ "no" )) %>% 
                                    filter(source_correct == "yes") %>% 
                                    select(-source_correct, -sourcesd)

irec_creel_cnr<- irec_creel_cnr %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S", "WCVI AABM S", "WCVI ISBM S")) %>% 
                                    filter(year>2008)


#Plots cnr
theme_set(theme_bw())
pkept <- ggplot(irec_creel_cnr %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept

ggsave("Plots/kept_irec_creel_cnr.tiff")

pkept2 <- ggplot(irec_creel_cnr %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept2 <- pkept2 + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery) + geom_line()+theme(legend.position = "bottom")
pkept2

ggsave("Plots/kept_irec_creel_cnr_same_axis.tiff")


pReleased <- ggplot(irec_creel_cnr %>% filter(disposition=="Released") ,aes(x=as.factor(year), y=values, color=source, group=source))
pReleased <- pReleased + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pReleased

ggsave("Plots/Released_irec_creel_cnr.tiff")

pReleased2 <- ggplot(irec_creel_cnr %>% filter(disposition=="Released") ,aes(x=as.factor(year), y=values, color=source, group=source))
pReleased2 <- pReleased2 + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery) + geom_line()+theme(legend.position = "bottom")
pReleased2

ggsave("Plots/Released_irec_creel_cnr_same_axis.tiff")


##### Just WCVI

pkept_WCVI <- ggplot(irec_creel_cnr %>% filter(disposition=="Kept", erafishery %in% c("WCVI ISBM S", "WCVI AABM S")) ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept_WCVI <- pkept_WCVI + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept_WCVI

ggsave("Plots/kept_WCVI_irec_creel_cnr.tiff")


pReleased_WCVI <- ggplot(irec_creel_cnr %>% filter(disposition=="Released", erafishery %in% c("WCVI ISBM S", "WCVI AABM S")) ,aes(x=as.factor(year), y=values, color=source, group=source))
pReleased_WCVI <- pReleased_WCVI + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pReleased_WCVI

ggsave("Plots/Released_WCVI_irec_creel_cnr.tiff")


#where did this come from? 
cnr_compare_table <-cnr_compare %>% mutate(Kept_diff_pseudo = (pseudocreel_sum_Kept - cnr_Kept), 
                                     Released_diff_pseudo = (pseudocreel_sum_Released - cnr_Released))




# C files -----------------------------------------------------------------
mrp_rec_recoveries<- getDfoRecRecoveries(2009:2022)

creel_nick_wide<- creel_nick %>% select(-SOURCE, -VALTYPE) %>% pivot_wider(names_from = DATASOURCE, values_from = VAL) %>% 
  mutate(flag = case_when(
    !is.na(`Creel Estimate`) & !is.na(`Creel in fill Estimate`) ~ "yes", 
    TRUE ~ "no"
  ))


View(mrp_rec_recoveries)
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
    left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
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

#Edit IREC data
irec_adipose <- irec %>% 
  filter(METHOD == "Angling from boat") %>% 
  mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
  filter(AREA != "Area 29 (In River)", YEAR > 2011) %>% 
  filter(DISPOSITION == "Kept")

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
irecall_adipose$VARIANCE[is.na(irecall_adipose$VARIANCE)]<-0
irecall_adipose <- irecall_adipose %>% left_join(arealu[, c("AREA", "LU_GROUPING3")])

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
                                                           year > 2011 & month %in% c(5:9) & is.na(creel) ~ as.numeric(irec/bcf),
                                                           year > 2011 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                           year < 2012 ~  NA_real_,
                                                           TRUE ~ as.numeric(creel)))


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
                                                    sum_creel_kris= ifelse(all(is.na(creel_kris)), NA, sum(creel_kris, na.rm=TRUE)))
#### load in mrp data
#mrp_recoveries<-getDfoTagRecoveries(2009:2022)
fishery_lookup_simple<-mrp_rec_recoveries %>% select(region, area, psc_fishery_id, area_name) %>% distinct()

#heads
mrp_rec_recoveries_heads<- mrp_rec_recoveries %>% group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)
mrp_rec_recoveries_heads_filter1<- mrp_rec_recoveries %>% filter(tag_code != "Not readable") %>%  group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)
mrp_rec_recoveries_heads_filter2<- mrp_rec_recoveries %>% filter(cwt_estimate<2) %>%  group_by(recovery_year, region, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)



head(mrp_rec_recoveries )
mrp_tag_recoveries<- mrp_rec_recoveries %>% filter( ! is.na(cwt_estimate))
mrp_tag_recoveries_simple<- mrp_tag_recoveries %>% select(recovery_id, tag_code, recovery_year, rec_month, region, cwt_estimate) %>% 
  rename(year = recovery_year, month=rec_month)

mrp_tag_recoveries_simple_check<- mrp_tag_recoveries_simple %>% select(region, year, month, cwt_estimate) %>% distinct() %>% arrange(region, year, month)  
mrp_tag_recoveries_simple_check_mean_year<- mrp_tag_recoveries_simple_check %>% filter(cwt_estimate %notin% c(1,2,3,4,5,8) ) %>%  group_by(region, year) %>%  summarise(cwt_estimate_mean= mean(cwt_estimate, na.rm=TRUE))  
mrp_tag_recoveries_simple_check_mean_year_include_artificial<-mrp_tag_recoveries_simple_check %>%  group_by(region, year) %>%  summarise(cwt_estimate_mean_artificial= mean(cwt_estimate, na.rm=TRUE))  
mrp_tag_recoveries_simple_check_mean_month_all_years<-mrp_tag_recoveries_simple_check %>%  group_by(region, month) %>%  summarise(cwt_estimate_mean_month= mean(cwt_estimate, na.rm=TRUE))  


mrp_irec_creel<-merge(irec_creel_merged_adipose_pseudo_region, mrp_rec_recoveries_heads_filter2, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_year, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_year_include_artificial, all=TRUE) %>% as_tibble()
mrp_irec_creel<-merge(mrp_irec_creel,mrp_tag_recoveries_simple_check_mean_month_all_years, all=TRUE) %>% as_tibble()


mrp_irec_creel_tags<-merge(mrp_irec_creel, mrp_tag_recoveries_simple, all.y =TRUE) %>% as_tibble()
mrp_irec_creel_tags

#View(mrp_tag_recoveries)
View(mrp_irec_creel_tags)
mrp_irec_creel_tags<- mrp_irec_creel_tags %>% mutate(submission_rate = 1/cwt_estimate, 
                                                     accatch = heads/submission_rate, 
                                                     flag= case_when(submission_rate %in% c(0.5, 1, 0.25) ~ "artificial sub_rate", 
                                                                     cwt_estimate %in% c(1, 1.01, 1.02, 2.5, 3, 5, 8) ~ "artificial sub_rate", 
                                                                     cwt_estimate == cwt_estimate_mean ~ "average_sub_rate",
                                                                     cwt_estimate == cwt_estimate_mean_artificial ~ "average_sub_rate_w_artificial",
                                                                     cwt_estimate == cwt_estimate_mean_month ~ "average_sub_rate_month",
                                                                     TRUE ~ "calculated sub_rate"), 
                                                     #used_heads = submission_rate*sum_creel, 
                                                     used_sub_rate = heads/sum_creel, 
                                                     calculated_cwt_norah = 1/used_sub_rate, 
                                                     sum_pseudocreel2 = case_when(
                                                     year < 2018 ~ sum_creel, 
                                                     TRUE ~ sum_pseudocreel
                                                     ))
#mrp_irec_creel_tags$region[mrp_irec_creel_tags$region=="22"]<-"62"
#View(mrp_irec_creel_tags)


#### to-do - take out where creel and psuedocreel = 0 call that NA, take out artifical sub rate

mrp_irec_creel_tags_plotting<- mrp_irec_creel_tags %>% select(-cwt_estimate, -submission_rate, -heads, -sum_irec, -recovery_id, -tag_code) %>% pivot_longer(cols=c(sum_creel, sum_pseudocreel, accatch), names_to = "source", values_to = "values")
mrp_irec_creel_tags_plotting<- mrp_irec_creel_tags_plotting %>%  mutate(year_month = lubridate::make_date(year, month)) %>% distinct()


mrp_irec_creel_tags_simple<- mrp_irec_creel_tags %>% select(-recovery_id, -tag_code) %>% distinct()

mrp_irec_creel_tags_simple_diff<-mrp_irec_creel_tags_simple %>% mutate(diff=accatch - sum_creel)


#View(mrp_irec_creel_tags_simple_diff)
##View(mrp_irec_creel_tags_simple)
#ggplot(mrp_irec_creel_tags_plotting,aes(x=year_month, y=values, color=source, group=source))+ geom_point(size=3, alpha=.5) +  
 # facet_wrap(~region, scales="free") + geom_line()+theme(legend.position = "bottom")


ggplot(mrp_irec_creel_tags_simple %>% filter(region ==28), aes(x=sum_creel, y= sum_creel_kris, fill=as.factor(year), col=as.factor(year)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


#years
ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", region %in% c(27,28)), aes(x=accatch, y= sum_creel, fill=as.factor(year), col=as.factor(year)))+geom_point()+geom_abline(slope=1)+
  facet_wrap(~region, scales="free")

#region
ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate", region %in% c(27,28) ), aes(x=accatch, y= sum_pseudocreel2, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
  geom_smooth(method="lm")+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags_simple %>% filter(flag=="calculated sub_rate"), aes(x=accatch, y= sum_creel, fill=as.factor(region), col=as.factor(region)))+geom_point()+geom_abline(slope=1)+
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





# Section for looking at proportion of adipose, unchecked and non adipose -----------------------------------------------------------------

creel_adipose_all <- creel_nick %>%
    rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
    filter(DATASOURCE == "Creel Estimate") %>%
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
    select(AREA, YEAR, MONTH, TYPE, ESTIMATE, MARKS_DESC) %>% 
    group_by(AREA, YEAR, MONTH, TYPE, MARKS_DESC) %>% 
    summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE)) %>%  
    filter(AREA %notin% c("Area 20 (West)", "Area 20 (East)") ) %>% 
    left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
    rename(DISPOSITION = TYPE, CREEL = ESTIMATE, adipose=MARKS_DESC) %>% 
    mutate(adipose = case_when(adipose == "Not Adipose Checked" ~ "Not_Adipose_Checked",
                               adipose == "Adipose Marked" ~ "Adipose_Marked",
                               adipose == "Not Applicable" ~ "Not_Applicable",
                               adipose == "Not Adipose Marked" ~ "Not_Adipose_Marked",
                                        TRUE ~ adipose))
  
  irec_adipose_all <- irec %>% 
    filter(METHOD == "Angling from boat") %>% 
    mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
    filter(AREA != "Area 29 (In River)", YEAR > 2011) %>% 
    mutate(ADIPOSE_MODIFIER = case_when(ADIPOSE_MODIFIER== "Not Checked" ~ "Not_Adipose_Checked",
                                        ADIPOSE_MODIFIER== "Not Applicable" ~ "Not_Applicable",
                                        ADIPOSE_MODIFIER== "Adipose Marked" ~ "Adipose_Marked",
                                        ADIPOSE_MODIFIER== "Not Adipose Marked" ~ "Not_Adipose_Marked",
                                        TRUE ~ ADIPOSE_MODIFIER)) %>% 
    rename(adipose =ADIPOSE_MODIFIER) %>% as_tibble()
  

  # Expand Irec data
  # input 0s for missing observations on irec
  # create df for all possible observations - using variables of interest only
  # if other variables are considered, need to include them here
  allobs_adipose_all <- expand.grid(list(
    AREA = unique(irec_adipose$AREA),
    YEAR = unique(irec_adipose$YEAR),
    MONTH = unique(irec_adipose$MONTH),
    DISPOSITION = unique(irec_adipose$DISPOSITION), 
    adipose = unique(irec_adipose_all$adipose)
  ))
  
  
  #create zero observations, with 0 variance
  irecall_adipose_all <- left_join(allobs_adipose_all, irec_adipose_all)
  irecall_adipose_all$ESTIMATE[is.na(irecall_adipose_all$ESTIMATE)] <- 0
  irecall_adipose_all$VARIANCE[is.na(irecall_adipose_all$VARIANCE)]<-0
  irecall_adipose_all <- irecall_adipose_all %>% left_join(arealu[, c("AREA", "LU_GROUPING3")])
  
  ireccc_adipose_all <- irecall_adipose_all %>%
    select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, VARIANCE, LU_GROUPING3, adipose)) %>%
    group_by(AREA, YEAR, MONTH, DISPOSITION, LU_GROUPING3, adipose) %>%
    summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE), VARIANCE = sum(VARIANCE, na.rm = TRUE)) %>%
    mutate(SD = sqrt(VARIANCE)) %>%
    select(c(!VARIANCE)) %>%
    rename(IREC = ESTIMATE, SDIREC = SD)
  
  
  ### merge irec and creel adipose
  irec_creel_merged_adipose_all <- merge(creel_adipose_all, ireccc_adipose_all, all=TRUE) %>% as_tibble() 
  names(irec_creel_merged_adipose_all) <- tolower(names(irec_creel_merged_adipose_all ))
  irec_creel_merged_adipose_all <- rename(irec_creel_merged_adipose_all, region = lu_grouping3)
  irec_creel_merged_adipose_all
  

  
  irec_creel_merged_adipose_all<-irec_creel_merged_adipose_all%>% mutate(erafishery = case_when(
    area%in%c("Area 121", "Area 122", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") ~ "WCVI AABM S",
    area%in%c("Area 21", "Area 22", "Area 24") & month%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",
    grepl("Area 23", area) & month%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",
    area%in%c("Area 21", "Area 22", "Area 24") & month%in%c(8,9) ~ "WCVI ISBM S",
    grepl("Area 23", area) & month%in%c(8,9) ~ "WCVI ISBM S",
    area%in%c("Area 25", "Area 26", "Area 27") & month%in%c(10,11,12,1,2,3,4,5,6) ~ "WCVI AABM S",
    area%in%c("Area 25", "Area 26", "Area 27") & month%in%c(7,8,9) ~ "WCVI ISBM S",
    area %in% treaty_cbc~ "CBC S", 
    area %in% treaty_nbc_aabm~ "NBC AABM S", 
    area %in% treaty_nbc_isbm~ "NBC ISBM S", 
    area %in% gst~ "GEO ST S",
    area %in% jst~ "JNST S",
    area %in% jdf~ "BC JF S"))

  
  irec_creel_merged_adipose_all_1<-  irec_creel_merged_adipose_all %>% mutate(licence.year= case_when(
    month > 3 ~ as.numeric(year),
    month < 4 ~ as.numeric(year - 1 )))
  irec_creel_merged_adipose_all_1<-merge( irec_creel_merged_adipose_all_1, bcf_short, all=TRUE)%>% as_tibble()
  

  
  #create a pseudocreel_version1 - do this on an area by area basis
  #no variation - took out those parts
  irec_creel_merged_adipose_all_pseudo<-irec_creel_merged_adipose_all_1 %>%  mutate(
    #irec_var = sdirec ^ 2, 
    # creel_var = sdcreel ^ 2, 
    pseudocreel = case_when(
      year > 2011 & month %in% c(5:9) & is.na(creel) ~ as.numeric(irec/bcf),
      year > 2011 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
      year < 2012 ~  NA_real_,
      TRUE ~ as.numeric(creel))
    #, 
    # pseudocreel_var = case_when(
    #  month %in% c(5:9) & is.na(creel_var) ~ as.numeric(irec_var/bcf),
    # month %in% c(1:4,10:12) ~ as.numeric(irec_var/bcf),
    #TRUE ~ as.numeric(creel_var))
  )
  

  
  
  #### Summarise across year:
  irec_creel_merged_adipose_all_pseudo_sum_erafishery<- irec_creel_merged_adipose_all_pseudo %>% group_by(erafishery, year, disposition, adipose) %>% 
    summarise(creel_sum=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
              # creel_var_sum=ifelse(all(is.na(creel_var)), NA, sum(creel_var, na.rm=TRUE)),
              #  creel_sd_sum = sqrt(creel_var_sum),
              pseudocreel_sum=sum(pseudocreel, na.rm = TRUE)
              #, 
              # pseudocreel_var_sum=sum(pseudocreel_var, na.rm = TRUE), 
              #  pseudocreel_sd_sum=sqrt(pseudocreel_var_sum)
    ) 
  #%>% 
  #  select(-pseudocreel_var_sum, -creel_var_sum)
  
  

  adipose_all_long<- irec_creel_merged_adipose_all_pseudo_sum_erafishery %>% pivot_longer(cols=c("creel_sum", "pseudocreel_sum"), names_to = "source", values_to = "values")

  adipose_all_long 
  
  adipose_kept <- ggplot(adipose_all_long %>% filter(disposition=="Kept", source=="creel_sum") ,aes(x=as.factor(year), y=values, shape=source, color=adipose, group=adipose))
  adipose_kept <- adipose_kept + geom_point(size=3, alpha=.5)+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
  adipose_kept 
  
  adipose_kept <- ggplot(adipose_all_long %>% filter(disposition=="Kept", source=="pseudocreel_sum") ,aes(x=as.factor(year), y=values, shape=source, color=adipose, group=adipose))
  adipose_kept <- adipose_kept + geom_point(size=3, alpha=.5)+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
  adipose_kept 
  
 

  adipose_all_wide<- irec_creel_merged_adipose_all_pseudo_sum_erafishery %>% pivot_wider(names_from = adipose, values_from = c(creel_sum, pseudocreel_sum))
  adipose_all_wide<-  adipose_all_wide %>% mutate(total_creel = creel_sum_Adipose_Marked +  creel_sum_Not_Adipose_Marked +  creel_sum_Not_Adipose_Checked, 
                                                  total_pseudocreel = pseudocreel_sum_Adipose_Marked +  pseudocreel_sum_Not_Adipose_Marked +  pseudocreel_sum_Not_Adipose_Checked, 
                                                  prop_creel_notchecked = creel_sum_Not_Adipose_Checked/total_creel, 
                                                  prop_pseudocreel_notchecked = pseudocreel_sum_Not_Adipose_Checked/total_pseudocreel,
                                                  prop_creel_marked = creel_sum_Adipose_Marked/total_creel, 
                                                  prop_pseudocreel_marked = pseudocreel_sum_Adipose_Marked/total_pseudocreel,
                                                  prop_creel_unmarked = creel_sum_Not_Adipose_Marked/total_creel, 
                                                  prop_pseudocreel_unmarked = pseudocreel_sum_Not_Adipose_Marked/total_pseudocreel
                                                  )
  
  adipose_all_wide_long<- adipose_all_wide %>% pivot_longer(cols=contains("creel"), names_to = "variable", values_to = "values")
  adipose_all_wide_long_pseudo_prop<- adipose_all_wide_long %>% filter(variable %in% c("prop_pseudocreel_unmarked", "prop_pseudocreel_marked", "prop_pseudocreel_notchecked")) 
  adipose_all_wide_long_creel_prop<- adipose_all_wide_long %>% filter(variable %in% c("prop_creel_unmarked", "prop_creel_marked", "prop_creel_notchecked")) 
  
  #this plot is proportion of adipose kept changing over time? 
  adipose_kept_marked <- ggplot(adipose_all_wide_long_pseudo_prop ,aes(x=as.factor(year), y=values,  color=variable, group=variable ))
  adipose_kept_marked <- adipose_kept_marked + geom_point(size=3, alpha=.5)+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
  adipose_kept_marked  
  

  
ggplot(adipose_all_long %>% filter(disposition=="Kept", source=="pseudocreel_sum") ,aes(x=as.factor(year), y=values, fill=adipose))+geom_bar(position="stack", stat="identity")+ facet_wrap(~disposition + erafishery, scales="free") 

ggplot(adipose_all_long %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, fill=adipose))+geom_bar(position="stack", stat="identity")+ facet_wrap(~disposition + erafishery + source, scales="free") 

ggplot(adipose_all_long %>% filter(disposition=="Released") ,aes(x=as.factor(year), y=values, fill=adipose))+geom_bar(position="stack", stat="identity")+ facet_wrap(~disposition + erafishery + source, scales="free") 

