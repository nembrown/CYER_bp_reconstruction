#=======================================================================
# Base period reconstruction for CYER 
# #Norah Brown
# May 2022
#=======================================================================



# load packages -----------------------------------------------------------

# devtools::install_github("jwb133/smcfcs")
library(ggplot2)
library(dplyr)
library(grid)
library(brms)
library(bayesplot)

library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(cowplot)
library(rstan)
library(ggrepel)
library(posterior)
library(hmi)
library(smcfcs)
library(lme4)
library(simputation)
library(naniar)
library(visdat)

library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr)



options("install.lock"=FALSE)

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
creel <- read.csv(here::here("data/creel_filter_2008_2021.csv"))
irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))
arealu <- read.csv(here::here("data/areaLU.csv"))

#useful function
"%notin%" <- Negate("%in%")

#Edit creel data
#Sum the eastern and western portions of 23, 19, 2 - before April 2014 and Area 20 - before April 2020
#Area 20 occasionally only had W or E, not both
#The list below is only combining the estimates for months which contain BOTH east and West
#SURVEY can identify different types of creel ... but need to combine them to get the proper estimates

creelcc <- creel %>%
  rename(AREA = PFMA) %>%
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
  group_by(AREA, YEAR, MONTH, TYPE) %>% 
  summarise(ESTIMATE = sum(ESTIMATE),STANDARD_ERROR = sum(STANDARD_ERROR)) %>%  
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
  summarise(ESTIMATE = sum(ESTIMATE), VARIANCE = sum(VARIANCE)) %>%
  mutate(SD = sqrt(VARIANCE)) %>%
  select(c(!VARIANCE)) %>%
  rename(IREC = ESTIMATE, SDIREC = SD)

#should we change this to June since there are 91% missing data in May for creel? Changed to 6 here
irec_creel_merged <- merge(creelcc, ireccc, all=TRUE) %>% as_tibble() %>% 
  mutate(SEASON = if_else(MONTH < 6 | MONTH > 9, "offseason", "peakseason"))

names(irec_creel_merged) <- tolower(names(irec_creel_merged))
irec_creel_merged <- rename(irec_creel_merged, region = lu_grouping3)
irec_creel_merged

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
irec_creel_merged_pseudo<-irec_creel_merged1 %>%  mutate(irec_var = sdirec ^ 2, 
                                                        creel_var = sdcreel ^ 2, 
                                                        pseudocreel = case_when(
                                                                      year > 2011 & month %in% c(5:9) & is.na(creel) ~ as.numeric(irec/bcf),
                                                                      year > 2011 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                                      year < 2012 ~  NA_real_,
                                                                      TRUE ~ as.numeric(creel)), 
                                                        pseudocreel_var = case_when(
                                                                          year > 2011 & month %in% c(5:9) & is.na(creel_var) ~ as.numeric(irec_var/bcf),
                                                                          year > 2011 & month %in% c(1:4,10:12) ~ as.numeric(irec_var/bcf),
                                                                          year < 2012 ~  NA_real_,
                                                                          TRUE ~ as.numeric(creel_var)))
#### Summarise across year:
irec_creel_merged_pseudo_sum_erafishery<- irec_creel_merged_pseudo %>% group_by(erafishery, year, disposition) %>% 
                                                                       summarise(creel_sum=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
                                                                                 creel_var_sum=ifelse(all(is.na(creel_var)), NA, sum(creel_var, na.rm=TRUE)),
                                                                                 creel_sd_sum = sqrt(creel_var_sum),
                                                                                 pseudocreel_sum=sum(pseudocreel), 
                                                                                 pseudocreel_var_sum=sum(pseudocreel_var), 
                                                                                 pseudocreel_sd_sum=sqrt(pseudocreel_var_sum)) %>% 
                                                                       select(-pseudocreel_var_sum, -creel_var_sum)



View(irec_creel_merged_pseudo_sum_erafishery)


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
fishery_lookup_simple<-mrp_rec_recoveries %>% select(region, area, psc_fishery_id, area_name) %>% distinct()
View(fishery_lookup_simple)


mrp_rec_recoveries_heads<- mrp_rec_recoveries %>% group_by(recovery_year, period_id, region, rec_month) %>% summarise(heads=n() )


mrp_recoveries<-getDfoTagRecoveries(2009:2022)


### Load in the creel and irec data:
creel_nick <- read.csv(here::here("data/2009 to 2021 Creel Data.csv"))
irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))
arealu <- read.csv(here::here("data/areaLU.csv"))

#Problem: Nick's data doesn't have variance
#Problem: Nick doesn't have released

#Edit creel data

creel_adipose <- creel_nick %>%
    rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
    filter(DATASOURCE == "Creel Estimate") %>%
    filter(MARKS_DESC == "Adipose Marked") %>%
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
    select(AREA, YEAR, MONTH, TYPE, ESTIMATE) %>% 
    group_by(AREA, YEAR, MONTH, TYPE) %>% 
    summarise(ESTIMATE = sum(ESTIMATE)) %>%  
    filter(AREA %notin% c("Area 20 (West)", "Area 20 (East)") ) %>% 
    left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
    rename(DISPOSITION = TYPE, CREEL = ESTIMATE)
  


creel_adipose_and_unchecked <- creel_nick %>%
  rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
  filter(DATASOURCE == "Creel Estimate") %>%
  filter(MARKS_DESC != "Not Adipose Marked") %>%
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
  select(AREA, YEAR, MONTH, TYPE, ESTIMATE) %>% 
  group_by(AREA, YEAR, MONTH, TYPE) %>% 
  summarise(ESTIMATE = sum(ESTIMATE)) %>%  
  filter(AREA %notin% c("Area 20 (West)", "Area 20 (East)") ) %>% 
  left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)

  
#Edit IREC data
irec_adipose <- irec %>% 
  filter(METHOD == "Angling from boat") %>% 
  mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
  filter(AREA != "Area 29 (In River)", YEAR > 2011) %>% 
  filter(ADIPOSE_MODIFIER == "Adipose Marked")

irec_adipose_and_unchecked <- irec %>% 
  filter(METHOD == "Angling from boat") %>% 
  mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
  filter(AREA != "Area 29 (In River)", YEAR > 2011) %>% 
  filter(ADIPOSE_MODIFIER != "Not Adipose Marked")



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
  select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, VARIANCE, LU_GROUPING3)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION, LU_GROUPING3) %>%
  summarise(ESTIMATE = sum(ESTIMATE), VARIANCE = sum(VARIANCE)) %>%
  mutate(SD = sqrt(VARIANCE)) %>%
  select(c(!VARIANCE)) %>%
  rename(IREC = ESTIMATE, SDIREC = SD)

#create zero observations, with 0 variance
irecall_adipose_and_unchecked <- left_join(allobs_adipose, irec_adipose_and_unchecked)
irecall_adipose_and_unchecked$ESTIMATE[is.na(irecall_adipose_and_unchecked$ESTIMATE)] <- 0
irecall_adipose_and_unchecked$VARIANCE[is.na(irecall_adipose_and_unchecked$VARIANCE)]<-0
irecall_adipose_and_unchecked <- irecall_adipose_and_unchecked %>% left_join(arealu[, c("AREA", "LU_GROUPING3")])

ireccc_adipose_and_unchecked <- irecall_adipose_and_unchecked %>%
  select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, VARIANCE, LU_GROUPING3)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION, LU_GROUPING3) %>%
  summarise(ESTIMATE = sum(ESTIMATE), VARIANCE = sum(VARIANCE)) %>%
  mutate(SD = sqrt(VARIANCE)) %>%
  select(c(!VARIANCE)) %>%
  rename(IREC = ESTIMATE, SDIREC = SD)


### merge irec and creel adipose
irec_creel_merged_adipose <- merge(creel_adipose, ireccc_adipose, all=TRUE) %>% as_tibble() 
names(irec_creel_merged_adipose) <- tolower(names(irec_creel_merged_adipose ))

irec_creel_merged_adipose_and_unchecked <- merge(creel_adipose_and_unchecked, ireccc_adipose_and_unchecked, all=TRUE) %>% as_tibble() 
names(irec_creel_merged_adipose_and_unchecked) <- tolower(names(irec_creel_merged_adipose_and_unchecked))


#Just for completeness, I identify these other areas, but they don't get used in psl/cnr 
#treaty
treaty_cbc<-c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130",  "Area 108", "Area 109", "Area 107")
treaty_nbc_aabm<-c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")
treaty_nbc_isbm<-c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")
gst<-c("Area 13", "Area 14", "Area 15", "Area 16", "Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") 
jst<-c("Area 11", "Area 111", "Area 12")
jdf<-c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)")


irec_creel_merged_adipose<-irec_creel_merged_adipose%>% mutate(erafishery = case_when(
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

irec_creel_merged_adipose_and_unchecked<-irec_creel_merged_adipose_and_unchecked %>% mutate(erafishery = case_when(
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


irec_creel_merged_adipose_1<-  irec_creel_merged_adipose %>% mutate(licence.year= case_when(
  month > 3 ~ as.numeric(year),
  month < 4 ~ as.numeric(year - 1 )))
irec_creel_merged_adipose_1<-merge( irec_creel_merged_adipose_1, bcf_short, all=TRUE)%>% as_tibble()

irec_creel_merged_adipose_and_unchecked_1<-  irec_creel_merged_adipose_and_unchecked %>% mutate(licence.year= case_when(
  month > 3 ~ as.numeric(year),
  month < 4 ~ as.numeric(year - 1 )))
irec_creel_merged_adipose_and_unchecked_1<-merge( irec_creel_merged_adipose_and_unchecked_1, bcf_short, all=TRUE)%>% as_tibble()



#create a pseudocreel_version1 - do this on an area by area basis
#no variation - took out those parts
irec_creel_merged_adipose_pseudo<-irec_creel_merged_adipose_1 %>%  mutate(
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


### add in regions here: 
fishery_simple<- irec_creel_merged_adipose_pseudo %>% select(area) %>% distinct()
fishery_simple<- fishery_simple %>%  mutate(region = case_when(
                                       area %in% c("Area 13", "Area 14", "Area 15", "Area 16") ~ "22", 
                                       area %in% c("Area 28", "Area 29",  "Area 17", "Area 18", "Area 19 (GS)") ~ "23", 
                                       area %in% c("Area 19 (JDF)", "Area 19", "Area 20", "Area 20 (East)", "Area 20 (West)") ~ "24", 
                                       area %in% c("Area 2","Area 1", "Area 101", "Area 102", "Area 142", "Area 2E", "Area 2W", "Area 3", "Area 4", "Area 104", "Area 103", "Area 5") ~ "25", 
                                       area %in% c("Area 10", "Area 11", "Area 111", "Area 12" , "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 108", "Area 109", "Area 107") ~ "26",  
                                       area %in% c("Area 23", "Area 123", "Area 24", "Area 124", "Area 25", "Area 125", "Area 26", "Area 126", "Area 27", "Area 127", "Area 23 (Barkley)", "Area 21", "Area 22", "Area 121") ~ "27",
                                       area == "Area 23 (Alberni Canal)" ~ "28")) %>% 
                                       add_row(area = "Area 13", region="61") %>% 
                                       add_row(area = "Area 14", region="62") %>% 
                                       add_row(area = "Area 15", region="62") %>% 
                                       add_row(area = "Area 16", region="62")
                                       
irec_creel_merged_adipose_pseudo_region<- merge(irec_creel_merged_adipose_pseudo, fishery_simple, all=TRUE) %>% as_tibble()
  


irec_creel_merged_adipose_and_unchecked_pseudo<- irec_creel_merged_adipose_and_unchecked_1 %>%  mutate(
  #irec_var = sdirec ^ 2, 
                                                       #  creel_var = sdcreel ^ 2, 
                                                         pseudocreel = case_when(
                                                          year > 2011 & month %in% c(5:9) & is.na(creel) ~ as.numeric(irec/bcf),
                                                          year > 2011 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                          year < 2012 ~  NA_real_,
                                                           TRUE ~ as.numeric(creel))
                                                         # , 
                                                         # pseudocreel_var = case_when(
                                                         #   month %in% c(5:9) & is.na(creel_var) ~ as.numeric(irec_var/bcf),
                                                         #   month %in% c(1:4,10:12) ~ as.numeric(irec_var/bcf),
                                                         #   TRUE ~ as.numeric(creel_var))
                                                         )
                                                          
                                                          


#### Summarise across year:
irec_creel_merged_adipose_pseudo_sum_erafishery<- irec_creel_merged_adipose_pseudo %>% group_by(erafishery, year, disposition) %>% 
  summarise(creel_sum=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
           # creel_var_sum=ifelse(all(is.na(creel_var)), NA, sum(creel_var, na.rm=TRUE)),
          #  creel_sd_sum = sqrt(creel_var_sum),
            pseudocreel_sum=sum(pseudocreel)
          #, 
           # pseudocreel_var_sum=sum(pseudocreel_var), 
          #  pseudocreel_sd_sum=sqrt(pseudocreel_var_sum)
          ) 
#%>% 
#  select(-pseudocreel_var_sum, -creel_var_sum)


irec_creel_merged_adipose_and_unchecked_pseudo_sum_erafishery<- irec_creel_merged_adipose_and_unchecked_pseudo %>% group_by(erafishery, year, disposition) %>% 
  summarise(creel_sum=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
            # creel_var_sum=ifelse(all(is.na(creel_var)), NA, sum(creel_var, na.rm=TRUE)),
            #  creel_sd_sum = sqrt(creel_var_sum),
            pseudocreel_sum=sum(pseudocreel)
            #, 
            # pseudocreel_var_sum=sum(pseudocreel_var), 
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
    summarise(ESTIMATE = sum(ESTIMATE)) %>%  
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
    summarise(ESTIMATE = sum(ESTIMATE), VARIANCE = sum(VARIANCE)) %>%
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
              pseudocreel_sum=sum(pseudocreel)
              #, 
              # pseudocreel_var_sum=sum(pseudocreel_var), 
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

