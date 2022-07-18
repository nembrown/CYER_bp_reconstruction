#=======================================================================
# This script is for recreating exactly the 2021 CNR file 
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

#useful function
"%notin%" <- Negate("%in%")

# load irec and creel data -------------------------------------------------------------------------

#bcf is the calibration factor
bcf<-read.csv(here::here("data/bcf.csv"))
bcf<- bcf %>% filter(Species=="Chinook") %>% as_tibble()
names(bcf) <- tolower(names(bcf))
bcf_short<-bcf %>% select(licence.year, disposition, bcf) %>% filter(licence.year>2019)

### Load in the creel and irec data, this is the exact data that Katie used
creel_katie<- read_excel(here::here("data/2021 Creel Catch.xlsx"), sheet="Export Worksheet")
irec_2021<- read_excel(here::here("data/2021 iREC Catch.xlsx"), sheet="Angling from boat") 


# filter creel data ---------------------------------------------------------

#Split by datasource just to see if that's where inconsistencies come in 
#need to have retainable as a factor 
creel_katie_wide<- creel_katie %>% pivot_wider(names_from = SOURCE, values_from = VAL) %>% 
                                   rename(creel_in_fill_sc =`In Fill`, 
                                              logbook_estimate =`Log Estimate`,
                                              creel =`Creel Estimate`,
                                              lodge_log =`Lodge Log`, AREA_NUM=AREA, AREA=AREA_GROUP, DISPOSITION=TYPE, RETAINABLE=SUB_TYPE
                                              )%>% 
                                   rowwise() %>% 
                                   mutate(creel_log_total = sum(logbook_estimate, creel, creel_in_fill_sc, lodge_log, na.rm=TRUE), 
                                   log_total = sum(logbook_estimate, lodge_log, na.rm=TRUE))
names(creel_katie_wide) <- tolower(names(creel_katie_wide))

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
creel_katie_wide<- creel_katie_wide %>%  mutate(erafishery = case_when(
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
creel_katie_wide_erafishery<- creel_katie_wide %>%  group_by(erafishery, year, month, disposition, retainable) %>% 
                                            summarise(creel = sum(creel, na.rm=TRUE),
                                                      lodge_log = sum(lodge_log, na.rm=TRUE),
                                                      logbook_estimate = sum(logbook_estimate, na.rm=TRUE),
                                                      creel_in_fill_sc = sum(creel_in_fill_sc, na.rm=TRUE),
                                                      creel_log_total = sum(creel_log_total, na.rm=TRUE),
                                                      log_total = sum(log_total, na.rm=TRUE))  
                                          
#summarize by era fishery by year
creel_katie_wide_erafishery_year<- creel_katie_wide %>%  group_by(erafishery, year, disposition, retainable) %>% 
                                                         summarise(creel = sum(creel, na.rm=TRUE),
                                                         lodge_log = sum(lodge_log, na.rm=TRUE),
                                                         logbook_estimate = sum(logbook_estimate, na.rm=TRUE),
                                                         creel_in_fill_sc = sum(creel_in_fill_sc, na.rm=TRUE),
                                                         creel_log_total = sum(creel_log_total, na.rm=TRUE),
                                                         log_total = sum(log_total, na.rm=TRUE))  
                                             
#summarize by area AND erafishery by month 
creelcc_katie<- creel_katie_wide %>%  group_by(area, erafishery, year, month, disposition, retainable) %>% 
                                      summarise(creel = sum(creel, na.rm=TRUE),
                                      lodge_log = sum(lodge_log, na.rm=TRUE),
                                      logbook_estimate = sum(logbook_estimate, na.rm=TRUE),
                                      creel_in_fill_sc = sum(creel_in_fill_sc, na.rm=TRUE),
                                      creel_log_total = sum(creel_log_total, na.rm=TRUE),
                                      log_total = sum(log_total, na.rm=TRUE))  



# filter irec data --------------------------------------------------------

#include retainable
irec <- irec_2021 %>% filter(METHOD == "Angling from boat") %>% 
                      mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
                      filter(AREA != "Area 29 (In River)") %>% 
                      mutate(YEAR = as.numeric(YEAR), MONTH = as.numeric(MONTH)) %>% 
                      mutate(RETAINABLE = case_when(
                             grepl("Legal", RETAINABLE)  ~ "LEGAL", 
                             grepl("Sublegal", RETAINABLE)  ~ "SUB-LEGAL"))
                     
# Expand Irec data
allobs <- expand.grid(list(
  AREA = unique(irec$AREA),
  YEAR = 2021,
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
                      summarise(irec = sum(irec, na.rm=TRUE)) %>% 
                      filter(year>2020)

#group by era fishery and month
ireccc_erafishery<- ireccc %>%  group_by(erafishery, year, month, disposition, retainable) %>% 
                                summarise(irec = sum(irec, na.rm=TRUE)) %>% 
                                filter( year>2020)

# Merge creel and irec option 1 ----------------------------------------------------

#Merge creel and irec (by era fishery AND area)
irec_creel_merged <- merge(creelcc_katie, ireccc1, all=TRUE) %>% as_tibble() 

#add in calibration factor to option 1
irec_creel_merged1<- irec_creel_merged %>% mutate(licence.year= case_when(
                                       month > 3 ~ as.numeric(year),
                                       month < 4 ~ as.numeric(year - 1 )))
irec_creel_merged1<-merge(irec_creel_merged1, bcf_short, all=TRUE)%>% as_tibble()

#create a pseudocreel_version1 - do this on an area by area basis
#this is how I would calculate this
irec_creel_merged_pseudo<-irec_creel_merged1 %>%  mutate(pseudocreel = case_when(
                                                  year > 2011 & month %in% c(5:9) & is.na(creel_log_total) ~ as.numeric(irec/bcf),
                                                  year > 2011 & month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                  year < 2012 ~  NA_real_,
                                                  TRUE ~ as.numeric(creel_log_total)))

#### Summarize across year:
irec_creel_merged_pseudo_sum_erafishery<- irec_creel_merged_pseudo %>% group_by(erafishery, year, disposition, retainable) %>% 
                                                                       summarise(creel_sum=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                       log_total_sum=ifelse(all(is.na(log_total)), NA, sum(log_total, na.rm=TRUE)),
                                                                       creel_log_total_sum=ifelse(all(is.na(creel_log_total)), NA, sum(creel_log_total, na.rm=TRUE)),
                                                                       creel_infill_sum=ifelse(all(is.na(creel_in_fill_sc)), NA, sum(creel_in_fill_sc, na.rm=TRUE)),
                                                                       irec_sum=ifelse(all(is.na(irec)), NA, sum(irec, na.rm=TRUE)),
                                                                       pseudocreel_sum=ifelse(all(is.na(pseudocreel)), NA, sum(pseudocreel, na.rm=TRUE))) 



# Merge creel and irec option 2 -------------------------------------------

#second option is merge creel and irec just by era fishery 
irec_creel_merged2 <- merge(creel_katie_wide_erafishery, ireccc_erafishery, all=TRUE) %>% as_tibble() 
irec_creel_merged2$creel_log_total[is.na(irec_creel_merged2$creel_log_total)] <- 0
irec_creel_merged2$irec[is.na(irec_creel_merged2$irec)]<-0


#add in calibration factor to option 2
irec_creel_merged2 <-  irec_creel_merged2 %>% rename(irec2 = irec, creel_log_total2=creel_log_total) %>% 
  mutate(licence.year= case_when(
    month > 3 ~ as.numeric(year),
    month < 4 ~ as.numeric(year - 1 )))
irec_creel_merged2 <- merge(irec_creel_merged2, bcf_short, all=TRUE)%>% as_tibble()

#This is Katie's method
irec_creel_merged_pseudo2<-irec_creel_merged2 %>%  mutate(pseudocreel2_irec_portion = case_when(
                                                  month %in% c(5:9) & creel_log_total2 == 0 ~ as.numeric(irec2/bcf),
                                                  month %in% c(1:4,10:12) ~ as.numeric(irec2/bcf),
                                                   TRUE ~ 0))
#### Summarize across year:
irec_creel_merged_pseudo_sum_erafishery2<- irec_creel_merged_pseudo2 %>% group_by(erafishery, year, disposition, retainable) %>% 
                                                                         summarise(creel_log_total_sum2=ifelse(all(is.na(creel_log_total2)), NA, sum(creel_log_total2, na.rm=TRUE)),
                                                                         irec_sum2=ifelse(all(is.na(irec2)), NA, sum(irec2, na.rm=TRUE)),
                                                                         pseudocreel_irec_portion_sum2=ifelse(all(is.na(pseudocreel2_irec_portion)), NA, sum(pseudocreel2_irec_portion, na.rm=TRUE))) 


# Join option 1 and 2 -----------------------------------------------------

irec_creel_merged_pseudo_sum_erafishery_3<- merge(irec_creel_merged_pseudo_sum_erafishery, irec_creel_merged_pseudo_sum_erafishery2, all=TRUE) %>% as_tibble()

irec_creel_merged_pseudo_sum_erafishery_3<- irec_creel_merged_pseudo_sum_erafishery_3 %>%  mutate(cnr_recreated = creel_log_total_sum2 + pseudocreel_irec_portion_sum2)


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

cwdb<-tbl(campdb, "cwdbrecovery")%>% as_tibble()

cnr<- merge(cnr, fishery.era) %>% as_tibble() %>% rename(erafishery=Name, year = ERAYear)
cnr_canada_sport<- cnr %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S", "WCVI AABM S", "WCVI ISBM S", "BC JF S", "GEO ST S", "JNST S"))   
cnr_canada_sport<- cnr_canada_sport %>% rename(Kept = CNRValue1, Released= CNRValue2) %>% select(erafishery, year, Kept, Released) 
cnr_canada_sport<- cnr_canada_sport %>% pivot_longer(cols=c("Kept", "Released"), names_to = "disposition", values_to = "cnr")
cnr_canada_sport


# Merge CNR with irec and creel -------------------------------------------

irec_creel_cnr<-merge(irec_creel_merged_pseudo_sum_erafishery_3, cnr_canada_sport, all=TRUE) %>% as_tibble()  %>%  filter(year>2020)                              

irec_creel_cnr_plot<- irec_creel_cnr %>% pivot_longer(cols=contains(c("sum", "cnr")), names_to = "source", values_to = "values")  


# Plotting ----------------------------------------------------------------

pkept <- ggplot(irec_creel_cnr_plot %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept

#katies
pkept <- ggplot(irec_creel_cnr %>% filter(disposition=="Kept", source %in% c("irec_sum", "creel_log_total_sum", "cnr", "pseudocreel_sum", "pseudocreel_sum2")) ,aes(x=as.factor(year), y=values, color=source, shape=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) + facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept

