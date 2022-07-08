
# Combining creel and iREC data for updating catch database and CNR/.psl file

library(tidyverse)
library(readxl)
library(openxlsx)

setwd("~/ANALYSIS/data")

creel.raw <- read_excel("2021 Creel Catch.xlsx", sheet="Export Worksheet")
irec.boat.raw <- read_excel("2021 iREC Catch.xlsx", sheet="Angling from boat")       # angling from boat = marine, you want this!
#irec.shore.raw <- read_excel("2021 iREC Catch.xlsx", sheet="Angling from Shore")    # don't want from shore - this is freshwater which you get from other sources
bcfs.raw <- read_excel("BCFs across time.xlsx", sheet="Sheet1")

###################################################################################################################################################

#                                                         CLEAN

#----- Calibration factors 
bcfs <- bcfs.raw %>% 
  rename(year=`Licence Year`) %>%
  filter(Species=="Chinook", year%in%c(2020,2021)) %>%
  print()
bcf_kept_2020 <- bcfs[bcfs]


#----- Creel - roll up into PSC fisheries 
creel <- creel.raw %>% 
  rename(CREEL = VAL) %>%
  mutate(region_rollup = case_when(AREA%in%c(1,2,3,4,5,101,102,103,104,105,130,142)~"NBC",
                                   AREA%in%c(6,7,8,9,10,106,110) ~ "CBC",
                                   AREA%in%c(11,12,111) ~ "JST",
                                   AREA%in%c(13,14,15,16,17,18,28) ~ "GST",
                                   AREA%in%c(19,29)~paste0(REGION), 
                                   AREA==20 ~ "JDF",
                                   AREA%in%c(21,22,23,24,25,26,27,121,122,123,124,125,126,127) ~ paste("WCVI", MANAGEMENT, sep=" ")),
         region_rollup = ifelse(region_rollup=="GS", "GST", region_rollup),
         excel_data_source = "2021 Creel Catch.xlsx") %>%
  mutate_at("AREA", as.character) %>%
  mutate_at("YEAR", as.numeric)


#----- iREC boat data - change to match Creel headers, roll up into PSC fisheries 
irec.boat <- irec.boat.raw %>% 
  rename(DATASOURCE = METHOD,
         SPECIES_TXT = ITEM,
         MARKS_DESC = ADIPOSE_MODIFIER,
         TYPE = DISPOSITION,
         SUB_TYPE = RETAINABLE,
         GRP = ITEM_GROUP) %>%
  mutate(calibration_factor = case_when(MONTH%in%c(1,2,3) & TYPE=="Kept" ~ bcfs[bcfs$Disposition=="Kept"&bcfs$year=="2020",]$BCF,
                                        MONTH%in%c(1,2,3) & TYPE=="Released" ~ bcfs[bcfs$Disposition=="Released"&bcfs$year=="2020",]$BCF,
                                        !MONTH%in%c(1,2,3) & TYPE=="Kept" ~ bcfs[bcfs$Disposition=="Kept"&bcfs$year=="2021",]$BCF,
                                        !MONTH%in%c(1,2,3) & TYPE=="Released" ~ bcfs[bcfs$Disposition=="Released"&bcfs$year=="2021",]$BCF,),
         iREC_cal = ESTIMATE/calibration_factor,
         region_rollup = case_when(AREA%in%c("Area 1", "Area 2", "Area 2E", "Area 2W", "Area 3", "Area 4", "Area 5", "Area 101", "Area 102", 
                                             "Area 103", "Area 104", "Area 105", "Area 130", "Area 142") ~ "NBC",
                                   AREA%in%c("Area 6", "Area 7", "Area 8", "Area 9", "Area 10", "Area 106", "Area 107", "Area 108", "Area 109",
                                             "Area 110") ~ "CBC",
                                   AREA%in%c("Area 11", "Area 12", "Area 111") ~ "JST",
                                   AREA%in%c("Area 13", "Area 14", "Area 15", "Area 16", "Area 17", "Area 18", "Area 19 (GS)", "Area 28", 
                                             "Area 29 (Marine)") ~ "GST",
                                   AREA=="Area 19 (JDF)" ~ "JDF", 
                                   grepl("Area 20", AREA) ~ "JDF",
                                   AREA%in%c("Area 29 (In River)") ~ "Fraser",
                                   AREA%in%c("Area 121", "Area 122", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") ~ "WCVI AABM",
                                   
                                   AREA%in%c("Area 21", "Area 22", "Area 24") & MONTH%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM",
                                   grepl("Area 23", AREA) & MONTH%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM",
                                   
                                   AREA%in%c("Area 21", "Area 22", "Area 24") & MONTH%in%c(8,9) ~ "WCVI ISBM",
                                   grepl("Area 23", AREA) & MONTH%in%c(8,9) ~ "WCVI ISBM",
                                   
                                   AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(10,11,12,1,2,3,4,5,6) ~ "WCVI AABM",
                                   
                                   AREA%in%c("Area 25", "Area 26", "Area 27") & MONTH%in%c(7,8,9) ~ "WCVI ISBM"),
         SOURCE = DATASOURCE,
         SPECIES_TXT2 = SPECIES_TXT,
         excel_data_source = "2021 iREC Catch.xlsx") %>%
  mutate_at("YEAR", as.numeric) %>%
  select(-c(calibration_factor, ESTIMATE))



###################################################################################################################################################

#                                                            JOIN & SUMMARIZE

#------ JOIN
# Remove area 22 as it is not in the treaty 
combo <- full_join(creel, irec.boat) %>%
  mutate(SUB_TYPE = case_when(SUB_TYPE%in%c("Legal Size (All or Lower)", "Legal Size (Upper)") ~ "LEGAL",
                              SUB_TYPE=="Sublegal Size"~"SUB-LEGAL",
                              TRUE ~ as.character(SUB_TYPE)),
         estimate_cal = ifelse(is.na(iREC_cal), CREEL, iREC_cal)) %>%
  filter(!AREA%in%c(22, "Area 22")) %>%
  print()


#------ SUMMARIZE 
# To recreate the big pivot table (as seen in "2020 Combined Creel & iREC Catch.xlsx">"Pivot" )
pivot_full <- combo %>%
  group_by(region_rollup, MONTH, TYPE) %>%
  summarize(sum_creel = sum(CREEL,na.rm=T), sum_irec = sum(iREC_cal,na.rm=T)) %>%
  arrange(region_rollup, MONTH, TYPE) %>%
  print()

# creel estimates given criteria (use creel for May-Sept)
region_CREELS <- pivot_full %>%
  filter(!region_rollup%in%c("NBC", "CBC", "Fraser"), MONTH%in%c(5,6,7,8,9)) %>%
  group_by(region_rollup, TYPE) %>%
  summarize(total = round(sum(sum_creel),0)) %>%
  mutate(group = "creel") %>%
  print()

# iREC estimates given criteria (use iREC for times with no creel)
region_iREC <- pivot_full %>%
  filter(!region_rollup%in%c("NBC", "CBC", "Fraser"), 
         MONTH%in%c(10,11,12,1,2,3,4) | MONTH%in%c(5,6,7,8,9) & sum_creel==0 & sum_irec>0) %>%
  group_by(region_rollup, TYPE) %>%
  summarize(total = round(sum(sum_irec),0)) %>%
  mutate(group = "iREC") %>%
  print()

# join for viewing
final_ests <- full_join(region_CREELS, region_iREC) %>%
  arrange(region_rollup)


###################################################################################################################################################

#                                                                      METADATA 

# Make lil metadata df to add to Excel export 
meta <- data.frame(info = c(paste("created", Sys.time(), sep=" "), "combined in R"))


###################################################################################################################################################

#                                                                      EXPORT 

combo.wkb <- createWorkbook()

addWorksheet(combo.wkb, "metadata")
addWorksheet(combo.wkb, "Combined data")
addWorksheet(combo.wkb, "Pivot")
addWorksheet(combo.wkb, "Summary")

writeData(combo.wkb, sheet="metadata", x=meta)
writeData(combo.wkb, sheet="Combined data", x=combo)
writeData(combo.wkb, sheet="Pivot", x=pivot_full)
writeData(combo.wkb, sheet="Summary", x=final_ests)

saveWorkbook(combo.wkb, "2021 Combined Creel & iREC Catch.xlsx", overwrite = T)









