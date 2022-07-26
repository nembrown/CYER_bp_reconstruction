
ireccc_adipose <- irecall_adipose %>%
  select(c(AREA, YEAR, MONTH, DISPOSITION, ADIPOSE_MODIFIER, ESTIMATE, VARIANCE, LU_GROUPING3)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION, ADIPOSE_MODIFIER, LU_GROUPING3) %>%
  summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE), VARIANCE = sum(VARIANCE, na.rm = TRUE)) %>%
  mutate(SD = sqrt(VARIANCE)) %>%
  select(c(!VARIANCE)) %>%
  rename(IREC = ESTIMATE, SDIREC = SD)
names(ireccc_adipose) <- tolower(names(ireccc_adipose ))





creel_kept_marked_prop<- creel_nick %>% 
  rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
  filter(TYPE == "Kept") %>%
  select(AREA, SUBAREA, YEAR, MONTH, TYPE, MARKS_DESC, DATASOURCE, STATUS, ESTIMATE) %>% 
  group_by(AREA,SUBAREA, YEAR, MONTH, TYPE, MARKS_DESC, DATASOURCE, STATUS) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)
names(creel_kept_marked_prop) <- tolower(names(creel_kept_marked_prop))

creel_kept_marked_prop<- creel_kept_marked_prop %>% mutate(marks_desc = case_when(
  marks_desc=="Adipose Marked" ~ "Adipose_Marked", 
  marks_desc=="Not Adipose Checked" ~ "Not_Adipose_Checked", 
  marks_desc=="Not Adipose Marked" ~ "Not_Adipose_Marked", 
  marks_desc=="Not Applicable" ~ "Not_Applicable")) %>% 
  pivot_wider(names_from=marks_desc, values_from=creel) %>% 
  mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked), 
         Not_Adipose_Checked_marked = marked_prop*Not_Adipose_Checked, 
         creel = Adipose_Marked + Not_Adipose_Checked_marked)


creel_adipose<- creel_kept_marked_prop %>% select(area, year, month, disposition, creel) %>% 
  group_by(area,year, month, disposition) %>% 
  summarise(creel = sum(creel, na.rm=TRUE))



creel_adipose_for_irec <- creel_kept_marked_prop %>%
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
    area == "Area 20" & month %in% c(1) & year %in% c(2008,2009,2012) ~ "Area 20 (East)", 
    area == "Area 20" & month %in% c(2) & year %in% c(2008,2009,2011,2012,2014,2015,2018) ~ "Area 20 (East)", 
    area == "Area 20" & month %in% c(3) & year %in% c(2008,2009,2010,2011,2012,2013,2015,2016,2017,2018) ~ "Area 20 (East)", 
    area == "Area 20" & month %in% c(4) & year %in% c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019) ~ "Area 20 (East)", 
    area == "Area 20" & month %in% c(5) & year %in% c(2009,2010,2011,2012,2014,2015,2016,2018) ~ "Area 20 (East)", 
    area == "Area 20" & month %in% c(10) & year %in% c(2015,2017,2018, 2019) ~ "Area 20 (East)", 
    area == "Area 20" & month %in% c(11) & year %in% c(2008,2009,2011) ~ "Area 20 (East)", 
    area == "Area 20" & month %in% c(12) & year %in% c(2008,2009,2011) ~ "Area 20 (East)", 
    TRUE ~ as.character(area)
  )) %>%
  select(area, year, month, disposition, creel) %>% 
  group_by(area,year, month, disposition) %>% 
  summarise(creel = sum(creel, na.rm=TRUE))



#Edit IREC data

irec_adipose <- irec %>% 
  filter(METHOD == "Angling from boat") %>% 
  mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
  filter(AREA != "Area 29 (In River)", YEAR > 2011) %>% 
  filter(DISPOSITION == "Kept")

#expand irec data:
allobs_adipose <- expand.grid(list(
  AREA = unique(irec_adipose$AREA),
  YEAR = unique(irec_adipose$YEAR),
  MONTH = unique(irec_adipose$MONTH),
  DISPOSITION = unique(irec_adipose$DISPOSITION), 
  ADIPOSE_MODIFIER = unique(irec_adipose$ADIPOSE_MODIFIER)
))


irecall_adipose <- left_join(allobs_adipose, irec_adipose)
irecall_adipose$ESTIMATE[is.na(irecall_adipose$ESTIMATE)] <- 0
irecall_adipose<- irecall_adipose %>% rename(marks_desc=ADIPOSE_MODIFIER) %>% select(- c(VARIANCE, ITEM_GROUP, ITEM, METHOD))

irec_kept_marked_prop<- irecall_adipose %>% mutate(marks_desc = case_when(
  marks_desc=="Adipose Marked" ~ "Adipose_Marked", 
  marks_desc=="Not Checked" ~ "Not_Adipose_Checked", 
  marks_desc=="Not Adipose Marked" ~ "Not_Adipose_Marked")) %>% 
  pivot_wider(names_from=marks_desc, values_from=ESTIMATE) %>% 
  mutate(marked_prop = Adipose_Marked/(Adipose_Marked + Not_Adipose_Marked), 
         Not_Adipose_Checked_marked = marked_prop*Not_Adipose_Checked, 
         irec = Adipose_Marked + Not_Adipose_Checked_marked)

ireccc_adipose <- irec_kept_marked_prop %>%
  select(c(AREA, YEAR, MONTH, DISPOSITION, irec)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION) %>%
  summarise(irec= sum(irec, na.rm = TRUE))
names(ireccc_adipose) <- tolower(names(ireccc_adipose))


### merge irec and creel adipose
irec_creel_merged_adipose <- merge(creel_adipose_for_irec, ireccc_adipose, all=TRUE) %>% as_tibble() 








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














#### moved the just adipose here: 

#Edit creel data
creel_adipose <- creel_nick %>%
  rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
  #  filter(DATASOURCE == "Creel Estimate") %>%
  # filter(STATUS == "Published Estimate - Full Month") %>% 
  filter(MARKS_DESC == "Adipose Marked") %>%
  filter(TYPE == "Kept") %>%
  # mutate(ESTIMATE = case_when(
  #   DATASOURCE == "Lodge Reported Catch" ~ ESTIMATE*3, 
  #   TRUE ~ ESTIMATE))%>% 
  select(AREA, YEAR, MONTH, TYPE, ESTIMATE) %>% 
  group_by(AREA,YEAR, MONTH, TYPE) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
  left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)
names(creel_adipose) <- tolower(names(creel_adipose))


### moved just adipose here: 

creel_adipose_for_irec <- creel_nick %>%
  rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
  #    filter(DATASOURCE == "Creel Estimate") %>%
  #  filter(STATUS == "Published Estimate - Full Month") %>% 
  filter(MARKS_DESC == "Adipose Marked") %>%
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
  select(AREA, YEAR, MONTH, TYPE, ESTIMATE) %>% 
  group_by(AREA,YEAR, MONTH, TYPE) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
  left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)














#### moved the "and unchecked here"


creel_adipose_and_unchecked <- creel_nick %>%
  rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
  # filter(DATASOURCE == "Creel Estimate") %>%
  filter(MARKS_DESC != "Not Adipose Marked") %>%
  filter(TYPE == "Kept") %>%
  select(AREA, YEAR, MONTH, TYPE, ESTIMATE) %>% 
  group_by(AREA, YEAR, MONTH, TYPE) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
  filter(AREA %notin% c("Area 20 (West)", "Area 20 (East)") ) %>% 
  left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)
names(creel_adipose_and_unchecked) <- tolower(names(creel_adipose_and_unchecked))

creel_adipose_and_unchecked_for_irec <- creel_nick %>%
  rename(AREA_NUM = AREA, AREA = AREA_GROUP, ESTIMATE=VAL) %>%
  # filter(DATASOURCE == "Creel Estimate") %>%
  filter(MARKS_DESC != "Not Adipose Marked") %>%
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
  select(AREA, YEAR, MONTH, TYPE, ESTIMATE) %>% 
  group_by(AREA, YEAR, MONTH, TYPE) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE)) %>%  
  filter(AREA %notin% c("Area 20 (West)", "Area 20 (East)") ) %>% 
  left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
  rename(DISPOSITION = TYPE, CREEL = ESTIMATE)

irec_adipose_and_unchecked <- irec %>% 
  filter(METHOD == "Angling from boat") %>% 
  mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
  filter(AREA != "Area 29 (In River)", YEAR > 2011) %>% 
  filter(ADIPOSE_MODIFIER != "Not Adipose Marked") %>% 
  filter(DISPOSITION == "Kept")


#create zero observations, with 0 variance
irecall_adipose_and_unchecked <- left_join(allobs_adipose, irec_adipose_and_unchecked)
irecall_adipose_and_unchecked$ESTIMATE[is.na(irecall_adipose_and_unchecked$ESTIMATE)] <- 0
irecall_adipose_and_unchecked$VARIANCE[is.na(irecall_adipose_and_unchecked$VARIANCE)]<-0
irecall_adipose_and_unchecked <- irecall_adipose_and_unchecked %>% left_join(arealu[, c("AREA", "LU_GROUPING3")])

ireccc_adipose_and_unchecked <- irecall_adipose_and_unchecked %>%
  select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, VARIANCE, LU_GROUPING3)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION, LU_GROUPING3) %>%
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=TRUE), VARIANCE = sum(VARIANCE, na.rm=TRUE)) %>%
  mutate(SD = sqrt(VARIANCE)) %>%
  select(c(!VARIANCE)) %>%
  rename(IREC = ESTIMATE, SDIREC = SD)


irec_creel_merged_adipose_and_unchecked <- merge(creel_adipose_and_unchecked_for_irec, ireccc_adipose_and_unchecked, all=TRUE) %>% as_tibble() 
names(irec_creel_merged_adipose_and_unchecked) <- tolower(names(irec_creel_merged_adipose_and_unchecked))

irec_creel_merged_adipose_and_unchecked_1<-  irec_creel_merged_adipose_and_unchecked %>% mutate(licence.year= case_when(
  month > 3 ~ as.numeric(year),
  month < 4 ~ as.numeric(year - 1 )))
irec_creel_merged_adipose_and_unchecked_1<-merge( irec_creel_merged_adipose_and_unchecked_1, bcf_short, all=TRUE)%>% as_tibble()



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

### take out creel here and add it in from before 
irec_creel_merged_adipose_and_unchecked_pseudo<- irec_creel_merged_adipose_and_unchecked_pseudo %>% select(-creel)
irec_creel_merged_adipose_and_unchecked_pseudo<- merge(irec_creel_merged_adipose_and_unchecked_pseudo, creel_adipose_and_unchecked, all=TRUE) %>% as_tibble()


irec_creel_merged_adipose_and_unchecked_pseudo_region<- merge(irec_creel_merged_adipose_and_unchecked_pseudo, fishery_simple, all=TRUE) %>% as_tibble()
irec_creel_merged_adipose_and_unchecked_pseudo_region<- irec_creel_merged_adipose_and_unchecked_pseudo_region %>% group_by(year, month, region) %>% 
  summarise(sum_creel = ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
            sum_irec = ifelse(all(is.na(irec)), NA, sum(irec, na.rm=TRUE)), 
            sum_pseudocreel= ifelse(all(is.na(pseudocreel)), NA, sum(pseudocreel, na.rm=TRUE)))

mrp_irec_creel_unchecked<-merge(irec_creel_merged_adipose_and_unchecked_pseudo_region, mrp_rec_recoveries_heads, all=TRUE) %>% as_tibble()

mrp_irec_creel_tags_unchecked<-merge(mrp_irec_creel_unchecked, mrp_tag_recoveries_simple, all.y=TRUE) %>% as_tibble()
mrp_irec_creel_tags_unchecked


mrp_irec_creel_tags_unchecked<- mrp_irec_creel_tags_unchecked %>% mutate(submission_rate = 1/cwt_estimate, 
                                                                         accatch = heads/submission_rate,
                                                                         flag= case_when(submission_rate %in% c(0.5, 1, 0.25) ~ "artificial sub_rate", 
                                                                                         TRUE ~ "calculated sub_rate"))
#mrp_irec_creel_tags_unchecked$region[mrp_irec_creel_tags_unchecked$region=="22"]<-"62"

mrp_irec_creel_tags_unchecked_simple<- mrp_irec_creel_tags_unchecked %>% select(-recovery_id, -tag_code) %>% distinct()






#A few different ways to summarise the data:

irec_creel_merged_pseudo_sum1<- irec_creel_merged_pseudo %>% group_by(area, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                                             pseudocreel_sum1=sum(pseudocreel))
p <- ggplot(irec_creel_merged_pseudo_sum1 ,aes(x=creel_sum1, y=pseudocreel_sum1))
p <- p + geom_point(size=2, alpha=.5, aes(color=as.factor(year), shape=disposition)) +  facet_wrap(~disposition, scales="free")
p <- p + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p <- p + theme_bw(16)+labs( x="creel only", y="creel updated with irec - pseudocreel")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p

irec_creel_merged_pseudo_sum2<- irec_creel_merged_pseudo %>% group_by(erafishery, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                                                   pseudocreel_sum1=sum(pseudocreel))


p2 <- ggplot(irec_creel_merged_pseudo_sum2 ,aes(x=creel_sum1, y=pseudocreel_sum1, color=erafishery, fill=erafishery, shape=disposition))
p2 <- p2 + geom_point(size=2, alpha=.5) +  facet_wrap(~disposition, scales="free")
p2 <- p2 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5) # to add regression line
p2 <- p2 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p2 <- p2 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p2 <- p2 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p2

irec_creel_merged_pseudo_sum3<- irec_creel_merged_pseudo %>% filter(erafishery %in% c("WCVI AABM S", "WCVI ISBM S")) %>% 
  group_by(erafishery, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                        pseudocreel_sum1=sum(pseudocreel))

p3 <- ggplot(irec_creel_merged_pseudo_sum3 ,aes(x=creel_sum1, y=pseudocreel_sum1))
p3 <- p3 + geom_point(size=2, alpha=.5, aes(color=as.factor(year), shape=erafishery)) +  facet_wrap(~disposition, scales="free")
p3 <- p3 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p3 <- p3 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p3 <- p3 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p3 <- p3 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p3

p3 <- ggplot(irec_creel_merged_pseudo_sum3 ,aes(x=creel_sum1, y=pseudocreel_sum1, color=erafishery, fill=erafishery))
p3 <- p3 + geom_point(size=2, alpha=.5) +  facet_wrap(~disposition+erafishery, scales="free")
p3 <- p3 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5) # to add regression line
p3 <- p3 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p3 <- p3 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p3 <- p3 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p3



### chelsea
dat_chel<- datxy_pseudo %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S")) 


p <- ggplot(dat_chel,aes(x=year, y=irec))
p <- p + geom_point(size=2, alpha=.5, aes(color=erafishery, shape=disposition)) +  facet_wrap(~disposition, scales="free")
p <- p + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p <- p + theme_bw(16)+labs( x="year", y="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p


p <- ggplot(dat_chel,aes(x=creel, y=irec))
p <- p + geom_point(size=2, alpha=.5, aes(color=erafishery, shape=disposition)) +  facet_wrap(~disposition, scales="free")
p <- p + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p <- p + theme_bw(16)+labs( x="year", y="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p

#####








# source(here::here("R/format-data-NB.R"))

# # Issue 7 - Licences flagged
# irec_liscences_flag<- irec_raw_combined %>% mutate(kept_high= case_when(total_kept_pp>4 ~ 1, TRUE ~ 0), 
#                                                    released_high = case_when(total_released_pp>20 ~1, TRUE ~ 0), 
#                                                    total_high = case_when(total_caught_pp>20 ~ 1, TRUE ~0), 
#                                                    flag_count = kept_high + released_high + total_high, 
#                                                    flag_day = case_when(flag_count >0 ~ 1, TRUE ~0)) %>% 
#                                                    select (licence.id, kept_high, released_high, total_high, flag_count, flag_day) %>% 
#                                                    group_by(licence.id) %>% 
#                                                    summarise_if(is.numeric, sum) %>%       
#                                                    filter(flag_count > 0) %>%       
#                                                    arrange(desc(flag_count)) 


# irec to creel -----------------------------------------------------------


# are there PFMA levels in creel not in irec? 
creel_levels<-levels(as.factor(creel$PFMA))
irec_levels<-levels(as.factor(irec$AREA))

creel_levels  %in% irec_levels
setdiff(creel_levels, irec_levels)


irec_levels %in% creel_levels

set.diff<-setdiff(irec_levels,creel_levels)
length(setdiff(irec_levels,creel_levels))

arealu %>% filter(AREA %in% set.diff) %>% arrange(LU_GROUPING3)

# survey
creel_survey<- creel %>% mutate(SURVEY = case_when(
  Include..20. == "Y" ~ 2,
  Include..15. == "Y" ~ 3,
  TRUE ~ 4
)) %>% group_by(PFMA, YEAR, MONTH, TYPE, SURVEY) %>% 
  summarise(ESTIMATE = sum(ESTIMATE),SURVEY = mean(SURVEY)) 



# Irec estimates ----------------------------------------------------------

# plotting irec estimates data
irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))
irec<-irec %>% as_tibble()

p <- ggplot(irec %>% filter(AREA=="Area 2"),aes(x=ESTIMATE))
p <- p + stat_bin(colour = "gray", alpha = 0.5, position = "identity", aes(y = ..density..)) + geom_density(fill = NA, size=1)
p <- p + theme_bw(16)
p <- p + theme(legend.position="bottom")
p

p <- ggplot(irec,aes(y=ESTIMATE, x=MONTH, colour=as.factor(YEAR), shape=DISPOSITION))
p <- p + geom_point()+  facet_wrap(~AREA, scales="free")
p


p <- ggplot(irec ,aes(x=ESTIMATE))
p <- p + stat_bin(colour = "gray", alpha = 0.5, position = "identity", aes(y = ..density..)) + geom_density(fill = NA, size=1)
p <- p + theme_bw(16)+xlim(1000,60000)
p <- p + theme(legend.position="bottom")
p

p <- ggplot(irec,aes(y=ESTIMATE, x=AREA, colour=DISPOSITION))
p <- p + geom_point() + coord_flip()
p

ireccc
p <- ggplot(ireccc,aes(y=IREC, x=AREA, colour=DISPOSITION))
p <- p + geom_point() + coord_flip()
p


# Comparing irec raw to irec estimated:
irec_raw_summed<-irec_raw_combined %>% filter(method == "Angling from boat") %>% group_by(year, month, area) %>% 
  summarise(Kept = sum(total_kept), 
            Released = sum(total_released)) 


irec_raw_summed<-irec_raw_summed %>% pivot_longer(c(Kept, Released), names_to="disposition", values_to="response")


irec_raw_summed <- irec_raw_summed %>% mutate(area = case_when(
  area == "Area 19 Main Portion" ~ "Area 19 (JDF)", 
  area == "Area 19 Saanich Inlet only" ~ "Area 19 (GS)", 
  area == "Area 23 Alberni Inlet" ~ "Area 23 (Alberni Canal)", 
  area == "Area 23 Barkley Sound" ~ "Area 23 (Barkley)", 
  area == "Area 2 East" ~ "Area 2E", 
  area == "Area 2 West" ~ "Area 2W", 
  area == "Area 29 Georgia Strait" ~ "Area 29 (Marine)", 
  area == "Area 29 In River" ~ "Area 29 (In River)", 
  TRUE ~ as.character(area)))

###need to expand the irec_raw_summed to zeros
allobs_raw <- expand.grid(list(
  area = unique(irec_raw_summed$area),
  year = unique(irec_raw_summed$year),
  month = unique(irec_raw_summed$month),
  disposition = unique(irec_raw_summed$disposition)
))

#create zero observations, with 0 variance
irec_raw_summed_all <- left_join(allobs_raw, irec_raw_summed)
irec_raw_summed_all$response[is.na(irec_raw_summed_all$response)] <- 0
irec_raw_summed_all<- as_tibble(irec_raw_summed_all)
irec_raw_summed_all

#ireccc - this is the expanded one with all the zeros - make sure to include 2012
names(ireccc) <- tolower(names(ireccc))
irec_calculated_summed<-ireccc %>% select (- lu_grouping3) %>% group_by(year, month, area, disposition) %>% 
  summarise(estimate = sum(irec), estimate_var = sum(sdirec)) 
irec_calculated_summed
# Combine the two expanded ones
irec_compare<-merge(irec_calculated_summed, irec_raw_summed_all, all=TRUE) %>% as_tibble()

p <- ggplot(irec_compare) +
  geom_point(aes(x = estimate, y = total.chinook.caught, color=as.factor(year)), size = 2, alpha = .5) +
  theme_bw(16)+
  scale_color_viridis_d(end = 0.8, option = "C") +
  geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = "bottom")
p


p <- ggplot(irec_compare ,aes(y=response, x=estimate,fill=as.factor(year), color=as.factor(year), shape=disposition, linetype=disposition))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~x,  size = 2, alpha  = .2) # to add regression line
p <- p + theme_bw(16)+labs( y="irec response", x="irec estimate expanded")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p



# Plotting kept and releases ----------------------------------------------
library(ggh4x)
library(patchwork)


problem_areas<-irec_raw_combined %>% filter(total_kept_pp>4) %>% distinct(area)
problem_areas <-as.list(problem_areas$area)

problem_areas_rele<-irec_raw_combined %>% filter(total_released_pp>20) %>% distinct(area)
problem_areas_rele <-as.list(problem_areas_rele$area)
# ~region + area, strip=strip_nested(bleed=FALSE)

# facet_nested_wrap
kept_plot <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot <- kept_plot + geom_point()+ geom_hline(yintercept = 4)+ facet_nested_wrap(vars(region, area), scales="free", ncol = 15)
kept_plot
ggsave("Plots/kept_plot.png", kept_plot)

# facet_wrap2
kept_plot <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot <- kept_plot + geom_point()+ geom_hline(yintercept = 4)+ facet_wrap2(vars(region, area), scales="free", strip=strip_nested(bleed=FALSE), ncol = 15)
kept_plot

# facet_manual
kept_plot <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot <- kept_plot + geom_point()+ geom_hline(yintercept = 4)+ facet_manual(vars(region, area), scales="free", strip=strip_nested(bleed=FALSE), design=layout4)
kept_plot

# p1<- p1  + plot_annotation(tag_levels = list(c('West Coast Vancouver Island')))

# p2<-p2 + ggtitle( 'Johnstone Strait          Georgia Strait')
# p2.1<-wrap_plots(ap_kept_jst, ncol=3)+  ggtitle(title = 'Johnstone Strait')
# p2.2<-wrap_plots(ap_kept_gst, ncol=12)+  ggtitle(title = 'Georgia Strait')

# p3<- p3 +  ggtitle(title = 'Juan de Fuca               Central BC')
# p3.1<-wrap_plots(ap_kept_jdf, ncol=5)+ ggtitle(title = 'Juan de Fuca')
# p3.2<-wrap_plots(ap_kept_cbc, ncol=10) +  ggtitle(title = 'Central BC') 
# p5<- p5 +  ggtitle(title = 'Northern BC')
# p6<-p5 + guide_area()
#patchwork<- p1 / (p2.1 + p2.2) / (p3.1 + p3.2) / p6 
patchwork<-p1 / p2 / p3 / p6 

patchwork + plot_layout(guides = 'collect') 

# +  
theme(plot.title.position = c(0, 1),
      plot.title = element_text(size = 8, hjust = 0, vjust = 0))


#hjust
?plot.title
#+ plot_annotation(tag_levels = list(c('WCVI', 'JST, GST', 'JDF,CBC','NBC'), ''))

# wrap_plots(c(p1,p2,p3,p6), nrow=4, tag_level = 'new')+ plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'A')


# just zoom in on the area that contain a >4 value
kept_plot_filt <- ggplot(irec_raw_combined %>% filter(area %in% problem_areas),aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot_filt <- kept_plot_filt + geom_point()+ geom_hline(yintercept = 4)+ facet_wrap(~area, scales="free")
kept_plot_filt
ggsave("Plots/kept_plot_filt.png", kept_plot_filt)


released_plot <- ggplot(irec_raw_combined,aes(y=total_released_pp, x=as.factor(month), colour=as.factor(year)))
released_plot <- released_plot + geom_point()+ geom_hline(yintercept = 20)+ facet_wrap(~area, scales="free")
released_plot
ggsave("Plots/released_plot.png", released_plot)


released_plot_filt <- ggplot(irec_raw_combined %>% filter(area %in% problem_areas_rele),aes(y=total_released_pp, x=as.factor(month), colour=as.factor(year)))
released_plot_filt <- released_plot_filt + geom_point()+ geom_hline(yintercept = 20)+ facet_wrap(~area, scales="free")
released_plot_filt
ggsave("Plots/released_plot_filt.png", released_plot_filt)

sublegal_released_plot <- ggplot(irec_raw_combined,aes(y=sublegal_released_pp, x=as.factor(month), colour=as.factor(year)))
sublegal_released_plot <- sublegal_released_plot + geom_point()+ geom_hline(yintercept = 20)+ facet_wrap(~area, scales="free")
sublegal_released_plot

legal_released_plot <- ggplot(irec_raw_combined,aes(y=legal_released_pp, x=as.factor(month), colour=as.factor(year)))
legal_released_plot <- legal_released_plot + geom_point()+ geom_hline(yintercept = 20)+ facet_wrap(~area, scales="free")
legal_released_plot


p <- ggplot(irec_raw_combined,aes(y=total_released_pp, x=as.factor(month), colour=as.factor(year)))
p <- p + geom_point()+  facet_wrap(~area, scales="free")
p

p <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(year), colour=as.factor(year)))
p <- p + geom_boxplot()
p

p <- ggplot(irec_raw_combined,aes(y=total_released_pp, x=as.factor(year), colour=as.factor(year)))
p <- p + geom_boxplot()
p


p <- ggplot(irec_raw_summed_all %>% filter(disposition=="Released"),aes(y=response, x=month, colour=as.factor(year)))
p <- p + geom_boxplot()+  facet_wrap(~area, scales="free")
p


# Outlier analysis --------------------------------------------------------



# outlier stuff

# outlier by zscores
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

#outlier by median absolute deviation 
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

# tukey
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}

isnt_out_funs <- funs(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)



### find the outliers https://www.r-bloggers.com/2017/12/combined-outlier-detection-with-dplyr-and-ruler/
IsOutlier <- function(data) {
  lowerq = quantile(data, na.rm = TRUE)[2]
  upperq = quantile(data, na.rm = TRUE)[4]
  iqr = upperq - lowerq 
  threshold_upper = (iqr * 1.5) + upperq
  # threshold_lower = lowerq - (iqr * 1.5)
  data > threshold_upper 
}

# took out | data < threshold lower

# outlier by zscores
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

#outlier by median absolute deviation 
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

# tukey
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) 
  # & (x <= quar[2] + k * iqr)
}

isnt_out_funs <- funs(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)


# this gives you 5000 days
irec_outliers<-irec_raw_combined %>% select(licence.id, year, area, month, method, total_released_pp, total_kept_pp) %>% 
  group_by(year, area, month, method) %>% 
  mutate_if(is.numeric, isnt_out_funs)

View(irec_outliers)
#Explorations
irec_kept_outliers<-irec_raw_combined %>% filter(IsOutlier(total_kept_pp))
irec_released_outliers<-irec_raw_combined %>% group_by(month, area, method) %>% filter(IsOutlier(total_released_pp)== "TRUE") 

View(irec_released_outliers)

# Extra code for next steps -----------------------------------------------




#
# quantile(irec_raw_combined$total_kept_pp, na.rm=TRUE)

## problem is outlier analysis is saying everything is an outlier.... 
#might just be easier to say over this amount kept etc... 



# Is extreme?
IsExtreme <- function(data) {
  lowerq = quantile(data, na.rm = TRUE)[2]
  upperq = quantile(data, na.rm = TRUE)[4]
  iqr = upperq - lowerq 
  threshold_upper = (iqr * 1.5) + upperq
  # threshold_lower = lowerq - (iqr * 1.5)
  data > threshold_upper 
}


### find the outliers https://www.r-bloggers.com/2017/12/combined-outlier-detection-with-dplyr-and-ruler/
IsOutlier <- function(data) {
  lowerq = quantile(data, na.rm = TRUE)[2]
  upperq = quantile(data, na.rm = TRUE)[4]
  iqr = upperq - lowerq 
  threshold_upper = (iqr * 1.5) + upperq
  # threshold_lower = lowerq - (iqr * 1.5)
  data > threshold_upper 
}

# took out | data < threshold lower

# outlier by zscores
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

#outlier by median absolute deviation 
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

# tukey
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) 
  # & (x <= quar[2] + k * iqr)
}

isnt_out_funs <- funs(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)

# might need tot ake out zeros for this to work.... 
# this gives you 5000 days
irec_outliers<-irec_raw_combined %>% select(licence.id, year, area, month, method, total_released_pp, total_kept_pp) %>% 
  group_by(year, area, month, method) %>% 
  mutate_if(is.numeric, isnt_out_funs)

View(irec_outliers)
#Explorations
irec_kept_outliers<-irec_raw_combined %>% filter(identify_outliers(total_kept_pp))
irec_released_outliers<-irec_raw_combined  %>% filter(IsOutlier(total_released_pp)== "TRUE") 


irec_raw_combined  %>% anomalize(total_kept_pp) %>% filter(anomaly=="yes")

View(irec_released_outliers)

# identify_outliers() from the rstatix package is good, also have is.extreme
irec_kept_outliers<-irec_raw_combined %>%  filter(total_kept_pp>1) %>% group_by(year, month, area, method) %>%  filter(is_extreme(total_kept_pp))
irec_released_outliers<-irec_raw_combined %>%  filter(total_released_pp>1) %>% group_by(year, month, area, method) %>%  filter(is_extreme(total_released_pp))



View(irec_kept_outliers)


# Plotting layout ---------------------------------------------------------


layout <- '
ABCDEFGHIJKLMNO
ABCDEFGHIJKLMNO
ABCD#FGHIJKLMNO
ABCDEFGHIJKLMNO
'
layout2 <- '
AAAAAAAAAAAAAAA
BBBCCCCCCCCCCCC
DDDD#EEEEEEEEEE
FFFFFFFFFFFFFFF
'
layout3<- layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                          2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,
                          4,4,4,4,0,5,5,5,5,5,5,5,5,5,5,
                          6,6,6,6,6,6,6,6,6,6,6,6,6,6,0
), 4, 15, byrow = TRUE))

layout4<- layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                          16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
                          31,32,33,34,0,35,36,37,38,39,40,41,42,43,44,
                          45,46,47,48,49,50,51,52,53,54,55,56,57,58, 0), 4, 15, byrow = TRUE))




# Imputation --------------------------------------------------------------




library(smcfcs)

set.seed(1234)
n <- 1000
x <- rnorm(n)
w <- x+rnorm(n)
y <- x+rnorm(n)
x[(n*0.1):n] <- NA
simData <- data.frame(x,w,y)


imps <- smcfcs(simData, smtype="lm", smformula="y~x",
               method=c("norm", "", ""),m=5)

predMat <- array(0, dim=c(3,3))
predMat[1,2] <- 1


imps <- smcfcs(simData, smtype="lm", smformula="y~x",
               method=c("norm", "", ""),m=5,
               predictorMatrix=predMat)

library(mitools)
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y~x))
summary(MIcombine(models))


x <- rnorm(n)
w1 <- x+rnorm(n)
w2 <- x+rnorm(n)
w2[(n*0.1):n] <- NA
y <- x+rnorm(n)
x <- rep(NA,n)
simData <- data.frame(x,w1,w2,y)
