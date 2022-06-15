library(tidyverse)
library(readxl)
library(openxlsx)

#Fine (241), coarse (195) and PSC (80) are the three levels. 
CAMPFisheryFine <- read_excel("data/CAMPFisheryFine.xlsx")
CAMPFisheryFine <- CAMPFisheryFine %>%  rename(FineFishery_ID= FisheryFineID, 
                                               CoarseFishery_ID =FisheryCoarseID, 
                                               FineFishery = Name,
                                               FineFishery_Description = Description) %>% select(-c("AgeIncrementStartYear", "AgeIncrementMonthDay"))

length(unique(CAMPFisheryFine$FineFishery))#Fine 241 (=="fishery")
length(unique(CAMPFisheryFine$CoarseFishery_ID))#Coarse 192 (== "era fishery)

CAMPFisheryCoarse <- read_excel("data/Norah/CAMPFisheryCoarse.xlsx")
CAMPFisheryCoarse <- CAMPFisheryCoarse %>%  rename(CoarseFishery_ID= FisheryCoarseID, 
                                                   CoarseFishery =Name, 
                                                   CoarseFishery_Description = Description,
                                                   PSCFishery_ID = FisheryERAID) #renamed this PSC fishery
length(unique(CAMPFisheryCoarse$CoarseFishery))#Coarse 195 (== ERA Fishery)
length(unique(CAMPFisheryCoarse$PSCFishery_ID))


fishery_mapping_main <-merge(CAMPFisheryFine, CAMPFisheryCoarse, all=TRUE) # 245 rows

ERA_ERAFisheryToPSCFisheryMapping <- read_excel("data/Norah/ERA_ERAFisheryToPSCFisheryMapping.xlsx")
ERA_ERAFisheryToPSCFisheryMapping<- ERA_ERAFisheryToPSCFisheryMapping %>% rename(CoarseFishery=ERAFishery) %>% select(CoarseFishery, PSCFishery)

fishery_mapping_main<- merge(fishery_mapping_main, ERA_ERAFisheryToPSCFisheryMapping, all=TRUE) #246 rows
fishery_mapping_main <- fishery_mapping_main %>% as_tibble() %>% 
                  select(PSCFishery_ID, PSCFishery, CoarseFishery_ID, CoarseFishery, CoarseFishery_Description, FineFishery_ID, FineFishery, FineFishery_Description, Terminal) %>% 
                  arrange(PSCFishery_ID, CoarseFishery_ID, FineFishery_ID)

fishery_mapping_main<-fishery_mapping_main %>% rename(ERAFishery= PSCFishery, ERAFishery_ID = PSCFishery_ID)

### Up to here has the 3 main fisheries 
writexl::write_xlsx(fishery_mapping_main, path="fishery_mapping_main.xlsx")

#After here has some added bonus fisheries information

ERA_DistributionFisheryMapping <- read_excel("data/Norah/ERA_DistributionFisheryMapping.xlsx")
ERA_DistributionFisheryMapping <- ERA_DistributionFisheryMapping %>%  rename(PSCFishery= PSCFisheryName, 
                                                                             DistributionFishery_ID = DistributionFishery,
                                                                             DistributionFishery = DistributionFisheryName)
length(unique(ERA_DistributionFisheryMapping$PSCFishery)) #PSC 81
length(unique(ERA_DistributionFisheryMapping$DistributionFishery))#Distribution 14 

fishery_mapping<- merge(fishery_mapping_main, ERA_DistributionFisheryMapping, all=TRUE) #247
fishery_mapping %>% as_tibble()

PSCtoModelFisheryMapping <- read_excel("data/Norah/PSCtoModelFisheryMapping.xlsx")
PSCtoModelFisheryMapping <- PSCtoModelFisheryMapping %>%  rename(PSCFishery_ID = PSCFishery, PSCFishery= PSCFisheryName,
                                                                 ModelFishery_ID=ModelFishery, ModelFishery= ModelFisheryName)

length(unique(PSCtoModelFisheryMapping$PSCFishery))#PSC 79
length(unique(PSCtoModelFisheryMapping$ModelFishery))#Model 27

fishery_mapping<- merge(fishery_mapping, PSCtoModelFisheryMapping, all=TRUE) #255
fishery_mapping<- fishery_mapping %>% as_tibble() %>% 
                                     select(DistributionFishery_ID, DistributionFishery, ModelFishery_ID, ModelFishery,
                                            PSCFishery_ID, PSCFishery, CoarseFishery_ID, CoarseFishery, FineFishery_ID, FineFishery, 
                                            Terminal, AABMorISBM, GearName,
                                            CoarseFishery_Description,FineFishery_Description) %>% 
                                     arrange(DistributionFishery_ID, ModelFishery_ID, PSCFishery_ID, CoarseFishery_ID, FineFishery_ID)

#PSC is the legacy name for ERAFishery
#Cfilefishery is the legacy name for CoarseFishery

fishery_mapping<-fishery_mapping %>% rename(ERAFishery= PSCFishery, ERAFishery_ID = PSCFishery_ID)

writexl::write_xlsx(fishery_mapping, path="fishery_mapping.xlsx")


#to do next: add PFMA to this.... it should map to something? 


##### old 

ERA_FisheryERAFishery <- read_excel("data/Norah/ERA_FisheryERAFishery.xlsx")
length(unique(ERA_FisheryERAFishery$Fishery)) #Fishery 231 (==Fine)
length(unique(ERA_FisheryERAFishery$ERAFishery)) #ERA 192


ERA_ERAFishery <- read_excel("data/Norah/ERA_ERAFishery.xlsx")
ERA_ERAFishery<- ERA_ERAFishery %>%  rename(ERA_ID = Id, ERAFishery = Name, ERA_Description = Description)
length(unique(ERA_ERAFishery$ERAFishery)) #ERA 193

FisheryCFileFishery <- read_excel("data/Norah/FisheryCFileFishery.xlsx")
FisheryCFileFishery <- FisheryCFileFishery %>%  rename(FineFishery_ID= Fishery, 
                                                       CFileFishery_ID = CFileFishery)

CFileFishery <- read_excel("data/CFileFishery.xlsx")
CFileFishery<-CFileFishery %>% rename(CFileFishery_ID =Id,
                                      CFileFishery = Name, 
                                      CFileFishery_Description = Description)
CFileFishery<-merge(CFileFishery, FisheryCFileFishery[,1:3])
length(unique(CFileFishery$CFileFishery))#Cfile 191 (=="era fishery")

