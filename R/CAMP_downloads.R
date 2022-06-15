# load libraries ----------------------------------------------------------

options("install.lock"=FALSE)

library(ggplot2)
library(odbc)
 #remotes::install_git("https://github.com/Pacific-salmon-assess/tagFisheryMapping")
library(tagFisheryMapping)
library(dbplyr)
library(janitor)
library(xlsx)
library(purrr)
library(writexl)
library(stringr)

# Option 1: Open local connection to CAMP -------------------------------

openCasConnection <- function(db_filename) {
  if (!requireNamespace("odbc", quietly = TRUE)) {
    stop("The package 'odbc' is needed for this function to work -
         and may only work in MS Windows. Please install it.",
         call. = FALSE)
  }

  driver_name <-
    paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
           "DBQ=",
           db_filename)

  db_conn <- dbConnect(odbc(), .connection_string = driver_name)
  return(db_conn)
}

selectAccessDb <- function(display_caption = "Select MS Access Database") {
  msaccess_filter <- rbind(cbind("All files (*.*)", "*.*"),
                           cbind("MS Access database (*.mdb, *.accdb)", "*.mdb;*.accdb"))

  if (exists('choose.files', where = asNamespace("utils"), mode = 'function')) {
    db_filename <- utils::choose.files(default = "",
                                       caption = display_caption,
                                       multi = FALSE,
                                       filters = msaccess_filter,
                                       index = nrow(msaccess_filter))
  } else {
    stop("Can not use the selectAccessDb because the choose.files function is not available in utils package")
  }

  return(db_filename)
}

#Choose the .mdb file or the .accdb file (change default), should be in your working directory
#campdb<-openCasConnection(selectAccessDb())

# You can also specify the exact file you want like this:
# casdb<-openCasConnection(file.path(getwd(), "CAMP2022BE.accdb"))


# Option 2: Open cloud connection to CAMP database ------------------------------------------------
#need to have text file camp.config in your working (project) directory
fileName <- 'camp.config'
conn_string<-readChar(fileName, file.info(fileName)$size)

campdb <- dbConnect(odbc::odbc(),
                    .connection_string = conn_string)

str(campdb)
# Pull tables from CAS

cnr<-tbl(campdb, "REAMCNRData")

View(cnr)

fishery.era<-tbl(campdb, "CAMPFisheryERA")
fishery.era<- fishery.era %>% rename(ERAFishery = FisheryERAID)
fishery.era

cnr<- merge(cnr, fishery.era) %>% as_tibble() %>% rename(erafishery=Name, year = ERAYear)
cnr_canada_sport<- cnr %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S", "WCVI AABM S", "WCVI ISBM S"))   
cnr_canada_sport<- cnr_canada_sport %>% rename(Kept = CNRValue1, Released= CNRValue2) %>% select(erafishery, year, Kept, Released) 

cnr_canada_sport<- cnr_canada_sport %>% pivot_longer(cols=c("Kept", "Released"), names_to = "disposition", values_to = "cnr")
cnr_canada_sport





cwdbrecovery<-tbl(campdb, "CWDBRecovery")


#some tidying functions to make the explorations work
"%notin%" <- Negate("%in%")
cwdbrecovery<- cwdbrecovery %>% as_tibble()

wiretagcode<-tbl(campdb, "WireTagCode") %>% as_tibble()
wiretagcode

cwdb_wiretag<-merge(cwdbrecovery, wiretagcode)%>% as_tibble()
cwdb_wiretag<- cwdb_wiretag %>% relocate(Stock, RunYear, BroodYear, Age, Fishery, TagCode, AdjustedEstimatedNumber)
cwdb_wiretag<- cwdb_wiretag %>% rename(FineFishery_ID=Fishery)


fishmap<-read.csv("fishery_mapping_main.csv")

cwdb_wiretag<-merge(cwdb_wiretag, fishmap) %>% as_tibble() %>% relocate(ERAFishery, Stock, RunYear, BroodYear, Age,  TagCode, AdjustedEstimatedNumber)
cwdb_wiretag

cwdb_wiretag_sum<-cwdb_wiretag %>% group_by(ERAFishery, RunYear, Age) %>% summarise(CWT = sum(AdjustedEstimatedNumber))
cwdb_wiretag_sum_can<- cwdb_wiretag_sum %>% filter(ERAFishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S", "WCVI AABM S", "WCVI ISBM S")) 
ggplot(cwdb_wiretag_sum_can,aes(x=RunYear, y=CWT, color=as.factor(Age), group=as.factor(Age))) + geom_point(size=3, alpha=.5) + facet_wrap(~ERAFishery, scales="free") + geom_line()+theme(legend.position = "bottom")

cwdb_wiretag_stock_sum<-cwdb_wiretag %>% group_by(Stock, RunYear, Age) %>% summarise(CWT = sum(AdjustedEstimatedNumber))
cwdb_wiretag_stock_sum_can<- cwdb_wiretag_stock_sum %>% filter(Stock %in% c("ATN", "KLM", "RBT", "QUI", "PHI", "COW", "NIC", "SHU", "HAR")) 


ggplot2(cwdb_wiretag_stock_sum_can,aes(x=RunYear, y=CWT, color=as.factor(Age), group=as.factor(Age))) + geom_point(size=3, alpha=.5) + facet_wrap(~Stock, scales="free") + geom_line()+theme(legend.position = "bottom")

# Open connection to MRP  -------------------------------------------------
#this takes a while if you load everything, you can select only current year
mrp_recoveries<-getDfoTagRecoveries(1950:2022)

#query to be implemented: 
mrp_rec_recoveries<- getDfoRecRecoveries(2009:2022)


