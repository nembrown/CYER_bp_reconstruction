#=======================================================================
# irec calibration model fit experimentations
# Catarina Wor #Norah Brown
# January 2022
#=======================================================================
# install.packages("devtools")
# 
# devtools::install_github("jwb133/smcfcs")
source(here::here("R/format-data-NB.R"))
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


source(here::here("R/format-data-NB.R"))
dat <- format_data_NB()
### datxy is the filled data with NAs filled in for creel 
### datnna is the data without NAs for creel

View(datxy)

datxy$irec<-as.integer(datxy$irec)
datxy$sdirec<-as.integer(datxy$sdirec)
datxy$creel<-as.integer(datxy$creel)
datxy$sdcreel<-as.integer(datxy$sdcreel)
datxy$year<-as.factor(datxy$year)
datxy$region<-as.factor(datxy$region)

datxy<-as.data.frame(datxy)
datxy<-as_tibble(datxy)
datxy_no2021<-as.data.frame(datxy_no2021)

#creel 2021 data hasn't been added yet
datxy <- datxy %>% filter(year != "2021")

da5 <- datxy %>% filter(disposition=="Kept") %>% impute_rlm(creel + sdcreel ~ irec + sdirec) %>% as_tibble()

da6 <- datxy %>% filter(disposition=="Kept") %>% impute_lm(creel ~ irec | year)

da6 <- datxy %>% group_by(year) %>%  impute_lm(creel ~ irec)

View(da6)

?impute_lm

p <- ggplot(datnna %>% filter(disposition=="Kept"),aes(y=creel, x=irec,fill=as.factor(year), color=as.factor(year)))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .3) # to add regression line
p <- p + theme_bw(16)+labs( y="creel", x="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p <- p + geom_abline(intercept = 0, slope = 1, linetype="dashed", size=2)
p <- p +  facet_wrap(~survey)
p


p <- ggplot(datnna %>% filter(disposition=="Kept", survey=="creel20"),aes(y=creel, x=irec,fill=as.factor(year), color=as.factor(year)))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~x,  size = 2, alpha  = .2) # to add regression line
p <- p + theme_bw(16)+labs( y="creel", x="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")+xlim(0,20000)+ylim(0,20000)
p <- p + geom_abline(intercept = 0, slope = 1, linetype="dashed", size=2)
p

p2 <- ggplot(da5 %>% filter(disposition=="Kept"),aes(y=creel, x=irec,fill=as.factor(year), color=as.factor(year)))
p2 <- p2 + geom_point(size=2, alpha=.5)
p2 <- p2 + geom_smooth(method = lm, formula= y~x,  size = 2, alpha  = .2) # to add regression line
p2 <- p2 + theme_bw(16)+labs( y="creel", x="irec")
p2 <- p2 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p2 <- p2 + theme(legend.position="bottom") +xlim(0,20000)+ylim(0,20000)
p2 <- p2 + geom_abline(intercept = 0, slope = 1, linetype="dashed", size=2)
p2

p3 <- ggplot(da6 %>% filter(disposition=="Kept"),aes(y=creel, x=irec,fill=as.factor(year), color=as.factor(year)))
p3 <- p3 + geom_point(size=2, alpha=.5)
p3 <- p3 + geom_smooth(method = lm, formula= y~x,  size = 2, alpha  = .2) # to add regression line
p3 <- p3 + theme_bw(16)+labs( y="creel", x="irec")
p3 <- p3 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p3 <- p3 + theme(legend.position="bottom")+xlim(0,20000)+ylim(0,20000)
p3 <- p3 + geom_abline(intercept = 0, slope = 1, linetype="dashed", size=2)
p3


hist(datxy$irec)
View(datxy)
