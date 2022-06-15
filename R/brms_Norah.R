#=======================================================================
# irec calibration model fit experimentations
# Catarina Wor #Norah Brown
# January 2022
#=======================================================================


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


datxy$diff <- datxy$creel - datxy$irec
p <- ggplot(datxy,aes(y=diff, x=as.factor(region)))
p <- p + geom_boxplot(width=1, position = position_dodge(width = 1))
p <- p +geom_hline(aes(yintercept=0),size=1.1, alpha=.5)
p <- p + scale_color_viridis_d(end = 0.8,option = "A")
p <- p + theme_bw(16)+labs( x="month", y="creel-irec")

p

#===========================
#histograms by groupings - of the data that is complete
datnna<-datxy[!is.na(datxy$creel),]


#proportion of 0's in creel 

prop0<-function(x){sum(x==0)/length(x)}

# some zeros in the creel data
aggregate(datnna$creel,by=list(datnna$MONTH),prop0)
aggregate(datnna$creel,by=list(datnna$region),prop0)
aggregate(datnna$creel,by=list(datnna$year),prop0)

aggregate(datxy$irec,by=list(datxy$MONTH),prop0)
aggregate(datxy$irec,by=list(datxy$region),prop0)
aggregate(datxy$irec,by=list(datxy$year),prop0)

#no 0s in rec? 
propna<-function(x){sum(is.na(x))/length(x)}
aggregate(datxy$creel,by=list(datxy$MONTH),propna)
#No creel data for Nov, Dec, January, very limited Feb thru May

datxypeak<-datxy  %>% filter(MONTH %in% c(6,7,8,9))
aggregate(datxypeak$creel,by=list(datxypeak$region),propna)

aggregate(datxy$creel,by=list(datxy$year),propna)

View(datxy)
#===========================================
#plot data by area
p <- ggplot(datnna,aes(y=creel, x=irec,color=region))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( y="creel", x="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "C")
p <- p + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=2)
p

#plot data by month
p <- ggplot(datnna %>% filter(season=="offseason"),aes(y=creel, x=irec,color=as.factor(MONTH)))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( y="creel", x="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=2)
p

#plot data by year
p <- ggplot(datnna,aes(y=creel, x=irec,color=as.factor(year)))
p <- p + geom_point(size=2, alpha=.5, aes(shape=disposition))
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( y="creel", x="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p <- p + geom_abline(intercept = 0, slope = 1, linetype="dashed", size=2)
p



#Just plot 2020
p <- ggplot(datnna %>% filter(year==2020),aes(y=creel, x=irec,color=as.factor(region)))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( y="creel", x="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p <- p + geom_abline(intercept = 0, slope = 1, linetype="dashed", size=2)
p


#plot data by kept v release
p <- ggplot(datnna,aes(y=creel, x=irec,color=disposition))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( y="creel", x="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "C")
p <- p + theme(legend.position="bottom")
p

#============================= Histograms
datgath<- datxy %>% pivot_longer(names_to ="grp", values_to ="estimate", c(irec, creel))
datgath.peak<-datgath %>% filter(season=="peakseason")

p <- ggplot(datgath.peak,aes(x=estimate, group = grp, fill=grp, colour=grp))
p <- p + stat_bin(colour = "gray", alpha = 0.5, position = "identity", aes(y = ..density..)) + geom_density(fill = NA, size=1)
p <- p + facet_wrap(~region, scales="free")
p <- p + theme_bw(16) + xlim(0, max(datgath$estimate))
p <- p + theme(legend.position="bottom")
p

p <- ggplot(datgath,aes(x=estimate, group = grp, fill=grp, colour=grp))
p <- p + stat_bin(colour = "gray", alpha = 0.5, position = "identity", aes(y = ..density..)) + geom_density(fill = NA, size=1)
p <- p + facet_wrap(~region, scales="free")
p <- p + theme_bw(16) + xlim(0, 10000)
p <- p + theme(legend.position="bottom")
p



p <- ggplot(datnna,aes(irec))
p <- p + geom_histogram()
p <- p + facet_wrap(~region, scales="free")
p <- p + theme_bw(16)
p <- p + theme(legend.position="bottom")
p



#=============================
#example model fit - learning brms 
#code stolen from the brms vignette

#resample the data
#Does sampling needs to be startified?
#Sampling should occur on or after the exclusion of the NAs?
dats <- datnna#[sample(seq_len(nrow(datnna)), nrow(datnna), replace = TRUE),]

summary(dats)
# This model is just a first attempt to get brm working
# Do not run when rendering as the results are not meaningful and it takes a loooong time to run
##QUestion: I am not sure baout the model formulation: 
# Should it be: irec  ~ -1 + creel+ (-1 + creel  |region)
#Also: distribution? What should I consider? Lognormal (without 0s) an dhurdle lognormal are the 
#only ones that seem to converge, but diagnostics do not look good, data seems to be overdispersed (hurdle).
# and not fitting higher values with the positive only model.
#fit2 <- brm(formula= irec  ~ -1 + s(MONTH, k=3) + (-1 + creel  |region) , 
#  data=dats, family=hurdle_lognormal, iter = 800,chains=2 )


#dats$creelint<- round(dats$creel)

#Try the gaussian model with the area hierarchical effect
#same as current model but just 
fit0 <- brm(formula =  creel ~ -1 +  irec + (-1 +  irec |region ) , 
  data=dats, family="gaussian", iter = 1000,chains=3, prior = c(set_prior("normal(5000, 2000)", class = "b") ))

??brm
summary(fit0)
#these are not possible because there is only one parameter.
#pairs(fit1)
plot(fit0, ask = FALSE)

#Something is wrong here. I think it has something to do with the log link
#as I am getting similar patterns for all distributions with log link.
# The one prediction for Berkely in July gets a craxy high number, lixe 2x the highest observation. 

fitted_values0 <- fitted(fit0)
pred0<-predict(fit0)


plot(standata(fit0)$Y,pred0[,1])
#Seems like a bunch of 0's are not 
dat <- as.data.frame(cbind(Y = standata(fit0)$Y, fitted_values0))
ggplot(dat) + geom_point(aes(x = Y, y = Estimate))

conditional_effects(fit0, method="posterior_predict")
loofit0<-loo(fit0, save_psis = TRUE)

plot(loofit0)


#=============
#Try the poisson model
#round response to nearest integer
dats$creelint<- round(dats$creel)

#Try the poisson model without the area hierarchical effect
fit1 <- brm(formula =  creelint ~ -1 +  irec  , 
  data=dats, family="poisson", iter = 3000,chains=3 )


summary(fit1)
#these are not possible because there is only one parameter.
#pairs(fit1)
plot(fit1, ask = FALSE)

#Something is wrong here. I think it has something to do with the log link
#as I am getting simmilar patterns for all distributions with log link.
# The one prediction for Berkely in July gets a craxy high number, lixe 2x the highest observation. 

fitted_values <- fitted(fit1)
pred1<-predict(fit1)


plot(standata(fit1)$Y,pred1[,1])
dat <- as.data.frame(cbind(Y = standata(fit1)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Y, y = Estimate))


dat[which.max(dat$Estimate),]
dats[dats$creelint==dat$Y[which.max(dat$Estimate)],]
aggregate(datnna$creel,by=list(datnna$region),length)


conditional_effects(fit1, method="posterior_predict")
loofit1<-loo(fit1, save_psis = TRUE)
plot(loofit1)

yrep <- posterior_predict(fit1)

#model is overdispersed - the thick line should e uniform
#not optimal for discrete observations though
ppc_loo_pit_overlay(
  y = dats$irec[!is.na(dats$creel)],
  yrep = yrep,
  lw = weights(loofit1$"psis_object")
)
 #===================================


#now with the area effect
fit2 <- brm(formula =  creelint ~ -1 +  irec +(-1 +  irec |region) , 
  data=dats, family="poisson", iter = 3000,chains=3 )


summary(fit2)
#these look pretty good.
pairs(fit2)
plot(fit2, ask = FALSE)

fitted_values2 <- fitted(fit2)
pred2<-predict(fit2)

plot(standata(fit2)$Y,pred2[,1])
dat2 <- as.data.frame(cbind(Y = standata(fit2)$Y, fitted_values2))
ggplot(dat2) + geom_point(aes(x = Y, y = Estimate))

conditional_effects(fit2, method="posterior_predict")
loofit1<-loo(fit2, save_psis = TRUE)
plot(loofit1)

yrep <- posterior_predict(fit2)

#model is overdispersed - the thick line should be uniform
#I need to get a bit more guidance on how to interpret it
#not optimal for discrete observations though
ppc_loo_pit_overlay(
  y = dats$irec[!is.na(dats$creel)],
  yrep = yrep,
  lw = weights(loofit1$"psis_object")
)


#====================================================
#model with prediction 




#things to look at:
#what is the observation w=resulting in the wonky predicted value. which area is it from, 
#if from area of few obs,  exclude the area and refit the model





#hurdle lognormal model
#flip the model 
#if the s(MONTH, k=3) term is included then loo() crashes R
fit2 <- brm(formula = bf( creel ~ -1 +  irec + (-1 +  irec |region), 
  hu ~ -1 +  irec + (-1 +  irec |region)),
  data=dats, family=hurdle_lognormal, iter = 1000,chains=2 )



?brm
dim(dats)

summary(fit2)
pairs(fit2)
plot(fit2, ask = FALSE)

#I do not undertand what is causing the extreme outliers in the predicted values

fitted_values <- fitted(fit2)
head(fitted_values)
dat <- as.data.frame(cbind(Y = standata(fit2)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))



#the problematic data 
dats[which.max(fitted_values[,1]),]



#this plot makes no sense to me:
conditional_effects(fit2, method="posterior_predict")


#fit2posterior<-posterior_samples(fit2)

#fit2posteriorp<-reshape::melt(fit2posterior)




stancode(fit2)
sdata <- standata(fit2)

#how do interpret loo? 
#look at loo vignette: https://cran.r-project.org/web/packages/loo/vignettes/loo2-example.html
LOO(fit2)
loofit2<-loo(fit2, save_psis = TRUE)
plot(loofit2)

#Marginal posterior predictive checks
yrep <- posterior_predict(fit2)

#model is overdispersed - the thick line should e uniform
ppc_loo_pit_overlay(
  y = dats$irec[!is.na(dats$creel)],
  yrep = yrep,
  lw = weights(loofit2$"psis_object")
)


bayes_R2(fit2)


#plot fit2
# following the guidance on https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html
get_variables(fit2)


fit2 %>%
  spread_draws(r_region[area,term]) 

fit2 %>%
  spread_draws(r_region[area,]) %>%
  median_qi()


fit2 %>%
  spread_draws(r_region[area,]) %>%
  summarise_draws()


fit2 %>%
  spread_draws(r_region[area,]) %>%
  mutate(area_mean = exp(r_region)) %>%
  ggplot(aes(y = area, x = area_mean) )+
  stat_halfeye()

# posterior predictions

dats %>%
  data_grid(region,creel) %>%
  add_epred_draws(fit2) %>%
  ggplot(aes(x = .epred, y = region)) +
  stat_pointinterval(.width = c(.66, .95))
