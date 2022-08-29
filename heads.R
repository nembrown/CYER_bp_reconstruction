
ggplot(mrp_irec_creel_tags_simple %>% filter( direct_heads=="not_direct", flag_summary == "calculated"), aes(x=month, y=sum_creel, fill=as.factor(region), col=as.factor(region)))+geom_point()+facet_wrap(~region, scales="free")

#heads plotting 
ggplot(mrp_rec_recoveries_heads, aes(x=month, y=heads, fill=as.factor(region), col=as.factor(region)))+geom_point()+facet_wrap(~region, scales="free")
mrp_tag_recoveries_heads<- mrp_rec_recoveries%>% filter( ! is.na(cwt_estimate))%>% group_by(recovery_year, region, rec_month) %>% summarise(tagged_heads=n() ) %>% rename(month=rec_month, year=recovery_year)
ggplot(mrp_tag_recoveries_heads, aes(x=month, y=tagged_heads, fill=as.factor(region), col=as.factor(region)))+geom_point()+facet_wrap(~region, scales="free")
heads_combined<- merge(mrp_tag_recoveries_heads, mrp_rec_recoveries_heads, all=TRUE) %>% as_tibble()
heads_combined

heads_combined$tagged_heads[is.na(heads_combined$tagged_heads)] <- 0
heads_combined <- heads_combined %>% mutate(prop_tagged = tagged_heads/heads)

ggplot(heads_combined, aes(x=month, y=prop_tagged, fill=as.factor(year), col=as.factor(region)))+geom_point()+facet_wrap(~region, scales="free")



heads_combined_means<- heads_combined %>% group_by(region, month) %>%  summarise(prop_tagged_mean = mean(prop_tagged), 
                                                                                 prop_tagged_mean_SD = sd(prop_tagged),
                                                                                 heads_mean = mean(heads), 
                                                                                 heads_mean_SD = sd(heads), 
                                                                                 tagged_heads_mean = mean(tagged_heads), 
                                                                                 tagged_heads_mean_SD = sd(tagged_heads))


ggplot(heads_combined_means, aes(x=as.factor(month), y=prop_tagged_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=prop_tagged_mean-prop_tagged_mean_SD, ymax=prop_tagged_mean+prop_tagged_mean_SD))+facet_wrap(~region, scales="free")
ggplot(heads_combined_means, aes(x=as.factor(month), y=heads_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=heads_mean-heads_mean_SD, ymax=heads_mean+heads_mean_SD))+facet_wrap(~region, scales="free")
ggplot(heads_combined_means, aes(x=as.factor(month), y=tagged_heads_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=tagged_heads_mean-tagged_heads_mean_SD, ymax=tagged_heads_mean+tagged_heads_mean_SD))+facet_wrap(~region, scales="free")



### now by PFMA: 

mrp_rec_recoveries_heads_area<- mrp_rec_recoveries%>% group_by(recovery_year, region, area, rec_month) %>% summarise(heads=n() ) %>% rename(month=rec_month, year=recovery_year)

mrp_tag_recoveries_heads_area<- mrp_rec_recoveries%>% filter( ! is.na(cwt_estimate))%>% group_by(recovery_year,region, area, rec_month) %>% summarise(tagged_heads=n() ) %>% rename(month=rec_month, year=recovery_year)
heads_combined_area<- merge(mrp_tag_recoveries_heads_area, mrp_rec_recoveries_heads_area, all=TRUE) %>% as_tibble()
heads_combined_area

heads_combined_area$tagged_heads[is.na(heads_combined_area$tagged_heads)] <- 0
heads_combined_area <- heads_combined_area %>% mutate(prop_tagged = tagged_heads/heads)

ggplot(heads_combined_area, aes(x=month, y=prop_tagged, fill=as.factor(year), col=as.factor(area)))+geom_point()+facet_wrap(~area, scales="free")



heads_combined_area_means<- heads_combined_area %>% group_by(region, area, month) %>%  summarise(prop_tagged_mean = mean(prop_tagged), 
                                                                                 prop_tagged_mean_SD = sd(prop_tagged),
                                                                                 heads_mean = mean(heads), 
                                                                                 heads_mean_SD = sd(heads), 
                                                                                 tagged_heads_mean = mean(tagged_heads), 
                                                                                 tagged_heads_mean_SD = sd(tagged_heads))

heads_combined_area_means<- heads_combined_area_means %>%  filter(str_detect(area, 'P') )


ggplot(heads_combined_area_means, aes(x=as.factor(month), y=prop_tagged_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=prop_tagged_mean-prop_tagged_mean_SD, ymax=prop_tagged_mean+prop_tagged_mean_SD))+facet_wrap(~area, scales="free")
ggplot(heads_combined_area_means, aes(x=as.factor(month), y=heads_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=heads_mean-heads_mean_SD, ymax=heads_mean+heads_mean_SD))+facet_wrap(~area, scales="free")
ggplot(heads_combined_area_means, aes(x=as.factor(month), y=tagged_heads_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=tagged_heads_mean-tagged_heads_mean_SD, ymax=tagged_heads_mean+tagged_heads_mean_SD))+facet_wrap(~area, scales="free")



##### adding in catch to get submission rates: 
heads_combined_area<- heads_combined_area%>% rename(CWT_area=area)
irec_creel_merged_adipose_PFMA<- merge(irec_creel_merged_adipose_pseudo, fishery_simple, all=TRUE) %>% as_tibble()
irec_creel_merged_adipose_PFMA_heads<- merge(irec_creel_merged_adipose_PFMA, heads_combined_area) %>% as_tibble()
irec_creel_merged_adipose_PFMA_heads<- irec_creel_merged_adipose_PFMA_heads %>% mutate(submission_rate = heads/pseudocreel)
irec_creel_merged_adipose_PFMA_heads_means<- irec_creel_merged_adipose_PFMA_heads%>% group_by(region, area, month) %>%  summarise(submission_rate_mean = mean(submission_rate, na.rm=TRUE), 
                                                                                                                                  submission_rate_mean_SD = sd(submission_rate, na.rm = TRUE))
ggplot(irec_creel_merged_adipose_PFMA_heads_means, aes(x=as.factor(month), y=submission_rate_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=submission_rate_mean-submission_rate_mean_SD, ymax=submission_rate_mean+submission_rate_mean_SD))+facet_wrap(~area, scales="free")


irec_creel_merged_adipose_PFMA_heads<- irec_creel_merged_adipose_PFMA_heads %>% mutate(submission_rate = heads/pseudocreel)
irec_creel_merged_adipose_PFMA_heads<- irec_creel_merged_adipose_PFMA_heads %>% filter(submission_rate <1)
irec_creel_merged_adipose_PFMA_heads_means<- irec_creel_merged_adipose_PFMA_heads%>% group_by(region, area, month) %>%  summarise(submission_rate_mean = mean(submission_rate, na.rm=TRUE), 
                                                                                                                                  submission_rate_mean_SD = sd(submission_rate, na.rm = TRUE))
ggplot(irec_creel_merged_adipose_PFMA_heads_means, aes(x=as.factor(month), y=submission_rate_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=submission_rate_mean-submission_rate_mean_SD, ymax=submission_rate_mean+submission_rate_mean_SD))+facet_wrap(~area, scales="free")


View(irec_creel_merged_adipose_PFMA_heads)


##### adding in catch to get submission rates by region:
mrp_irec_creel_tags_means<- mrp_irec_creel_tags %>%group_by(region, month) %>% summarise(creel_with_irec_sub_rate_mean = mean(creel_with_irec_sub_rate, na.rm=TRUE), 
                                                                                         creel_with_irec_sub_rate_sd = sd(creel_with_irec_sub_rate, na.rm=TRUE))                                                                                                                           
                                                                                                                                  
ggplot(mrp_irec_creel_tags_means, aes(x=as.factor(month), y=creel_with_irec_sub_rate_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=creel_with_irec_sub_rate_mean-creel_with_irec_sub_rate_sd, ymax=creel_with_irec_sub_rate_mean+creel_with_irec_sub_rate_sd))+facet_wrap(~region, scales="free")


ggplot(mrp_irec_creel_tags_means, aes(x=as.factor(month), y=creel_with_irec_sub_rate_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=creel_with_irec_sub_rate_mean-creel_with_irec_sub_rate_sd, ymax=creel_with_irec_sub_rate_mean+creel_with_irec_sub_rate_sd))+facet_wrap(~region, scales="free")




####### 
heads_combined
irec_creel_merged_adipose_pseudo_region

irec_creel_merged_adipose_region_heads<- merge(irec_creel_merged_adipose_pseudo_region, heads_combined, all=TRUE) %>% as_tibble()
irec_creel_merged_adipose_region_heads<- irec_creel_merged_adipose_region_heads %>% mutate(submission_rate = heads/sum_pseudocreel)
irec_creel_merged_adipose_region_heads<- irec_creel_merged_adipose_region_heads %>% filter(submission_rate <1)

irec_creel_merged_adipose_region_heads_means<- irec_creel_merged_adipose_region_heads%>% group_by(region, month) %>%  summarise(submission_rate_mean = mean(submission_rate, na.rm=TRUE), 
                                                                                                                                submission_rate_mean_SD = sd(submission_rate, na.rm = TRUE))
                                                                                                                                  
ggplot(irec_creel_merged_adipose_region_heads_means, aes(x=as.factor(month), y=submission_rate_mean,col=as.factor(region)))+geom_point()+geom_errorbar(aes(ymin=submission_rate_mean-submission_rate_mean_SD, ymax=submission_rate_mean+submission_rate_mean_SD))+facet_wrap(~region, scales="free")
