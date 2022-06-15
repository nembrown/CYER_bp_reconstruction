library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)

source(here::here("R/format-data-NB.R"))
dat <- format_data_NB()
dat<-dat %>% filter(disposition=="Kept")


p <- ggplot(dat) +
  geom_point(aes(x = creel, y = irec, color = disposition, shape = disposition), size = 2, alpha = .5) +
  facet_wrap(~region, scales = "free") +
  theme_bw(16) +
  labs(x = "CREEL", y = "iREC") +
  scale_color_viridis_d(end = 0.8, option = "C") +
  geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = "bottom")
p

dat$creel_orig <- dat$creel
dat$creel <- round(dat$creel)
dat$log_irec1 <- log(10 + dat$irec)
dat$fourth_irec <- dat$irec^.25

ggplot(dat, aes(x = fourth_irec, y = log(creel + 10), color = region)) +
  geom_point(alpha = .5) +
  theme_light() +
  labs(x = "CREEL", y = "iREC") +
  scale_color_viridis_d(end = 0.8, option = "C") +
  # geom_smooth(formula = y ~ poly(x, 2), method = "lm", se = FALSE)
  geom_smooth(formula = y ~ 1 + x, method = "lm", se = FALSE)
  # geom_smooth(formula = y ~ x, method = "lm", se = FALSE)

m <- glmmTMB(creel ~ 0 + log_irec1 + (0 + log_irec1 | region),
  data = dat, family = nbinom2())
summary(m)

m <- glmmTMB(creel ~ 1 + log_irec1 + (1 + log_irec1 | region),
  data = dat, family = nbinom2())
summary(m)

dat$positive_creel <- as.integer(dat$creel > 0)

m <- glmmTMB(positive_creel ~ 1 + log_irec1 + (1 + log_irec1 | region),
  data = dat, family = binomial())
summary(m)

nd <- expand.grid(
  log_irec1 = seq(min(dat$log_irec1), max(dat$log_irec1), 
    length.out = 200), 
  region = unique(dat$region))
nd$irec_pos <- 1
nd$irec_pos[nd$log_irec1 == min(nd$log_irec1)] <- 0

nd$pred <- predict(m, newdata = nd, re.form = NULL)

ggplot(nd, aes(log_irec1, plogis(pred), colour = region)) + 
  geom_line() +
  geom_jitter(data = dat, aes(y = positive_creel), width = 0.05, height = 0.1)

dat$irec_pos <- as.integer(dat$irec > 0)
dat_pos <- filter(dat, positive_creel == 1L)

m_pos <- glmmTMB(creel ~ 1 + irec_pos + log_irec1 + (1 + irec_pos + log_irec1 | region),
  data = dat_pos, family = Gamma(link = "log"))
summary(m_pos)
r_pos <- DHARMa::simulateResiduals(m_pos, n = 300)
plot(r_pos)

nd$pred_pos <- predict(m_pos, newdata = nd, re.form = NULL)
ggplot(nd, aes(log_irec1, sqrt(exp(pred_pos)), colour = region)) + 
  geom_line() +
  geom_jitter(data = dat_pos, aes(y = sqrt(creel)), width = 0.05, height = 0.1)

m_nb <- glmmTMB(creel ~ 1 + log_irec1 + (1 + log_irec1 | region),
  data = dat, family = nbinom2())
summary(m_nb)

nd$pred_nb <- predict(m_nb, newdata = nd, re.form = NULL)
ggplot(nd, aes(log_irec1, exp(pred_nb), colour = region)) + 
  geom_line() +
  geom_jitter(data = dat_pos, aes(y = creel), width = 0.05, height = 0.1)

s_nb <- simulate(m_nb, nsim = 100)
mean(s_nb == 0)
mean(dat$creel == 0)

m_nb1 <- glmmTMB(creel ~ 1 + log_irec1 + (1 + log_irec1 | region),
  data = dat, family = nbinom1())
summary(m_nb1)

nd$pred_nb1 <- predict(m_nb1, newdata = nd, re.form = NULL)
ggplot(nd, aes(log_irec1, exp(pred_nb1), colour = region)) + 
  geom_line() +
  geom_jitter(data = dat_pos, aes(y = creel), width = 0.05, height = 0.1)

s_nb1 <- simulate(m_nb1, nsim = 100)
mean(s_nb1 == 0)
mean(dat$creel == 0)

r_nb <- DHARMa::simulateResiduals(m_nb1, n = 300)
plot(r_nb)

m_nbz <- glmmTMB(creel ~ 1 + poly(log_irec1, 2) + poly(month, 3) + season + (1 + poly(log_irec1, 2) | region),
  data = dat, family = nbinom2(), ziformula = ~ log_irec1 + poly(month, 3) + season)
summary(m_nbz)
r_nb <- DHARMa::simulateResiduals(m_nbz, n = 300)
plot(r_nb)



m_nbz1 <- glmmTMB(creel ~ 1 + log_irec1 + as.factor(month) + (1 + log_irec1 | region),
  data = dat, family = nbinom1(), ziformula = ~ log_irec1, verbose = TRUE)


m_nbz2 <- glmmTMB(creel ~ 1 + log_irec1 + as.factor(month) + (1 + log_irec1 | region),
  data = dat, family = nbinom2(), ziformula = ~ log_irec1, verbose = TRUE)

AIC(m_nbz2, m_nbz1)

summary(m_nbz1)
r_nb <- DHARMa::simulateResiduals(m_nbz1, n = 300)
plot(r_nb)


m_pois <- glmmTMB(creel ~ 1 + log_irec1 + as.factor(month) + (1 + log_irec1 | region),
  data = dat, family = poisson(), ziformula = ~ log_irec1, verbose = TRUE)

AIC(m_nbz2, m_nbz1, m_pois)

summary(m_pois)
r_pois <- DHARMa::simulateResiduals(m_pois, n = 300)
plot(r_pois)

m_tw <- glmmTMB(creel ~ 1 + log_irec1 + as.factor(month) + (1 + log_irec1 | region),
  data = dat, family = tweedie(), ziformula = ~ log_irec1, verbose = TRUE)
AIC(m_nbz2, m_nbz1, m_tw)
r_tw <- DHARMa::simulateResiduals(m_tw, n = 300)
plot(r_tw)

simulationOutput <- simulateResiduals(fittedModel = m_nbz1)
plotResiduals(simulationOutput)
plotResiduals(simulationOutput, form = dat$log_irec1)

m_nbz1 <- glmmTMB(creel ~ 1 + fourth_irec + poly(month, 2) + (1 + fourth_irec | region),
  data = dat, family = nbinom1(), ziformula = ~ fourth_irec, verbose = F)

r_nb <- DHARMa::simulateResiduals(m_nbz1, n = 300)
plotResiduals(r_nb, form = dat$log_irec1)
plot(r_nb)


dat_pos <- filter(dat, creel > 0, irec > 0)


m_nb2 <- glmmTMB(creel ~ 1 + log(irec) + poly(month, 2) + (1 + log(irec) | region),
  data = dat_pos, family = truncated_nbinom1())

summary(m_nb2)

r_nb <- DHARMa::simulateResiduals(m_nb2, n = 100)
plot(r_nb)

plot(log(dat_pos$irec), log(dat_pos$creel))
plot((dat_pos$irec), log(dat_pos$creel))


m <- glmmTMB(log(creel_orig) ~ 1 + log(irec) + poly(month, 3) + (1 + log(irec) | region),
  data = dat_pos, dispformula = ~ log(irec) + region)

summary(m)

r <- DHARMa::simulateResiduals(m, n = 100)
plot(r)

plot(log(dat_pos$irec), log(dat_pos$creel))

na <- filter(dat_pos, region == "Barkley")
plot(sqrt(na$irec), sqrt(na$creel))
plot(na$irec, na$creel)

dat_pos$sqrt_irec <- sqrt(dat_pos$irec)


dat$sqrt_irec <- sqrt(dat$irec)

m <- glmmTMB(sqrt(creel_orig) ~ 1 + sqrt_irec * poly(month, 2) + (1 + sqrt_irec | region),
  data = dat_pos)

r <- DHARMa::simulateResiduals(m, n = 100)
plot(r)


dat_pos <- filter(dat, irec > 0)
plot(log(dat_pos$irec), log(dat_pos$creel+1))

ggplot(dat_pos, aes(x = irec, y = creel, color = region)) +
  geom_point(alpha = .5) +
  scale_x_log10() + scale_y_log10() +
  geom_smooth(formula = y ~ 1 + x, method = "lm", se = FALSE)

dat_pos$log_irec <- log(dat_pos$irec)
m1 <- glmmTMB(creel ~ 1 + log_irec * poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = nbinom1())

m2 <- glmmTMB(creel ~ 1 + log_irec * poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = nbinom2())

AIC(m1, m2)

m1.1 <- glmmTMB(creel ~ 1 + log_irec + poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = nbinom1(), dispformula = ~log_irec)
AIC(m1, m1.1)
summary(m1.1)

r <- DHARMa::simulateResiduals(m1.1, n = 100)
plot(r)

dat_zero <- filter(dat, irec == 0)
ggplot(dat_zero, aes(x = month, y = creel, color = region)) +
  geom_jitter(alpha = .5, height = 0)

m1.1a <- glmmTMB(creel ~ 1 + log_irec * poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = nbinom1(), dispformula = ~log_irec)

m1.1b <- glmmTMB(creel ~ 1 + log_irec + poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = nbinom1(), dispformula = ~log_irec)


AIC(m1.1a, m1.1b)
summary(m1.1)

m1.2 <- glmmTMB(creel ~ 1 + log_irec + poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = nbinom1(), dispformula = ~log_irec)

m1.3 <- glmmTMB(creel ~ 1 + log_irec + poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = nbinom2(), dispformula = ~log_irec)

m1.4 <- glmmTMB(creel ~ 1 + log_irec + poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = genpois(), dispformula = ~log_irec)
summary(m1.4)

AIC(m1.2, m1.3, m1.4)

r <- DHARMa::simulateResiduals(m1.4, n = 100)
plot(r)

library(brms)

dat_pos$log_irec_cent <- dat_pos$log_irec - mean(dat_pos$log_irec)
fit1 <- brm(
  bf(
    creel ~ log_irec_cent + s(month, k = 3) + 
      (log_irec_cent | region), 
    shape ~ log_irec_cent), 
  data = dat_pos, 
  family = negbinomial(),
  iter = 600, chains = 2, cores = 2
)

fit1

dat_zero <- filter(dat, irec == 0)
ggplot(dat_zero, aes(x = month, y = log(creel+1), color = region)) +
  geom_jitter(alpha = .5, height = 0, width = 0.1) +
  # facet_wrap(~region) +
  geom_smooth(se = FALSE,method = lm, formula = y ~ splines::bs(x, 3))
1

fit2.1 <- glmmTMB(
  creel ~ poly(month, 2) + (1 | region), 
  data = dat_zero,
  family = nbinom1()
)
summary(fit2.1)
r <- DHARMa::simulateResiduals(fit2.1, n = 300)
plot(r)

fit2.2 <- glmmTMB(
  creel ~ poly(month, 2) + (1 | region), 
  data = dat_zero,
  family = nbinom2()
)
summary(fit2.2)
AIC(fit2.1, fit2.2)
r <- DHARMa::simulateResiduals(fit2.1, n = 300)
plot(r)


s <- simulate(fit2.2, nsim = 1000)
mean(s == 0)
mean(dat_zero$creel == 0)

1

fit2 <- brm(
  bf(creel ~ s(month, k = 3) + (1 | region)), 
  data = dat_zero, 
  family = negbinomial(),
  iter = 600, chains = 2, cores = 2
)

fit2

plot(conditional_smooths(fit2), rug = TRUE, ask = FALSE)

nd <- expand.grid(month = seq(min(dat_zero$month), max(dat_zero$month), length.out = 100), region = unique(dat_zero$region))

pp2 <- posterior_linpred(fit2, newdata = nd, transform = TRUE, ndraws = 10)


lu <- data.frame(region = unique(dat_zero$region))


x <- tidybayes::add_linpred_draws(nd, fit2, ndraws = 100, transform = TRUE)
x %>% 
  ggplot(aes(month, .linpred, group = paste(region, .draw), colour = region)) +
  geom_line(alpha = 0.2) +
  geom_point(data = dat_zero, mapping = aes(x = month, y = creel, colour = region), inherit.aes = FALSE) +
  facet_wrap(~region, scales = "free_y")


nd1 <- expand.grid(
  month = unique(dat_pos$month), 
  region = unique(dat_pos$region),
  log_irec_cent = seq(min(dat_pos$log_irec_cent), max(dat_pos$log_irec_cent), length.out = 100)
)
nd1$log_irec <- nd1$log_irec_cent + mean(dat_pos$log_irec)
nd1$irec <- exp(nd1$log_irec)

x1 <- tidybayes::add_linpred_draws(nd1, fit1, ndraws = 100, transform = TRUE)
x1 %>% 
  filter(month == unique(dat_pos$month)[1]) %>% 
  ggplot(aes(irec, .linpred, group = paste(region, .draw), colour = region)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~region, scales = "free_y") +
  scale_x_log10() +
  scale_y_log10()

x1 <- tidybayes::add_predicted_draws(nd1, fit1, ndraws = 100)
x1 %>% 
  filter(month == unique(dat_pos$month)[1]) %>% 
  ggplot(aes(irec, .prediction, group = paste(region, .draw), colour = region)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~region, scales = "free_y")
