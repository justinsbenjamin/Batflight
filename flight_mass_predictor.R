library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)
library(ggplot2); theme_set(theme_bw())

# if (packageVersion("broom.mixed") < "0.2.9.5") {
#     warning("please install the latest version of broom.mixed via remotes::install_github('bbolker/broom.mixed')")
# }


mass_sims <- readRDS("mass_dataset.RDS")

set.seed(404) 


n_per_group=15; days=seq(0, 60, by=3) 
n_groups <- 2

#flight time parameters
beta0_f=2; beta_day_f=(0.5/60); beta_daytreat_f=(2/60)
corr_trans <- function(rho) rho/(sqrt(1+rho^2))
sdint_f=0.5; sdslope_f=(0.5/60); corr=corr_trans(0.75); sdres_f=0.1


#mass parameters
beta0_m=30; beta_day_m=(-1.5/60); beta_daytreat_m=(-3/60)
sdint_m=5; sdslope_m=(1/60); sdres_m=0.5

form0 <- ~ 1 + day + treatment:day + (day | batID)
form1 <- flightTime ~ 1 + day + mass + day:treatment + mass:treatment + mass:day + mass:day:treatment + (day | batID) #this looks v complicated lol
form2 <- flightTime ~ 1 + day + mass + day:treatment +  (day | batID) #?
form3 <- flightTime ~ 1 + day + mass + day:treatment + mass:day:treatment + (day | batID) #?


n_bats <- n_per_group*n_groups


#create simulation function
sim <- function(n_per_group, days, beta0, beta_day, beta_daytreat
                , sdint, sdslope, corr, sdres, ...
){
  batID <- as.factor(1:n_bats) 
  treatment <- as.factor(rep(c("control", "exercise"), each=n_per_group)) 
  tdat <- data.frame(batID, treatment) 
  dat1 <- expand.grid(batID=batID, day=days) 
  flight_data <- full_join(dat1, tdat, by="batID") 
  flightTime <- simulate_new(form0, 
                             nsim = 1,
                             family = "gaussian",
                             newdata = flight_data,
                             newparams = list(
                               beta = c(beta0_f, beta_day_f, beta_daytreat_f) 
                               , theta = c(log(sdint_f), log(sdslope_f), corr)
                               , betad = log(sdres_f) 
                             )
  )
  mass <- simulate_new(form0, 
                       nsim = 1,
                       family = "gaussian",
                       newdata = flight_data,
                       newparams = list(
                         beta = c(beta0_m, beta_day_m, beta_daytreat_m) 
                         , theta = c(log(sdint_m), log(sdslope_m), corr)
                         , betad = log(sdres_m) 
                       )
  )
  flight_data$flightTime <- flightTime[[1]]
  flight_data$mass <- mass[[1]] 
  return(flight_data) 
}

#simulate once and plot to ensure the simulated output looks reasonable
s1 <- sim(n_per_group=n_per_group, days=days
          , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
          , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres)

plot_sim_flight <- function(flight_data) {
  ggplot(flight_data, aes(day, flightTime, colour = treatment)) +
    geom_line(aes(group=batID))
}
plot_sim_flight(s1)

plot_sim_mass <- function(flight_data) {
  ggplot(flight_data, aes(day, mass, colour = treatment)) +
    geom_line(aes(group=batID))
}
plot_sim_mass(s1)

fit <- function(flight_data){
  return(glmmTMB(form1 
                 , data=flight_data
                 , family = "gaussian"
  ))
}


simCIs <- function(simfun, fitfun, ...){ 
  dat <- simfun(...) 
  fit <- fitfun(dat) 
  tt <- (tidy(fit, effects = "fixed", conf.int = TRUE, conf.method = "profile") 
         %>% select(term, est = estimate, lwr = conf.low, upr = conf.high) 
         %>% mutate(across(where(is.character), factor)) 
  )
  return(tt) 
}


print(simCIs(simfun=sim, fitfun=fit
             , n_per_group=n_per_group, days=days 
             , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
             , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
))

nReps<- 1000

system.time(
  cis <- map_dfr(1:nReps, simCIs, .id="sim" 
                      , .progress = interactive() 
                      , simfun=sim, fitfun=fit 
                      , n_per_group = n_per_group, days=days 
                      , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
                      , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
  )
)

summary(cis)


beta_daytreat_mass=(2/60)


treatmentTrue_mass <- beta_daytreat_mass

dt <- (cis
           %>% filter(term=="day:treatmentexercise") 
           %>% drop_na() 
)

dt  %>% summarize(
  toohigh=mean(lwr>0) 
  , toolow=mean(upr<0)  
  , ci_width=mean(upr-lwr) 
)

dt  <- (dt 
           %>% mutate(across(sim, ~ reorder(factor(.), est)))
)

no_x_axis <- theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

gg_dt<- ggplot(dt, aes(sim, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ## blank x axis
  no_x_axis +
  ## reference line for coverage (do CIs include true value?)
  geom_hline(yintercept = 0,
             colour = "red", linewidth = 2) +
  geom_hline(yintercept = treatmentTrue_mass,
             colour = "blue", linewidth = 2) + 
  expand_limits(x=0) +
  ggtitle("day:treatmentexercise")

print(gg_dt)

m <- (cis
       %>% filter(term=="mass") 
       %>% drop_na() 
)

m  %>% summarize(
  toohigh=mean(lwr>0) 
  , toolow=mean(upr<0)  
  , ci_width=mean(upr-lwr) 
)

m  <- (m
        %>% mutate(across(sim, ~ reorder(factor(.), est)))
)

gg_m<- ggplot(m, aes(sim, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ## blank x axis
  no_x_axis +
  ## reference line for coverage (do CIs include true value?)
  geom_hline(yintercept = 0,
             colour = "red", linewidth = 2) +
  expand_limits(x=0) +
  ggtitle("mass")

print(gg_m)


mt <- (cis
      %>% filter(term=="mass:treatmentexercise") 
      %>% drop_na() 
)

mt %>% summarize(
  toohigh=mean(lwr>0) 
  , toolow=mean(upr<0)  
  , ci_width=mean(upr-lwr) 
)

mt <- (mt
       %>% mutate(across(sim, ~ reorder(factor(.), est)))
)

gg_mt<- ggplot(mt, aes(sim, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ## blank x axis
  no_x_axis +
  ## reference line for coverage (do CIs include true value?)
  geom_hline(yintercept = 0,
             colour = "red", linewidth = 2) +
  expand_limits(x=0) +
  ggtitle("mass:treatmentexercise")

print(gg_mt)

dmt <- (cis
        %>% filter(term=="day:mass:treatmentexercise") 
        %>% drop_na() 
)

dmt %>% summarize(
  toohigh=mean(lwr>0) 
  , toolow=mean(upr<0)  
  , ci_width=mean(upr-lwr) 
)

dmt <- (dmt
        %>% mutate(across(sim, ~ reorder(factor(.), est)))
)

gg_dmt<- ggplot(dmt, aes(sim, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ## blank x axis
  no_x_axis +
  ## reference line for coverage (do CIs include true value?)
  geom_hline(yintercept = 0,
             colour = "red", linewidth = 2) +
  expand_limits(x=0) +
  ggtitle("day:mass:treatmentexercise")

print(gg_dmt)

gg_arranged <- grid.arrange(gg_dt, gg_m, gg_mt, gg_dmt)

#ggsave("mass_predictor_on_flighttime.png", plot = gg_arranged, width = 6, height = 4, dpi = 300)

fit2 <- function(flight_data){
  return(glmmTMB(form2 
                 , data=flight_data
                 , family = "gaussian"
  ))
}


simCIs2 <- function(simfun, fitfun, ...){ 
  dat <- simfun(...) 
  fit <- fitfun(dat) 
  tt <- (tidy(fit, effects = "fixed", conf.int = TRUE, conf.method = "profile") 
         %>% select(term, est = estimate, lwr = conf.low, upr = conf.high) 
         %>% mutate(across(where(is.character), factor)) 
  )
  return(tt) 
}


print(simCIs2(simfun=sim, fitfun=fit2
             , n_per_group=n_per_group, days=days 
             , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
             , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
))

nReps<- 1000

system.time(
  cis2 <- map_dfr(1:nReps, simCIs2, .id="sim" 
                 , .progress = interactive() 
                 , simfun=sim, fitfun=fit 
                 , n_per_group = n_per_group, days=days 
                 , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
                 , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
  )
)

summary(cis2)
