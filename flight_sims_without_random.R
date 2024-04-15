library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)
library(ggplot2); theme_set(theme_bw())

set.seed(303)

n_per_group=15; days=seq(0, 60, by=3) 
n_groups <- 2

beta0=2; beta_day=(0.5/60); beta_daytreat=(2/60)


form0 <- ~ 1 + day + treatment:day 
form1 <- flightTime ~ 1 + day + treatment:day 

sdres=0.1

n_bats <- n_per_group*n_groups

sim <- function(n_per_group, days, beta0, beta_day, beta_daytreat
                ,sdres, ...
){
  batID <- as.factor(1:n_bats) 
  treatment <- as.factor(rep(c("control", "exercise"), each=n_per_group))
  tdat <- data.frame(batID, treatment) 
  dat1 <- expand.grid(batID=batID, day=days)
  flight_data_nore <- full_join(dat1, tdat, by="batID") 
  flightTime <- simulate_new(form0, 
                             nsim = 1,
                             newdata = flight_data_nore,
                             newparams = list(
                               beta = c(beta0, beta_day, beta_daytreat) 
                               , betad = log(sdres) 
                             )
  )
  
  flight_data_nore$flightTime <- flightTime[[1]] 
  return(flight_data_nore) 
}

s1 <- sim(n_per_group=n_per_group, days=days
          , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
          , sdres=sdres)

plot_sim <- function(flight_data_nore) {
  ggplot(flight_data_nore, aes(day, flightTime, colour = treatment)) +
    geom_line(aes(group=batID))
}
plot_sim(s1)

fit <- function(flight_data_nore){
  return(lm(form1 
                 , data=flight_data_nore
  ))
}

simCIslm <- function(simfun, fitfun, ...){ 
  dat <- simfun(...) 
  fit <- fitfun(dat) 
  tt <- (tidy(fit, effects = "fixed", conf.int = TRUE) 
         %>% select(term, est = estimate, lwr = conf.low, upr = conf.high) 
         %>% mutate(across(where(is.character), factor)) 
  )
  return(tt) 
}


print(simCIslm(simfun=sim, fitfun=fit
             , n_per_group=n_per_group, days=days
             , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
             , sdres=sdres
))

nReps <- 4000

system.time(
  cis <- map_dfr(1:nReps, simCIslm, .id="sim_flight" 
                 , .progress = interactive() 
                 , simfun=sim, fitfun=fit 
                 , n_per_group = n_per_group, days=days 
                 , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
                 , sdres=sdres
  )
)


summary(cis)

treatmentTrue_flightTime <- beta_daytreat 

dt_flight <- (cis
              %>% filter(term=="day:treatmentexercise") 
              %>% drop_na() 
)

dt_flight %>% summarize(
  toohigh=mean(lwr>treatmentTrue_flightTime) 
  , toolow=mean(upr<treatmentTrue_flightTime) 
  , ci_width=mean(upr-lwr) 
  , power = mean(lwr>0) 
)

dt_flight <- (dt_flight
              %>% mutate(across(sim_flight, ~ reorder(factor(.), est)))
)

no_x_axis <- theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

gg1 <- ggplot(dt_flight, aes(sim_flight, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  no_x_axis +
  geom_hline(yintercept = beta_daytreat,
             colour = "red", linewidth = 1) +
  geom_hline(yintercept = 0,
             colour = "blue", linewidth = 1) + 
  expand_limits(x=0)

print(gg1)

