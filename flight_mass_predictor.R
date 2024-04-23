library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)
library(ggplot2); theme_set(theme_bw())

# if (packageVersion("broom.mixed") < "0.2.9.5") {
#     warning("please install the latest version of broom.mixed via remotes::install_github('bbolker/broom.mixed')")
# }

#mass <- readRDS("mass_dataset.RDS")

set.seed(404) #because this will probably just give me an error

n_per_group=15; days=seq(0, 60, by=3) #seq(first day, last day, step size)
n_groups <- 2

#set parameters
beta0_f=2; beta_day_f=(0.5/60); beta_daytreat_f=(2/60)

#model
form0 <- ~ 1 + day + treatment:day + (day | batID)
form1 <- flightTime ~ 1 + day + treatment:day + mass + mass:day + mass:treatment + (day | batID) #interaction between mass and treatment, mass and day????

corr_trans <- function(rho) rho/(sqrt(1+rho^2))

sdint_f=0.5; sdslope_f=(0.5/60); corr=corr_trans(0.75); sdres_f=0.1

n_bats <- n_per_group*n_groups

#set parameters
beta0=30; beta_day=(-1.5/60); beta_daytreat=(-3/60)
sdint=5; sdslope=(1/60); sdres=0.5


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
                         beta = c(beta0, beta_day, beta_daytreat) 
                         , theta = c(log(sdint), log(sdslope), corr)
                         , betad = log(sdres) 
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

plot_sim <- function(flight_data) {
  ggplot(flight_data, aes(day, mass, colour = treatment)) +
    geom_line(aes(group=batID))
}
plot_sim(s1)

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
             , n_per_group=n_per_group, days=days #apply set parameters
             , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
             , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
))

nReps<- 10

system.time(
  cis_1000 <- map_dfr(1:nReps, simCIs, .id="sim" #iterates over 1000 reps (set above), applying simCIs for each iteration. adds "sim" column for simulation number
                      , .progress = interactive() #progress bar
                      , simfun=sim, fitfun=fit #functions fit to simCIs
                      , n_per_group = n_per_group, days=days #parameters fit to simCIs
                      , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat#these parameters are fit into simfunin simCIs with ... above
                      , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
  )
)

summary(cis_1000)

beta_daytreat_mass=(-3/60)
treatmentTrue_mass <- beta_daytreat_mass #average *additional* loss per day (relative to control) in treatment group

dt_mass_1000 <- (cis_1000
                 %>% filter(term=="day:treatmentexercise") #retains only rows where the value in the column term is day:treatmentexercise
                 %>% drop_na() #removes rows missing values
)

dt_mass_1000  %>% summarize(
  toohigh=mean(lwr>treatmentTrue_mass) #proportion of CIs where the lower bound is greater than treatmentTrue 
  , toolow=mean(upr<treatmentTrue_mass) #proportion of CIs where the upper bound is lower than treatmentTrue 
  , ci_width=mean(upr-lwr) #mean width of CIs
  , power = mean(upr<0) #proportion of CIs where the upper bound is lower than zero. We are only interested in “power” to detect the true effect direction
)

## caterpillar plot

## arrange sims in order of increasing estimate value
dt_mass_1000  <- (dt_mass_1000 
                  %>% mutate(across(sim, ~ reorder(factor(.), est)))
)

## https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
no_x_axis <- theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

gg1_1000 <- ggplot(dt_mass_1000 , aes(sim, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ## blank x axis
  no_x_axis +
  ## reference line for coverage (do CIs include true value?)
  geom_hline(yintercept = beta_daytreat_mass,
             colour = "red", linewidth = 2) +
  ## reference line for power (do CIs include 0?)
  geom_hline(yintercept = 0,
             colour = "blue", linewidth = 2) + 
  expand_limits(x=0)

print(gg1_1000)

