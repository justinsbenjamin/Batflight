library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)

# if (packageVersion("broom.mixed") < "0.2.9.5") {
#     warning("please install the latest version of broom.mixed via remotes::install_github('bbolker/broom.mixed')")
# }

set.seed(505)

n_per_group=15; days=seq(0, 60, by=3) 
n_groups <- 2

beta0=4; beta_day=(-0.5/60); beta_daytreat=(-2/60)

form0 <- ~ 1 + day + treatment:day + (day | batID)
form1 <- shakes ~ 1 + day + treatment:day + (day | batID)

corr_trans <- function(rho) rho/(sqrt(1+rho^2))

sdint=0.5; sdslope=(0.5/60); corr=corr_trans(-0.75); sdres=0.1

n_bats <- n_per_group*n_groups



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
                             family = "poisson",
                             newdata = flight_data,
                             newparams = list(
                               beta = c(beta0, beta_day, beta_daytreat) 
                               , theta = c(log(sdint), log(sdslope)), corr)
                              # , betad = log(sdres) 
                             )
  )
  
  flight_data$flightTime <- flightTime[[1]] 
  return(flight_data) 
}

s1 <- sim(n_per_group=n_per_group, days=days
          , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
          , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres)

plot_sim <- function(flight_data) {
  ggplot(flight_data, aes(day, flightTime, colour = treatment)) +
    geom_line(aes(group=batID))
}
plot_sim(s1)