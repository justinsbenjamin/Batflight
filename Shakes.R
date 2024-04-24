library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(lme4)

set.seed(606)

n_per_group=15; days=seq(0, 60, by=3) 
n_groups <- 2

beta0=2; beta_day=(-1.0/60); beta_daytreat=(-1.0/60)
#betas. beta0: initial number of "shakes"; beta_day: slope (average decrease in shakes per day in control group)
#beta_daytreat: average *additional* decrease in shakes per day (relative to control) in treatment group

#model
form0 <- ~ 1 + day + treatment:day + (day | batID)
form1 <- shakes ~ 1 + day + treatment:day + (day | batID)

corr_trans <- function(rho) rho/(sqrt(1+rho^2))
#correlation between intercepts and slopes
#we have set rho to -0.75 below, but the calculation is needed to change from -1 and 1 to negative infinity and positive infinity (don't quite get this)

sdint=0.1; sdslope=(0.1/60); corr=corr_trans(-0.75); sdres=0.1
#thetas, and betad. sdint: standard deviation of intercept (plus/minus 10); sdslope: standard deviation of the slope
#corr: see above; sdred (betad): standard deviation of residual variance (plus/minus 2) 
#sdint, sdslope and betad are on log scale to ensure they are positive 
n_bats <- n_per_group*n_groups



# Update simulation function for count data with Poisson distribution
sim <- function(n_per_group, days, beta0, beta_day, beta_daytreat
                              , sdint, sdslope, corr, sdres...
){
  batID <- as.factor(1:n_bats) 
  treatment <- as.factor(rep(c("control", "exercise"), each=n_per_group)) 
  tdat <- data.frame(batID, treatment) 
  dat1 <- expand.grid(batID=batID, day=days) 
  count_data <- full_join(dat1, tdat, by="batID")
  shakes <- simulate_new(form0, 
                                    nsim = 1,
                                    family = negative.binomial(theta = 3),
                                    newdata = count_data,
                                    newparams = list(
                                      beta = c(beta0, beta_day, beta_daytreat),
                                      theta = c(log(sdint), log(sdslope), corr),
                                      betad = log(sdres)
                                    )
                        )
  count_data$shakes <- shakes[[1]] 
  
  return(count_data)
}

s1 <- sim(n_per_group=n_per_group, days=days,
                               beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat,
                               sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres)

# Plot the simulated count data
plot_count_sim <- function(count_data) {
  ggplot(count_data, aes(day, shakes, colour = treatment)) +
    geom_line(aes(group=batID)) 
}

plot_count_sim(s1)




#percent difference
# wide_poisson <- pivot_wider(s_poisson, 
#                             names_from = day,  
#                             values_from = shakes)
# 
# first_values <- wide_poisson[, 3]  # Assuming the first variable starts from column 2
# last_values <- wide_poisson[, ncol(wide_poisson)] 
# 
# percent_difference <- ( 1 - (last_values / first_values) ) *100
# 
# wide_poisson <- (wide_poisson %>% left_join(percent_difference, by = "60")
#                  %>% mutate(percent_differences= percent_difference))
# 
# print(percent_difference)
# 

#create fit function for model and return fitted model object 
fit <- function(count_data){
  return(glmmTMB(form1 
                 , data=count_data
                 , family = negative.binomial(theta = 3)
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

nReps <- 1000


system.time(
  cis <- map_dfr(1:nReps, simCIs, .id="sim_shake" 
                 , .progress = interactive() 
                 , simfun=sim, fitfun=fit 
                 , n_per_group = n_per_group, days=days 
                 , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
                 , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
  )
)

saveRDS(cis, "bat_shakes_sims.RDS")