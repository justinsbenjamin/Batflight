library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(lme4)

set.seed(202)

n_per_group=15; days=seq(0, 60, by=3) 
n_groups <- 2

beta0=6; beta_day=(-1.0/60); beta_daytreat=(-4.0/60)
#betas. beta0: initial number of "shakes"; beta_day: slope (average decrease in shakes per day in control group)
#beta_daytreat: average *additional* decrease in shakes per day (relative to control) in treatment group

#model
form0 <- ~ 1 + day + treatment:day + (day | batID)
form1 <- shakes ~ 1 + day + treatment:day + (day | batID)
#shakes: response variable; 1: intercept (shakes when all predictor variables are set to 0)
#day: the effect of day on shakes; treatment:day: interaction (the effect of day on shakes varies depending on treatment level)
#(day | batID); random effects (allows for individual variability in the intercept and slope of the relationship between "day" and "shakes" among different bats)

corr_trans <- function(rho) rho/(sqrt(1+rho^2))
#correlation between intercepts and slopes
#we have set rho to -0.75 below, but the calculation is needed to change from -1 and 1 to negative infinity and positive infinity (don't quite get this)

sdint=1; sdslope=(0.5/60); corr=corr_trans(-0.75); sdres=0.1
#thetas, and betad. sdint: standard deviation of intercept (plus/minus 10); sdslope: standard deviation of the slope
#corr: see above; sdred (betad): standard deviation of residual variance (plus/minus 2) 
#sdint, sdslope and betad are on log scale to ensure they are positive 
n_bats <- n_per_group*n_groups


# Poisson GLMM
form_poisson <- ~ 1 + day + treatment:day + (1 | batID) # Poisson GLMM

# Update simulation function for count data with Poisson distribution
sim_count_poisson <- function(n_per_group, days, beta0, beta_day, beta_daytreat
                              , sdint, sdslope, corr, ...
){
  batID <- as.factor(1:n_bats) 
  treatment <- as.factor(rep(c("control", "exercise"), each=n_per_group)) 
  tdat <- data.frame(batID, treatment) 
  dat1 <- expand.grid(batID=batID, day=days) 
  count_data <- full_join(dat1, tdat, by="batID")
  
  # Simulate flightCount using simulate_new directly
  count_data$shakes <- simulate_new(form_poisson, 
                                    nsim = 1,
                                    family = "poisson",
                                    newdata = count_data,
                                    newparams = list(
                                      beta = c(beta0, beta_day, beta_daytreat),
                                      theta = c(log(sdint), log(sdslope), corr),
                                      betad = log(sdres)
                                    ))[[1]]
  
  return(count_data)
}

# Simulate count data with Poisson distribution
s_poisson <- sim_count_poisson(n_per_group=n_per_group, days=days,
                               beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat,
                               sdint=sdint, sdslope=sdslope, corr=corr)

# Plot the simulated count data
plot_count_sim <- function(count_data) {
  ggplot(count_data, aes(day, shakes, colour = treatment)) +
    geom_line(aes(group=batID)) +
    ggtitle("Simulated Shake Count Data (Poisson)") +
    ylab("Shake Count") + xlab("Day")
}

plot_count_sim(s_poisson)




#percent difference
wide_poisson <- pivot_wider(s_poisson, 
                            names_from = day,  
                            values_from = shakes)

first_values <- wide_poisson[, 3]  # Assuming the first variable starts from column 2
last_values <- wide_poisson[, ncol(wide_poisson)] 

percent_difference <- ( 1 - (last_values / first_values) ) *100

wide_poisson <- (wide_poisson %>% left_join(percent_difference, by = "60")
                 %>% mutate(percent_differences= percent_difference))

print(percent_difference)


#create fit function for model and return fitted model object 
fit <- function(count_data){
  return(glmmTMB(form1 #specified model
                 , data=count_data
                 , family = "poisson"
  ))
}

## BMB: could speed up by combining simfun and fitfun, using
## nsim = nsim ...
#create function that simulates data, models it, and extracts coefficients and CIs
simCIs <- function(simfun, fitfun, ...){ #takes 2 arguments (one that simulates data, one that fits a model to the simulated data)
  dat <- simfun(...) #simulates data, ... allows passing additional arguments to simfun if needed
  fit <- fitfun(dat) #fits model 
  tt <- (tidy(fit, effects = "fixed", conf.int = TRUE) #extracts the model coefficients and their confidence intervals
         %>% select(term, est = estimate, lwr = conf.low, upr = conf.high) #selects specific columns: coefficient names (term), estimates (estimate), lower CI bounds (conf.low), and upper CI bounds (conf.high)
         %>% mutate(across(where(is.character), factor)) #converts character columns to factors (for downstream analyses, plotting)
  )
  return(tt) #returns extracted model coefficients and their CIs
}

## test once before running a lot
#Print estimates, lower CI bounds, and upper CI bounds 
print(simCIs(simfun=sim, fitfun=fit#Call simCI function, apply sim and fit functions,
             , n_per_group=n_per_group, days=days #apply set parameters
             , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
             , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
))

#how many reps of the simulation we want
nReps <- 1000

## BMB: for slow stuff it is probably better to take apart the pieces
## (1) generate a big list of fitted objects
## (2) then run a separate workflow step to compute the summary
##     statistics you're interested in

#apply the simCIs function iteratively to simulate data, fit models, and calculate confidence intervals, save dataframe as cis
system.time(
  cis <- map_dfr(1:nReps, simCIs, .id="sim_flight" #iterates over 1000 reps (set above), applying simCIs for each iteration. adds "sim" column for simulation number
                 , .progress = interactive() #progress bar
                 , simfun=sim, fitfun=fit #functions fit to simCIs
                 , n_per_group = n_per_group, days=days #parameters fit to simCIs
                 , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat#these parameters are fit into simfunin simCIs with ... above
                 , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
  )
)
## BMB: about 6 minutes for 1000 reps on my laptop
## (could make faster by parallelizing etc., but adds complexity)

saveRDS(cis, "bat_flight_sims.RDS")