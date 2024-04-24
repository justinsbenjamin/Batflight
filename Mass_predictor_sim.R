library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)
library(ggplot2); theme_set(theme_bw())

set.seed(101)

n_per_group=15; days=seq(0, 60, by=3)
n_groups <- 2

#set parameters
beta0=30; beta_day=(-1.5/60); beta_daytreat=(-3/60)
#betas. beta0: initial mass in g; beta_day: slope (average loss (g per day) in control group)
#beta_daytreat: average *additional* loss per day (relative to control) in treatment group

#model
form0 <- ~ 1 + day + treatment:day + (day | batID)

#mass: response variable; 1: intercept (mass when all predictor variables are set to 0)
#day: the effect of day on mass; treatment:day: interaction (the effect of day on mass varies depending on treatment level)
#(day | batID); random effects (allows for individual variability in the intercept and slope of the relationship between "day" and "mass" among different bats)

corr_trans <- function(rho) rho/(sqrt(1+rho^2))
#correlation between intercepts and slopes
#we have set rho to -0.75 below, but the calculation is needed to change from -1 and 1 to negative infinity and positive infinity (don't quite get this)

sdint=5; sdslope=(1/60); corr=corr_trans(-0.75); sdres=0.5
#thetas, and betad. sdint: standard deviation of intercept (plus/minus 10); sdslope: standard deviation of the slope
#corr: see above; sdred (betad): standard deviation of residual variance (plus/minus 2), needed because we're simulating a gaussain distribution and we don't just want straight lines (e.g. they just took a drink so their mass is bigger)
#sdint, sdslope and betad are on log scale to ensure they are positive 
n_bats <- n_per_group*n_groups


#create simulation function
sim <- function(n_per_group, days, beta0, beta_day, beta_daytreat
                , sdint, sdslope, corr, sdres, ...
){
  batID <- as.factor(1:n_bats) #create individual bat IDs ## JD: Make this look less like a number with paste()
  treatment <- as.factor(rep(c("control", "exercise"), each=n_per_group)) #create treatment groups, 15 per group
  tdat <- data.frame(batID, treatment) #create data frame with batIDs and treatment groups
  dat1 <- expand.grid(batID=batID, day=days) #expand so each batID occurs for each day measured 
  bat_data <- full_join(dat1, tdat, by="batID") #combine to have batID, treatment groups, days
  mass_data <- simulate_new(form0, #simulate mass data with gaussian distribution, using set parameters
                            nsim = 1,
                            family = "gaussian",
                            newdata = bat_data,
                            newparams = list(
                              beta = c(beta0, beta_day, beta_daytreat) 
                              , theta = c(log(sdint), log(sdslope), corr)
                              , betad = log(sdres) 
                            )
  )
  
  bat_data$mass <- mass_data[[1]] #assigns simulated mass data to a mass column in bat_data
  return(bat_data) # allows the modified bat_data object to be used or assigned to a variable outside of the function
}

#simulate once and plot to ensure the simulated output looks reasonable
s1 <- sim(n_per_group=n_per_group, days=days
          , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
          , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres)

plot_sim <- function(bat_data) {
  ggplot(bat_data, aes(day, mass, colour = treatment)) +
    geom_line(aes(group=batID))
}
plot_sim(s1)


set.seed(404) 


#set parameters
beta0_f=2; beta_day_f=(0.5/60); beta_daytreat_f=(2/60); beta_mass_f=(0.1); beta_daymass_f=(1/60); beta_masstreat_f=(0.5/60)

#betas meanings:
#beta0_f: initial flight time in s; beta_day_f: slope average flight time gain in control group)
#beta_daytreat_f: average *additional* flight time gain per day (relative to control) in treatment group

#model
form1 <- ~ 1 + day + treatment:day + mass + mass:day + mass:treatment + (day | batID) 

sdint_f=0.5; sdslope_f=(0.5/60); corr_f=corr_trans(0.75); sdres_f=0.1

mass <- s1$mass


#create simulation function
sim <- function(n_per_group, days, beta0_f, beta_day_f, beta_daytreat_f
                , sdint_f, sdslope_f, corr_f, sdres_f, ...
){
  batID <- as.factor(1:n_bats) 
  mass <- s1$mass
  treatment <- as.factor(rep(c("control", "exercise"), each=n_per_group)) 
  tdat <- data.frame(batID, treatment) 
  dat1 <- expand.grid(batID=batID, day=days) 
  flight_data <- full_join(dat1, tdat, by="batID") 
  flightTime <- simulate_new(form1, 
                             nsim = 1,
                             family = "gaussian",
                             newdata = flight_data,
                             newparams = list(
                               beta = c(beta0_f, beta_day_f, beta_daytreat_f, 
                                        beta_mass_f, beta_daymass_f, beta_masstreat_f) 
                               , theta = c(log(sdint_f), log(sdslope_f), corr)
                               , betad = log(sdres_f) 
                       )
  )
  flight_data$flightTime <- flightTime[[1]]
  return(flight_data) 
}


#simulate once and plot to ensure the simulated output looks reasonable
s2 <- sim(n_per_group=n_per_group, days=days
          , beta0_f=beta0_f, beta_day_f=beta_day_f, beta_daytreat_f=beta_daytreat_f
          , beta_mass_f= beta_mass_f, beta_daymass_f= beta_daymass_f, beta_masstreat_f=beta_masstreat_f
          , sdint_f=sdint_f, sdslope_f=sdslope_f, corr_f=corr_f, sdres_f=sdres_f)

plot_sim <- function(flight_data) {
  ggplot(flight_data, aes(day, flightTime, colour = treatment)) +
    geom_line(aes(group=batID))
}
plot_sim(s2)

summary(form1)


#create fit function for model and return fitted model object 
fit <- function(flight_data, form){
  return(glmmTMB(form1 #specified model
                 , data=flight_data
                 , family = "gaussian"
  ))
}

## BMB: could speed up by combining simfun and fitfun, using
## nsim = nsim ...
#create function that simulates data, models it, and extracts coefficients and CIs
simCIs <- function(simfun, fitfun, ...){ #takes 2 arguments (one that simulates data, one that fits a model to the simulated data)
  dat <- simfun(...) #simulates data, ... allows passing additional arguments to simfun if needed
  fit <- fitfun(dat, form1) #fits model 
  tt <- (tidy(fit, effects = "fixed", conf.int = TRUE, conf.method = "profile") #extracts the model coefficients and their confidence intervals
         %>% select(term, est = estimate, lwr = conf.low, upr = conf.high) #selects specific columns: coefficient names (term), estimates (estimate), lower CI bounds (conf.low), and upper CI bounds (conf.high)
         %>% mutate(across(where(is.character), factor)) #converts character columns to factors (for downstream analyses, plotting)
  )
  return(tt) #returns extracted model coefficients and their CIs
}


simCIs <- function(simfun, fitfun, ...) {
  dat <- simfun(...) 
  fit <- fitfun(dat)
  
  # Print out the structure of the fit object for debugging
  print(str(fit))
  coef_table <- coef(summary(fit))
  coef_table <- coef_table[, c("Estimate", "2.5 %", "97.5 %")]
  names(coef_table) <- c("est", "lwr", "upr")
  coef_table <- as.data.frame(coef_table)
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL
  coef_table <- coef_table[, c("term", "est", "lwr", "upr")]
  coef_table <- mutate(coef_table, across(where(is.character), factor))
  return(coef_table)
}

print(simCIs(simfun=sim, fitfun=fit))

## test once before running a lot
#Print estimates, lower CI bounds, and upper CI bounds 
print(simCIs(simfun=sim, fitfun=fit #Call simCI function, apply sim and fit functions,
             , n_per_group=n_per_group, days=days
             , beta0_f=beta0_f, beta_day_f=beta_day_f, beta_daytreat_f=beta_daytreat_f
             , beta_mass_f= beta_mass_f, beta_daymass_f= beta_daymass_f, beta_masstreat_f=beta_masstreat_f
             , sdint_f=sdint_f, sdslope_f=sdslope_f, corr_f=corr_f, sdres_f=sdres_f
))

print(simCIs(simfun=sim, fitfun=fit))

#how many reps of the simulation we want
nReps_10 <- 10

#apply the simCIs function iteratively to simulate data, fit models, and calculate confidence intervals, save dataframe as cis
system.time(
  cis_10 <- map_dfr(1:nReps_10, simCIs, .id="sim" #iterates over 1000 reps (set above), applying simCIs for each iteration. adds "sim" column for simulation number
                      , .progress = interactive() #progress bar
                      , simfun=sim, fitfun=fit #functions fit to simCIs
                      , n_per_group = n_per_group, days=days #parameters fit to simCIs
                      , beta0_f=beta0_f, beta_day_f=beta_day_f, beta_daytreat_f=beta_daytreat_f
                      , beta_mass_f= beta_mass_f, beta_daymass_f= beta_daymass_f, beta_masstreat_f=beta_masstreat_f
                      , sdint_f=sdint_f, sdslope_f=sdslope_f, corr_f=corr_f, sdres_f=sdres_f
  )
)

nReps_2500 <- 2500


system.time(
  cis_2500 <- map_dfr(1:nReps_2500, simCIs, .id="sim" #iterates over 1000 reps (set above), applying simCIs for each iteration. adds "sim" column for simulation number
                      , .progress = interactive() #progress bar
                      , simfun=sim, fitfun=fit #functions fit to simCIs
                      , n_per_group = n_per_group, days=days #parameters fit to simCIs
                      , beta0_f=beta0_f, beta_day_f=beta_day_f, beta_daytreat_f=beta_daytreat_f
                      , beta_mass_f= beta_mass_f, beta_daymass_f= beta_daymass_f, beta_masstreat_f=beta_masstreat_f
                      , sdint_f=sdint_f, sdslope_f=sdslope_f, corr_f=corr_f, sdres_f=sdres_f
  )
)







saveRDS(cis_1000, "bat_mass_sims_1000.RDS")

saveRDS(cis_2500, "bat_mass_sims_2500.RDS")




#percent difference
wide_s1 <- pivot_wider(s1, 
                       names_from = day,  
                       values_from = mass)

first_values <- wide_s1[, 3]  # Assuming the first variable starts from column 2
last_values <- wide_s1[, ncol(wide_s1)] 

percent_difference <- ( 1 - (last_values / first_values) ) *100

wide_s1 <- (wide_s1 %>% left_join(percent_difference, by = "60")
            %>% mutate(percent_differences= percent_difference))

print(percent_difference)

#I can't figure out how to do this