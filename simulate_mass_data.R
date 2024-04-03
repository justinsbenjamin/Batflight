library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)

# if (packageVersion("broom.mixed") < "0.2.9.5") {
#     warning("please install the latest version of broom.mixed via remotes::install_github('bbolker/broom.mixed')")
# }

#set.seed() ensures reproducibility when randomizing
set.seed(101)

n_per_group=15; days=seq(0, 60, by=3) #seq(first day, last day, step size)
n_groups <- 2

#set parameters
β0=30; β_day=(-3/60); β_daytreat=(-1.5/60)
#betas. β0: initial mass in g; β_day: slope (average loss (g per day) in control group)
#β_daytreat: average *additional* loss per day (relative to control) in treatment group

#model
form0 <- ~ 1 + day + treatment:day + (day | batID)
form1 <- mass ~ 1 + day + treatment:day + (day | batID)
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


## JD: These ...'s aren't the best practice; should figure out more about map
## BMB: get JD to explain this comment, I don't understand it out of context


#create simulation function
sim <- function(n_per_group, days, β0, β_day, β_daytreat
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
                       beta = c(β0, β_day, β_daytreat) 
                     , theta = c(log(sdint), log(sdslope), corr)
                     , betad = log(sdres) 
      )
    )

     bat_data$mass <- mass_data[[1]] #assigns simulated mass data to a mass column in bat_data
     return(bat_data) # allows the modified bat_data object to be used or assigned to a variable outside of the function
}
    
#simulate once and plot to ensure the simulated output looks reasonable
s1 <- sim(n_per_group=n_per_group, days=days
  , β0=β0, β_day=β_day, β_daytreat=β_daytreat
  , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres)

plot_sim <- function(bat_data) {
    ggplot(bat_data, aes(day, mass, colour = treatment)) +
        geom_line(aes(group=batID))
}
plot_sim(s1)

##stop here and check plot

#create fit function for model and return fitted model object 
fit <- function(bat_data){
  return(glmmTMB(form1 #specified model
                 , data=bat_data
                 , family = "gaussian"
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
           , β0=β0, β_day=β_day, β_daytreat=β_daytreat
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
    cis <- map_dfr(1:nReps, simCIs, .id="sim" #iterates over 1000 reps (set above), applying simCIs for each iteration. adds "sim" column for simulation number
                 , .progress = interactive() #progress bar
                 , simfun=sim, fitfun=fit #functions fit to simCIs
                 , n_per_group = n_per_group, days=days #parameters fit to simCIs
                 , β0=β0, β_day=β_day, β_daytreat=β_daytreat#these parameters are fit into simfunin simCIs with ... above
                 , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
                   )
)
## BMB: about 6 minutes for 1000 reps on my laptop
## (could make faster by parallelizing etc., but adds complexity)

saveRDS(cis, "bat_mass_sims.RDS")






