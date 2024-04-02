library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)

if (packageVersion("broom.mixed") < "0.2.9.5") {
    warning("please install the latest version of broom.mixed via remotes::install_github('bbolker/broom.mixed')")
}

#example from lecture, but for mass----
set.seed(101)

## BMB: using variable names like β is cute but may not be completely portable
n_per_group=15; days=seq(3, 60, by=3)
n_groups <- 2

## BMB: this matches what we talked about better
β0=30; β_day=(-3/60); β_daytreat=(-1/60)

form0 <- ~ 1 + day + treatment:day + (day | batID)
form1 <- mass ~ 1 + day + treatment:day + (day | batID)

corr_trans <- function(rho) rho/(sqrt(1+rho^2))
sdint=5; sdslope=(1/60); corr=corr_trans(-0.75); sdres=1
n_bats <- n_per_group*n_groups


## JD: These ...'s aren't the best practice; should figure out more about map
## BMB: get JD to explain this comment, I don't understand it out of context

sim <- function(n_per_group, days, β0, β_day, β_daytreat
                , sdint, sdslope, corr, sdres, ...
){
  batID <- as.factor(1:n_bats) ## JD: Make this look less like a number with paste()
  treatment <- as.factor(rep(c("control", "exercise"), each=n_per_group))
  tdat <- data.frame(batID, treatment)
  dat1 <- expand.grid(batID=batID, day=days)
  bat_data <- full_join(dat1, tdat, by="batID")
  mass_data <- simulate_new( form0,
                   nsim = 1,
                   family = "gaussian",
                   newdata = bat_data,
                   newparams = list(
                       beta = c(β0, β_day, β_daytreat) 
                     , theta = c(log(sdint), log(sdslope), corr)
                     , betad = log(sdres) #making this smaller makes variation smaller
      )
    )

     bat_data$mass <- mass_data[[1]]
     return(bat_data)
}
    

s1 <- sim(n_per_group = 15, days=seq(3, 60, by=3)
  , β0=β0, β_day=β_day, β_daytreat=β_daytreat
  , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres)

plot_sim <- function(bat_data) {
    ggplot(bat_data, aes(day, mass, colour = treatment)) +
        geom_line(aes(group=batID))
}
plot_sim(s1)
## BMB: you should **stop here** until you get parameter values
## that make the simulated output look reasonable!

fit <- function(bat_data){
  return(glmmTMB(form1
                 , data=bat_data
                 , family = "gaussian"
  ))
}

## BMB: could speed up by combining simfun and fitfun, using
## nsim = nsim ...
simCIs <- function(simfun, fitfun, ...){
  dat <- simfun(...)
  fit <- fitfun(dat)
  tt <- (tidy(fit, effects = "fixed", conf.int = TRUE)
      %>% select(term, est = estimate, lwr = conf.low, upr = conf.high)
      %>% mutate(across(where(is.character), factor))
  )
  return(tt)
}

## test once before running a lot
print(simCIs(simfun=sim, fitfun=fit
           , n_per_group = 15, days=seq(3, 60, by=3)
             ## reuse values from above
           , β0=β0, β_day=β_day, β_daytreat=β_daytreat
           , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
))

  
nReps <- 1000 ## made smaller for now

## BMB: for slow stuff it is probably better to take apart the pieces
## (1) generate a big list of fitted objects
## (2) then run a separate workflow step to compute the summary
##     statistics you're interested in


system.time(
    cis <- map_dfr(1:nReps, simCIs, .id="sim"
                 , .progress = interactive()
                 , simfun=sim, fitfun=fit
                 , n_per_group = 15, days=seq(3, 60, by=3)
                 , β0=β0, β_day=β_day, β_daytreat=β_daytreat#these parameters are fit into sim with ...
                 , sdint=sdint, sdslope=sdslope, corr=corr, sdres=sdres
                   )
)
## BMB: about 6 minutes for 1000 reps on my laptop
## (could make faster by parallelizing etc., but adds complexity)

saveRDS(cis, "bat_mass_sims.RDS")







