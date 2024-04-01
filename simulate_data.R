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
nGroup=15; days=seq(3, 60, by=3)

## BMB: this matches what we talked about better
β0=30; β_day=(-5/60); β_daytreat=(-1/60)

form0 <- ~ 1 + day + treatment:day + (day | batID)
form1 <- mass ~ 1 + day + treatment:day + (day | batID)

corr_trans <- function(rho) rho/(sqrt(1+rho^2))
sdint=5; sdslope=(1/60); corr=corr_trans(-0.75); sdres=5
nBats <- nGroup*2


## BMB: I think these comments may be getting out of date. 1 + day + treatment:day
##      should be good as long as we are happy to assume no difference in weight on day 0
##      (which is reasonable if you are assigning bats to treatments randomly)

## JD: These ...'s aren't the best practice; should figure out more about map
## BMB: get JD to explain this comment, I don't understand it out of context

sim <- function(nGroup, days, β0, β_day, β_daytreat
                , sdint, sdslope, corr, sdres, ...
){
  batID <- as.factor(1:nBats) ## JD: Make this look less like a number with paste()
  
  treatment <- as.factor(rep(c("control", "exercise"), each=nGroup))

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
    

fit <- function(bat_data){
  return(glmmTMB(form1
                 , data=bat_data
                 , family = "gaussian"
  ))
}

simCIs <- function(simfun, fitfun, ...){
  dat <- simfun(...)
  fit <- fitfun(dat)
  tt <- (tidy(fit, effects = "fixed", conf.int = TRUE)
      %>% select(term, est = estimate, lwr = conf.low, upr = conf.high)
      %>% mutate(across(where(is.character), factor))
  )
  return(tt)
}

  
nReps <- 100 #made smaller for now

print(simCIs(simfun=sim, fitfun=fit
           , nGroup = 15, days=seq(3, 60, by=3)
             ## reuse values from above
           , β0=β0, β_day=β_day, β_daytreat=β_daytreat

           , sdint=sdint, sdslope=sdslope, corr=corr, sdres=5
))


cis <- map_dfr(1:nReps, simCIs, .id="sim", simfun=sim, fitfun=fit
               , nGroup = 15, days=seq(3, 60, by=3)
               , β0=30, β_treat=(-5/60), β_day=(-0.04)
               , sdint=sdint, sdslope=sdslope, corr=corr, sdres=5
)

summary(cis)
## BMB: *simulated* (true) value of the parameter we're testing
treatmentTrue <- β_daytreat

print(cis
      %>% filter(term=="day:treatmentexercise")
      %>% summarize(
            toohigh=mean(lwr>treatmentTrue)
          , toolow=mean(upr<treatmentTrue)
          , ci_width=mean(upr-lwr)
          , power = mean(lwr>0)
      )
)

saveRDS(cis, "bat_mass.RDS")

#For original sim data:
# ggplot(bat_data, aes(x = day, y = mass, colour = batID))  +
#       geom_point() +
#       geom_line()
# # #LOL WHAT IS THAT 






