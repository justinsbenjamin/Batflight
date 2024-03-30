library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)

#This is the example from lecture----
set.seed(101)


nGroup=15; days=seq(3, 60, by=3)
β0=30; β_treat=(-5/60); β_day=(-0.04)
sdint=5; sdslope=(0.5/60); corr=((-0.75) / (sqrt(1 + (-0.75^2)))); sdres=1
nBats <- nGroup*2
###These aren't what we decided on with Ben
##I played around with them because the plot looked weird
#Change back to original and compare once we've figured this out


## JD: Think about using treatment*day instead; what are the units of that β??
  #RS: ¯\_(ツ)_/¯
## JD: These ...'s aren't the best practice; should figure out more about map

sim <- function(nGroup, days, β0, β_treat, β_day
                , sdint, sdslope, corr, sdres, ...
){
  batID <- as.factor(1:nBats) ## JD: Make this look less like a number with paste()
  
  treatment <- as.factor(rep(c("control", "exercise"), each=nGroup))

  tdat <- data.frame(batID, treatment)
  
  dat1 <- expand.grid(batID=batID, day=days)
  
  bat_data <- full_join(dat1, tdat, by="batID")
  
  mass_data <- simulate_new( ~ 1  + day + treatment:day, 
                   nsim = 1,
                   family = "gaussian",
                   newdata = bat_data,
                   newparams = list(
                     beta = c(β0, β_treat, β_day),  #when I change this from 0, the treatment group increases in mass? when it's set at 0, it also looks crazy 
                     theta = c(log(c(sdint, sdslope)), corr), 
                     betad = log(sdres) #making this smaller makes variation smaller
      )
    )

     bat_data$mass <- mass_data[[1]]
     return(bat_data)
}


fit <- function(bat_data){
  return(glmmTMB(mass ~  1  + day + treatment:day
                 , data=bat_data
                 , family = "gaussian"
  ))
}

simCIs <- function(simfun, fitfun, ...){
  dat <- simfun(...)
  fit <- fitfun(dat)
  c <- as.data.frame(confint(fit))
  return(data.frame(vname=rownames(c)
                    , est = c$Est
                    , low = c$"2.5"
                    , high = c$"97.5"
  ))
}

  
nReps <- 100 #make smaller for now

print(simCIs(simfun=sim, fitfun=fit
             , nGroup = 15, days=seq(3, 60, by=3)
             , β0=30, β_treat=(-5/60), β_day=(-0.04)
             , sdint=5, sdslope=(0.5/60), corr=((-0.75) / (sqrt(1 + (-0.75^2)))), sdres=1
))

cis <- map_dfr(1:nReps, simCIs, .id="sim", simfun=sim, fitfun=fit
               , nGroup = 15, days=seq(3, 60, by=3)
               , β0=30, β_treat=(-5/60), β_day=(-0.04)
               , sdint=5, sdslope=(0.5/60), corr=((-0.75) / (sqrt(1 + (-0.75^2)))), sdres=1
)

summary(cis)
summary(cis |> mutate_if(is.character, as.factor))

treatmentTrue <- 10 #what is this

print(cis
      |> filter(vname=="treatmentexercise")
      |> summarize(
        toohigh=mean(low>treatmentTrue)
        , toolow=mean(high<treatmentTrue)
        , ci_width=mean(high-low)
        , power = mean(low>0)
      )
)
#I'm guessing this isn't just supposed to give me NaN, so something's wrong




#For original sim data:
# ggplot(bat_data, aes(x = day, y = mass, colour = batID))  +
#       geom_point() +
#       geom_line()
# #LOL WHAT IS THAT 






