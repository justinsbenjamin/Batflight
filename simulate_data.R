library(glmmTMB)
library(dplyr)
library(tidyverse)

#This is the example from lecture
set.seed(101)

nGroup <- 15
nBats <- nGroup*2

batID <- as.factor(1:nBats) ## JD: Make this look less like a number with paste()
day <- seq(3, 60, by=3)
treatment <- as.factor(rep(c("control", "exercise"), each=nGroup))

tdat <- data.frame(batID, treatment)
dat1 <- expand.grid(batID=batID, day=day)
bat_data <- full_join(dat1, tdat, by="batID")

#print(dat)

#flight ability----
##flightTime ~ treatment + day + (day | batID)

## JD: Think about using treatment*day instead; what are the units of that β??
  #RS: ¯\_(ツ)_/¯
correlation <- (-0.75) / (sqrt(1 + (-0.75^2)))
#correlation

mass_data <- simulate_new( ~ 1  + day + treatment:day, 
                   nsim = 1,
                   family = "gaussian",
                   newdata = bat_data,
                   newparams = list(
                     beta = c(30, (-5/60), -0.04),  #when I change this from 0, the treatment group increases in mass? when it's set at 0, it also looks crazy 
                     theta = c(log(c(5, (0.5/60))), correlation), 
                     betad = log(1) #making this smaller makes variation smaller
                   )
)

bat_data$mass <- mass_data[[1]]

fit <- glmmTMB(mass ~ 1  + day + treatment:day
               , data=bat_data
               , family = "gaussian"
)

summary(fit)
confint(fit)


ggplot(bat_data, aes(x = day, y = mass, colour = batID))  +
      geom_point() +
      geom_line()
#LOL WHAT IS THAT 



