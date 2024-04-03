library(tidyverse)
library(dplyr)

#file created by "simulate_data.R"
cis <- readRDS("bat_mass_sims.RDS")
β_daytreat=(-1.5/60)

summary(cis)#In an earlier version there were some NAs for lwr and upr, but seems okay now

## BMB: *simulated* (true) value of the parameter we're testing
treatmentTrue <- β_daytreat #average *additional* loss per day (relative to control) in treatment group

dt_cis <- (cis
           %>% filter(term=="day:treatmentexercise") #retains only rows where the value in the column term is day:treatmentexercise
           %>% drop_na() #removes rows missing values
)

dt_cis %>% summarize(
  toohigh=mean(lwr>treatmentTrue) #proportion of CIs where the lower bound is greater than treatmentTrue 
  , toolow=mean(upr<treatmentTrue) #proportion of CIs where the upper bound is lower than treatmentTrue 
  , ci_width=mean(upr-lwr) #mean width of CIs
  , power = mean(lwr>0) #proportion of CIs where the lower bound is greater than zero. We are only interested in “power” to detect the true effect direction
)
