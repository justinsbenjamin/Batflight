library(tidyverse)

set.seed(101)

#file created by "stimulate_data.R"
cis <- readRDS("bat_mass_sims.RDS")
β_daytreat=(-1/60)

summary(cis)
## BMB: *simulated* (true) value of the parameter we're testing
treatmentTrue <- β_daytreat
dt_cis <- (cis
           %>% filter(term=="day:treatmentexercise")
           %>% drop_na()
)

dt_cis %>% summarize(
  toohigh=mean(lwr>treatmentTrue)
  , toolow=mean(upr<treatmentTrue)
  , ci_width=mean(upr-lwr)
  , power = mean(lwr>0)
)

#hi
