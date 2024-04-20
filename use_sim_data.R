library(tidyverse); theme_set(theme_bw())
library(dplyr)

#file created by "simulate_mass_data.R"
mass_1000 <- readRDS("bat_mass_sims_1000.RDS")
mass_2500 <- readRDS("bat_mass_sims_2500.RDS")
#file created by "sim_flight_data.R"
flightTime_1000 <- readRDS("bat_flight_sims_1000.RDS")
#flightTime_4000 <- readRDS("bat_flight_sims_4000.RDS")

##mass sim data----
## BMB: this should probably be retrieved from an earlier stored
##  value, so we don't risk it getting out of sync
## (possibly stored in a 'true_val' column?)
beta_daytreat_mass=(-3/60)

#1000 mass sims
summary(mass_1000)#In an earlier version there were some NAs for lwr and upr, but seems okay now

## BMB: *simulated* (true) value of the parameter we're testing
treatmentTrue_mass <- beta_daytreat_mass #average *additional* loss per day (relative to control) in treatment group

dt_mass_1000 <- (mass_1000
           %>% filter(term=="day:treatmentexercise") #retains only rows where the value in the column term is day:treatmentexercise
           %>% drop_na() #removes rows missing values
)

dt_mass_1000  %>% summarize(
                toohigh=mean(lwr>treatmentTrue_mass) #proportion of CIs where the lower bound is greater than treatmentTrue 
              , toolow=mean(upr<treatmentTrue_mass) #proportion of CIs where the upper bound is lower than treatmentTrue 
              , ci_width=mean(upr-lwr) #mean width of CIs
              , power = mean(upr<0) #proportion of CIs where the upper bound is lower than zero. We are only interested in “power” to detect the true effect direction
            )

## caterpillar plot

## arrange sims in order of increasing estimate value
dt_mass_1000  <- (dt_mass_1000 
              %>% mutate(across(sim, ~ reorder(factor(.), est)))
)

## https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
no_x_axis <- theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

gg1_1000 <- ggplot(dt_mass_1000 , aes(sim, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ## blank x axis
  no_x_axis +
  ## reference line for coverage (do CIs include true value?)
  geom_hline(yintercept = beta_daytreat_mass,
             colour = "red", linewidth = 2) +
  ## reference line for power (do CIs include 0?)
  geom_hline(yintercept = 0,
             colour = "blue", linewidth = 2) + 
  expand_limits(x=0)

print(gg1_1000)

#ggsave("mass_CIs_1000.png", plot = gg1_1000, width = 6, height = 4, dpi = 300)

#2500 mass sims
summary(mass_2500)#In an earlier version there were some NAs for lwr and upr, but seems okay now

dt_mass_2500 <- (mass_2500
                 %>% filter(term=="day:treatmentexercise") #retains only rows where the value in the column term is day:treatmentexercise
                 %>% drop_na() #removes rows missing values
)

dt_mass_2500  %>% summarize(
  toohigh=mean(lwr>treatmentTrue_mass) #proportion of CIs where the lower bound is greater than treatmentTrue 
  , toolow=mean(upr<treatmentTrue_mass) #proportion of CIs where the upper bound is lower than treatmentTrue 
  , ci_width=mean(upr-lwr) #mean width of CIs
  , power = mean(upr<0) #proportion of CIs where the upper bound is lower than zero. We are only interested in “power” to detect the true effect direction
)

## caterpillar plot

## arrange sims in order of increasing estimate value
dt_mass_2500  <- (dt_mass_2500 
                  %>% mutate(across(sim, ~ reorder(factor(.), est)))
)

## https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
no_x_axis <- theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

gg1_2500 <- ggplot(dt_mass_2500 , aes(sim, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ## blank x axis
  no_x_axis +
  ## reference line for coverage (do CIs include true value?)
  geom_hline(yintercept = beta_daytreat_mass,
             colour = "red", linewidth = 2) +
  ## reference line for power (do CIs include 0?)
  geom_hline(yintercept = 0,
             colour = "blue", linewidth = 2) + 
  expand_limits(x=0)

print(gg1_2500)

ggsave("mass_CIs_2500.png", plot = gg1_2500, width = 6, height = 4, dpi = 300)


#flight sim data----
beta_daytreat_flightTime=(2/60)

summary(flightTime)

## BMB: *simulated* (true) value of the parameter we're testing
treatmentTrue_flightTime <- beta_daytreat_flightTime #average *additional* loss per day (relative to control) in treatment group

dt_flight <- (flightTime
              %>% filter(term=="day:treatmentexercise") #retains only rows where the value in the column term is day:treatmentexercise
              %>% drop_na() #removes rows missing values
)

dt_flight %>% summarize(
  toohigh=mean(lwr>treatmentTrue_flightTime) #proportion of CIs where the lower bound is greater than treatmentTrue 
  , toolow=mean(upr<treatmentTrue_flightTime) #proportion of CIs where the upper bound is lower than treatmentTrue 
  , ci_width=mean(upr-lwr) #mean width of CIs
  , power = mean(lwr>0) #proportion of CIs where the upper bound is lower than zero. We are only interested in “power” to detect the true effect direction
)

## caterpillar plot

## arrange sims in order of increasing estimate value
dt_flight <- (dt_flight
    %>% mutate(across(sim_flight, ~ reorder(factor(.), est)))
)

## https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
no_x_axis <- theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

gg2 <- ggplot(dt_flight, aes(sim_flight, est)) +
    geom_pointrange(aes(ymin = lwr, ymax = upr)) +
    ## blank x axis
    no_x_axis +
    ## reference line for coverage (do CIs include true value?)
    geom_hline(yintercept = beta_daytreat_flightTime,
               colour = "red", linewidth = 2) +
    ## reference line for power (do CIs include 0?)
    geom_hline(yintercept = 0,
               colour = "blue", linewidth = 2) + 
    expand_limits(x=0)

print(gg2)

ggsave("flighttime_CIs_1000.png", plot = gg2, width = 6, height = 4, dpi = 300)

