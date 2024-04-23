library(glmmTMB)
library(dplyr)
library(purrr)
library(broom.mixed)
library(tidyverse)
library(ggplot2); theme_set(theme_bw())

set.seed(707)

n_per_group=15; days=seq(0, 60, by=3) 
n_groups <- 2

beta0=30; beta_day=(-1.5/60); beta_daytreat=(-3/60)


form0 <- ~ 1 + day + treatment:day 
form1 <- mass ~ 1 + day + treatment:day 

sdres=0.5

n_bats <- n_per_group*n_groups

sim <- function(n_per_group, days, beta0, beta_day, beta_daytreat
                ,sdres, ...
){
  batID <- as.factor(1:n_bats) 
  treatment <- as.factor(rep(c("control", "exercise"), each=n_per_group))
  tdat <- data.frame(batID, treatment) 
  dat1 <- expand.grid(batID=batID, day=days)
  mass_nore <- full_join(dat1, tdat, by="batID") 
  mass <- simulate_new(form0, 
                             nsim = 1,
                             newdata = mass_nore,
                             newparams = list(
                               beta = c(beta0, beta_day, beta_daytreat) 
                               , betad = log(sdres) 
                             )
  )
  
  mass_nore$mass <- mass[[1]] 
  return(mass_nore) 
}

s1 <- sim(n_per_group=n_per_group, days=days
          , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
          , sdres=sdres)

plot_sim <- function(mass_nore) {
  ggplot(mass_nore, aes(day, mass, colour = treatment)) +
    geom_line(aes(group=batID))
}
plot_sim(s1)

fit <- function(mass_nore){
  return(lm(form1 
            , data=mass_nore
  ))
}

simCIslm <- function(simfun, fitfun, ...){ 
  dat <- simfun(...) 
  fit <- fitfun(dat) 
  tt <- (tidy(fit, effects = "fixed", conf.int = TRUE, conf.method = "profile") 
         %>% select(term, est = estimate, lwr = conf.low, upr = conf.high) 
         %>% mutate(across(where(is.character), factor)) 
  )
  return(tt) 
}


print(simCIslm(simfun=sim, fitfun=fit
               , n_per_group=n_per_group, days=days
               , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
               , sdres=sdres
))

nReps <- 4000

system.time(
  cis <- map_dfr(1:nReps, simCIslm, .id="sim_mass" 
                 , .progress = interactive() 
                 , simfun=sim, fitfun=fit 
                 , n_per_group = n_per_group, days=days 
                 , beta0=beta0, beta_day=beta_day, beta_daytreat=beta_daytreat
                 , sdres=sdres
  )
)


summary(cis)

treatmentTrue_mass<- beta_daytreat 

dt_mass <- (cis
              %>% filter(term=="day:treatmentexercise") 
              %>% drop_na() 
)

dt_mass %>% summarize(
  toohigh=mean(lwr>treatmentTrue_mass) 
  , toolow=mean(upr<treatmentTrue_mass) 
  , ci_width=mean(upr-lwr) 
  , power = mean(lwr>0) 
)

dt_mass <- (dt_mass
              %>% mutate(across(sim_mass, ~ reorder(factor(.), est)))
)

no_x_axis <- theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

gg1 <- ggplot(dt_mass, aes(sim_mass, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  no_x_axis +
  geom_hline(yintercept = beta_daytreat,
             colour = "red", linewidth = 1) +
  geom_hline(yintercept = 0,
             colour = "blue", linewidth = 1) + 
  expand_limits(x=0)

print(gg1)

#ggsave("flighttime_CIs_no_random.png", plot = gg1, width = 6, height = 4, dpi = 300)