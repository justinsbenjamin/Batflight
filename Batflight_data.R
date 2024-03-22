# Simulation data for bat flight project 
# BIO 708: Quantitative Methods in Ecology and Evolution
# Author: Justin 

# Here is a preliminary version of the simulated data. It still needs some work.
# I'm not quite sure how to make the flight times "semi-random" by having some
# bats doing some flying and others not doing much or any flying. 
# The data on the number of shakes is not useful here yet, still going to work on that. 
# I started trying to do some work on the weights of the bats but didn't include it 
# in this chunk of code. My general plan was making all bats have a random initial 
# weight of between 5 and 9 grams (typical body size for little brown bats) and then 
# have the weight randomly change by up to 0.5 grams over the course of the 60 days but
# I was having some issues with it so left it out for now. It's likely their weight
# will also decrease over the study as well and I don't know how to simulate that
# randomly. 

library(glmmTMB)
library(performance)

# Set seed for reproducibility
set.seed(123)

# Number of individuals
num_individuals <- 30
ID <- rep(1:num_individuals, each = 20)

data <- data.frame(
  ID = rep(1:num_individuals, each = 20), # Repeated measures every 3 days for 60 days
  Day = rep(seq(1, 60, by = 3), times = num_individuals), # Day of the experiment
  Flight_time = sample(0:60, num_individuals*20, replace = TRUE), # Flight time in seconds
  Treatment = ifelse(ID %% 2 != 0, "Treatment", "Control"), # Assigning treatment/control
  Sex = ifelse(ID <= 5, "F", "M"), # First 5 are females, rest are males
  Shakes = sample(1:3, replace = TRUE) # Number of shakes
)

print(head(data, 140))

# Here's a mixed model with Day and ID as random effects. I'm well aware that
# it's not the best designed model. There's likely some interactions and correlations
# between the predictor variables that will change the model. 

model <- glmmTMB(Flight_time ~ 1 + Treatment + Day + Sex + (1 + Day | ID),
               data=data,
               family = "gaussian",
)

performance::check_model(model)




