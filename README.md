# Batflight

**March 15**


Our project will examine captive big brown bat flight, and specifically whether non-flight exercise has a meaningful effect on their flight ability/willingness. This question comes from my (Renata's) observations that the captive big brown bats in our colony do not/cannot fly well. However, they do crawl, which I will exploit to exercise a group of bats and compare their flight ability/willingness to a non-exercise group. 

For flight ability, we are interested in 1) how long a bat sustains flight, and 2) how many laps a bat completes. For flight willingness (i.e. how willing a bat is to begin flight), we are interested in 1) how long it takes for the bat to take off from the experimenter's hand, and 2) how many "shakes" it takes for the bat to take off. These variables (which I am considering to signify flight ability/willingness) will be compared between the exercise and non-exercise groups to determine whether crawling exercise has a meaningful effect on flight.

This is a repeated measures design, as the same bats will be recorded multiple times (every **three days** over a period of **two months**). Based on some pilot work I've done, I will need to fast the bats and use food as a reward to get them to crawl. I will also fast the non-exercise group to avoid comparing excercise vs. non-exercise *and* fasted vs. regular diet. So, I know the bats will decrease in mass during the experiment, and bat mass is a fifth variable. 

This is something I am planning to do in the near future, so we will be using simulated data for this project. Thus, simulating the data is our first step. We need to simulate data for two bat groups (exercise and non-exercise) for the five variables listed above. 

Based on Ian's lecture, we can simulate data in R by setting sample_size, mean, and st_dev for the variables, then using the rnorm function. We will have to work out how to correctly do this for the two bat groups, measured every **three days** over the **two month** peiod.

Flight time, time to take off, and mass are continuous variables, while number of laps and number of shakes are discrete variables. Ben's (very helpful) comment on Teams suggests that the continuous variables will likely be either log-Normal or Gamma, while the discrete variables will likely be negative binomial models. This is how we will model our simulated data. Ben also mentioned that because this is a repeated measures design, we can test other interesting things, such as: does bat willingness correspond with bat flight, do individual bats have good vs. bad flight days? We will need to decide how to accurately test these. 

**March 22**


Originally, I planned to include number of laps flown as a measure of flight ability. However, I spoke with a colleague who pointed out that some bats will fly in laps, while others might just fly back and forth. Overall, flying back and forth involves less movement than flying in laps, so I've realized that this probably isn't a reliable measure. We could possbily look at distance flown, but this seems much more challenging. I think we should just stick to measuring flight time as the measure for flight ability. 

I have also been thinking about the measures for flight willingness (i.e. time to take off and number of "shakes" needed). If I haven't been clear up until this point, we release a bat for flight from hand, and a "shake" involves the experimenter swiftly shaking their hand downwards to encourage the bat to take off. I know that some bats would probably never take off without the "shakes", so it would result in incomplete trials for this project. Though, I also think it could be interesting to see which bats *do* take off without a "shake". Does it make sense to: start the trial, wait some time (e.g. 30s), then do the first "shake", wait some more time (e.g. 5s), then do the second "shake", and etc. until the bat takes off? Some bats might take off on their own (i.e. without the shake) after 30s, which I would not know. Conversely, does it make more sense to just start the "shakes" right away, and still measure time to take off? Someone smarter than me please tell me what makes most sense. 

If you have any suggestions on how to help us simulate "semi-random" data that would be nice. Different bats will have different weights, personalities etc which each make them separate. I'm trying to find a way to use randomly generated data but keeping each bat kind of personalized. Like the weight of a particular bat won't change much from 1 measurement to the next but it may change a fair amount over the course of the study. Or for the flight times and number of shakes necessary to get them to fly some bats will be stubborn and not fly or fly very little and others might fly a bit more so randomizing flight times can throw off that data. Thanks!
