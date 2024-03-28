#hard to do one measure as count and one as continuous. start by just doing one alone, then see if we can figure out how to combine

#random slopes model. what's the mean at beginning (mean initial weight), what's average change in weight per day, guess something that seems reasonable, build machine I think should do that. 
  #for random slopes model, specify avg mass at beginning and avg slope, and st dev of mass at beginning and of slope. specifying st dev: 95% range is 4 sigma (plus/minus two). ii think initial mass will be 30 plus minus 10, set st dev to 5.
  #initial mass 30, st dev of 5
  #slope avg loss is 5 grams over whole exp. 5/60 per day. st dev:  better to start by lowballing. eg st dev 2/60 so probably everyone will lose weight. numbers will be neg. correlation btw intercept and slope, neg correlation, eg. -.99 or -.75.
  #do this, plot, see if it makes sense. 

#JD eg. hardest part: specify fixed effect parameters (avg initial mass and avg weight loss), specify log of st dev of intercept (if st dev goes neg, then in trouble. have to be pos and simplest way when doing computations is to do on log scale), could do weight on log scale but harder to think about (could do as second step if first doesn't work).
  #just put in log of 5. will ask to specify theta (vector of what are parameters that determine random effects), first will be log of 5, second will be log of st dev of slope is log 1 over 60, put ndays=60 at top of script then could do log(nday), third number is the correlation, but not -.99 or -.75 bc need calculation (always easier to fit parameters when they can have any value btw neg and pos infinity. for pos variables, logging does trick nicely, the equation transformation changes something btw minus 1 and 1 to neg and pos infinity). write it out (rho =, transformation equation =)

#for mass, don't necessarily want them to be perfect straight lines, glmmTMB wants to sim from some ??
  #if simulating gaussian dist, also need residual variance. 
  #log of std dev of residual variance, 
  #if perfectly straight, would just be uncertainty of scale. but actually bigger than that (eg they just took a drink)

## see https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html,
## search for "inverse transformation" (this is under the 'AR1' section but
## applies to the correlation between intercepts and slopes  in a random-slopes
## model as well
correlation <- (-0.75) / (sqrt(1 - (-0.75^2)))
correlation

## BMB: probably want to use ~ 1 + day + treatment:day
##  as discussed in class
##  beta[1] = mass in g on day 0
##  beta[2] = avg loss (g per day) in control group
##  beta[3] = avg *additional* loss per day (relative to control)
##            in treatment group

mass_data <- simulate_new( ~ 1 + treatment #(remove bc if balanced = 0) + day + (1 + day | batID), #leave out main effect of treatment with 1 + day + treatment:day (additional weight loss in exercise)
                           nsim = 1,
                           family = "gaussian",
                           newdata = dat,
                           newparams = list(
                             beta = c(30, (-5/60), 2),   #RS: I don't think I understand these. model we're specifying says on day 0 there's avg weight of ind in the unexercised condition (intercept), on day 0 there's a diff btw exercised and non-exercise (but this isn't actually likely if randomized assignment or did pairs by taking two fattest and flipping coin to decide group, etc is even smaller), unexercised as baseline condition, 
                             #first is mass at time 0, second is loss per day in control, third is difference in loss per day btw two groups
                             #could start by setting third to 0
                             #set third as -1/60 then exercise lose one more gram relative to control
                             theta = c(log(c(5, (1/60))), correlation), 
                             betad = log(3) #st dev of mass at day 0. 3 is plus or minus 6 grams 
                           )
)


#Thursday's class
#neg binomial for the project. can say if bigger than 18, just set as 18
  #in the future decide if neg binomial or ordinal analysis better

#neg binomial simulation and analysis for number of shakes
#experiment with diff diagnostic plots, dhamrma or something
#for real data, start by looking at diagnostic plots. if they don't look good like simulated ones do, think about how to analyze differently.

#pick mean for each observation, using R n binomial (to pick neg binomial deviate). can probably use built in sim function to sim means. 
  #give size (0 to infinity, opp of how much variation, when goes to infinity, neg binomial is poisson, as goes to o, very high variability), mu is mean (use instead of probability)
  ## trials, size?, mu is mean
  #size is how much variability there is large size is less variability, small is more. 

hist(rnbinom(100, size=100, mu=4))

