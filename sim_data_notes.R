#hard to do one measure as count and one as continuous. start by just doing one alone, then see if we can figure out how to combine

#random slopes model. whats the mean at beginning (mean initial weight), whats avergae change in weight per day, guess something that seems reasonable, build machine I think should do that. 
  #for random slopes model, specify avg mass at beginning and avg slope, and st dev of mass at beginning and of slope. specifying st dev: 95% range is 4 sigma (plus/minus two). ii think initial mass will be 30 plus minus 10, set st dev to 5.
  #initial mass 30, st dev of 5
  #slope avg loss is 5 grams over whole exp. 5/60 per day. st dev:  better to start by lowballing. eg st dev 2/60 so probably everyone will lose weight. numebrs will be neg. correlation btw intercept and slope, neg correlation, eg. -.99 or -.75.
  #do this, plot, see if it makes sense. 

#JD eg. hardest part: specify fixed effect parameters (avg initial mass and avg weight loss), specify log of st dev of intercept (if st dev goes neg, then in trouble. have to be pos and simplest way when doing computations is to do on log scale), could do weight on log scale but harder to think about (could do as second step if first doesn't work).
  #just put in log of 5. will ask to specify theta (vector of what are parameters that determine random effects), first will be log of 5, second will be log of st dev of slope is log 1 over 60, put ndays=60 at top of script then could do log(nday), third number is the correlation, but not -.99 or -.75 bc need calculation (always easier to fit parameters when they can have any value btw neg and pos infinity. for pos variables, logging does trick nicely, the equation transformation changes something btw minus 1 and 1 to neg and pos infinity). write it out (roe =, transformation equation =)

#for mass, don't necessarily want them to be perfect straight lines, glmmtmb wants to sim from some ??
  #if simulating gaussian dist, also need residual variance. 
  #log of std dev of residual variance, 
  #if perfectly straihgt, would just be uncertainty of scale. but actually bigger than that (eg they just took a drink)

correlation <- (-0.75) / (sqrt(1 + (-0.75^2)))
correlation

mass_data <- simulate_new( ~ 1 + treatment #(remove bc if balanced = 0) + day + (1 + day | batID), #leave out main effect of treatment with 1 + day + treatment:day (additional weight loss in exercise)
                           nsim = 1,
                           family = "gaussian",
                           newdata = dat,
                           newparams = list(
                             beta = c(30, (-5/60), 2),   #RS: I don't think I understand these. model we're specifying says on day 0 theres avg weight of ind in the unexercised condition (intercept), on day 0 there's a diff btw exercised and non-exercise (but this isn't actually likely if randomized assignment or did paires by taking two fattest and flipping coin to decide group, etc is even smaller), unexercised as baseline condition, 
                             #first is mass at time 0, second is loss per day in control, third is difference in loss per day btw two groups
                             #could start by setting third to 0
                             #set third as -1/60 then exercise lose one more gram relative to control
                             theta = c(log(c(5, (1/60))), correlation), 
                             betad = log(3) #st dev of mass at day 0. 3 is plus or minus 6 grams 
                           )
)