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