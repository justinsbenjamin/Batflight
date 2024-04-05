library(glmmTMB)
library(broom.mixed)
library(dplyr)

data("sleepstudy", package = "lme4")
m1 <- glmmTMB(Reaction ~ Days + (Days | Subject), sleepstudy)
m2 <- update(m1, REML = TRUE)

tt <- function(x,...) {
    res <- (tidy(x, conf.int = TRUE, effect = "fixed", ...)
        |> dplyr::select(term, estimate, lwr = conf.low, upr = conf.high)
        ## change to df so we print more digits ...
        |> as.data.frame()
    )
    return(res)
}

tt(m1)
##          term  estimate        lwr      upr
## 1 (Intercept) 251.40434 238.405523 264.4032
## 2        Days  10.46722   7.522954  13.4115
tt(m1, conf.method = "profile")
##          term  estimate        lwr       upr
## 1 (Intercept) 251.40434 237.680491 265.12947
## 2        Days  10.46722   7.358607  13.57591
tt(m2)
##          term  estimate        lwr       upr
## 1 (Intercept) 251.40510 238.029219 264.78099
## 2        Days  10.46729   7.437595  13.49698
try(tt(m2, conf.method= "profile"))
## Error in profile.glmmTMB(object, parm = parm, level_max = level, parallel = parallel,  : 
##  can't compute profiles for REML models at the moment (sorry)
