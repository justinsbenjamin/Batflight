library(glmmTMB)
library(broom.mixed)
library(dplyr)
library(purrr)
library(ggplot2); theme_set(theme_bw())

## could use lower conf.level, would require fewer sims, but I'm concerned
## that it would also lessen the observed phenomenon
nsim <- 5000
set.seed(101)

data("sleepstudy", package = "lme4")
m1 <- glmmTMB(Reaction ~ Days + (Days | Subject), sleepstudy)
m2 <- update(m1, REML = TRUE) 



## we'll treat the fitted values as *true* values for simulations
true_vals <- tidy(m1, effects = "fixed") |> select(term, true_val = estimate)
#RS: the coefficient estimates?


simfun <- function(i, method = c("ML", "REML"), ...) {
    ## first arg is ignored (index from map) 
    method <- match.arg(method)
    ci_types <- c("profile", "wald")
    if (method == "REML") ci_types <- "wald"
    ss <- simulate(m1)[[1]]
    fitted_model <- if(method == "ML") refit(m1, ss) else update(m1, data = mutate(sleepstudy, Reaction = ss), REML = TRUE)
    res <- (ci_types
        |> setNames(ci_types)  ## so .id works properly (uses *names* of the list passed to it)
        |> map(~ tidy(fitted_model, effects = "fixed", conf.int = TRUE, conf.method = .))         ## get CIs
        |> map_dfr(~select(., term, estimate, lwr = conf.low, upr = conf.high), .id = "ci_type")  ## pick elements and combine
        |> full_join(true_vals, by = c("term"))
        |> mutate(method = method, .before = 1)
    )
    return(res)
}
#ML: Maximum likelihood, aims to find the parameter values that make observed data most probable under assumed statistical model, doesn't account for estimation of fixed effects when estimating variance components in mixed-effects models
#REML: Restricted Maximum Likelihood, used for estimating parameters of a mixed-effects model, aims to maximize the likelihood of the data conditional on the fixed effects, effectively removing the bias introduced by estimating fixed effects when estimating the variance components
#Profile CI: Maximizes the likelihood function over  parameter space, while holding other parameters at fixed values, particularly useful for nonlinear or complex models
#Wald CI: Constructed using asymptotic normality of the parameter estimates, based on  assumption that  parameter estimates follow a normal distribution with mean equal to the maximum likelihood estimate and variance equal to inverse of the Fisher information matrix, simpler to compute and  used for large sample sizes or when the likelihood surface is approximately symmetric

## could do this in yet another map() call, or a for loop, but things are getting a little complicated ...
sim_res <- map_dfr(seq(nsim), simfun, .id = "sim",
                   .progress = interactive())

sim_res_REML <- map_dfr(seq(nsim), ~simfun(., method = "REML"), .id = "sim",
                        .progress = interactive())

sim_res_comb <- bind_rows(sim_res, sim_res_REML)

saveRDS(sim_res_comb, file = "CI_comparison_full.rds")

get_binCI <- function(dat) {
    get_vals <- function(x) {
        CI <- binom.test(sum(x), length(x))$conf.int
        tibble(est = mean(x), lwr = CI[1], upr = CI[2])
    }
    ## ugly
    with(dat[1,],
         tibble(method, ci_type, term,
                bind_rows(tibble(side = "lwr", get_vals(dat$true_val < dat$lwr)),
                          tibble(side = "upr", get_vals(dat$true_val > dat$upr)))))
}

get_binCI(filter(sim_res_comb, ci_type == "wald", term == "Days", method == "ML"))

summ <- (sim_res_comb
    |> tidyr::drop_na()
    |> split(~ ci_type + term + method)
    |> discard(\(x) nrow(x) == 0)
    |> map_dfr(get_binCI)
)

ggplot(summ, aes(est, interaction(method, term), colour = ci_type)) +
    geom_pointrange(aes(xmin = lwr, xmax = upr), position = position_dodge(width = 0.25)) +
    facet_wrap(~side, ncol = 1) +
    geom_vline(xintercept = 0.025, lty = 2)

#if use wald, the binomial ci ...
#points should be line, but all are above (the 2.5). reml is not converging/available for profile
#maybe profile CIs are okay.
#maybe a little suspicious that all are a little high, but maybe not too suspicious bc CIs cross lines.

#should check with lm

#reml missing: doesn't work for technical reasons
#a priori, don't necessarily expect reml to be better 

#reml vs ml: main diff is in actually how it estimates random effects variances. 
  #analogy: when calc variance from data, calc sum of sqares, and usually divide by n-1 (instead of n), bc you don't know true mean of dist that came from, just your est of the mean. est never equal to true mean
  #plugging in est of mean in calc of variance: don't know mu so calc observed mean instead. can figure out how over confident you are in est compared to mu, so divide by n-1. 
  #basically what reml is doing (calc random effects variances, not assuming we got all the fixed right)
  #ml is equivalent to dividing by N, not n-1, ignoring problem

#code is too clever. competing urges: like to be compact, but also understandable.
#try this with lm example, then don't have ml vs reml. this data reaction is days, leave out subjects. or sim new, simpler data and try lm. this should be basically perfect if do lm


#probably should be using profile CI, but don't need to worry about reml (can use ml). reml intercept actually bigger 


#for the flight data: leave out batid, do sim and fit with lm, should work perfectly


