library(glmmTMB)
library(broom.mixed)
library(dplyr)
library(purrr)
library(ggplot2); theme_set(theme_bw())

## could use lower conf.level, would require fewer sims, but I'm concerned
## that it would also lessen the observed phenomenon
nsim <- 500
set.seed(101)

data("sleepstudy", package = "lme4")
m1 <- glmmTMB(Reaction ~ Days + (Days | Subject), sleepstudy)
m2 <- update(m1, REML = TRUE)

## we'll treat the fitted values as *true* values for simulations
true_vals <- tidy(m1, effects = "fixed") |> select(term, true_val = estimate)



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

## could do this in yet another map() call, or a for loop, but things are getting a little complicated ...
sim_res <- map_dfr(seq(nsim), simfun, .id = "sim",
                   .progress = interactive())

sim_res_REML <- map_dfr(seq(nsim), ~simfun(., method = "REML"), .id = "sim",
                        .progress = interactive())

sim_res_comb <- bind_rows(sim_res, sim_res_REML)

saveRDS(sim_res_comb.rds, file = "CI_comparison_full.rds")

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
