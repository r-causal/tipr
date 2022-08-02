library(tidyverse)

set.seed(930)

n <- 1000

## Continuous outcome ----
exdata_continuous <- tibble(
  .unmeasured_confounder = c(rnorm(n), rnorm(n, 0.5)),
  measured_confounder = c(rnorm(n), rnorm(n, 0.5)),
  exposure = rep(c(0, 1), each = n),
  outcome = measured_confounder + .unmeasured_confounder + rnorm(n * 2)
)


lm(outcome ~ exposure + measured_confounder, data = exdata_continuous)
exdata_continuous %>%
  group_by(exposure) %>%
  summarise(m = mean(.unmeasured_confounder)) %>%
  pivot_wider(names_from = exposure,
              values_from = m,
              names_prefix = "x_") %>%
  summarise(estimate = x_1 - x_0)

usethis::use_data(exdata_continuous)

## Relative risk ----

exdata_rr <- tibble(
  .unmeasured_confounder = c(rnorm(n), rnorm(n, 0.5)),
  measured_confounder = c(rnorm(n), rnorm(n, 0.5)),
  exposure = rep(c(0, 1), each = n),
  outcome = log(100 + measured_confounder + .unmeasured_confounder + rnorm(n * 2))
)


# effect_observed
#lm(y ~ t + z)
# outcome_confounder_effect = outcome_association
#lm(y ~ t + z + u)

# confounder_exposure_effect = smd
# difference in means between exposure groups
#lm(u ~ t + z)

#confounder_exposed_prev
#confounder_unexposed_prev
