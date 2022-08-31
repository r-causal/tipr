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

## Risk ratio ----

set.seed(930)

exdata_rr <- tibble(
  .unmeasured_confounder = c(rnorm(n), rnorm(n, 0.5)),
  measured_confounder = c(rnorm(n), rnorm(n, 0.5)),
  exposure = rep(c(0, 1), each = n),
  outcome = rbinom(n * 2, 1,
            pmin(exp((-4 + measured_confounder + .unmeasured_confounder)), 1)
  )
)

sum(exdata_rr$outcome)

glm(outcome ~ exposure + measured_confounder, data = exdata_rr, family = poisson(link = "log"))
glm(outcome ~ exposure + measured_confounder + .unmeasured_confounder, data = exdata_rr, family = poisson(link = "log"))

usethis::use_data(exdata_rr, overwrite = TRUE)

