% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tip_coef.R
\name{tip_coef}
\alias{tip_coef}
\alias{tip_coef_with_continuous}
\title{Tip a linear model coefficient with a continuous confounder.}
\usage{
tip_coef(
  effect_observed,
  exposure_confounder_effect = NULL,
  confounder_outcome_effect = NULL,
  verbose = getOption("tipr.verbose", TRUE)
)

tip_coef_with_continuous(
  effect_observed,
  exposure_confounder_effect = NULL,
  confounder_outcome_effect = NULL,
  verbose = getOption("tipr.verbose", TRUE)
)
}
\arguments{
\item{effect_observed}{Numeric. Observed exposure - outcome effect from
a regression model. This can be the beta coefficient, the lower
confidence bound of the beta coefficient, or the upper confidence bound
of the beta coefficient.}

\item{exposure_confounder_effect}{Numeric. Estimated scaled mean difference
between the unmeasured confounder in the exposed population and unexposed
population}

\item{confounder_outcome_effect}{Numeric positive value. Estimated relationship
between the unmeasured confounder and the outcome}

\item{verbose}{Logical. Indicates whether to print informative message.
Default: \code{TRUE}}
}
\value{
Data frame.
}
\description{
choose one of the following, and the other will be estimated:
\itemize{
\item \code{exposure_confounder_effect}
\item \code{confounder_outcome_effect}
}
}
\examples{
## to estimate the relationship between an unmeasured confounder and outcome
## needed to tip analysis
tip_coef(1.2, exposure_confounder_effect = -2)

## to estimate the number of unmeasured confounders specified needed to tip
## the analysis
tip_coef(1.2, exposure_confounder_effect = -2, confounder_outcome_effect = -0.05)

## Example with broom
if (requireNamespace("broom", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {
  lm(wt ~ mpg, data = mtcars) \%>\%
   broom::tidy(conf.int = TRUE) \%>\%
   dplyr::filter(term == "mpg") \%>\%
   dplyr::pull(conf.low) \%>\%
   tip_coef(confounder_outcome_effect = 2.5)
}
}
