# Multivariate model with the average school grade and all covariates
# Multivariate model with all school subject grades and all covariates
#	One model for each outcome. Include all school grades (splines) and covariates in the model
#	Define smoothing parameter lambda for each thin plate smoothing spline using generalized cross validation
#	Fit models and save model summaries for later visualization

source("R/dm02_pkg.r")

# Read data ---------------------------------------------------------------

# If simulated data
source("R/dm03_sim_data.R")
fbc <- df_all

# Mutate data -------------------------------------------------------------

# If scaling
fbc_scaled <- fbc %>%
  mutate_at(vars(starts_with("time")), ~ {
    ("2003-05-31" %--% .) / years(1)
  }) %>%
  mutate_at(vars(!!vars_avera, !!vars_grade), scale)

# If no scaling
fbc <-
  fbc %>%
  mutate_at(vars(starts_with("time")), ~ {
    ("2003-05-31" %--% .) / years(1)
  })

# describe the models that need to be fit
model_descriptions <- crossing(
  outcome  = c("psy", "bipo", "dep"),
  grades   = c(vars_grade, vars_avera, "All"),
  adjusted = c(FALSE, TRUE)
)

# needed for splines in model formulae
max_knots <- n_distinct(fbc$Literature) - 1

build_formula <- function(outcome, grades, adjusted, max_knots = 7) {
  f <- str_glue("event_{outcome} ~ offset(log(time_{outcome}))")

  if (adjusted) {
    f <- paste(f, "+", str_flatten(vars_covar, " + "))
  }

  if (grades == "All") {
    grades <- vars_grade
  }

  grades <- str_glue("s({grades}, k = {max_knots})")
  f <- paste(f, "+", str_flatten(grades, " + "))

  as.formula(f)
}

# takes about 3 minutes
# Scaled
model_fits_scaled <- model_descriptions %>%
  mutate(formula = pmap(., build_formula)) %>%
  mutate(fit = map(formula, poisson_gam, data = fbc_scaled))

# write_rds(model_fits, data_path("fits.rds"), compress = "gz")

# non-scaled
model_fits_nonscaled <- model_descriptions %>%
  mutate(formula = pmap(., build_formula)) %>%
  mutate(fit = map(formula, poisson_gam, data = fbc))

# write_rds(model_fits, data_path("fits_non_scaled.rds"), compress = "gz")
