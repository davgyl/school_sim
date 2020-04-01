
# Utils -------------------------------------------------------------------

# Loading packages
# function from https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Checking that no pids are duplicated
distinct_pids <-
  function(data = data, pid = pid) {
    data %>%
      distinct(pid) %>%
      count() %>%
      pull()
  }

all_pids_distinct <-
  function(data = data, pid = pid) {
    nrow(data) ==
      distinct_pids(data = data, pid = pid)
  }


# Calculate contrasts for points given in nd, compared to first row

# crit can be set to default to 1.96 i.e. significance level at 0.05
# To change alpha -> crit = qnorm(1-(0.05/NUMBER_TESTS/2))

contrast <- function(nd, model, crit = qnorm(1-(0.05/21/2))) {
  lp <- predict(model, nd, type = "lpmatrix")
  d <- sweep(lp, 2, lp[1, ])

  estimate <- d %*% coef(model)
  se <- sqrt(diag(d %*% vcov(model) %*% t(d)))

  est_ci <- estimate %*% rep(1, 3) + tcrossprod(se, crit * c(0, -1, 1))
  colnames(est_ci) <- c("estimate", "conf.low", "conf.high")

  cbind(nd, model$family$linkinv(est_ci))
}

poisson_gam <- function(formula, data = fbc) {
  expr <- rlang::expr(gam(!!formula, family = poisson(),
                          data = data, method = "GCV.Cp"))
  rlang::eval_tidy(expr)
}

# Vectors containing variable names

vars_avera <- "Average"
vars_grade <- c("Literature",
                "Mathematics",
                "Phys_Edu",
                "Music",
                "Arts",
                "Handicrafts")
