
source("R/dm02_pkg.R")


# README ------------------------------------------------------------------

# pid = id-number. In simulation those dg1 start with 1,
# those with dg2 start with 2 etc. and those without start with 4

# subject_1 ... subject_2 = correlated grades

# covar_1 ... covar_6 = correlated covariates

# time_dg1 ... time_dg3 = time to be censored for dg1 ... dg3

# event_dg1 ... event_dg3 = event for dg1 ... dg3

# Do first data for dg1, dg2 and dg3 and fainlly for those without diagnoses

# Simulate data for dg1 ---------------------------------------------------

n <- 600

par1 <- 2.5 # As all other
par2 <- 2.8 # As all other
par3 <- 2.7 # Unique (low grades over-represented)
par4 <- 2.5 # As all other
par5 <- 2.4 # As all other diagnoses (high grades over-represented)
par6 <- 2.5 # As all other

set.seed(100)

df_dg1 <-

  bind_cols(

    # simulate grades of specific subjects
    simstudy::genCorGen(
      n,
      nvars = 6,
      params1 = c(par1, par2, par3, par4, par5, par6),
      dist = "poisson",
      rho = 0.5,
      corstr = "cs",
      wide = TRUE,
      cnames = paste0("subject_", 1:6)
    ),

    # simulate covariates
    simstudy::genCorGen(
      n,
      nvars = 6,
      params1 = seq(0.07, 0.5, length.out = 6),
      dist = "binary",
      rho = 0.2,
      corstr = "cs",
      wide = TRUE,
      cnames = paste0("covar_", 1:6)
    ) %>%
      select(-id)

  ) %>%
  as_tibble() %>%
  mutate(

    # pid
    pid = id + 1e6,  # mark to start with 1

    # start of follow-up
    d_start = as.Date("2003-05-31"),

    # time to dg and event of dg
    time_dg1 = d_start + runif(n, 0, 12.5*365.25), # between 0 and 12.5 years
    event_dg1 = 1 %>% rep(n) # event == 1

  )

# Simulate data for dg2 ---------------------------------------------------

n <- 400

par1 <- 2.5 # As all other
par2 <- 2.8 # As all other
par3 <- 2.6 # As for dg3 (low grades over-represented but not as low for dg1)
par4 <- 2.5 # As all other
par5 <- 2.4 # As all other diagnoses (high grades over-represented)
par6 <- 2.5 # As all other

set.seed(100)

df_dg2 <-

  bind_cols(

    # simulate grades of specific subjects
    simstudy::genCorGen(
      n,
      nvars = 6,
      params1 = c(par1, par2, par3, par4, par5, par6),
      dist = "poisson",
      rho = 0.5,
      corstr = "cs",
      wide = TRUE,
      cnames = paste0("subject_", 1:6)
    ),

    # simulate covariates
    simstudy::genCorGen(
      n,
      nvars = 6,
      params1 = seq(0.07, 0.5, length.out = 6), # less common than among dg1
      dist = "binary",
      rho = 0.2,
      corstr = "cs",
      wide = TRUE,
      cnames = paste0("covar_", 1:6)
    ) %>%
      select(-id)

  ) %>%
  as_tibble() %>%
  mutate(

    # pid
    pid = id + 2e6,  # mark to start with 2

    # start of follow-up
    d_start = as.Date("2003-05-31"),

    # time to dg and event of dg

    time_dg2 = d_start + runif(n, 0, 12.5*365.25), # between 0 and 12.5 years
    event_dg2 = 1 %>% rep(n) # event == 1

  )


# Simulate data for dg3 ---------------------------------------------------

n <- 2500

par1 <- 2.5 # As all other
par2 <- 2.8 # As all other
par3 <- 2.6 # As for dg2 (low grades over-represented but not as low for dg1)
par4 <- 2.5 # As all other
par5 <- 2.4 # As all other diagnoses (high grades over-represented)
par6 <- 2.5 # As all other

set.seed(100)

df_dg3 <-

  bind_cols(

    # simulate grades of specific subjects
    simstudy::genCorGen(
      n,
      nvars = 6,
      params1 = c(par1, par2, par3, par4, par5, par6),
      dist = "poisson",
      rho = 0.5,
      corstr = "cs",
      wide = TRUE,
      cnames = paste0("subject_", 1:6)
    ),

    # simulate covariates
    simstudy::genCorGen(
      n,
      nvars = 6,
      params1 = seq(0.05, 0.5, length.out = 6), # less common than among dg1
      dist = "binary",
      rho = 0.2,
      corstr = "cs",
      wide = TRUE,
      cnames = paste0("covar_", 1:6)
    ) %>%
      select(-id)

  ) %>%
  as_tibble() %>%
  mutate(

    # pid
    pid = id + 3e6,  # mark to start with 3

    # start of follow-up
    d_start = as.Date("2003-05-31"),

    # time to dg and event of dg

    time_dg3 = d_start + runif(n, 0, 12.5*365.25), # between 0 and 12.5 years
    event_dg3 = 1 %>% rep(n) # event == 1

  )

# Simulate data for those without diagnoses -------------------------------

n <- 56500

par1 <- 2.5 # As all other
par2 <- 2.8 # As all other
par3 <- 2.3 # Higher than for diagnoses
par4 <- 2.5 # As all other
par5 <- 2.5 # Lower than for diagnoses
par6 <- 2.5 # As all other

set.seed(100)

df_dg0 <-

  bind_cols(

    # simulate grades of specific subjects
    simstudy::genCorGen(
      n,
      nvars = 6,
      params1 = c(par1, par2, par3, par4, par5, par6),
      dist = "poisson",
      rho = 0.5,
      corstr = "cs",
      wide = TRUE,
      cnames = paste0("subject_", 1:6)
    ),

    # simulate covariates
    simstudy::genCorGen(
      n,
      nvars = 6,
      params1 = seq(0.03, 0.45, length.out = 6), # less common than among those with dg
      dist = "binary",
      rho = 0.2,
      corstr = "cs",
      wide = TRUE,
      cnames = paste0("covar_", 1:6)
    ) %>%
      select(-id)

  ) %>%
  as_tibble() %>%
  mutate(

    # pid
    pid = id + 4e6,  # mark to start with 4

    # start of follow-up
    d_start = as.Date("2003-05-31"),

    # time to dg and event of dg
    time_dg1 = d_start + runif(n, 11.5*365.25, 12.5*365.25), # between 11.5and 12.5 years
    event_dg1 = 0 %>% rep(n), # none

    time_dg2 = d_start + runif(n, 11.5*365.25, 12.5*365.25),
    event_dg2 = 0 %>% rep(n),

    time_dg3 = d_start + runif(n, 11.5*365.25, 12.5*365.25),
    event_dg3 = 0 %>% rep(n)

  )


# Combine and mutate data -------------------------------------------------

# df_dg0 %>% summary()
# df_dg1 %>% summary()
# df_dg2 %>% summary()
# df_dg3 %>% summary()

df_all <-
  bind_rows(
    df_dg1,
    df_dg2,
    df_dg3,
    df_dg0
  ) %>%
  mutate(
    subject_1 = 1-subject_1,  # inverse the tail of the distribution
    subject_2 = 1-subject_2,
    subject_3 = 1-subject_3,
    subject_4 = 1-subject_4,
    subject_5 = 1-subject_5,
    subject_6 = 1-subject_6,
    subject_1 = subject_1 + 9,
    subject_2 = subject_2 + 9,
    subject_3 = subject_3 + 9,
    subject_4 = subject_4 + 9,
    subject_5 = subject_5 + 9,
    subject_6 = subject_6 + 9,

    # full analysis sample diagnosis
    fas_dg = case_when(
      event_dg1 == 1 ~ "dg1",
      event_dg2 == 1 ~ "dg2",
      event_dg3 == 1 ~ "dg3",
      T ~ "none"
    ),

    # date of birth
    dob = as.Date("1987-01-01") + runif(nrow(.), 0, 365),

    # full analysis sample
    fas = 1

  ) %>%
  select(-id)

# df_all %>% summary()
# df_all %>% slice(1:5) %>% view()




# Mutate grades <4 and >10 ------------------------------------------------

# Inspect by plotting
# df_all %>%
#   pivot_longer(
#     contains("subject"),
#     names_to = "subject",
#     values_to = "grade"
#   ) %>%
#   count(subject, grade) %>%
#   ggplot(aes(x = grade, y = n)) +
#   facet_wrap("subject", nrow = 2) +
#   geom_bar(stat = "identity")


df_all_long <-
  df_all %>%
  select(pid, contains("subject")) %>%
  pivot_longer(
    contains("subject"),
    names_to = "subject",
    values_to = "grade"
  ) %>%
  mutate(
    grade = case_when(
      grade > 10 ~ 10,
      grade < 4 ~ 4,
      T ~ grade
    )
  )

df_all <-
  df_all %>%
  select(-contains("subject")) %>%
  left_join(
    df_all_long %>%
      pivot_wider( # wide again
        names_from = subject,
        values_from = grade
      )
  ) %>%
  mutate(
    # Average grade
    Average = (
      subject_1 + subject_2 + subject_3 + subject_4 + subject_5 + subject_6
      ) / 6
  ) %>%
  select(pid, fas_dg, d_start,
         starts_with("time"),
         starts_with("event"),
         Average,
         everything())




# Rename variables to same as in real data --------------------------------

df_all <-
  df_all %>%
  rename(
    Literature  = subject_1,
    Mathematics = subject_2,
    Phys_Edu    = subject_3,
    Handicrafts = subject_4,
    Arts        = subject_5,
    Music       = subject_6,
    time_psy    = time_dg1,
    time_bipo   = time_dg2,
    time_dep    = time_dg3,
    event_psy    = event_dg1,
    event_bipo   = event_dg2,
    event_dep    = event_dg3
  ) %>%
  mutate(
    fas_dg = case_when( # dg during follow-up of fas
      event_psy == 1 ~ "psy",
      event_bipo == 1 ~ "bipo",
      event_dep == 1 ~ "dep",
      T ~ "none"
    )
  )

# Write over vars_covar (in utils)
vars_covar <-
  df_all %>%
  select(contains("covar")) %>%
  colnames()
