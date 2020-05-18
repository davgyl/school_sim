
source("R/dm02_pkg.R")

# README ------------------------------------------------------------------

# pid = id-number. In simulation those dg1 start with 1,
# those with dg2 start with 2 etc. and those without start with 4

# subject_1 ... subject_2 = correlated grades

# covar_1 ... covar_6 = correlated covariates

# time_dg1 ... time_dg3 = time to be censored for dg1 ... dg3

# event_dg1 ... event_dg3 = event for dg1 ... dg3

# NOTE: Rename variables to same in real data although the cumulative incidence and associations are different from the real data.

# Simulate total data and add survival outcomes ---------------------------

n <- 60000

par1 <- 2.5 
par2 <- 2.8 
par3 <- 2.3 
par4 <- 2.5 
par5 <- 2.5 
par6 <- 2.5 

set.seed(100)

df_tot <-
  
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
      params1 = seq(0.03, 0.45, length.out = 6), 
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
    pid = id + 1e6,  
    
    # start of follow-up
    d_start = as.Date("2003-05-31")
  
  )

# Utlize and modify function in example of cox.ph {mgcv}
ph.weibull.sim <- function(eta, gamma=0.5, h0=1e-3, t1=12.5*365.25) { 
  lambda <- h0*exp(eta)
  n <- length(eta)
  U <- runif(n)
  t <- (-log(U)/lambda)^(1/gamma)
  d <- as.numeric(t <= t1)
  t[!d] <- t1
  list(t=t,d=d)
}

# dg1
f0 <- function(x) -0.5*x
f1 <- function(x) -0.5*x
f2 <- function(x) -0.3*x + 0.03*x^3
f3 <- function(x) 0.6*x
f4 <- function(x) 0.3*x
f <- 
  f0(df_tot$subject_1) + 
  f1(df_tot$subject_2) + 
  f2(df_tot$subject_3) +
  f3(df_tot$covar_1) + 
  f4(df_tot$covar_2) 
  
g <- (f-mean(f))/5
ssurv_1 <- ph.weibull.sim(g)
# ssurv_1 %>% as_tibble() %>% mutate(t = round(t, 1))
# ssurv_1 %>% as_tibble() %>% count(d)

# dg2
f0 <- function(x) -0.4*x
f1 <- function(x) -0.4*x
f2 <- function(x) -0.3*x + 0.02*x^3
f3 <- function(x) 0.5*x
f4 <- function(x) 0.3*x
f <- 
  f0(df_tot$subject_1) + 
  f1(df_tot$subject_2) + 
  f2(df_tot$subject_3) +
  f3(df_tot$covar_1) + 
  f4(df_tot$covar_2) 

g <- (f-mean(f))/5
ssurv_2 <- ph.weibull.sim(g)
# ssurv_2 %>% as_tibble() %>% count(d)

# dg3
f0 <- function(x) -0.4*x
f1 <- function(x) -0.4*x
f2 <- function(x) -0.2*x + 0.01*x^3
f3 <- function(x) 0.4*x
f4 <- function(x) 0.3*x
f <- 
  f0(df_tot$subject_1) + 
  f1(df_tot$subject_2) + 
  f2(df_tot$subject_3) +
  f3(df_tot$covar_1) + 
  f4(df_tot$covar_2) 

g <- (f-mean(f))/5
ssurv_3 <- ph.weibull.sim(g)
# ssurv_3 %>% as_tibble() %>% count(d)

df_all <- 
  bind_cols(
    df_tot, 
    ssurv_1 %>% 
      as_tibble() %>% 
      rename(
        time_dg1 = t, 
        event_dg1 = d
      ), 
    ssurv_2 %>% 
      as_tibble() %>% 
      rename(
        time_dg2 = t, 
        event_dg2 = d
      ), 
    ssurv_3 %>% 
      as_tibble() %>% 
      rename(
        time_dg3 = t, 
        event_dg3 = d
      )
  ) %>% 
  mutate(
    time_dg1 = d_start + time_dg1, 
    time_dg2 = d_start + time_dg2, 
    time_dg3 = d_start + time_dg3, 
    
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
    fas = 1,
    
    # Average = (
    #   subject_1 + subject_2 + subject_3 + subject_4 + subject_5 + subject_6
    #   )/6
  ) %>% 
  select(-id)
 

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

