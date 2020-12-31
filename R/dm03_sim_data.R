
# Read previous scripts
# source("R/dm02_pkg.R")

# README ------------------------------------------------------------------

# pid = id-number. In simulation those dg1 start with 1,
# those with dg2 start with 2 etc. and those without start with 4

# subject_1 ... subject_2 = correlated grades

# covar_1 ... covar_6 = correlated covariates

# time_dg1 ... time_dg3 = time to be censored for dg1 ... dg3

# event_dg1 ... event_dg3 = event for dg1 ... dg3

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

df_tot <- 
  df_tot %>% 
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
    subject_6 = subject_6 + 9
  ) %>% 
  select(-id)


df_tot_long <-
  df_tot %>%
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

df_tot <-
  df_tot %>%
  select(-contains("subject")) %>%
  left_join(
    df_tot_long %>%
      pivot_wider( # wide again
        names_from = subject,
        values_from = grade
      ), 
    by = "pid"
  ) %>%
  mutate(
    # Average grade
    Average = (
      subject_1 + subject_2 + subject_3 + subject_4 + subject_5 + subject_6
    ) / 6
  ) %>%
  select(pid, 
         # fas_dg, 
         d_start,
         # starts_with("time"),
         # starts_with("event"),
         Average,
         everything())



# Outcomes ----------------------------------------------------------------

x <- df_tot %>% 
  select(-d_start) %>% 
  as.matrix()

# Add intercept and effects
b_dg1 <- 
  # 5.3 + 
  4.1 +
  0.97*x[,  "covar_2"] +
  0.37*x[,  "covar_4"] +
  0.30*x[,  "covar_5"] +
  0.23*x[,  "covar_6"] +
  -0.36*x[,  "subject_3"] +
  -0.33*x[,  "subject_4"] +
  -1.22*x[,  "subject_5"] +
  0.085*x[,  "subject_5"]^2

b_dg2 <- 
  # 0.68 +
  # -36 +
  # 1 +
  -3 +
  0.74*x[,  "covar_1"] +
  0.72*x[,  "covar_4"] +
  0.21*x[,  "covar_5"] +
  -0.55*x[,  "covar_6"] +
  0.17*x[,  "subject_1"] +
  -0.14*x[,  "subject_2"] +
  -0.35*x[,  "subject_3"] +
  -0.26*x[,  "subject_4"] +
  -0.56*x[,  "subject_5"] +
  0.1*x[,  "subject_5"]^2
# 0.044*x[,  "subject_5"]^2
# 0.44*x[,  "subject_5"]^2

b_dg3 <- 
  # 3.11 +
  2.4 +
  0.67*x[,  "covar_4"] +
  0.16*x[,  "covar_5"] +
  -0.67*x[,  "covar_6"] +
  0.08*x[,  "subject_3"] +
  -0.09*x[,  "subject_3"] +
  -0.38*x[,  "subject_3"] +
  -0.14*x[,  "subject_4"] +
  -0.52*x[,  "subject_5"] +
  0.04*x[,  "subject_5"]^2

# Inverse logit function
prob_dg1 = 1/(1 + exp(-b_dg1)) 
prob_dg2 = 1/(1 + exp(-b_dg2)) 
prob_dg3 = 1/(1 + exp(-b_dg3)) 

set.seed(100)

event_dg1 = rbinom(n, 1, prob_dg1)
event_dg2 = rbinom(n, 1, prob_dg2)
event_dg3 = rbinom(n, 1, prob_dg3)

table(event_dg1)
table(event_dg2)
table(event_dg3)

df_tot <- 
  bind_cols(
    df_tot, 
    event_dg1 = event_dg1,
    event_dg2 = event_dg2,
    event_dg3 = event_dg3
  )

df_tot <-
  df_tot %>%
  mutate(
    time_dg1 = case_when(
      event_dg1 == 0 ~ d_start + runif(nrow(.), 11.5*365.25, 12.5*365.25),
      event_dg1 == 1 ~ d_start + runif(nrow(.), 1, 12.5*365.25)
    ),
    time_dg2 = case_when(
      event_dg2 == 0 ~ d_start + runif(nrow(.), 11.5*365.25, 12.5*365.25),
      event_dg2 == 1 ~ d_start + runif(nrow(.), 1, 12.5*365.25)
    ),
    time_dg3 = case_when(
      event_dg3 == 0 ~ d_start + runif(nrow(.), 11.5*365.25, 12.5*365.25),
      event_dg3 == 1 ~ d_start + runif(nrow(.), 1, 12.5*365.25)
    )
  )

# Other variables ---------------------------------------------------------

df_tot <- 
  df_tot %>% 
  mutate(
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
  )

# Write over vars_grade (in utils)
vars_grade <-
  df_tot %>%
  select(contains("subject")) %>%
  colnames()

vars_covar <-
  df_tot %>%
  select(contains("covar")) %>%
  colnames()
