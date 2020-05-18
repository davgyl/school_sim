# Time-to-event data 
#
# Number of subjects at start of follow-up stratified by covariates
# Number of person-years
# Number of events
# Age at event (mean, SD, median, range)

source("R/dm02_pkg.r")

# If simulated data
# NOTE: not resembling real data although same variable names
source("R/dm03_sim_data.R")
fbc <- df_all


# Table 1 -----------------------------------------------------------------

# With original grades (non-scaled)

fbc_tab <-
  fbc %>%
  mutate_at(
    vars(starts_with("time")),
    ~ ((d_start - 1) %--% .) / years(1)
  ) %>%
  mutate(
    all = "all" ,
    Average_int = as.integer(Average),
    Average_round = round(Average, 0),
    Average_c = cut(Average, breaks = c(-Inf, 6, 7, 8, 9, Inf)) ,
    Average_c2 = cut(Average, breaks = c(-Inf, 4:10, Inf)) ,
    Literature_c = cut(Literature, breaks = c(-Inf, 6, 7, 8, 9, Inf)) ,
    Mathematics_c = cut(Mathematics, breaks = c(-Inf, 6, 7, 8, 9, Inf)),
    Phys_Edu_c = cut(Phys_Edu, breaks = c(-Inf, 6, 7, 8, 9, Inf)) ,
    Handicrafts_c = cut(Handicrafts, breaks = c(-Inf, 6, 7, 8, 9, Inf)),
    Arts_c = cut(Arts, breaks = c(-Inf, 6, 7, 8, 9, Inf)),
    Music_c = cut(Music, breaks = c(-Inf, 6, 7, 8, 9, Inf))
  )

fbc_long <-
  fbc_tab %>%
  pivot_longer(
    cols = matches("^(event|time)_"),
    names_sep = "_",
    names_to = c(".value", "outcome")
  )


#' @param x a survfit object
cumulative_incidence <- function(x, at = Inf) {
  broom::tidy(x) %>%
    filter(time <= !!at) %>%
    slice(n()) %>%
    select(estimate, conf.high, conf.low) %>%
    mutate_all(~ (1 - .) * 100)
}

strata <- c("all", vars_covar,
            "Average_c", "Average_c2", "Average_int", "Average_round",
            vars_grade, paste0(vars_grade, "_c"))

table_1_data_nonsc <-
  map_df(strata, ~ {
  fbc_long %>%
    group_by(outcome, chracteristic = .x, value = !!sym(.x)) %>%
    summarize(
      n = n(),
      n_risk = sum(time > 0, na.rm = T),
      pyears = sum(time, na.rm = T),
      events = sum(event, na.rm = T),
      km_fit = list(survfit(Surv(time, event) ~ 1))
    ) %>%
    mutate(ci = map(km_fit, cumulative_incidence)) %>%
    unnest_wider(ci, names_sep = "_") %>%
    mutate(value = as.character(value))
})


# openxlsx::write.xlsx(
#   table_1_data_nonsc %>% select(-km_fit),
#   paste0(
#     "figures/table_1_data_nonsc_",
#     Sys.Date(),
#     ".xlsx"
#   )
# )


# Mean age ----------------------------------------------------------------

fbc_age <-
  fbc %>%
  filter(fas == 1, fas_dg != "none") %>%
  mutate_at(
    vars(starts_with("time")),
    ~ ((dob) %--% .) / years(1)
  ) %>%
  select(
    contains("time"))


summary_age <-
  bind_rows(
  fbc_age %>%
    summarise_all( ~ (mean(., na.rm = T))),
  fbc_age %>%
    summarise_all( ~ (sd(., na.rm = T))),
  fbc_age %>%
    summarise_all( ~ (min(., na.rm = T))),
  fbc_age %>%
    summarise_all( ~ (median(., na.rm = T))),
  fbc_age %>%
    summarise_all( ~ (max(., na.rm = T)))
) %>%
  mutate(
    measure = c(
      "mean",
      "sd",
      "min",
      "median",
      "max"
    )
  )

# openxlsx::write.xlsx(
#   summary_age,
#   paste0(
#     "figures/tab_summary_age_",
#     Sys.Date(),
#     ".xlsx"
#   )
# )
# openxlsx::write.xlsx(
#   summary(fbc_age),
#   paste0(
#     "figures/tab_summary_age2_",
#     Sys.Date(),
#     ".xlsx"
#   )
# )
