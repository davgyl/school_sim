# Time-to-event data 
#
# Number of subjects at start of follow-up stratified by covariates
# Number of person-years
# Number of events
# Age at event (mean, SD, median, range)

source("R/dm02_pkg.r")

# If simulated data
source("R/dm03_sim_data.R")
# fbc <- df_all # If with other colnames
fbc <- df_tot


# Table 1 -----------------------------------------------------------------

# Scaled grades
fbc_tab <-
  fbc %>%
  mutate_at(c(vars_avera, vars_grade), compose(c, scale)) %>%
  mutate_at(
    vars(starts_with("time")),
    ~ ((d_start - 1) %--% .) / years(1)
  ) %>%
  mutate(
    all = "all",
    
    # For simulation data
    Average_c = cut(Average, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    
    subject_1_c = cut(subject_1, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F) ,
    subject_2_c = cut(subject_2, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    subject_3_c = cut(subject_3, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F) ,
    subject_4_c = cut(subject_4, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    subject_5_c = cut(subject_5, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    subject_6_c = cut(subject_6, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    
    Average_c2 = cut(Average, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F),
    
    subject_1_c2 = cut(subject_1, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F) ,
    subject_2_c2 = cut(subject_2, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F),
    subject_3_c2 = cut(subject_3, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F) ,
    subject_4_c2 = cut(subject_4, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F),
    subject_5_c2 = cut(subject_5, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F),
    subject_6_c2 = cut(subject_6, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F)
    
    
    # For real data
    # Literature_c = cut(Literature, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F) ,
    # Mathematics_c = cut(Mathematics, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    # Phys_Edu_c = cut(Phys_Edu, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F) ,
    # Handicrafts_c = cut(Handicrafts, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    # Arts_c = cut(Arts, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    # Music_c = cut(Music, breaks = c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf), right = F),
    # 
    # Average_c2 = cut(Average, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F),
    # Literature_c2 = cut(Literature, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F) ,
    # Mathematics_c2 = cut(Mathematics, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F),
    # Phys_Edu_c2 = cut(Phys_Edu, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F) ,
    # Handicrafts_c2 = cut(Handicrafts, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F),
    # Arts_c2 = cut(Arts, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F),
    # Music_c2 = cut(Music, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), right = F)
  )

strata <- c("all", vars_covar,
            "Average_c",
            paste0(vars_grade, "_c"),
            "Average_c2",
            paste0(vars_grade, "_c2"))

# With original grades (non-scaled)

# fbc_tab_orig <-
#   fbc %>%
#   mutate_at(
#     vars(starts_with("time")),
#     ~ ((d_start - 1) %--% .) / years(1)
#   ) %>%
#   mutate(
#     all = "all" ,
#     Average_int = as.integer(Average),
#     Average_round = round(Average, 0),
#     Average_c = cut(Average, breaks = c(-Inf, 6, 7, 8, 9, Inf)) ,
#     Average_c2 = cut(Average, breaks = c(-Inf, 4:10, Inf)) ,
#     Literature_c = cut(Literature, breaks = c(-Inf, 6, 7, 8, 9, Inf)) ,
#     Mathematics_c = cut(Mathematics, breaks = c(-Inf, 6, 7, 8, 9, Inf)),
#     Phys_Edu_c = cut(Phys_Edu, breaks = c(-Inf, 6, 7, 8, 9, Inf)) ,
#     Handicrafts_c = cut(Handicrafts, breaks = c(-Inf, 6, 7, 8, 9, Inf)),
#     Arts_c = cut(Arts, breaks = c(-Inf, 6, 7, 8, 9, Inf)),
#     Music_c = cut(Music, breaks = c(-Inf, 6, 7, 8, 9, Inf))
#   )
# 
# strata <- c("all", 
#             vars_covar,
#             "Average_c", 
#             "Average_c2", 
#             "Average_int",
#             "Average_round",
#             vars_grade, 
#             paste0(vars_grade, "_c"))

# Same for scaled and non-scaled
fbc_long <-
  fbc_tab %>% # or fbc_tab_orig if non-scaled
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


table_1_data <-
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
#   table_1_data %>% select(-km_fit),
#   paste0(
#     "figures/table_1_data_",
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
