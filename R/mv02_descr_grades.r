# School grades (descr_grades.r)
#
# Report separately for 1987- and 1997 cohorts
# Histogram of non-scaled measures in total sample and subjects with an event
# Mean, SD, median, range of non-scaled measures in total sample and subjects
# with an event
# Histogram of scaled measures in total sample and subjects with an event
# Mean, SD, median, range of scaled measures in total sample (mean = 0, SD = 1)
# and subjects with an event

source("R/dm02_pkg.r")

# Read data ---------------------------------------------------------------

# If simulated data
source("R/dm03_sim_data.R")
fbc <- df_all

# Scaled vs non-scaled ----------------------------------------------------

fbc_nonsc <-
  fbc %>%
  select(pid, vars_avera, vars_grade)

fbc_sc <-
  fbc %>%
  mutate_at(c(vars_avera, vars_grade), compose(c, scale) )%>%
  select(pid, vars_avera, vars_grade) %>%
  rename_all(funs(paste0(., "_sc")))

nonsc_sc <-
  fbc_nonsc %>%
  left_join(fbc_sc, by = c("pid" = "pid_sc"))

nonsc_sc %>%
  select(contains("Average")) %>%
  distinct() %>%
  arrange(Average) %>%
  filter(Average %in% c(4:10))

fbc_nonsc %>% summary()
fbc_sc %>% summary()

tab_nonsc_sc <-
  bind_cols(
    nonsc_sc %>%
      select(contains("Literature")) %>%
      distinct() %>%
      arrange(Literature) %>%
      na.omit(),
    nonsc_sc %>%
      select(contains("Mathematics")) %>%
      distinct() %>%
      arrange(Mathematics) %>%
      na.omit(),
    nonsc_sc %>%
      select(contains("Phys_Edu")) %>%
      distinct() %>%
      arrange(Phys_Edu) %>%
      na.omit(),
    nonsc_sc %>%
      select(contains("Handicrafts")) %>%
      distinct() %>%
      arrange(Handicrafts) %>%
      na.omit(),
    nonsc_sc %>%
      select(contains("Arts")) %>%
      distinct() %>%
      arrange(Arts) %>%
      na.omit(),
    nonsc_sc %>%
      select(contains("Music")) %>%
      distinct() %>%
      arrange(Music) %>%
      na.omit()
  ) %>%
  rename(
    original_grade = Literature
  ) %>%
  select(original_grade,  contains("_sc")) %>%
  mutate(
    Average_sc = c(
      # NA,
      nonsc_sc %>%
        select(contains("Average")) %>%
        distinct() %>%
        arrange(Average) %>%
        filter(Average %in% c(4:10)) %>%
        pull(Average_sc)
    )
  )

# openxlsx::write.xlsx(
#   tab_nonsc_sc,
#   paste0(
#     "figures/tab_nonsc_sc_",
#     Sys.Date(),
#     ".xlsx"
#   )
# )


# Summary tables of grades ------------------------------------------------

tab_summary_grade <-
  fbc %>%
  select(fas_dg, vars_avera, vars_grade) %>%
  pivot_longer(-fas_dg,names_to = "subject", values_to = "grade") %>%
  group_by(fas_dg, subject) %>%
  summarise(
    n = n(),
    mean = mean(grade),
    sd = sd(grade),
    min = min(grade),
    q1 = quantile(grade, probs = 0.25),
    median = quantile(grade, probs = 0.5),
    q3 = quantile(grade, probs = 0.75),
    max = max(grade)
  ) %>%
  ungroup()

tab_summary_grade %>%
  filter(fas_dg == "none") %>%
  select(subject, mean, sd)

tab_summary_grade_sc <-
  fbc %>%
  select(fas_dg, vars_avera, vars_grade) %>%
  mutate_at(c(vars_avera, vars_grade), compose(c, scale)) %>%
  pivot_longer(-fas_dg,names_to = "subject", values_to = "grade") %>%
  group_by(fas_dg, subject) %>%
  summarise(
    n = n(),
    mean = mean(grade),
    sd = sd(grade),
    min = min(grade),
    q1 = quantile(grade, probs = 0.25),
    median = quantile(grade, probs = 0.5),
    q3 = quantile(grade, probs = 0.75),
    max = max(grade)
  ) %>%
  ungroup() %>%
  mutate(subject = paste("Scaled", subject))

bind_rows(
  tab_summary_grade,
  tab_summary_grade_sc
)

# openxlsx::write.xlsx(
#   bind_rows(
#     tab_summary_grade,
#     tab_summary_grade_sc
#   ),
#   paste0(
#     "figures/tab_summary_grade_",
#     Sys.Date(),
#     ".xlsx"
#   )
# )

# Charts of grades --------------------------------------------------------

subject_labels <-
  tibble(
    Subject = c(
      "Average"     ,
      "Literature"  ,
      "Mathematics" ,
      "Phys_Edu"    ,
      "Handicrafts" ,
      "Arts"        ,
      "Music"       ),
    Subject_labels = c(
      "Average",
      "Native language",
      "Mathematics",
      "Physical Education",
      "Handicrafts",
      "Arts",
      "Music"
    )
  )

fas_dg_labels <-
  tibble(
    fas_dg = c(
      "psy"     ,
      "bipo"  ,
      "dep" ,
      "none"       ),
    Diagnosis = c(
      "Non-affective psychosis",
      "Bipolar disorder",
      "Depression",
      "None"
    )
  )

fill_order <- c("Non-affective psychosis",
                "Bipolar disorder",
                "Depression",
                "None")

facet_order <- c(
  "Native language",
  "Mathematics",
  "Physical Education",
  "Handicrafts",
  "Arts",
  "Music"
)

# fbc %>%
#   select(Average) %>%
#   summary()
#
# fbc %>%
#   select(Average) %>%
#   filter(Average <= 5) %>%
#   count() # 11
#
# fbc %>%
#   mutate(Average = cut(Average, breaks = c(4:10))) %>%
#   count(Average) # 11 have 5 or smaller ("(4,5]")

plot_bar_av <-
  fbc %>%
  select(vars_avera, fas_dg) %>%
  mutate(Average = cut(Average, breaks = c(4:10))) %>%
  gather(Subject, Grade, -fas_dg) %>%
  left_join(subject_labels) %>%
  left_join(fas_dg_labels) %>%
  group_by(Subject_labels, Grade, Diagnosis) %>%
  count() %>%
  group_by(Subject_labels, Diagnosis) %>%
  filter(!is.na(Grade)) %>%
  mutate(
    total = sum(n),
    Percent = n/total*100
  ) %>%
  ungroup() %>%
  mutate(Diagnosis = factor(Diagnosis, levels = fill_order)) %>%
  ggplot(aes(x = Grade, y = Percent, fill = Diagnosis)) +
  facet_wrap( ~ Subject_labels, ncol = 2) +
  geom_bar(stat="identity", position = "dodge") +
  scale_x_discrete(labels = c(
    "4-5",
    ">5-6",
    ">6-7",
    ">7-8",
    ">8-9",
    ">9"
  )) +
  theme_minimal()

plot_bar_av
# ggsave("figures/plot_bar_av.pdf", h = 3, w = 6, s = 1.8)

plot_bar_spec <-
  fbc %>%
  select(vars_grade, fas_dg) %>%
  gather(Subject, Grade, -fas_dg) %>%
  left_join(subject_labels) %>%
  left_join(fas_dg_labels) %>%
  group_by(Subject_labels, Grade, Diagnosis) %>%
  count() %>%
  group_by(Subject_labels, Diagnosis) %>%
  filter(!is.na(Grade)) %>%
  mutate(
    total = sum(n),
    Percent = n/total*100
  ) %>%
  ungroup() %>%
  mutate(Diagnosis = factor(Diagnosis, levels = fill_order)) %>%
  mutate(Subject_labels = factor(Subject_labels, levels = facet_order)) %>%
  ggplot(aes(x = Grade, y = Percent, fill = Diagnosis)) +
  facet_wrap( ~ Subject_labels, ncol = 2) +
  geom_bar(stat="identity", position = "dodge") +
  theme_minimal()

plot_bar_spec
# ggsave("figures/plot_bar_spec.pdf", h = 7, w = 6, s = 1.8)


# Correlation -------------------------------------------------------------

# Modifed example: http://padamson.github.io/r/ggally/ggplot2/ggpairs/2016/02/16/multiple-regression-lines-with-ggpairs.html
pair_fn <- 
  function(data, mapping, ...) {
    p <- ggplot(data = data, mapping = mapping) +
      geom_jitter(size = 0.5, alpha = 0.05) + 
      geom_smooth(method=gam, ...
                  # , fill="red", color="red", ...
                  )
      # geom_smooth(method=lm, fill="blue", color="blue", ...) 
    p
  }

fbc %>% 
  select(fas_dg, vars_avera, vars_grade) %>% 
  # sample_n(1000) %>% # Sample for quick testing
  mutate(fas_dg = case_when(
    fas_dg == "none" ~ "None", 
    fas_dg == "psy" ~ "Psychosis",
    fas_dg == "bipo" ~ "Bipolar",
    fas_dg == "dep" ~ "Depression"
  )) %>% 
  GGally::ggpairs(
    # columns = 2:4, # few columns for quick testing
    columns = 2:ncol(.),
    mapping = ggplot2::aes(color = fas_dg),
    lower = list(continuous = pair_fn), 
    diag = list(continuous = wrap("densityDiag", adjust = 4, alpha = 0.4))
  ) +
  theme_minimal()

