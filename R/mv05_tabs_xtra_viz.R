library(tidyverse)
library(openxlsx)
library(ggsci)

path <- file.path("")


# Descriptive table -------------------------------------------------------


raw_table <- "table_1_data_scaled_DATE"

df <- 
  read.xlsx(
    file.path(
      path, paste0(raw_table, ".xlsx")
    )
  ) %>% as_tibble()

df

df %>% 
  count(value) 

df <-
  df %>% 
  mutate_at(
    vars(pyears, ci_estimate, ci_conf.high, ci_conf.low),
    round, 2
  ) %>% 
  mutate(
    ci = paste0(
      ci_estimate, " (", ci_conf.high, " - ", ci_conf.low, ")"
    ), 
    n_cuminc = paste0(
      events, " (", ci_estimate, ")"
    ) 
  ) 

table_edited <- 
  
  df %>% 
  select(chracteristic, value, n) %>% 
  distinct() %>% 
  
  left_join(
    df %>% 
      select(outcome, pyears, chracteristic, value) %>% 
      distinct() %>% 
      spread(outcome, pyears) %>% 
      rename(
        psy_pyears = psy, 
        bipo_pyears = bipo,
        dep_pyears = dep
      )
  ) %>% 
  
  left_join(
    df %>% 
      select(outcome, events, chracteristic, value) %>% 
      distinct() %>% 
      spread(outcome, events) %>% 
      rename(
        psy_events = psy, 
        bipo_events = bipo,
        dep_events = dep
      )
  ) %>% 
  
  left_join(
    df %>% 
      select(outcome, ci, chracteristic, value) %>% 
      distinct() %>% 
      spread(outcome, ci) %>% 
      rename(
        psy_ci = psy, 
        bipo_ci = bipo,
        dep_ci = dep
      )
  ) %>% 

  left_join(
    df %>% 
      select(outcome, n_cuminc, chracteristic, value) %>% 
      distinct() %>% 
      spread(outcome, n_cuminc) %>% 
      rename(
        psy_n_cuminc = psy, 
        bipo_n_cuminc = bipo,
        dep_n_cuminc = dep
      )
  ) %>% 
  
  select(
    chracteristic, value, n,
    contains("psy"), contains("bipo"), contains("dep")
  )

write.xlsx(table_edited, 
           file.path(
             path, 
             paste0(
               raw_table, 
               "_", 
               Sys.Date(),
               "_edited.xlsx"
               )
           )
           )


# Plot cumulative incidence -----------------------------------------------

raw_table <- "table_1_data_scaled_DATE"

df <- 
  read.xlsx(
    file.path(
      path, paste0(raw_table, ".xlsx")
    )
  ) %>% as_tibble()

df %>% 
  filter(chracteristic %>% str_detect("_c2")) %>% 
  count(value)

df %>% 
  filter(chracteristic %>% str_detect("_c2")) %>% 
  count(chracteristic)

outcome_labels <- c(
  psy  = "Non-affective psychosis",
  bipo = "Bipolar disorder",
  dep  = "Depression"
)

grades_labels <- c(
  Average_c2         = "Average",
  Literature_c2      = "Native Language",
  Mathematics_c2     = "Mathematics",
  Phys_Edu_c2        = "Physical Education",
  Handicrafts_c2     = "Handicrafts",
  Arts_c2            = "Arts",
  Music_c2           = "Music"
)

value_labels <- c(
  "[-Inf,-1.5)" = "< -1.5",
  "[-1.5,-0.5)" = ">= -1.5 to -0.5",
  "[-0.5,0.5)"  = ">= -0.5 to 0.5",
  "[0.5,1.5)"   = ">= 0.5 to 1.5",
  "[1.5, Inf)"  = ">= 1.5"
)

df_plot <- 
  df %>% 
  filter(chracteristic %>% str_detect("_c2")) %>% 
  mutate(
    value_labels = value_labels[value], 
    outcome_label = outcome_labels[outcome],
    grades_label = grades_labels[chracteristic]) %>% 
  mutate(
    outcome_label = fct_relevel(outcome_label, "Non-affective psychosis"),
    value_labels = fct_relevel(value_labels,
                               "< -1.5",
                               ">= -1.5 to -0.5",
                               ">= -0.5 to 0.5",
                               ">= 0.5 to 1.5",
                               ">= 1.5"
                               ),
    grades_label = fct_relevel(grades_label,
                               "Average",
                               "Native Language",
                               "Mathematics",
                               "Physical Education",
                               "Handicrafts",
                               "Arts",
                               "Music"
    )
  )
  
plot_ci_ave <- 
  df_plot %>% 
  filter(chracteristic == "Average_c2") %>%
  # filter(chracteristic != "Average_c2") %>%
  ggplot(aes(
    x = value_labels, y = ci_estimate, group = outcome_label, color = outcome_label)) +
  facet_wrap(. ~ grades_label, ncol = 3) +
  geom_point(shape = 15, size = 3) +
  # geom_errorbar(aes(ymin = ci_conf.low, ymax = ci_conf.high), width = 0.2) +
  geom_line(size = 1) +
  scale_colour_manual(
  "Outcome",
  aesthetics = c("colour", "fill"),
  # values = c("#6b778d", "#ff6768", "#17223b"),
  values = c("#DE9146", "#00A0D5", "#7F796B"),
  labels = scales::wrap_format(20)
  ) +
  labs(
    # y = "Cumulative incidence % (95% CI)",
    y = "Cumulative incidence %",
    x = "Z-score", 
    color = "Outcome") +
  theme_minimal() +
  theme(
    # legend.position = "none",
    # legend.position = "bottom",
    # legend.position = c(0.8, 0.005),
    axis.text.x = element_text(angle = 45, hjust = 1)
    ) 

plot_ci_ave

plot_ci_spec <-
  plot_ci_ave %+%
  list(data = df_plot %>%
         filter(chracteristic != "Average_c2")
  )

plot_ci_spec

cowplot::plot_grid(
  plot_ci_ave, plot_ci_spec,
  rel_widths = c(1, 1.35),
  labels = "AUTO")

ggsave(
  paste0(
    "FOR_MS_cum_inc_", 
    "both_",
    # "spec_",
    # "aver_",
    # "with_CI",
    Sys.Date(),
    ".pdf"
  ), 
  h = 6,
  w = 11,
  # h = 3.5, 
  # w = 3.5, 
  s = 1.2)

# Effect size table -------------------------------------------------------

raw_table2 <- "tab_contrast_scaled_bonf_DATE"

df2 <- 
  read.xlsx(
    file.path(
      path, paste0(raw_table2, ".xlsx")
    )
  ) %>% as_tibble()

df2

df2 %>% count(adjustment)
df2 %>% summary()

df2 <-
  df2 %>% 
  # filter(z_score %in% seq(-4, 2.25, by = 0.25)) %>% 
  filter(z_score %in% seq(-2, 2, by = 0.25)) %>% 
  mutate(
    sign = case_when(
      estimate > 1 & conf.low >= 1 ~ "*", 
      estimate < 1 & conf.high <= 1 ~ "*", 
      T ~ ""), 
    # conf.low = case_when(
    #   estimate != 1 & conf.low != 1 & conf.high != 1 ~ conf.low, 
    #   T ~ NA_real_), 
    # conf.high = case_when(
    #   estimate != 1 & conf.low != 1 & conf.high != 1 ~ conf.high, 
    #   T ~ NA_real_) 
  ) %>% 
  mutate_at(
    vars(estimate, conf.low, conf.high),
    round, 2
  ) %>% 
  mutate(
    RR = paste0(
      estimate, " (", conf.low, " - ", conf.high, ")", sign
    ),
    RR = case_when(
      RR != "1 (1 - 1)" ~ RR, 
      T ~ "1"
    ), 
    adjustment = case_when(
      adjustment == "Univariate" ~ "univar", 
      adjustment == "Multivariate with covariates" ~ "multivar_cov", 
      adjustment == "Multivariate with covariates and other grades" ~ "multivar_cov_gr"
    )
  ) 

df2 <- 
  
  df2 %>% 
  select(outcome_label, adjustment, subject, z_score, RR) %>% 
  distinct() %>% 
  spread(adjustment, RR) 

table_edited2 <- 
  
  df2 %>% 
  select(subject, z_score) %>% 
  distinct() %>% 
  
  left_join(
    df2 %>% 
      select(outcome_label, subject, z_score, univar) %>% 
      spread(outcome_label, univar) %>% 
      rename(
        univar_psy = `Non-affective psychosis`,
        univar_bipo = `Bipolar disorder`,
        univar_dep = Depression 
      ) %>% 
      select(subject, z_score, univar_psy, univar_bipo, univar_dep)
  ) %>% 
  
  left_join(
    df2 %>% 
      select(outcome_label, subject, z_score, multivar_cov) %>% 
      spread(outcome_label, multivar_cov)  %>% 
      rename(
        multivar_psy = `Non-affective psychosis`,
        multivar_bipo = `Bipolar disorder`,
        multivar_dep = Depression 
      ) %>% 
      select(subject, z_score, multivar_psy, multivar_bipo, multivar_dep)
  ) %>% 
  
  left_join(
    df2 %>% 
      select(outcome_label, subject, z_score, multivar_cov_gr) %>% 
      spread(outcome_label, multivar_cov_gr)  %>% 
      rename(
        multivar_gr_psy = `Non-affective psychosis`,
        multivar_gr_bipo = `Bipolar disorder`,
        multivar_gr_dep = Depression 
      ) %>% 
      select(subject, z_score, multivar_gr_psy, multivar_gr_bipo, multivar_gr_dep)
  ) 
  

table_edited2 <- 
  table_edited2 %>% 
  filter(z_score %in% c(-1.5, -0.5, 0, 0.5, 1.5))


write.xlsx(table_edited2, 
           file.path(
             path, paste0(raw_table2, 
                          "_edited_", 
                          Sys.Date(),
                          ".xlsx")
           )
)



# Contrast plots ----------------------------------------------------------

raw_table <- "tab_contrast_scaled_bonf_DATE"

model_contrasts <- 
  read.xlsx(
    file.path(
      path, paste0(raw_table, ".xlsx")
    )
  ) %>% as_tibble()

model_contrasts

outcome_labels <- c(
  psy  = "Non-affective psychosis",
  bipo = "Bipolar disorder",
  dep  = "Depression"
)

grades_labels <- c(
  Average         = "Average",
  Literature      = "Native Language",
  Mathematics     = "Mathematics",
  Phys_Edu        = "Physical Education",
  Handicrafts     = "Handicrafts",
  Arts            = "Arts",
  Music           = "Music"
)


model_contrasts <- 
  model_contrasts %>% 
  mutate(
    # outcome_labels = outcome[outcome_labels],
    grades_label = grades_labels[subject]
  ) %>%
  mutate(adjustment = fct_inorder(adjustment)) %>%
  mutate(
    outcome_label = fct_relevel(outcome_label, "Non-affective psychosis"),
    grades_label = fct_relevel(grades_label,
                               "Native Language",
                               "Mathematics",
                               "Physical Education",
                               "Handicrafts",
                               "Arts",
                               "Music"
    )
  )

model_contrasts %>%
  # filter(grades != "Average") %>%
  filter(z_score <= 2, z_score >= -2) %>% 
  summarise(
    min(conf.low),
    max(conf.high)
  )


# psy

plot_psy <-
  model_contrasts %>%
  filter(subject != "Average") %>%
  filter(adjustment == "Multivariate with covariates and other grades") %>%
  filter(outcome_label == "Non-affective psychosis") %>%
  ggplot(aes(z_score, estimate, fill = adjustment)) +
  facet_wrap(. ~ grades_label, nrow = 2) +
  scale_y_log10(breaks = c(.25, .5, 1, 2), limits = c(0.2, 3.3)) +
  scale_x_continuous(
    # breaks = c(4:10),
    # label = c("4 (failed)", "5", "6", "7", "8", "9", "10 (excellent)"),
    limits = c(-2, 2)
  ) +
  scale_colour_manual(
    "Analysis",
    aesthetics = c("colour", "fill"),
    values = c("#374E55", "#8BA3AA", "#17223b"),
    # values = c("#6b778d", "#ff6768", "#17223b"),
    labels = scales::wrap_format(20)
  ) +
  labs(y = "RR (Bonferroni-corrected CI)", x = "Z-score") +
  geom_hline(yintercept = 1, colour = "grey80", linetype = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(colour = adjustment), size = 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # axis.text.x = element_text(angle = 45, hjust = 1)
    ) 
plot_psy

plot_av_psy <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(adjustment == "Multivariate with covariates") %>% 
         filter(grades_label == "Average") %>%
         filter(outcome_label == "Non-affective psychosis")
  )

plot_av_psy

cowplot::plot_grid(
  plot_av_psy, plot_psy,
  rel_widths = c(1, 1.35),
  labels = "AUTO")

ggsave(
  paste0(
    "psy_av_spec_non_scaled_bonf_", 
    Sys.Date(), 
    ".pdf"
  ),
  h = 5, w = 11, s = 0.9)

# bipo

plot_bipo <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(subject != "Average") %>%
         filter(adjustment == "Multivariate with covariates and other grades") %>%
         filter(outcome_label == "Bipolar disorder")
  )

plot_bipo

plot_av_bipo <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(adjustment == "Multivariate with covariates") %>% 
         filter(grades_label == "Average") %>%
         filter(outcome_label == "Bipolar disorder")
  )

plot_av_bipo

cowplot::plot_grid(
  plot_av_bipo, plot_bipo,
  rel_widths = c(1, 1.35),
  labels = "AUTO")

ggsave(
  paste0(
    "bipo_av_spec_non_scaled_bonf_", 
    Sys.Date(), 
    ".pdf"
  ),
  h = 5, w = 11, s = 0.9)

# dep

plot_dep <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(subject != "Average") %>%
         filter(adjustment == "Multivariate with covariates and other grades") %>%
         filter(outcome_label == "Depression")
  )

plot_dep

plot_av_dep <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(adjustment == "Multivariate with covariates") %>% 
         filter(grades_label == "Average") %>%
         filter(outcome_label == "Depression")
  )

plot_av_dep

cowplot::plot_grid(
  plot_av_dep, plot_dep,
  rel_widths = c(1, 1.35),
  labels = "AUTO")

ggsave(
  paste0(
    "dep_av_spec_non_scaled_bonf_", 
    Sys.Date(), 
    ".pdf"
  ),
  h = 5, w = 11, s = 0.9)


# Percentage plots --------------------------------------------------------

raw_table <- "table_1_data_nonsc_DATE"

pt <- 
  read.xlsx(
    file.path(
      path, paste0(raw_table, ".xlsx")
    )
  ) %>% as_tibble()

subject_labels <-
  tibble(
    chracteristic = c(
      "Average_c2"     ,
      "Literature"  ,
      "Mathematics" ,
      "Phys_Edu"    ,
      "Handicrafts" ,
      "Arts"        ,
      "Music"       ),
    Subject_labels = c(
      "Average",
      "Native Language",
      "Mathematics",
      "Physical Education",
      "Handicrafts",
      "Arts",
      "Music"
    )
  )

fas_dg_labels <-
  tibble(
    outcome = c(
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
  "Native Language",
  "Mathematics",
  "Physical Education",
  "Handicrafts",
  "Arts",
  "Music"
)

pt <- 
  pt %>% 
  left_join(subject_labels) %>%
  left_join(fas_dg_labels) %>% 
  filter(!is.na(Subject_labels)) %>%
  distinct() %>% 
  group_by(chracteristic, value) %>% 
  mutate(
    events_grouped = sum(events)
  ) %>% 
  ungroup() %>% 
  select(outcome, chracteristic, value, n, events, events_grouped, 
         Subject_labels, Diagnosis) %>% 
  distinct()


pt <- 
  pt %>%
  select(-outcome, -Diagnosis, -events) %>% 
  distinct() %>% 
  mutate(
    outcome = "none", 
    Diagnosis = "None", 
    events = n - events_grouped
  ) %>% 
  bind_rows(
    pt
  ) %>% 
  group_by(outcome, chracteristic) %>% 
  mutate(
    total = sum(events),
    Percent = events/total*100
  ) %>%
  ungroup() %>%
  mutate(Diagnosis = factor(Diagnosis, levels = fill_order)) %>% 
  distinct()

plot_bar_av <-
  pt %>% 
  filter(chracteristic == "Average_c2") %>%
  rename(Grade = value) %>% 
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
  scale_colour_manual(
    "Diagnosis",
    aesthetics = c("colour", "fill"),
    values = c("#DE9146", "#00A0D5", "#7F796B", "#374E55"),
    labels = scales::wrap_format(20)
  ) +
  theme_minimal() +
  theme(
    # legend.position = "none",
    # legend.position = "bottom",
    # legend.position = c(0.8, 0.005),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

plot_bar_av
# ggsave("figures/plot_bar_av.pdf", h = 3, w = 6, s = 1.8)

plot_bar_spec <-
  pt %>% 
  filter(chracteristic != "Average_c2") %>%
  mutate(Subject_labels = factor(Subject_labels, levels = facet_order)) %>% 
  mutate(Grade = value %>% as.integer()) %>% 
  ggplot(aes(x = Grade, y = Percent, fill = Diagnosis)) +
  facet_wrap( ~ Subject_labels, ncol = 2) +
  geom_bar(stat="identity", position = "dodge") +
  scale_colour_manual(
    "Diagnosis",
    aesthetics = c("colour", "fill"),
    values = c("#DE9146", "#00A0D5", "#7F796B", "#374E55"),
    labels = scales::wrap_format(20)
  ) +
  # scale_x_discrete(labels = c(4:10 %>% as.character())) +
  theme_minimal() +
  theme(
    # legend.position = "none",
    # legend.position = "bottom",
    # legend.position = c(0.8, 0.005),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

plot_bar_spec
# ggsave("figures/plot_bar_spec.pdf", h = 7, w = 6, s = 1.8)

cowplot::plot_grid(
  plot_bar_av, plot_bar_spec,
  rel_widths = c(1, 1.35),
  labels = "AUTO")


ggsave(
  paste0(
    "percent_", 
    Sys.Date(), 
    ".pdf"
  ),
  h = 6, w = 11, s = 1.2)
