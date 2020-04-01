# Table of contrasts at given grade

# Multipanel plot: each plot represents one standardized school grade;
# x-axis = school grade with 7 as reference ; y-axis = IRR with CI of outcome;
# lines correspond to univariate and multivariate IRRs and CI


# Utils and packages ------------------------------------------------------

source("R/dm02_pkg.r")

# Read in data ------------------------------------------------------------

# Fits from previous script
model_fits <- model_fits_nonscaled

# If simulated data
source("R/dm03_sim_data.R")
fbc <- df_all

# Mutate

fbc_scaled <- fbc %>%
  mutate_at(vars(starts_with("time")), ~ {
    ("2003-05-31" %--% .) / years(1)
  }) %>%
  mutate_at(vars(!!vars_avera, !!vars_grade), scale)

fbc_non_scaled <- fbc %>%
  mutate_at(vars(starts_with("time")), ~ {
    ("2003-05-31" %--% .) / years(1)
  })

outcome_labels <- c(
  psy  = "Non-affective psychosis",
  bipo = "Bipolar disorder",
  dep  = "Depression"
)

grades_labels <- c(
  Average         = "Average",
  Literature      = "Native language",
  Mathematics     = "Mathematics",
  Phys_Edu        = "Physical Education",
  Handicrafts     = "Handicrafts",
  Arts            = "Arts",
  Music           = "Music"
)
# Extract contrasts -------------------------------------------------------


# Non-scaled
contrast_at <- function(model, x, at = seq(4, 10, by = .1), ref = 7, ...) {
  new_data <- fbc_non_scaled %>%
    data_grid(.model = model, !!x := !!at)

  new_data$fine_grade <- new_data[[x]] # fine grid of grades
  new_data <- new_data %>%
    filter(fine_grade == !!ref) %>%
    bind_rows(new_data)

  contrast(new_data, model, ...)
}

model_contrasts <-
  model_fits %>%
  filter(grades != "All") %>%
  mutate(adjustment = ifelse(adjusted,
                             "Multivariate with covariates",
                             "Univariate")) %>%
  bind_rows(
    model_fits %>%
      filter(grades == "All") %>%
      filter(adjusted) %>%
      mutate(grades = NULL) %>%
      expand_grid(grades = vars_grade) %>% # expand_grid instead of crossing https://github.com/tidyverse/tidyr/issues/735
      mutate(adjustment = "Multivariate with covariates and other grades")
  ) %>%
  unnest(estimate = map2(fit, grades, contrast_at)) %>%
  mutate(
    outcome_label = outcome_labels[outcome],
    grades_label = grades_labels[grades]
    ) %>%
  mutate(adjustment = fct_inorder(adjustment)) %>%
  mutate(
    outcome_label = fct_relevel(outcome_label, "Non-affective psychosis"),
    grades_label = fct_relevel(grades_label,
                         "Native language",
                         "Mathematics",
                         "Physical Education",
                         "Handicrafts",
                         "Arts",
                         "Music"
    )
  )


# Table of contrasts at given grades ------------------------------------

# Non-scaled
tab_contrast <-
  model_contrasts %>%
  filter(fine_grade %in% 4:10) %>%
  filter(!duplicated(.)) %>%
  select(outcome_label, adjustment,
         subject = grades, fine_grade,
         estimate, starts_with("conf")) %>%
  arrange_all()

# openxlsx::write.xlsx(
#   tab_contrast,
#   paste0(
#     "figures/tab_contrast_non_scaled_bonf_",
#     Sys.Date(),
#     ".xlsx"
#   )
# )

# One figure per diagnosis ------------------------------------------------

# Check min and max grades as well as conf.low and conf.high

fbc_non_scaled %>%
  group_by(fas_dg) %>%
  summarise(
    min(Average),
    max(Average)
  )

model_contrasts %>%
  filter(grades == "Average") %>%
  filter(
    (outcome == "psy" & fine_grade >= 4.63 & fine_grade <= 9.78) |
      (outcome == "bipo" & fine_grade >= 4.93 & fine_grade <= 9.88) |
      (outcome == "dep" & fine_grade >= 4.7 & fine_grade <= 9.94)
    ) %>%
  group_by(outcome) %>%
  summarise(
    min(conf.low),
    max(conf.high)
  )

model_contrasts %>%
  filter(grades != "Average") %>%
  summarise(
    min(conf.low),
    max(conf.high)
  )

# psy

plot_psy <-
  model_contrasts %>%
  filter(grades != "Average") %>%
  filter(adjustment != "Multivariate with covariates") %>%
  filter(outcome == "psy") %>%
  ggplot(aes(fine_grade, estimate, fill = adjustment)) +
  facet_wrap(. ~ grades_label, nrow = 2) +
  scale_y_log10(breaks = c(.25, .5, 1, 2, 4), limits = c(0.166, 26.6)) +
  scale_x_continuous(
    breaks = c(4:10),
    label = c("4 (failed)", "5", "6", "7", "8", "9", "10 (excellent)"),
    limits = c(4, 10)
  ) +
  scale_colour_manual(
    "Analysis",
    aesthetics = c("colour", "fill"),
    values = c("#6b778d", "#ff6768", "#17223b"),
    labels = scales::wrap_format(20)
  ) +
  labs(y = "RR (Bonferroni-corrected CI)", x = "Grade") +
  geom_hline(yintercept = 1, colour = "grey80", linetype = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(colour = adjustment), size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_psy

plot_av_psy <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(grades == "Average") %>%
         filter(fine_grade >= 4.63, fine_grade <= 9.78) %>%
         filter(outcome == "psy")
  )

plot_av_psy

cowplot::plot_grid(
  plot_av_psy, plot_psy,
  rel_widths = c(1, 1.35),
  labels = "AUTO")

# ggsave("figures/psy_av_spec_non_scaled_bonf.pdf", h = 5, w = 11, s = 1.2)

# bipo

plot_bipo <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(grades != "Average") %>%
         filter(adjustment != "Multivariate with covariates") %>%
         filter(outcome == "bipo")
  )

plot_bipo

plot_av_bipo <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(grades == "Average") %>%
         filter(fine_grade >= 4.93, fine_grade <= 9.88) %>%
         filter(outcome == "bipo")
  )

plot_av_bipo

cowplot::plot_grid(
  plot_av_bipo, plot_bipo,
  rel_widths = c(1, 1.35),
  labels = "AUTO")

# ggsave("figures/bipo_av_spec_non_scaled_bonf.pdf", h = 5, w = 11, s = 1.2)

# dep

plot_dep <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(grades != "Average") %>%
         filter(adjustment != "Multivariate with covariates") %>%
         filter(outcome == "dep")
  )

plot_dep

plot_av_dep <-
  plot_psy %+%
  list(data = model_contrasts %>%
         filter(grades == "Average") %>%
         filter(fine_grade >= 4.7, fine_grade <= 9.94) %>%
         filter(outcome == "dep")
  )

plot_av_dep

cowplot::plot_grid(
  plot_av_dep, plot_dep,
  rel_widths = c(1, 1.35),
  labels = "AUTO")

# ggsave("figures/dep_av_spec_non_scaled_bonf.pdf", h = 5, w = 11, s = 1.2)
