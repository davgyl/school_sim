# Read previous scripts
# source("R/dm02_pkg.r")
# If simulated data
# NOTE: not resembling real data although same variable names
# source("R/dm03_sim_data.R")
fbc <- df_tot

# Read in data ------------------------------------------------------------

# Fits from previous script
model_fits <- model_fits_scaled

# Table of contrasts at given grade

# Mutate

fbc_scaled <- fbc %>%
  mutate_at(vars(starts_with("time")), ~ {
    ("2003-05-31" %--% .) / years(1)
  }) %>%
  mutate_at(vars(!!vars_avera, !!vars_grade), scale)

# For non-scaled
# fbc_non_scaled <- fbc %>%
#   mutate_at(vars(starts_with("time")), ~ {
#     ("2003-05-31" %--% .) / years(1)
#   })


outcome_labels <- c(
  dg1  = "Diagnosis 1",
  dg2  = "Diagnosis 2",
  dg3  = "Diagnosis 3"
)

grades_labels <- c(
  Average         = "Average",
  subject_1       = "Subject 1",
  subject_2       = "Subject 2",
  subject_3       = "Subject 3",
  subject_4       = "Subject 4",
  subject_5       = "Subject 5",
  subject_6       = "Subject 6"
)

# For real data
# outcome_labels <- c(
#   psy  = "Non-affective psychosis",
#   bipo = "Bipolar disorder",
#   dep  = "Depression"
# )
# 
# grades_labels <- c(
#   Average         = "Average",
#   Literature      = "Native language",
#   Mathematics     = "Mathematics",
#   Phys_Edu        = "Physical Education",
#   Handicrafts     = "Handicrafts",
#   Arts            = "Arts",
#   Music           = "Music"
# )
# Extract contrasts -------------------------------------------------------

# Scaled

# Do not include z-scores out of bound
tab_minmax <-
  fbc %>%
  filter(fas == 1) %>%
  select(fas_dg, vars_avera, vars_grade) %>%
  mutate_at(c(vars_avera, vars_grade), compose(c, scale)) %>%
  pivot_longer(-fas_dg,names_to = "subject", values_to = "grade") %>%
  group_by(fas_dg, subject) %>%
  summarise(
    min = min(grade),
    max = max(grade)
  ) %>%
  ungroup() %>%
  rename(
    outcome = fas_dg,
    grades = subject
  )

tab_minmax %>%
  summarise(
    min(min),
    max(min),
    min(max),
    max(max)
  )

model_fits <- model_fits_scaled

contrast_at <- function(model, x, at = seq(-5, 3, by = .05), ref = 0, ...) {
  new_data <- fbc_scaled %>%
    data_grid(.model = model, !!x := !!at)
  
  new_data$z_score <- new_data[[x]]
  new_data <- new_data %>%
    filter(z_score == !!ref) %>%
    bind_rows(new_data)
  
  contrast(new_data, model, ...)
}

# Non-scaled

# Do not include grades out of bound
# tab_minmax_non_scaled <-
#   fbc_all %>%
#   filter(fas == 1) %>%
#   select(fas_dg, vars_avera, vars_grade) %>%
#   pivot_longer(-fas_dg,names_to = "subject", values_to = "grade") %>%
#   group_by(fas_dg, subject) %>%
#   summarise(
#     min = min(grade),
#     max = max(grade)
#   ) %>%
#   ungroup() %>%
#   rename(
#     outcome = fas_dg,
#     grades = subject
#   )
# 
# tab_minmax_non_scaled %>%
#   summarise(
#     min(min),
#     max(min),
#     min(max),
#     max(max)
#   )
# 
# model_fits <- model_fits_non_scaled
# 
# contrast_at <- function(model, x, at = seq(4, 10, by = .01), ref = 8, ...) {
#   new_data <- fbc_non_scaled %>%
#     data_grid(.model = model, !!x := !!at)
#   
#   new_data$fine_grade <- new_data[[x]] # fine grid of grades
#   new_data <- new_data %>%
#     filter(fine_grade == !!ref) %>%
#     bind_rows(new_data)
#   
#   contrast(new_data, model, ...)
# }

# Contrast

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
  # For simulation data
  mutate(    
    outcome_label = fct_relevel(outcome_label, "Diagnosis 1"),
    grades_label = fct_relevel(grades_label,
                               "Subject 1",
                               "Subject 2",
                               "Subject 3",
                               "Subject 4",
                               "Subject 5",
                               "Subject 6"
    )
  )
# For real data
    # mutate( 
    # outcome_label = fct_relevel(outcome_label, "Non-affective psychosis"),
    # grades_label = fct_relevel(grades_label,
    #                            "Native Language",
    #                            "Mathematics",
    #                            "Physical Education",
    #                            "Handicrafts",
    #                            "Arts",
    #                            "Music"
    # )
  # )


# Exclude z-scores out of bound (scaled)
model_contrasts <-
  model_contrasts %>%
  left_join(tab_minmax) %>%
  filter(z_score >= min, z_score <= max)

# Exclude grades out of bound (non-scaled)
# model_contrasts <-
#   model_contrasts %>%
#   left_join(tab_minmax_non_scaled) %>%
#   filter(fine_grade >= min, fine_grade <= max)



# Table of contrasts at given grades ------------------------------------

# Scaled

tab_contrast <-
  model_contrasts %>%
  # filter(z_score %in% seq(-2, 2, by = 0.05)) %>%
  select(outcome_label, adjustment,
         subject = grades, z_score,
         estimate, starts_with("conf")) %>%
  filter(!duplicated(.)) %>%
  arrange_all()

# openxlsx::write.xlsx(
#   tab_contrast,
#   paste0(
#     "figures/tab_contrast_scaled_bonf_",
#     Sys.Date(),
#     ".xlsx"
#   )
# )

# Non-scaled

# tab_contrast <-
#   model_contrasts %>%
#   filter(fine_grade %in% 4:10) %>%
#   filter(!duplicated(.)) %>%
#   select(outcome_label, adjustment,
#          subject = grades, fine_grade,
#          estimate, starts_with("conf")) %>%
#   arrange_all()

# openxlsx::write.xlsx(
#   tab_contrast,
#   paste0(
#     "figures/tab_contrast_non_scaled_bonf_contrast8_",
#     Sys.Date(),
#     ".xlsx"
#   )
# )


# FIGURES -----------------------------------------------------------------

# Figure of average scores ------------------------------------------------

fbc_scaled %>%
  group_by(fas_dg) %>%
  summarise(
    min(Average),
    max(Average)
  )

# Scaled
plot_avg <- model_contrasts %>%
  filter(grades == "Average") %>%
  ggplot(aes(z_score, estimate, fill = adjustment)) +
  facet_grid(grades_label ~ outcome_label) +
  scale_y_log10() +
  # scale_x_continuous(
  # breaks = c(-3.2:2.1)
  # ) +
  scale_colour_manual(
    "Analysis",
    aesthetics = c("colour", "fill"),
    values = c("#6b778d", "#ff6768", "#17223b"),
    labels = scales::wrap_format(20)
  ) +
  labs(y = "RR (Bonferroni-corrected CI)", x = "Z-score") +
  geom_hline(yintercept = 1, colour = "grey80", linetype = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(colour = adjustment), size = 1)

plot_avg

# ggsave(
#   paste0(
#     "figures/Average_contrasts_scaled_",
#     Sys.Date(),
#     ".pdf"
#   ), h = 3, w = 6, s = 1.8)

# Non-scaled
# plot_avg <-
#   model_contrasts %>%
#   filter(grades == "Average") %>%
#   ggplot(aes(fine_grade, estimate, fill = adjustment)) +
#   facet_grid(grades_label ~ outcome_label) +
#   scale_y_log10() +
#   scale_x_continuous(
#     breaks = c(4:10),
#     label = c("4 (failed)", "5", "6", "7", "8", "9", "10 (excellent)")
#   ) +
#   scale_colour_manual(
#     "Analysis",
#     aesthetics = c("colour", "fill"),
#     values = c("#6b778d", "#ff6768", "#17223b"),
#     labels = scales::wrap_format(20)
#   ) +
#   labs(y = "RR (Bonferroni-corrected CI)", x = "Grade") +
#   geom_hline(yintercept = 1, colour = "grey80", linetype = 2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line(aes(colour = adjustment), size = 1) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot_avg

# ggsave(
#   paste0(
#     "figures/Average_contrasts_non_scaled_",
#     "contrast8_",
#     Sys.Date(),
#     ".pdf"
#   ), h = 3, w = 6, s = 1.8)

# Figure of specific grades -----------------------------------------------

plot_all <- plot_avg %+%
  list(data = model_contrasts %>%
         filter(grades != "Average") %>%
         filter(adjustment != "Multivariate with covariates")
  )

plot_all

# Scaled
# ggsave(
#   paste0(
#     "figures/Subjects contrasts_scaled_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 7, w = 6, s = 1.8)

# Non-scaled
# ggsave(
#   paste0(
#     "figures/Subjects contrasts_non_scaled_",
#     "contrast8_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 7, w = 6, s = 1.8)



# One figure per diagnosis ------------------------------------------------

tab_minmax

model_contrasts %>%
  summarise(
    min(conf.low),
    max(conf.high)
  )

model_contrasts %>%
  summarise(
    min(z_score),
    max(z_score)
  )

model_contrasts %>%
  filter(z_score >= -2, z_score <= 2) %>%
  summarise(
    min(conf.low),
    max(conf.high)
  )

plot_dg1 <-
  model_contrasts %>%
  filter(grades != "Average") %>%
  filter(adjustment == "Multivariate with covariates and other grades") %>%
  filter(outcome == "dg1") %>%
  ggplot(aes(z_score, estimate, fill = adjustment)) +
  facet_wrap(. ~ grades_label, nrow = 2) +
  scale_y_log10(breaks = c(.25, .5, 1, 2, 4), limits = c(0.135, 6.7)) +
  scale_x_continuous(
    breaks = seq(-2, 2, by = 0.5),
    # label = c(),
    limits = c(-2, 2)
  ) +
  scale_colour_manual(
    "Analysis",
    aesthetics = c("colour", "fill"),
    values = c("#6b778d", "#ff6768", "#17223b"),
    labels = scales::wrap_format(20)
  ) +
  labs(y = "RR (Bonferroni-corrected CI)", x = "Z-score") +
  geom_hline(yintercept = 1, colour = "grey80", linetype = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(aes(colour = adjustment), size = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

plot_dg1

plot_av_dg1 <-
  plot_dg1 %+%
  list(data = model_contrasts %>%
         filter(grades == "Average") %>%
         filter(adjustment == "Multivariate with covariates") %>%
         filter(outcome == "dg1")
  )

plot_av_dg1

cowplot::plot_grid(
  plot_av_dg1, plot_dg1,
  rel_widths = c(1, 1.35),
  labels = "AUTO")

# ggsave(
#   paste0(
#     "figures/dg1_av_spec_",
#     "onlymultivar_",
#     "scaled_",
#     "bonf_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 5, w = 11, s = 1.2)


# Additional figures for real data ----------------------------------------

# Check min and max grades as well as conf.low and conf.high for non-scaled

# fbc_non_scaled %>%
#   group_by(fas_dg) %>%
#   summarise(
#     min(Average),
#     max(Average)
#   )

# model_contrasts %>%
#   filter(grades == "Average") %>%
#   filter(
#     (outcome == "psy" & fine_grade >= 4.63 & fine_grade <= 9.78) |
#       (outcome == "bipo" & fine_grade >= 4.93 & fine_grade <= 9.88) |
#       (outcome == "dep" & fine_grade >= 4.7 & fine_grade <= 9.94)
#   ) %>%
#   group_by(outcome) %>%
#   summarise(
#     min(conf.low),
#     max(conf.high)
#   )

# Same for scaled
# tab_minmax
# 
# model_contrasts %>%
#   summarise(
#     min(conf.low),
#     max(conf.high)
#   )
# 
# model_contrasts %>%
#   summarise(
#     min(z_score),
#     max(z_score)
#   )
# 
# model_contrasts %>%
#   filter(z_score >= -2, z_score <= 2) %>%
#   summarise(
#     min(conf.low),
#     max(conf.high)
#   )

# psy

# Scaled

# plot_psy <-
#   model_contrasts %>%
#   filter(grades != "Average") %>%
#   filter(adjustment == "Multivariate with covariates and other grades") %>%
#   # filter(adjustment != "Multivariate with covariates") %>%
#   filter(outcome == "psy") %>%
#   ggplot(aes(z_score, estimate, fill = adjustment)) +
#   facet_wrap(. ~ grades_label, nrow = 2) +
#   scale_y_log10(breaks = c(.25, .5, 1, 2, 4), limits = c(0.24, 4.1)) +
#   scale_x_continuous(
#     breaks = seq(-2, 2, by = 0.5),
#     # label = c(),
#     limits = c(-2, 2)
#   ) +
#   scale_colour_manual(
#     "Analysis",
#     aesthetics = c("colour", "fill"),
#     values = c("#6b778d", "#ff6768", "#17223b"),
#     labels = scales::wrap_format(20)
#   ) +
#   labs(y = "RR (Bonferroni-corrected CI)", x = "Z-score") +
#   geom_hline(yintercept = 1, colour = "grey80", linetype = 2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line(aes(colour = adjustment), size = 1) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "none"
#   )
# 
# plot_psy
# 
# plot_av_psy <-
#   plot_psy %+%
#   list(data = model_contrasts %>%
#          filter(grades == "Average") %>%
#          filter(adjustment == "Multivariate with covariates") %>%
#          filter(outcome == "psy")
#   )
# 
# plot_av_psy
# 
# cowplot::plot_grid(
#   plot_av_psy, plot_psy,
#   rel_widths = c(1, 1.35),
#   labels = "AUTO")

# ggsave(
#   paste0(
#     "figures/psy_av_spec_",
#     "onlymultivar_",
#     "scaled_",
#     "bonf_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 5, w = 11, s = 1.2)


# Non-scaled

# plot_psy <-
#   model_contrasts %>%
#   filter(grades != "Average") %>%
#   filter(adjustment != "Multivariate with covariates") %>%
#   filter(outcome == "psy") %>%
#   ggplot(aes(fine_grade, estimate, fill = adjustment)) +
#   facet_wrap(. ~ grades_label, nrow = 2) +
#   scale_y_log10(breaks = c(.25, .5, 1, 2, 4), limits = c(0.166, 26.6)) +
#   scale_x_continuous(
#     breaks = c(4:10),
#     label = c("4 (failed)", "5", "6", "7", "8", "9", "10 (excellent)"),
#     limits = c(4, 10)
#   ) +
#   scale_colour_manual(
#     "Analysis",
#     aesthetics = c("colour", "fill"),
#     values = c("#6b778d", "#ff6768", "#17223b"),
#     labels = scales::wrap_format(20)
#   ) +
#   labs(y = "RR (Bonferroni-corrected CI)", x = "Grade") +
#   geom_hline(yintercept = 1, colour = "grey80", linetype = 2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line(aes(colour = adjustment), size = 1) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
# plot_psy
#
# plot_av_psy <-
#   plot_psy %+%
#   list(data = model_contrasts %>%
#          filter(grades == "Average") %>%
#          filter(fine_grade >= 4.63, fine_grade <= 9.78) %>%
#          filter(outcome == "psy")
#   )
#
# plot_av_psy
#
# cowplot::plot_grid(
#   plot_av_psy, plot_psy,
#   rel_widths = c(1, 1.35),
#   labels = "AUTO")
#
# ggsave(
#   paste0(
#     "figures/psy_av_spec_non_scaled_bonf_",
#     "contrast8_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 5, w = 11, s = 1.2)

# bipo

# plot_bipo <-
#   plot_psy %+%
#   list(data = model_contrasts %>%
#          filter(grades != "Average") %>%
#          filter(adjustment == "Multivariate with covariates and other grades") %>%
#          # filter(adjustment != "Multivariate with covariates") %>%
#          filter(outcome == "bipo")
#   )
# 
# plot_bipo

# plot_av_bipo <-
#   plot_psy %+%
#   list(data = model_contrasts %>%
#          filter(grades == "Average") %>%
#          filter(adjustment == "Multivariate with covariates") %>%
#          # filter(fine_grade >= 4.93, fine_grade <= 9.88) %>%
#          filter(outcome == "bipo")
#   )

# plot_av_bipo

# cowplot::plot_grid(
#   plot_av_bipo, plot_bipo,
#   rel_widths = c(1, 1.35),
#   labels = "AUTO")

# Scaled
# ggsave(
#   paste0(
#     "figures/bipo_av_spec_",
#     "onlymultivar_",
#     "scaled_",
#     "bonf_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 5, w = 11, s = 1.2)

# Non-scaled
# ggsave(
#   paste0(
#     "figures/bipo_av_spec_non_scaled_bonf_",
#     "contrast8_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 5, w = 11, s = 1.2)

# dep

# plot_dep <-
#   plot_psy %+%
#   list(data = model_contrasts %>%
#          filter(grades != "Average") %>%
#          filter(adjustment == "Multivariate with covariates and other grades") %>%
#          # filter(adjustment != "Multivariate with covariates") %>%
#          filter(outcome == "dep")
#   )

# plot_dep

# plot_av_dep <-
#   plot_psy %+%
#   list(data = model_contrasts %>%
#          filter(grades == "Average") %>%
#          filter(adjustment == "Multivariate with covariates") %>%
#          # filter(fine_grade >= 4.7, fine_grade <= 9.94) %>%
#          filter(outcome == "dep")
#   )

# plot_av_dep

# cowplot::plot_grid(
#   plot_av_dep, plot_dep,
#   rel_widths = c(1, 1.35),
#   labels = "AUTO")

# Scaled
# ggsave(
#   paste0(
#     "figures/dep_av_spec_",
#     "onlymultivar_",
#     "scaled_",
#     "bonf_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 5, w = 11, s = 1.2)

# Non-scaled

# ggsave(
#   paste0(
#     "figures/dep_av_spec_non_scaled_bonf_",
#     "contrast8_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 5, w = 11, s = 1.2)

# Compare Multivariate with and without other covariates than grad --------

# plot_comp1 <- plot_avg %+%
#   list(data = model_contrasts %>%
#          filter(grades != "Average") %>%
#          filter(adjustment != "Multivariate with covariates and other grades")
#   )
# 
# plot_comp1
# 
# plot_comp2 <- plot_avg %+%
#   list(data = model_contrasts %>%
#          filter(grades != "Average") %>%
#          filter(adjustment != "Univariate")
#   )
# 
# plot_comp2
# 
# cowplot::plot_grid(
#   plot_comp1, plot_comp2,
#   labels = "AUTO")

# Scaled
# ggsave(
#   paste0(
#     "figures/comp_analyses_scaled_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 11, w = 11*1.6, s = 1)

# Non-scaled
# ggsave(
#   paste0(
#     "figures/comp_analyses_non_scaled_",
#     "contrast8_",
#     Sys.Date(),
#     ".pdf"
#   )
#   , h = 11, w = 11*1.6, s = 1)
