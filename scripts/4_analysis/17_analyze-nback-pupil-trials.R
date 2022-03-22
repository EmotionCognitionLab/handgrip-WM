# this script visualizes and analyzes 
# maximum phasic pupil diameter values during n-back trials
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# load fixation data for baseline correction ------------------------------

pupil_norm_nbfix <- data_ET_nback %>%
  
  # exclude practice run
  filter(!run == 0) %>%
  
  # include only fixation periods
  filter(event == 'initial_fixation') %>%
  
  # apply missingness threshold to individual segments
  mutate(pupil_mean = ifelse(frac_missing > thresh, NA, pupil_mean)) %>%
  
  # relabel pupil_mean column and remove irrelevant columns
  rename(pupil_nbfix = pupil_mean) %>%
  select(-pupil_sd, -pupil_min, -pupil_min, -pupil_max, -frac_missing)


# summarize n-back pupillary responses on trials --------------------------

summary_nb_pupil_trials <- data_ET_nback %>%
  
  # exclude practice run
  filter(!run == 0) %>%
  
  # include only fixation periods
  filter(event == 'trial') %>%
  
  # apply missingness threshold to individual segments
  mutate(pupil_mean = ifelse(frac_missing > thresh, NA, pupil_mean)) %>%
  
  # relabel pupil_max column and remove irrelevant columns
  rename(pupil_trial = pupil_max) %>%
  select(-pupil_sd, -pupil_min, -pupil_mean, -frac_missing) %>%
  
  # join baseline data to be used for correction
  left_join(pupil_norm_nbfix %>%
              select(-trial, -event),
            by = c('label_subject', 'hemi', 'run', 'block')) %>%
  
  # perform baseline correction
  rowwise() %>%
  mutate(pupil_corr = pupil_trial - pupil_nbfix) %>%
  
  # average over hemispheres
  group_by(label_subject, run, block, trial) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE)) %>%
  
  # join subject info
  left_join(data_subjects %>% select(label_subject, age_group, group),
            by = 'label_subject') %>%
  
  # join trial data (condition, accuracy, RT)
  left_join(data_nback %>%
                 select(label_subject, run, block, trial, condition, target, response_time, accuracy),
               by = c('label_subject', 'run', 'block', 'trial')) %>%
  
  # format condition as string
  rowwise() %>%
  mutate(condition = str_c(condition, '-back', sep = '')) %>%
  
  # select only relevant columns
  select(label_subject, age_group, group, run, block, trial, condition, target, accuracy, response_time, pupil_corr)


# figure: max pupil diameter by condition, averaged over runs -------------

# average over runs
summary_nb_pupil_trials_fig <- summary_nb_pupil_trials %>%
  group_by(label_subject, group, age_group, condition, target, accuracy) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE))

# relabel group for plotting
summary_nb_pupil_trials_fig <- summary_nb_pupil_trials_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group))

# relabel accuracy for plotting
summary_nb_pupil_trials_fig <- summary_nb_pupil_trials_fig %>%
  rowwise() %>%
  mutate(accuracy_str = ifelse(accuracy == 1, 'Correct', 
                               ifelse(accuracy == 0, 'Incorrect', NA)))

# set factor levels for plotting
summary_nb_pupil_trials_fig$group <- factor(summary_nb_pupil_trials_fig$group, levels = c('Handgrip', 'Control'))
summary_nb_pupil_trials_fig$age_group[summary_nb_pupil_trials_fig$age_group == 'YA'] <- 'Younger'
summary_nb_pupil_trials_fig$age_group[summary_nb_pupil_trials_fig$age_group == 'OA'] <- 'Older'
summary_nb_pupil_trials_fig$age_group <- factor(summary_nb_pupil_trials_fig$age_group, levels = c('Younger', 'Older'))
summary_nb_pupil_trials_fig$target <- factor(summary_nb_pupil_trials_fig$target, levels = c(0, 1))
summary_nb_pupil_trials_fig$accuracy_str <- factor(summary_nb_pupil_trials_fig$accuracy_str, levels = c('Correct', 'Incorrect'))


# figure for target trials only
fig_nback_pupil_trials_targetonly <- ggplot(data = summary_nb_pupil_trials_fig %>%
                                filter(target == 1),
                              aes(x = factor(condition), y = pupil_corr,
                                  colour = group, fill = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3),
             alpha = 0.1) +
  stat_summary(geom = 'crossbar', width = 0.25,
               position = position_dodge(width = 0.3),
               alpha = 0.3) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.3)) +
  scale_colour_manual(values = cols_group) +
  scale_fill_manual(values = cols_group) +
  labs(x = 'Working memory load',
       y = 'Maximum pupil diameter (au)\nrelative to pre-block fixation') +
  facet_wrap(~age_group*accuracy_str, nrow = 2) +
  theme_pubr() + theme_rotatex + theme_font_pub +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# figure for all trials (target + non-target)
fig_nback_pupil_trials <- ggplot(data = summary_nb_pupil_trials_fig,
       aes(x = factor(condition), y = pupil_corr,
           colour = group, fill = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3),
             alpha = 0.1) +
  stat_summary(geom = 'crossbar', width = 0.25,
               position = position_dodge(width = 0.3),
               alpha = 0.3) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.3)) +
  scale_colour_manual(values = cols_group) +
  scale_fill_manual(values = cols_group) +
  labs(x = 'Working memory load',
       y = 'Maximum pupil diameter (au)\nrelative to pre-block fixation') +
  facet_wrap(~age_group*accuracy_str, nrow = 1) +
  theme_pubr() + theme_rotatex + theme_font_pub +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# mixed effects regression: phasic pupil by group, condition, acc ---------

# set factor levels for model
summary_nb_pupil_trials$accuracy <- factor(summary_nb_pupil_trials$accuracy, levels = c(0, 1))
summary_nb_pupil_trials$group <- factor(summary_nb_pupil_trials$group, levels = c('control', 'handgrip'))
summary_nb_pupil_trials$condition <- factor(summary_nb_pupil_trials$condition, levels = c('0-back', '1-back', '2-back', '3-back'))
contrasts(summary_nb_pupil_trials$condition) <- contr.sum(4)  # sum coding scheme for condition

mod.nb_pupil_trials_YA <- lmer(pupil_corr ~ group * condition + (1 + condition | label_subject), 
                            data = summary_nb_pupil_trials %>% filter(age_group == 'YA'))

mod.nb_pupil_trials_OA <- lmer(pupil_corr ~ group * condition + (1 + condition | label_subject), 
                               data = summary_nb_pupil_trials %>% filter(age_group == 'OA'))


# aggregate pupil data for ANOVA ------------------------------------------

# (averaging over runs, blocks)
summary_nb_pupil_trials_avg <- summary_nb_pupil_trials %>%
  group_by(label_subject, group, age_group, condition, accuracy) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE))


# ANOVA -------------------------------------------------------------------

aov.nb_pupil_trials_YA <- aov_ez(id = 'label_subject',
                                 dv = 'pupil_corr',
                                 data = summary_nb_pupil_trials_avg %>% filter(age_group == 'YA', accuracy == 1),
                                 within = 'condition',
                                 between = 'group',
                                 type = 3,
                                 include_aov = TRUE,
                                 factorize = TRUE,
                                 anova_table = list(es = "pes"))

aov.nb_pupil_trials_OA <- aov_ez(id = 'label_subject',
                                 dv = 'pupil_corr',
                                 data = summary_nb_pupil_trials_avg %>% filter(age_group == 'OA', accuracy == 1),
                                 within = 'condition',
                                 between = 'group',
                                 type = 3,
                                 include_aov = TRUE,
                                 factorize = TRUE,
                                 anova_table = list(es = "pes"))


# planned pairwise comparisons --------------------------------------------

# return estimated marginal means
emm.nb_pupil_trials_YA <- emmeans(aov.nb_pupil_trials_YA, 
                                  ~ group | condition) 

emm.nb_pupil_trials_OA <- emmeans(aov.nb_pupil_trials_OA,
                                  ~ group | condition)

# pairwise comparisons
contrasts.nb_pupil_trials_YA <- pairs(emm.nb_pupil_trials_YA,
                                   # reverse contrasts to get handgrip-control contrast
                                   reverse = TRUE,
                                   adjust = 'bonferroni') %>% 
  as.data.frame() %>%
  rowwise() %>%
  mutate(Contrast = 'Handgrip - Control',
         `Age group` = 'Younger', .before = contrast,
         Load = str_c(str_remove_all(condition, '[X.back]'), '-back', sep = '')) %>%
  select(`Age group`,
         Contrast,
         Load,
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value) %>%
  arrange(Load)

contrasts.nb_pupil_trials_OA <- pairs(emm.nb_pupil_trials_OA,
                                   # reverse contrasts to get handgrip-control contrast
                                   reverse = TRUE,
                                   adjust = 'bonferroni') %>% 
  as.data.frame() %>%
  rowwise() %>%
  mutate(Contrast = 'Handgrip - Control',
         `Age group` = 'Older', .before = contrast,
         Load = str_c(str_remove_all(condition, '[X.back]'), '-back', sep = '')) %>%
  select(`Age group`,
         Contrast,
         Load,
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value) %>%
  arrange(Load)

# join comparison results together
contrasts.nb_pupil_trials <-
  as.data.frame(
    rbind(
      contrasts.nb_pupil_trials_YA,
      contrasts.nb_pupil_trials_OA
    )
  )




