# this script visualizes and analyzes 
# times of maximum phasic pupil diameter onset during n-back trials
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# gather trial-level missingness data -------------------------------------

summary_nb_pupil_trialmissing <- data_ET_nback %>%
  
  # exclude practice trials
  filter(!run == 0) %>%
  
  # use data from trials only
  filter(event == 'trial') %>%
  
  select(label_subject, run, block, trial, hemi, frac_missing) %>%
  
  # exclude segments with missingness above threshold
  rowwise() %>%
  mutate(exclude_trial = ifelse(frac_missing > thresh, 1, 0)) %>%
  select(-frac_missing)


# organize and baseline-correct n-back pupil timeseries data --------------

summary_nb_pupil_ts <- data_ET_nback_ts %>%

  # bind info about trial missingness
  left_join(summary_nb_pupil_trialmissing,
            by = c('label_subject', 'run', 'block', 'trial', 'hemi')) %>%

  # make pupil values for NA for trials/hemis with missingness over threshold
  rowwise() %>%
  mutate(pup_interp = ifelse(exclude_trial == 1, NA,
                             ifelse(exclude_trial == 0, pup_interp))) %>%

  # exclude practice trials
  filter(!run == 0) %>%

  # use data from trials only
  filter(event == 'trial') %>%

  # join with baseline & fixation data for baseline correction
  left_join(pupil_norm_baseline,
            by = c('label_subject', 'hemi')) %>%
  left_join(pupil_norm_nbfix %>% select(-trial, -event),
            by = c('label_subject', 'run', 'block', 'hemi')) %>%

  # perform baseline correction
  rowwise() %>%
  mutate(pupil_corr_b = pup_interp - pupil_baseline,
         pupil_corr = pup_interp - pupil_nbfix) %>%

  # average over hemispheres
  group_by(label_subject, run, block, trial, time) %>%
  summarize(pupil_corr_b = mean(pupil_corr_b, na.rm = TRUE),
            pupil_corr = mean(pupil_corr, na.rm = TRUE)) %>%

  # bind subject info
  left_join(data_subjects %>%
              select(label_subject, group, age_group), by = 'label_subject') %>%

  # bind condition & target & response info for trials
  left_join(data_nback %>%
              select(label_subject, run, block, trial, condition, target, response_time, accuracy),
            by = c('label_subject', 'run', 'block', 'trial')) %>%

  # name condition as "-back"
  rowwise() %>%
  mutate(condition = str_c(condition, '-back', sep = '')) %>%

  # select only relevant columns
  select(label_subject, group, age_group,
         run, block, trial, time, condition, target, response_time, accuracy,
         pupil_corr)

summary_nb_pupil_ts[summary_nb_pupil_ts == 'NaN'] <- NA


# reset trial timing in n-back pupil timeseries data ----------------------

# function to set time for each trial relative to stimulus onset
zero_pupil_trial <- function(time){
  onset_time <- time[1]
  new_time <- time - onset_time
  return(new_time)
}

# set time for each trial
summary_nb_pupil_ts <- summary_nb_pupil_ts %>%
  group_by(label_subject, group, age_group, 
           run, block, trial) %>%
  mutate(time_zeroed = zero_pupil_trial(time)) %>%
  filter(time_zeroed < 2.25) %>%
  ungroup()


# identify time of max phasic pupillary response for each trial -----------

summary_nb_pupil_onset <- summary_nb_pupil_ts %>%
  group_by(label_subject, group, age_group, run, block, trial, condition, target, accuracy) %>%
  summarize(pupil_corr_onset = time_zeroed[which.max(pupil_corr)])


# figure: time of max phasic pupillary response ---------------------------

# average over runs
summary_nb_pupil_onset_fig <- summary_nb_pupil_onset %>%
  group_by(label_subject, group, age_group, condition, target, accuracy) %>%
  summarize(pupil_corr_onset = mean(pupil_corr_onset, na.rm = TRUE))

# relabel group for plotting
summary_nb_pupil_onset_fig <- summary_nb_pupil_onset_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group))

# relabel accuracy for plotting
summary_nb_pupil_onset_fig <- summary_nb_pupil_onset_fig %>%
  rowwise() %>%
  mutate(accuracy_str = ifelse(accuracy == 1, 'Correct', 
                               ifelse(accuracy == 0, 'Incorrect', NA)))

summary_nb_pupil_onset_fig$group <- factor(summary_nb_pupil_onset_fig$group, levels = c('Handgrip', 'Control'))
summary_nb_pupil_onset_fig$age_group[summary_nb_pupil_onset_fig$age_group == 'YA'] <- 'Younger'
summary_nb_pupil_onset_fig$age_group[summary_nb_pupil_onset_fig$age_group == 'OA'] <- 'Older'
summary_nb_pupil_onset_fig$age_group <- factor(summary_nb_pupil_onset_fig$age_group, levels = c('Younger', 'Older'))
summary_nb_pupil_onset_fig$condition <- factor(summary_nb_pupil_onset_fig$condition, levels = c('0-back', '1-back', '2-back', '3-back'))
summary_nb_pupil_onset_fig$target <- factor(summary_nb_pupil_onset_fig$target, levels = c(0, 1))
summary_nb_pupil_onset_fig$accuracy_str <- factor(summary_nb_pupil_onset_fig$accuracy_str, levels = c('Correct', 'Incorrect'))

# figure for target trials only
fig_nback_pupil_onset_targetonly <- ggplot(data = summary_nb_pupil_onset_fig %>%
                                             filter(target == 1),
         aes(x = factor(condition), y = pupil_corr_onset,
             colour = group, fill = group, group = group)) +
    geom_point(position = position_dodge(width = 0.3),
               alpha = 0.2) +
    stat_summary(geom = 'crossbar', width = 0.25,
                 position = position_dodge(width = 0.3),
                 alpha = 0.3) +
    stat_summary(geom = 'line',
                 position = position_dodge(width = 0.3)) +
    scale_colour_manual(values = cols_group) +
    scale_fill_manual(values = cols_group) +
    labs(x = 'Working memory load',
         y = 'Onset time (seconds)\nof maximum pupil diameter') +
    coord_cartesian(ylim = c(0, 2.25)) +
    facet_wrap(~age_group*accuracy_str, nrow = 1) +
    theme_pubr() + theme_rotatex + theme_font_pub +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 9),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9))

# for target & non-target trials
fig_nback_pupil_onset <- ggplot(data = summary_nb_pupil_onset_fig,
       aes(x = factor(condition), y = pupil_corr_onset,
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
       y = 'Onset time (seconds)\nof maximum pupil diameter') +
  coord_cartesian(ylim = c(0, 2.25)) +
  facet_wrap(~age_group*accuracy_str, nrow = 1) +
  theme_pubr() + theme_rotatex + theme_font_pub +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# aggregate over pupil onsets for ANOVA -----------------------------------

summary_nb_pupil_onset_avg <- summary_nb_pupil_onset %>%
  group_by(label_subject, group, age_group, condition, accuracy) %>%
  summarize(pupil_corr_onset = mean(pupil_corr_onset, na.rm = TRUE))


# ANOVA: pupil onsets by group, condition ---------------------------------

# (due to low power on incorrect trials, including correct trials only)

aov.nb_pupil_onset_YA <- aov_ez(id = 'label_subject',
                                   dv = 'pupil_corr_onset',
                                   data = summary_nb_pupil_onset_avg %>% filter(age_group == 'YA', accuracy == 1),
                                   between = c('group'),
                                   within = c('condition'),
                                   type = 3,
                                   include_aov = TRUE,
                                   factorize = TRUE,
                                   anova_table = list(es = 'pes'))

aov.nb_pupil_onset_OA <- aov_ez(id = 'label_subject',
                                   dv = 'pupil_corr_onset',
                                   data = summary_nb_pupil_onset_avg %>% filter(age_group == 'OA', accuracy == 1),
                                   between = c('group'),
                                   within = c('condition'),
                                   type = 3,
                                   include_aov = TRUE,
                                   factorize = TRUE,
                                   anova_table = list(es = 'pes'))


# planned pairwise comparisons --------------------------------------------

# return estimated marginal means
emm.nb_pupil_onset_YA <- emmeans(aov.nb_pupil_onset_YA, 
                                  ~ group | condition) 

emm.nb_pupil_onset_OA <- emmeans(aov.nb_pupil_onset_OA,
                                  ~ group | condition)

# pairwise comparisons
contrasts.nb_pupil_onset_YA <- pairs(emm.nb_pupil_onset_YA,
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

contrasts.nb_pupil_onset_OA <- pairs(emm.nb_pupil_onset_OA,
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
contrasts.nb_pupil_onset <-
  as.data.frame(
    rbind(
      contrasts.nb_pupil_onset_YA,
      contrasts.nb_pupil_onset_OA
    )
  )

