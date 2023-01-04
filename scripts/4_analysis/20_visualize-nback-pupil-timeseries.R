# this script creates a visualization of the pupil timeseries
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# summarize pupil timeseries data -----------------------------------------

# compute average pupil timeseries across runs, blocks and trials
# (downsample to 10Hz in the process)
summary_nb_pupil_ts_fig <- summary_nb_pupil_ts %>%
  # cut `time_zeroed` into bins
  mutate(time_zeroed_int = cut(time_zeroed, breaks = seq(from = 0, to = 2.25, by = 0.1), include.lowest = TRUE, right = TRUE)) %>%
  # compute start and end time for each bin 
  # (NAs will get introduced by coercion -- some warnings will appear but this is fixed in next step)
  mutate(time_zeroed_int_start = as.numeric( sub("\\((.+),.*", "\\1", time_zeroed_int) ),
         time_zeroed_int_end = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", time_zeroed_int) )) %>%
  # correct NAs in start time for bins where starting time is 0
  mutate(time_zeroed_int_start = ifelse((is.na(time_zeroed_int_start) & time_zeroed_int_end == 0.1), 0, time_zeroed_int_start)) %>%
  # exclude bins that start > 2.2s after stim onset
  filter(!is.na(time_zeroed_int_start)) %>%
  # compute average interval time as halfway between start and end
  mutate(time_zeroed_int_avg = (time_zeroed_int_start + time_zeroed_int_end)/2) %>%
  group_by(label_subject, group, age_group, condition, accuracy, 
           time_zeroed_int_start, time_zeroed_int_end, time_zeroed_int_avg) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE))


# figure: n-back pupil timeseries by condition ----------------------------

# relabel group for plotting
summary_nb_pupil_ts_fig <- summary_nb_pupil_ts_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group))

# relabel accuracy for plotting
summary_nb_pupil_ts_fig <- summary_nb_pupil_ts_fig %>%
  rowwise() %>%
  mutate(accuracy_str = ifelse(accuracy == 1, 'Correct', 
                               ifelse(accuracy == 0, 'Incorrect', NA)))

# set factor levels for plotting
summary_nb_pupil_ts_fig$group <- factor(summary_nb_pupil_ts_fig$group, levels = c('Control', 'Handgrip'))
summary_nb_pupil_ts_fig$age_group[summary_nb_pupil_ts_fig$age_group == 'YA'] <- 'Younger'
summary_nb_pupil_ts_fig$age_group[summary_nb_pupil_ts_fig$age_group == 'OA'] <- 'Older'
summary_nb_pupil_ts_fig$age_group <- factor(summary_nb_pupil_ts_fig$age_group, levels = c('Younger', 'Older'))
summary_nb_pupil_ts_fig$condition <- factor(summary_nb_pupil_ts_fig$condition, levels = c('0-back', '1-back', '2-back', '3-back'))
summary_nb_pupil_ts_fig$accuracy_str <- factor(summary_nb_pupil_ts_fig$accuracy_str, levels = c('Correct', 'Incorrect'))

# create figure (correct trials only)
fig_nback_pupil_ts <- ggplot(data = summary_nb_pupil_ts_fig %>% filter(accuracy == 1), 
       aes(x = time_zeroed_int_avg, y = pupil_corr, 
           colour = group, group = group, fill = group)) +
  #stat_summary(geom = 'line', fun = mean, lwd = 1) + 
  geom_smooth(aes(colour = group), method = 'gam', se = TRUE) +
  #stat_summary(geom = 'ribbon', 
  #               fun.data = mean_se, alpha = 0.2, colour = NA) +
  scale_colour_manual(values = cols_group) +
  scale_fill_manual(values = cols_group) +
  labs(x = 'time relative to stim onset (sec)', 
       y = 'Pupil diameter (au),\nrelative to pre-block fixation',
       colour = '', fill = '') +
  coord_cartesian(ylim = c(-1.5, 2.0)) +
  scale_x_continuous(breaks = c(0.0, 1.0, 2.0)) +
  facet_wrap(~age_group*condition, nrow = 1, ncol = 8) +
  theme_pubr() + theme_font_pub +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9), 
        legend.position = 'right')

fig_nback_pupil_ts <- ggplot(data = summary_nb_pupil_ts_fig %>% filter(accuracy == 1), 
                             aes(x = time_zeroed_int_avg, y = pupil_corr, 
                                 colour = group, group = group, fill = group)) +
  #stat_summary(geom = 'line', fun = mean, lwd = 1) + 
  geom_smooth(aes(colour = group), 
              method = 'loess', 
              se = TRUE) +
  #stat_summary(geom = 'ribbon', 
  #               fun.data = mean_se, alpha = 0.2, colour = NA) +
  scale_colour_manual(values = cols_group) +
  scale_fill_manual(values = cols_group) +
  labs(x = 'time relative to stim onset (sec)', 
       y = 'Pupil diameter (au),\nrelative to pre-block fixation',
       colour = '', fill = '') +
  coord_cartesian(ylim = c(-1.5, 2.0)) +
  scale_x_continuous(breaks = c(0.0, 1.0, 2.0)) +
  facet_wrap(~age_group*condition, nrow = 1, ncol = 8) +
  theme_pubr() + theme_font_pub +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9), 
        legend.position = 'right')


# create composite figure 6 (phasic pupil) --------------------------------

figure6 <- ggarrange(
  
  fig_nback_pupil_ts +
    labs(subtitle = 'A. Pupil time course during n-back trials (correct trials only)') +
    theme(plot.subtitle = element_text(size = 11, face = 'bold')),
  
  ggarrange(
    fig_nback_pupil_trials +
      labs(subtitle = 'B. Maximum pupil diameter during n-back trials') +
      theme(plot.subtitle = element_text(size = 11, face = 'bold')),
    
    fig_nback_pupil_onset +
      labs(subtitle = 'C. Onset time of maximum pupil diameter during n-back trials') +
      theme(plot.subtitle = element_text(size = 11, face = 'bold')),
    
    nrow = 2, common.legend = TRUE, legend = 'none'
  ),
  
  common.legend = TRUE, legend = 'none',
  
  heights = c(1, 2.6),
  
  nrow = 2, ncol = 1, align = 'v'
)

fig6_legend <- get_legend(fig_nback_pupil_ts)

figure6 <- ggarrange(
  figure6,
  fig6_legend,
  nrow = 1, ncol = 2,
  widths = c(1, 0.1)
)

ggsave(here('figures', 'figure6_nback-pupil.png'), 
       figure6,
       device = 'png', dpi = fig_dpi, bg = 'white',
       width = 9, height = 12)
