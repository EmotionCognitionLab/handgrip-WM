# this script visualizes & analyzes relations between
# EMG signal and arousal measures during handgrip
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# participant-wise summaries of arousal during handgrip -------------------

# for each arousal measure,
# compute each participant's difference between squeeze & rest

summary_hg_arousal <- as.data.frame(
  left_join(
    ### pupil
    summary_hg_pupil %>%
      # average over rounds & runs
      group_by(label_subject, event) %>%
      summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE)) %>%
      # convert from long to wide
      pivot_wider(names_from = 'event', names_prefix = 'pupil_', values_from = pupil_corr) %>%
      # calculate squeeze vs. rest difference
      rowwise() %>%
      mutate(pupil_diff = pupil_squeeze - pupil_rest) %>%
      select(label_subject, pupil_diff),
    
    ### heart rate
    summary_hg_HR %>%
      # average over rounds & runs
      group_by(label_subject, event) %>%
      summarize(niHR_corr = mean(niHR_corr, na.rm = TRUE)) %>%
      # convert from long to wide
      pivot_wider(names_from = 'event', names_prefix = 'HR_', values_from = niHR_corr) %>%
      # calculate squeeze vs. rest difference
      rowwise() %>%
      mutate(HR_diff = HR_squeeze - HR_rest) %>%
      select(label_subject, HR_diff),
    by = 'label_subject'
  ) %>% left_join(
    ### aSKNA
    summary_hg_aSKNA %>%
      # average over rounds & runs
      group_by(label_subject, event) %>%
      summarize(aSKNA_corr = mean(aSKNA_corr, na.rm = TRUE)) %>%
      # convert from long to wide
      pivot_wider(names_from = 'event', names_prefix = 'aSKNA_', values_from = aSKNA_corr) %>%
      # calculate squeeze vs. rest difference
      rowwise() %>%
      mutate(aSKNA_diff = aSKNA_squeeze - aSKNA_rest) %>%
      select(label_subject, aSKNA_diff),
    by = 'label_subject'
  ) %>% left_join(
    ### EMG
    summary_hg_EMG %>%
      # average over rounds & runs
      group_by(label_subject, event) %>%
      summarize(emg_corr = mean(emg_corr, na.rm = TRUE)) %>%
      # convert from long to wide
      pivot_wider(names_from = 'event', names_prefix = 'EMG_', values_from = emg_corr) %>%
      # calculate squeeze vs. rest difference
      rowwise() %>%
      mutate(EMG_diff = EMG_squeeze - EMG_rest) %>%
      select(label_subject, EMG_diff),
    by = 'label_subject'
  ) %>% left_join(
    data_subjects %>% select(label_subject, group, age_group),
    by = 'label_subject'
  )
) %>%
  select(label_subject, group, age_group, pupil_diff:EMG_diff) %>%
  filter(group == 'handgrip')
summary_hg_arousal[summary_hg_arousal == 'NaN'] <- NA


# figure: EMG vs. arousal measures during handgrip ------------------------

# set factor levels for plotting
summary_hg_arousal <- summary_hg_arousal %>%
  rowwise() %>%
  mutate(group = str_to_title(group))
summary_hg_arousal$group <- factor(summary_hg_arousal$group, levels = c('Handgrip', 'Control'))

fig_handgrip_EMG_pupil <- ggplot(data = summary_hg_arousal,
                                 aes(x = EMG_diff, y = pupil_diff)) +
  geom_point(aes(colour = group), alpha = 0.5) +
  stat_cor(method = 'pearson', label.y = 1.5,
           cor.coef.name = 'r',
           p.accuracy = 0.001, r.accuracy = 0.001) +
  stat_smooth(colour = 'darkgray', method = 'lm', se = TRUE) +
  scale_colour_manual(values = cols_group) +
  labs(x = 'EMG signal, squeeze - rest',
       y = 'Pupil diameter (au),\nsqueeze - rest',
       colour = '') +
  theme_pubr() + theme_font_pub

fig_handgrip_EMG_HR <- ggplot(data = summary_hg_arousal,
                              aes(x = EMG_diff, y = HR_diff)) +
  geom_point(aes(colour = group), alpha = 0.5) +
  stat_cor(method = 'pearson', label.y = 14,
           cor.coef.name = 'r',
           p.accuracy = 0.001, r.accuracy = 0.001) +
  stat_smooth(colour = 'darkgray', method = 'lm', se = TRUE) +
  scale_colour_manual(values = cols_group) +
  labs(x = 'EMG signal, squeeze - rest',
       y = 'Heart rate (beats/min),\nsqueeze - rest',
       colour = '') +
  theme_pubr() + theme_font_pub

fig_handgrip_EMG_aSKNA <- ggplot(data = summary_hg_arousal,
                                 aes(x = EMG_diff, y = aSKNA_diff)) +
  geom_point(aes(colour = group), alpha = 0.5) +
  stat_cor(method = 'pearson', 
           cor.coef.name = 'r',
           p.accuracy = 0.001, r.accuracy = 0.001) +
  stat_smooth(colour = 'darkgray', method = 'lm', se = TRUE) +
  scale_colour_manual(values = cols_group) +
  labs(x = 'EMG signal, squeeze - rest',
       y = 'Sympathetic tone (microvolts),\nsqueeze - rest',
       colour = '') +
  theme_pubr() + theme_font_pub


# figure 2: arousal during handgrip ---------------------------------------

# figure2A: arousal during handgrip/control protocols
figure2A <- ggarrange(fig_handgrip_pupil,
                      fig_handgrip_HR,
                      fig_handgrip_aSKNA,
                      nrow = 3, ncol = 1,
                      align = 'v',
                      labels = c('A', '', ''),
                      common.legend = TRUE,
                      legend = 'none')

# figure2B: handgrip strength vs. arousal increases
figure2B <- ggarrange(fig_handgrip_EMG_pupil,
                      fig_handgrip_EMG_HR,
                      fig_handgrip_EMG_aSKNA,
                      nrow = 3, ncol = 1,
                      align = 'v',
                      labels = c('B', '', ''),
                      common.legend = TRUE,
                      legend = 'none')

# combine figure2A and figure2B
figure2 <- ggarrange(figure2A, 
                     figure2B,
                     nrow = 1, ncol = 2,
                     widths = c(1.3, 1))

ggsave(here('figures', 'figure2b_handgrip-arousal_hg-only.png'), 
       figure2B,
       device = 'png', dpi = fig_dpi, bg = 'white',
       width = 5, height = 11)

# extract legend
legend <- cowplot::get_legend(fig_handgrip_pupil)

# combine legend & figure2
figure2 <- ggarrange(
  legend, figure2,
  heights = c(.05, 1),
  nrow = 2
)

# save figure
ggsave(here('figures', 'figure2_handgrip-arousal.png'), 
       figure2,
       device = 'png', dpi = fig_dpi, bg = 'white',
       width = 10, height = 11)


# tests of associations between EMG and arousal measures ------------------

# perform pearson correlation analyses
cor.EMG_pupil <- cor.test(~pupil_diff + EMG_diff, data = summary_hg_arousal, method = 'pearson')
cor.EMG_HR <- cor.test(~HR_diff + EMG_diff, data = summary_hg_arousal, method = 'pearson')
cor.EMG_aSKNA <- cor.test(~aSKNA_diff + EMG_diff, data = summary_hg_arousal, method = 'pearson')

# arrange correlation results in a dataframe
cor.EMG_results <- data.frame(measure = c('pupil', 'HR', 'aSKNA'),
           r = c(cor.EMG_pupil$estimate, cor.EMG_HR$estimate, cor.EMG_aSKNA$estimate),
           df = c(cor.EMG_pupil$parameter, cor.EMG_HR$parameter, cor.EMG_aSKNA$parameter),
           p = c(cor.EMG_pupil$p.value, cor.EMG_HR$p.value, cor.EMG_aSKNA$p.value)
)





