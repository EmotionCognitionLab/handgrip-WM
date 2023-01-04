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
  stat_cor(method = 'pearson', label.y = 2.1,
           cor.coef.name = 'r',
           r.accuracy = 0.001,
           p.digits = 2) +
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
           r.accuracy = 0.001,
           p.accuracy = 0.001) +
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
           r.accuracy = 0.001,
           p.accuracy = 0.001) +
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


# summarize EMG/arousal increases for rmcorr ------------------------------

# for each arousal measure,
# compute each participant's difference between squeeze & rest
# for each round of each handgrip run
summary_hg_arousal_rmcorr <- as.data.frame(
  left_join(
    ### pupil
    summary_hg_pupil %>%
      # average over rounds & runs
      group_by(label_subject, event, run, round) %>%
      summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE)) %>%
      # convert from long to wide
      pivot_wider(names_from = 'event', names_prefix = 'pupil_', values_from = pupil_corr) %>%
      # calculate squeeze vs. rest difference
      rowwise() %>%
      mutate(pupil_diff = pupil_squeeze - pupil_rest) %>%
      select(label_subject, run, round, pupil_squeeze, pupil_rest, pupil_diff),
    
    ### heart rate
    summary_hg_HR %>%
      # average over rounds & runs
      group_by(label_subject, event, run, round) %>%
      summarize(niHR_corr = mean(niHR_corr, na.rm = TRUE)) %>%
      # convert from long to wide
      pivot_wider(names_from = 'event', names_prefix = 'HR_', values_from = niHR_corr) %>%
      # calculate squeeze vs. rest difference
      rowwise() %>%
      mutate(HR_diff = HR_squeeze - HR_rest) %>%
      select(label_subject, run, round, HR_squeeze, HR_rest, HR_diff),
    by = c('label_subject', 'run', 'round')
  ) %>% left_join(
    ### aSKNA
    summary_hg_aSKNA %>%
      # average over rounds & runs
      group_by(label_subject, event, run, round) %>%
      summarize(aSKNA_corr = mean(aSKNA_corr, na.rm = TRUE)) %>%
      # convert from long to wide
      pivot_wider(names_from = 'event', names_prefix = 'aSKNA_', values_from = aSKNA_corr) %>%
      # calculate squeeze vs. rest difference
      rowwise() %>%
      mutate(aSKNA_diff = aSKNA_squeeze - aSKNA_rest) %>%
      select(label_subject, run, round, aSKNA_squeeze, aSKNA_rest, aSKNA_diff),
    by = c('label_subject', 'run', 'round')
  ) %>% left_join(
    ### EMG
    summary_hg_EMG %>%
      # average over rounds & runs
      group_by(label_subject, event, run, round) %>%
      summarize(emg_corr = mean(emg_corr, na.rm = TRUE)) %>%
      # convert from long to wide
      pivot_wider(names_from = 'event', names_prefix = 'EMG_', values_from = emg_corr) %>%
      # calculate squeeze vs. rest difference
      rowwise() %>%
      mutate(EMG_diff = EMG_squeeze - EMG_rest) %>%
      select(label_subject, run, round, EMG_squeeze, EMG_rest, EMG_diff),
    by = c('label_subject', 'run', 'round')
  ) %>% left_join(
    data_subjects %>% select(label_subject, group, age_group),
    by = 'label_subject'
  )
) %>%
  select(label_subject, group, age_group, run, round, pupil_squeeze:EMG_diff)
summary_hg_arousal_rmcorr[summary_hg_arousal_rmcorr == 'NaN'] <- NA


# rm correlations: associations between EMG and arousal increases ---------

# define isa() function to handle rmcorr package's build for old version of R
isa <- function(x, what) {
  if (what == 'character') {
    return(is.character(x))
  }
}

# filter data to include only handgrip group for these analyses
data_rmcorr <- summary_hg_arousal_rmcorr %>%
  filter(group == 'handgrip') 

# make subject label a character variable
data_rmcorr$label_subject <- as.character(data_rmcorr$label_subject)

# rmcorr analysis: EMG change vs. pupil change
rmcorr.EMG_pupil <- rmcorr(participant = label_subject,
                           measure1 = EMG_diff,
                           measure2 = pupil_diff,
                           dataset = data_rmcorr,
                           CIs = 'bootstrap',
                           nreps = 1000,
                           bstrap.out = F)

# rmcorr analysis: EMG change vs. HR change
rmcorr.EMG_HR <- rmcorr(participant = label_subject,
                        measure1 = EMG_diff,
                        measure2 = HR_diff,
                        dataset = data_rmcorr,
                        CIs = 'bootstrap',
                        nreps = 1000,
                        bstrap.out = F)

# rmcorr analysis: EMG change vs. aSKNA change
rmcorr.EMG_aSKNA <- rmcorr(participant = label_subject,
                           measure1 = EMG_diff,
                           measure2 = aSKNA_diff,
                           dataset = data_rmcorr,
                           CIs = 'bootstrap',
                           nreps = 1000,
                           bstrap.out = F)

# arrange rmcorr results in a dataframe
rmcorr.EMG_results <- data.frame(measure = c('pupil', 'HR', 'aSKNA'),
                                 r = c(rmcorr.EMG_pupil$r, 
                                       rmcorr.EMG_HR$r, rmcorr.EMG_aSKNA$r),
                                 df = c(rmcorr.EMG_pupil$df, 
                                        rmcorr.EMG_HR$df, rmcorr.EMG_aSKNA$df),
                                 p = c(rmcorr.EMG_pupil$p, 
                                       rmcorr.EMG_HR$p, rmcorr.EMG_aSKNA$p),
                                 ci_lower = c(rmcorr.EMG_pupil$CI[1], 
                                              rmcorr.EMG_HR$CI[1], rmcorr.EMG_aSKNA$CI[1]),
                                 ci_upper = c(rmcorr.EMG_pupil$CI[2], 
                                              rmcorr.EMG_HR$CI[2], rmcorr.EMG_aSKNA$CI[2])
)

# generate model predictions for plotting
data_preds_pupil <- data.frame(pupil_diff = stats::predict(rmcorr.EMG_pupil$model),
                               EMG_diff = rmcorr.EMG_pupil$model$model$Measure1,
                               label_subject = rmcorr.EMG_pupil$model$model$Participant)
data_preds_HR <- data.frame(HR_diff = stats::predict(rmcorr.EMG_HR$model),
                            EMG_diff = rmcorr.EMG_HR$model$model$Measure1,
                            label_subject = rmcorr.EMG_HR$model$model$Participant)
data_preds_aSKNA <- data.frame(aSKNA_diff = stats::predict(rmcorr.EMG_aSKNA$model),
                               EMG_diff = rmcorr.EMG_aSKNA$model$model$Measure1,
                               label_subject = rmcorr.EMG_aSKNA$model$model$Participant)


# figure: repeated measures correlations ----------------------------------

# plot model-predicted pupil_diff vs. EMG_diff
colourCount <- length(unique(data_rmcorr$label_subject))
getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, 'Set1'))
subject_pal <- getPalette(colourCount)

# create named vector such that IDs have the same color across plots
subject_pal <- setNames(subject_pal, unique(data_rmcorr$label_subject))

fig_handgrip_EMG_pupil_rmcorr <- ggplot(data = data_rmcorr,
                                        aes(x = EMG_diff, y = pupil_diff, 
                                            colour = label_subject, 
                                            group = label_subject)) +
  geom_point() +
  geom_line(data = data_preds_pupil,
            aes(x = EMG_diff, y = pupil_diff),
            alpha = 0.6) +
  annotate(x = -0.25, y = 3.25, geom = 'text',
           label = paste0('italic(r[m]) == ', printnum(rmcorr.EMG_pupil$r, digits=3)),
           parse = TRUE) +
  annotate(x = 1.22, y = 3.25, geom = 'text',
           label = paste0('italic(p) == ', printp(rmcorr.EMG_pupil$p)), 
           parse = TRUE) +
  labs(x = 'EMG signal, squeeze - rest',
       y = 'Pupil diameter (au),\nsqueeze - rest',
       colour = '') +
  scale_colour_manual(values = subject_pal) +
  theme_pubr() + theme_font_pub

# plot model-predicted HR_diff vs. EMG_diff
fig_handgrip_EMG_HR_rmcorr <- ggplot(data = data_rmcorr,
                                     aes(x = EMG_diff,y = HR_diff,
                                         colour = label_subject,
                                         group = label_subject)) +
  geom_point() +
  geom_line(data = data_preds_HR,
            aes(x = EMG_diff, y = HR_diff),
            alpha = 0.6) +
  annotate(x = -0.25, y = 38, geom = 'text',
           label = paste0('italic(r[m]) == ', printnum(rmcorr.EMG_HR$r, digits=3)),
           parse = TRUE) +
  annotate(x = 1.22, y = 38, geom = 'text',
           label = paste0('italic(p) == ', printp(rmcorr.EMG_HR$p)), 
           parse = TRUE) +
  labs(x = 'EMG signal, squeeze - rest',
       y = 'Heart rate (beats/min),\nsqueeze - rest',
       colour = '') +
  scale_colour_manual(values = subject_pal) +
  theme_pubr() + theme_font_pub

# plot model-predicted aSKNA_diff vs. EMG_diff
fig_handgrip_EMG_aSKNA_rmcorr <- ggplot(data = data_rmcorr,
                                        aes(x = EMG_diff, y = aSKNA_diff,
                                            colour = label_subject,
                                            group = label_subject)) +
  geom_point() +
  geom_line(data = data_preds_aSKNA,
            aes(x = EMG_diff, y = aSKNA_diff),
            alpha = 0.6) +
  annotate(x = -0.25, y = 0.022, geom = 'text',
           label = paste0('italic(r[m]) == ', printnum(rmcorr.EMG_aSKNA$r, digits=3)),
           parse = TRUE) +
  annotate(x = 1.22, y = 0.022, geom = 'text',
           label = paste0('italic(p) == ', printp(rmcorr.EMG_aSKNA$p)), 
           parse = TRUE) +
  labs(x = 'EMG signal, squeeze - rest',
       y = 'Sympathetic tone (microvolts),\nsqueeze - rest',
       colour = '') +
  scale_colour_manual(values = subject_pal) +
  theme_pubr() + theme_font_pub

# arrange all plots into composite figure
figureS2_rmcorr <- ggarrange(fig_handgrip_EMG_pupil_rmcorr,
                             fig_handgrip_EMG_HR_rmcorr,
                             fig_handgrip_EMG_aSKNA_rmcorr,
                             nrow = 3, ncol = 1,
                             align = 'v',
                             labels = c('A', 'B', 'C'),
                             common.legend = TRUE,
                             legend = 'none')

# save figure
ggsave(here('figures', 'figureS2_handgrip-arousal-rmcorr.png'), 
       figureS2_rmcorr,
       device = 'png', dpi = fig_dpi, bg = 'white',
       width = 4, height = 11)
