# this script visualizes & analyzes heart rate data during handgrip
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# summarize HR during handgrip --------------------------------------------

summary_hg_HR <- data_HR_handgrip %>%
  
  # don't include initial rest data
  filter(!event == 'initial_rest') %>%
  
  # join data for baseline correction
  left_join(data_HR_baseline %>%
              select(label_subject, niHR_baseline = mean_niHR),
            by = 'label_subject') %>%
  mutate(niHR_corr = mean_niHR - niHR_baseline) %>%
  
  # bind subject info
  left_join(data_subjects %>% 
              select(label_subject, group, age_group),
            by = 'label_subject') %>%
  
  # select only relevant columns
  select(label_subject, group, age_group, run, event, round, niHR = mean_niHR, niHR_corr)


# figure: HR during handgrip, averaged over runs --------------------------

# average HR values over handgrip runs
summary_hg_HR_fig <- summary_hg_HR %>%
  group_by(label_subject, group, age_group, event, round) %>%
  summarize(niHR_corr = mean(niHR_corr))

# make first letters of relevant labels uppercase
summary_hg_HR_fig <- summary_hg_HR_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group),
         event = str_to_title(event))

# add 'part' variable combining event and round
summary_hg_HR_fig <- summary_hg_HR_fig %>%
  rowwise() %>%
  mutate(part = str_c(event, round, sep = ''))

# set factor levels for plotting
summary_hg_HR_fig$age_group[summary_hg_HR_fig$age_group == 'YA'] <- 'Younger'
summary_hg_HR_fig$age_group[summary_hg_HR_fig$age_group == 'OA'] <- 'Older'
summary_hg_HR_fig$age_group <- factor(summary_hg_HR_fig$age_group, levels = c('Younger', 'Older'))
summary_hg_HR_fig$group <- factor(summary_hg_HR_fig$group, levels = c('Handgrip', 'Control'))
summary_hg_HR_fig$part <- factor(summary_hg_HR_fig$part, levels = c('Squeeze1', 'Rest1', 'Squeeze2', 'Rest2', 'Squeeze3', 'Rest3', 'Squeeze4', 'Rest4'))

# create the figure
fig_handgrip_HR <- ggplot(data = summary_hg_HR_fig,
                                 aes(x = part, y = niHR_corr, 
                                     colour = group, 
                                     group = group)) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2), alpha = 0.2) +
  stat_summary(position = position_dodge(width = 0.2)) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.2)) + 
  scale_colour_manual(values = cols_group) +
  labs(x = '', y = 'Heart rate (beats/minute)', 
       colour = '') +
  facet_wrap(~age_group) +
  theme_pubr() + theme_rotatex + theme_font_pub


# summarize HR during handgrip, averaging over runs & rounds --------------

summary_hg_HR_avg <- summary_hg_HR %>%
  group_by(label_subject, group, age_group, event) %>%
  summarize(niHR_corr = mean(niHR_corr, na.rm = TRUE)) %>%
  mutate(niHR_corr = ifelse(niHR_corr == 'NaN', NA, niHR_corr))


# normality check ---------------------------------------------------------

sw.hg_HR <- summary_hg_HR_avg %>%
  group_by(event) %>%
  rstatix::shapiro_test(niHR_corr)


# ANOVA: HR during handgrip -----------------------------------------------

# set factor levels for ANOVA
summary_hg_HR_avg$group <- factor(summary_hg_HR_avg$group, levels = c('handgrip', 'control'))
summary_hg_HR_avg$age_group <- factor(summary_hg_HR_avg$age_group, levels = c('YA', 'OA'))
summary_hg_HR_avg$event <- factor(summary_hg_HR_avg$event, levels = c('squeeze', 'rest'))
summary_hg_HR_avg$label_subject <- factor(summary_hg_HR_avg$label_subject)

# rmANOVA testing effects of event, group, age group, and their interactions on HR
aov.hg_HR <- aov_ez(id = 'label_subject', 
                       dv = 'niHR_corr', 
                       data = summary_hg_HR_avg, 
                       within = 'event',
                       between = c('group', 'age_group'),
                       type = 3,
                       include_aov = TRUE,
                       factorize = TRUE,
                       anova_table = list(es = "pes"))


# planned pairwise comparisons --------------------------------------------

# return estimated marginal means
emm.hg_HR <- emmeans(aov.hg_HR,
                     ~event | group * age_group)

# pairwise comparisons with bonferroni correction
contrasts.hg_HR <- emm.hg_HR %>%
  contrast('pairwise') %>%
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame() %>%
  mutate(Contrast = 'Squeeze - Rest') %>%
  mutate(`Age group` = ifelse(age_group == 'YA', 'Younger',
                              ifelse(age_group == 'OA', 'Older', NA)),
         Group = str_to_title(group)) %>%
  select(`Age group`,
         Group,
         Contrast,
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value) %>%
  # format p-values for table
  mutate(p = fix_pval_table(p)) %>%
  arrange(desc(`Age group`), desc(Group))
