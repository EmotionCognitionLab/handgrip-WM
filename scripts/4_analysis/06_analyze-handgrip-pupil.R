# this script visualizes & analyzes pupil diameter data during handgrip
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# summarize pupil during handgrip -----------------------------------------

summary_hg_pupil <- data_ET_handgrip %>%
  
  # don't include initial rest data
  filter(!event == 'initial_rest') %>%
  rowwise() %>%
  
  # apply missingness threshold to squeeze/rest data
  #mutate(pupil_mean = ifelse(frac_missing > thresh, NA, pupil_mean)) %>%
  mutate(pupil_mean = ifelse(frac_missing > thresh_handgrip, NA, pupil_mean)) %>%
  
  # join data for baseline correction
  left_join(pupil_norm_baseline, 
            by = c('label_subject', 'hemi')) %>%
  rowwise() %>%
  mutate(pupil_corr = pupil_mean - pupil_baseline) %>%
  
  # average over left/right eyes
  group_by(label_subject, run, event, round) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE)) %>%
  
  # handle NaNs from completely missing data
  mutate(pupil_corr = ifelse(pupil_corr == 'NaN', NA, pupil_corr)) %>%
  
  # bind subject info
  left_join(data_subjects %>%
              select(label_subject, group, age_group), 
            by = 'label_subject') %>%
  
  # select only relevant columns
  select(label_subject, group, age_group, run, event, round, pupil_corr)


# figure: pupil during handgrip, averaged over runs -----------------------

# average pupil values over handgrip runs
summary_hg_pupil_fig <- summary_hg_pupil %>%
  group_by(label_subject, group, age_group, event, round) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE))

# make first letters of relevant labels uppercase
summary_hg_pupil_fig <- summary_hg_pupil_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group),
         event = str_to_title(event))

# add 'part' variable combining event and round
summary_hg_pupil_fig <- summary_hg_pupil_fig %>%
  rowwise() %>%
  mutate(part = str_c(event, round, sep = ''))

# set factor levels for figure
summary_hg_pupil_fig$age_group[summary_hg_pupil_fig$age_group == 'YA'] <- 'Younger'
summary_hg_pupil_fig$age_group[summary_hg_pupil_fig$age_group == 'OA'] <- 'Older'
summary_hg_pupil_fig$age_group <- factor(summary_hg_pupil_fig$age_group, levels = c('Younger', 'Older'))
summary_hg_pupil_fig$group <- factor(summary_hg_pupil_fig$group, levels = c('Handgrip', 'Control'))
summary_hg_pupil_fig$part <- factor(summary_hg_pupil_fig$part, levels = c('Squeeze1', 'Rest1', 'Squeeze2', 'Rest2', 'Squeeze3', 'Rest3', 'Squeeze4', 'Rest4'))

fig_handgrip_pupil <- ggplot(data = summary_hg_pupil_fig, 
                                    aes(x = part, y = pupil_corr, 
                                        colour = factor(group), 
                                        group = factor(group))) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2), alpha = 0.2) +
  stat_summary(position = position_dodge(width = 0.2)) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.2)) + 
  scale_colour_manual(values = cols_group) +
  labs(x = '', y = 'Pupil diameter (au)', colour = '') +
  facet_wrap(~age_group) +
  theme_pubr() + theme_rotatex + theme_font_pub


# summarize pupil during handgrip, averaging over runs and rounds ---------

summary_hg_pupil_avg <- summary_hg_pupil %>%
  group_by(label_subject, group, age_group, event) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE)) %>%
  mutate(pupil_corr = ifelse(pupil_corr == 'NaN', NA, pupil_corr))


# normality check ---------------------------------------------------------

sw.hg_pupil <- summary_hg_pupil_avg %>%
  group_by(event) %>%
  rstatix::shapiro_test(pupil_corr)


# ANOVA: pupil diameter during handgrip -----------------------------------

# set factor levels for ANOVA
summary_hg_pupil_avg$group <- factor(summary_hg_pupil_avg$group, levels = c('handgrip', 'control'))
summary_hg_pupil_avg$age_group <- factor(summary_hg_pupil_avg$age_group, levels = c('YA', 'OA'))
summary_hg_pupil_avg$event <- factor(summary_hg_pupil_avg$event, levels = c('squeeze', 'rest'))
summary_hg_pupil_avg$label_subject <- factor(summary_hg_pupil_avg$label_subject)

# rmANOVA testing effects of event, group, age group, and their interactions on pupil
aov.hg_pupil <- aov_ez(id = 'label_subject', 
                     dv = 'pupil_corr', 
                     data = summary_hg_pupil_avg, 
                     within = 'event',
                     between = c('group', 'age_group'),
                     type = 3, 
                     anova_table = list(es = "pes"),
                     include_aov = TRUE)


# planned pairwise comparisons --------------------------------------------

# return estimated marginal means
emm.hg_pupil <- emmeans(aov.hg_pupil,
                        ~event | age_group * group)

# pairwise comparisons with bonferroni correction
contrasts.hg_pupil <- emm.hg_pupil %>%
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


# analyze test-retest reliability across runs -----------------------------

# organize data for ICC calculations
# (averaging first over rounds, to get single squeeze/rest estimate per run)
icc_hg_pupil <- summary_hg_pupil %>%
  group_by(label_subject, group, age_group, run, event) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE))
icc_hg_pupil[icc_hg_pupil == 'NaN'] <- NA

# split data into rest and squeeze
# and reshape for ICC calculation
icc_hg_pupil_rest <- icc_hg_pupil %>%
  filter(event == 'rest') %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'pupil_corr') %>%
  ungroup() %>%
  select(run1, run2, run3) %>%
  filter(!is.na(run1) & !is.na(run2) & !is.na(run3))

icc_hg_pupil_squeeze <- icc_hg_pupil %>%
  filter(event == 'squeeze') %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'pupil_corr') %>%
  ungroup() %>%
  select(run1, run2, run3) %>%
  filter(!is.na(run1) & !is.na(run2) & !is.na(run3))

# calculate ICCs: rest
icc_hg_pupil_rest_estimate <- irr::icc(icc_hg_pupil_rest,
                                        type = "agreement",
                                        model = "twoway",
                                        unit = "average")

icc_hg_pupil_squeeze_estimate <- irr::icc(icc_hg_pupil_squeeze,
                                       type = "agreement",
                                       model = "twoway",
                                       unit = "average")

# function to extract ICC info
extract_icc_results <- function(icc_output) {
  ICC = round(icc_output$value, 3)
  CI = paste(round(icc_output$lbound, 3), ' - ', round(icc_output$ubound, 3), sep = '')
  Fval = paste('F(', icc_output$df1, 
              ', ', 
              round(icc_output$df2, 1), 
              ') = ', 
              round(icc_output$Fvalue, 2),
              sep = '')
  p = fix_pval_table(icc_output$p.value)
  
  return(c(ICC, CI, Fval, p))
}

# arrange tables containing ICC estimates and F-test results
icc_hg_pupil_results <- as.data.frame(
  cbind(c('Rest', 'Squeeze'),
  rbind(extract_icc_results(icc_hg_pupil_rest_estimate),
        extract_icc_results(icc_hg_pupil_squeeze_estimate))
))
names(icc_hg_pupil_results) <- c('Event', 'ICC', '95% CI', 'F (df1, df2)', 'p')



# analyze baseline differences in pupil diameter --------------------------

summary_agegr_baseline_pupil <- pupil_norm_baseline %>%
  
  # average over hemispheres
  group_by(label_subject) %>%
  summarize(pupil_baseline = mean(pupil_baseline, na.rm = TRUE)) %>%
  
  # bind subject info
  left_join(data_subjects %>%
              select(label_subject, age_group), 
            by = 'label_subject')

ttest_agegr_baseline_pupil <- t.test(pupil_baseline ~ age_group, data = summary_agegr_baseline_pupil)
