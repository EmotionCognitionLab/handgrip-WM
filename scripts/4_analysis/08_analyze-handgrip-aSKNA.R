# this script visualizes & analyzes aSKNA data during handgrip
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# summarize aSKNA during handgrip -----------------------------------------

summary_hg_aSKNA <- data_aSKNA_handgrip %>%

  # don't include initial rest data
  filter(!event == 'initial_rest') %>%
  
  # join data for baseline correction
  left_join(data_aSKNA_baseline %>% 
              select(label_subject, aSKNA_baseline = mean_iSKNA), 
            by = 'label_subject') %>%
  
  # baseline correct aSKNA values
  rowwise() %>%
  rename(aSKNA = mean_iSKNA) %>%
  mutate(aSKNA_corr = aSKNA - aSKNA_baseline) %>%
  
  # join subject info
  left_join(data_subjects %>%
              select(label_subject, age_group, group),
            by = 'label_subject') %>%
  select(label_subject, age_group, group, run, event, round, 
         aSKNA, aSKNA_baseline, aSKNA_corr)


# figure: aSKNA during handgrip, averaged over runs -----------------------

# average aSKNA values over handgrip runs
summary_hg_aSKNA_fig <- summary_hg_aSKNA %>%
group_by(label_subject, group, age_group, event, round) %>%
summarize(aSKNA_corr = mean(aSKNA_corr))

# make first letters of relevant labels uppercase
summary_hg_aSKNA_fig <- summary_hg_aSKNA_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group),
         event = str_to_title(event))

# add 'part' variable combining event and round
summary_hg_aSKNA_fig <- summary_hg_aSKNA_fig %>%
  rowwise() %>%
  mutate(part = str_c(event, round, sep = ''))
  
# set factor levels for plotting
summary_hg_aSKNA_fig$age_group[summary_hg_aSKNA_fig$age_group == 'YA'] <- 'Younger'
summary_hg_aSKNA_fig$age_group[summary_hg_aSKNA_fig$age_group == 'OA'] <- 'Older'
summary_hg_aSKNA_fig$age_group <- factor(summary_hg_aSKNA_fig$age_group, levels = c('Younger', 'Older'))
summary_hg_aSKNA_fig$group <- factor(summary_hg_aSKNA_fig$group, levels = c('Handgrip', 'Control'))
summary_hg_aSKNA_fig$part <- factor(summary_hg_aSKNA_fig$part, levels = c('Squeeze1', 'Rest1', 'Squeeze2', 'Rest2', 'Squeeze3', 'Rest3', 'Squeeze4', 'Rest4'))

# create figure
fig_handgrip_aSKNA <- ggplot(data = summary_hg_aSKNA_fig,
                                    aes(x = part, y = aSKNA_corr, colour = group, group = group)) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2), alpha = 0.2) +
  stat_summary(position = position_dodge(width = 0.2)) +
  stat_summary(position = position_dodge(width = 0.2), geom = 'line') + 
  scale_colour_manual(values = cols_group) +
  labs(x = '', y = 'Sympathetic tone (microvolts)', colour = '') +
  facet_wrap(~age_group) +
  theme_pubr() + theme_rotatex + theme_font_pub


# summarize aSKNA during handgrip, averaging over runs & rounds -----------

summary_hg_aSKNA_avg <- summary_hg_aSKNA %>%
  group_by(label_subject, age_group, group, event) %>%
  summarize(aSKNA_corr = mean(aSKNA_corr, na.rm = TRUE)) %>%
  mutate(aSKNA_corr = ifelse(aSKNA_corr == 'NaN', NA, aSKNA_corr))


# normality check ---------------------------------------------------------

sw.hg_aSKNA <- summary_hg_aSKNA_avg %>%
  group_by(event) %>%
  rstatix::shapiro_test(aSKNA_corr)


# ANOVA: aSKNA during handgrip --------------------------------------------

# set factor levels for ANOVA
summary_hg_aSKNA_avg$group <- factor(summary_hg_aSKNA_avg$group, levels = c('handgrip', 'control'))
summary_hg_aSKNA_avg$age_group <- factor(summary_hg_aSKNA_avg$age_group, levels = c('YA', 'OA'))
summary_hg_aSKNA_avg$event <- factor(summary_hg_aSKNA_avg$event, levels = c('squeeze', 'rest'))
summary_hg_aSKNA_avg$label_subject <- factor(summary_hg_aSKNA_avg$label_subject)

aov.hg_aSKNA <- aov_ez(id = 'label_subject', 
                    dv = 'aSKNA_corr', 
                    data = summary_hg_aSKNA_avg, 
                    within = 'event',
                    between = c('group', 'age_group'),
                    type = 3,
                    include_aov = TRUE,
                    factorize = TRUE,
                    anova_table = list(es = "pes"))


# planned pairwise comparisons --------------------------------------------

# return estimated marginal means
emm.hg_aSKNA <- emmeans(aov.hg_aSKNA,
                     ~event | group * age_group)

# pairwise comparisons with bonferroni correction
contrasts.hg_aSKNA <- emm.hg_aSKNA %>%
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
icc_hg_aSKNA <- summary_hg_aSKNA %>%
  group_by(label_subject, group, age_group, run, event) %>%
  summarize(aSKNA_corr = mean(aSKNA_corr, na.rm = TRUE))
icc_hg_aSKNA[icc_hg_aSKNA == 'NaN'] <- NA

# split data into rest and squeeze
# and reshape for ICC calculation
icc_hg_aSKNA_rest <- icc_hg_aSKNA %>%
  filter(event == 'rest') %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'aSKNA_corr') %>%
  ungroup() %>%
  select(run1, run2, run3) %>%
  filter(!is.na(run1) & !is.na(run2) & !is.na(run3))

icc_hg_aSKNA_squeeze <- icc_hg_aSKNA %>%
  filter(event == 'squeeze') %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'aSKNA_corr') %>%
  ungroup() %>%
  select(run1, run2, run3) %>%
  filter(!is.na(run1) & !is.na(run2) & !is.na(run3))

# calculate ICCs: rest
icc_hg_aSKNA_rest_estimate <- irr::icc(icc_hg_aSKNA_rest,
                                    type = "agreement",
                                    model = "twoway",
                                    unit = "average")

icc_hg_aSKNA_squeeze_estimate <- irr::icc(icc_hg_aSKNA_squeeze,
                                       type = "agreement",
                                       model = "twoway",
                                       unit = "average")

# arrange tables containing ICC estimates and F-test results
icc_hg_aSKNA_results <- as.data.frame(
  cbind(c('Rest', 'Squeeze'),
        rbind(extract_icc_results(icc_hg_aSKNA_rest_estimate),
              extract_icc_results(icc_hg_aSKNA_squeeze_estimate))
  ))
names(icc_hg_aSKNA_results) <- c('Event', 'ICC', '95% CI', 'F (df1, df2)', 'p')


# join all test-retest reliability tables ---------------------------------

icc_hg_results <- as.data.frame(
  cbind(c(rep('Mean pupil diameter',2),
          rep('Mean heart rate', 2),
          rep('Mean sympathetic tone', 2)),
  rbind(icc_hg_pupil_results,
        icc_hg_HR_results,
        icc_hg_aSKNA_results)
))
names(icc_hg_results)[1] <- 'Measure'


# analyze baseline differences in heart rate ------------------------------

summary_agegr_baseline_aSKNA <- data_aSKNA_baseline %>%
  
  # average over hemispheres
  group_by(label_subject) %>%
  summarize(aSKNA_baseline = mean(mean_iSKNA, na.rm = TRUE)) %>%
  
  # bind subject info
  left_join(data_subjects %>%
              select(label_subject, age_group), 
            by = 'label_subject')

ttest_agegr_baseline_aSKNA <- t.test(aSKNA_baseline ~ age_group, data = summary_agegr_baseline_aSKNA)
