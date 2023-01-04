# this script visualizes & analyses pupil diameter data
# from pre-block auditory n-back fixation periods
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# summarize pupil diameter during n-back fixations ------------------------

summary_nb_pupil_fixation <- data_ET_nback %>%
  
  # exclude practice run
  filter(!run == 0) %>%
  
  # include only fixation periods
  filter(event == 'initial_fixation') %>%
  
  # apply missingness threshold to individual segments
  mutate(pupil_mean = ifelse(frac_missing > thresh_nbfix, NA, pupil_mean)) %>%
  
  # exclude participants with >thresh missingness as determined above
#  filter(! label_subject %in% ET_missing_nbfix$label_subject) %>%
  
  # relabel pupil_mean column and remove irrelevant columns
  rename(pupil_nbfix = pupil_mean) %>%
  select(-pupil_sd, -pupil_min, -pupil_min, -pupil_max, -frac_missing) %>%
  
  # join baseline data to be used for correction
  left_join(pupil_norm_baseline %>% 
              select(label_subject, hemi, pupil_baseline),
            by = c('label_subject', 'hemi')) %>%
 
  # perform baseline correction
  rowwise() %>%
  mutate(pupil_nbfix_corr = pupil_nbfix - pupil_baseline) %>%
  
  # average over hemispheres
  group_by(label_subject, run, block) %>%
  summarize(pupil_nbfix_corr = mean(pupil_nbfix_corr, na.rm = TRUE)) %>%
  
  # join subject info
  left_join(data_subjects %>% select(label_subject, age_group, group),
            by = 'label_subject') %>%
  
  # recode block to reflect position wrt handgrip 
  rowwise() %>%
  mutate(block_recoded = ifelse(age_group == 'YA' & block %in% c(1, 5, 9), 1, 
                                ifelse(age_group == 'YA' & block %in% c(2, 6, 10), 2,
                                       ifelse(age_group == 'YA' & block %in% c(3, 7, 11), 3,
                                              ifelse(age_group == 'YA' & block %in% c(4, 8, 12), 4,
                                                     ifelse(age_group == 'OA' & block %in% c(1, 4, 7), 1,
                                                            ifelse(age_group == 'OA' & block %in% c(2, 5, 8), 2,
                                                                   ifelse(age_group == 'OA' & block %in% c(3, 6, 9), 3, NA)))))))) %>%
  # select only relevant columns
  select(label_subject, age_group, group, run, block, block_recoded, pupil_nbfix_corr)

summary_nb_pupil_fixation[summary_nb_pupil_fixation == 'NaN'] <- NA


# figure: pupil diameter during n-back fixations, averaged over runs --------

# average over runs
summary_nb_pupil_fixation_fig <- summary_nb_pupil_fixation %>%
  # make group values uppercase 
  mutate(group = str_to_title(group)) %>%
  group_by(label_subject, age_group, group, block_recoded) %>%
  summarize(pupil_nbfix_corr = mean(pupil_nbfix_corr, na.rm = TRUE))
summary_nb_pupil_fixation_fig[summary_nb_pupil_fixation_fig == 'NaN'] <- NA

# set factor levels for plotting
summary_nb_pupil_fixation_fig$group <- factor(summary_nb_pupil_fixation_fig$group, levels = c('Handgrip', 'Control'))
summary_nb_pupil_fixation_fig$age_group[summary_nb_pupil_fixation_fig$age_group == 'YA'] <- 'Younger'
summary_nb_pupil_fixation_fig$age_group[summary_nb_pupil_fixation_fig$age_group == 'OA'] <- 'Older'
summary_nb_pupil_fixation_fig$age_group <- factor(summary_nb_pupil_fixation_fig$age_group, levels = c('Younger', 'Older'))

# create figure
fig_nback_pupil_fixations <- ggplot(data = summary_nb_pupil_fixation_fig, 
                                           aes(x = block_recoded, y = pupil_nbfix_corr, 
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
  labs(x = 'n-back block, relative to handgrip offset', 
       y = 'Pupil diameter (au)\nrelative to baseline', 
       group = '', colour = '', fill = '') +
  facet_wrap(~age_group, nrow = 1) +
  theme_pubr() + theme_font_pub +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# summarize pupil diameter across runs and blocks -------------------------

summary_nb_pupil_fixation_avg <- summary_nb_pupil_fixation %>%
  group_by(label_subject, group, age_group, block_recoded) %>%
  summarize(pupil_nbfix_corr = mean(pupil_nbfix_corr, na.rm = TRUE))
summary_nb_pupil_fixation_avg[summary_nb_pupil_fixation_avg == 'NaN'] <- NA


# ANOVA: pupil diameter during n-back fixations ---------------------------

# set factors for ANOVA
summary_nb_pupil_fixation_avg$label_subject <- factor(summary_nb_pupil_fixation_avg$label_subject)
summary_nb_pupil_fixation_avg$group <- factor(summary_nb_pupil_fixation_avg$group, levels = c('handgrip', 'control'))
summary_nb_pupil_fixation_avg$block_recoded <- factor(summary_nb_pupil_fixation_avg$block_recoded, levels = c(1, 2, 3, 4))


aov.nb_pupil_fix_YA <- aov_ez(id = 'label_subject',
                              dv = 'pupil_nbfix_corr',
                              data = summary_nb_pupil_fixation_avg %>% filter(age_group == 'YA'),
                              within = 'block_recoded',
                              between = 'group',
                              type = 3,
                              include_aov = TRUE,
                              factorize = TRUE,
                              anova_table = list(es = "pes"))

aov.nb_pupil_fix_OA <- aov_ez(id = 'label_subject',
                              dv = 'pupil_nbfix_corr',
                              data = summary_nb_pupil_fixation_avg %>% filter(age_group == 'OA'),
                              within = 'block_recoded',
                              between = 'group',
                              type = 3,
                              include_aov = TRUE,
                              factorize = TRUE,
                              anova_table = list(es = "pes"))


# planned pairwise comparisons --------------------------------------------

# return estimated marginal means
emm.nb_pupil_fix_YA <- emmeans(aov.nb_pupil_fix_YA, 
                               ~ group | block_recoded)
emm.nb_pupil_fix_OA <- emmeans(aov.nb_pupil_fix_OA, 
                               ~ group | block_recoded)

# pairwise comparisons
contrasts.nb_pupil_fix_YA <- pairs(emm.nb_pupil_fix_YA,
                            # reverse contrasts to get handgrip-control contrast
                            reverse = TRUE,
                            adjust = 'bonferroni') %>% 
  as.data.frame() %>%
  rowwise() %>%
  mutate(Contrast = 'Handgrip - Control') %>%
  mutate(`Age group` = 'Younger', .before = contrast) %>%
  mutate('Block, relative to handgrip offset' = str_split(block_recoded, 'X')[[1]][2]) %>%
  select(`Age group`,
         Contrast,
         'Block, relative to handgrip offset',
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value)

contrasts.nb_pupil_fix_OA <- pairs(emm.nb_pupil_fix_OA,
                                   # reverse contrasts to get handgrip-control contrast
                                   reverse = TRUE,
                                   adjust = 'bonferroni') %>% 
  as.data.frame() %>%
  rowwise() %>%
  mutate(Contrast = 'Handgrip - Control') %>%
  mutate(`Age group` = 'Older', .before = contrast) %>%
  mutate('Block, relative to handgrip offset' = str_split(block_recoded, 'X')[[1]][2]) %>%
  select(`Age group`,
         Contrast,
         'Block, relative to handgrip offset',
         Estimate = estimate,
         SE, df, 
         t = t.ratio,
         p = p.value)

# join comparison results together
contrasts.nb_pupil_fix <-
  as.data.frame(
    rbind(
      contrasts.nb_pupil_fix_YA,
      contrasts.nb_pupil_fix_OA
    )
  )


# analyze test-retest reliability across runs -----------------------------

# organize data for ICC calculations
icc_nb_pupil_fixation <- summary_nb_pupil_fixation %>%
  group_by(label_subject, group, age_group, run) %>%
  summarize(pupil_nbfix_corr = mean(pupil_nbfix_corr, na.rm = TRUE))
icc_nb_pupil_fixation[icc_nb_pupil_fixation == 'NaN'] <- NA

# split data into rest and squeeze
# and reshape for ICC calculation
icc_nb_pupil_fixation <- icc_nb_pupil_fixation %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'pupil_nbfix_corr') %>%
  ungroup() %>%
  select(run1, run2, run3) %>%
  filter(!is.na(run1) & !is.na(run2) & !is.na(run3))

# calculate ICCs
icc_nb_pupil_fixation_estimate <- irr::icc(icc_nb_pupil_fixation,
                                       type = "agreement",
                                       model = "twoway",
                                       unit = "average")


# arrange tables containing ICC estimates and F-test results
icc_nb_pupil_fixation_results <- as.data.frame(
  cbind(c('Mean pupil diameter during n-back fixation periods'),
        rbind(extract_icc_results(icc_nb_pupil_fixation_estimate))
  ))
names(icc_nb_pupil_fixation_results) <- c('Measure', 'ICC', '95% CI', 'F (df1, df2)', 'p')  
