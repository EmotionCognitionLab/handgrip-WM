# this script visualizes & analyses heart rate data
# from auditory n-back blocks
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# organize n-back HR data -------------------------------------------------

summary_nb_HR <- data_HR_nback %>%
  
  # exclude practice trials
  filter(run != 0) %>%
  
  # only looking at trial periods
  filter(event == 'trial') %>%

  # join baseline data for baseline correction
  left_join(data_HR_baseline %>%
              select(label_subject, niHR_baseline = mean_niHR),
            by = 'label_subject') %>%
  mutate(niHR_corr = mean_niHR - niHR_baseline) %>%
  
  # bind subject info
  left_join(data_subjects %>%
              select(label_subject, group, age_group), 
            by = 'label_subject') %>%
  
  # recode block to reflect position wrt handgrip 
  rowwise() %>%
  mutate(block_recoded = ifelse(age_group == 'YA' & block %in% c(1, 5, 9), 1, 
                                ifelse(age_group == 'YA' & block %in% c(2, 6, 10), 2,
                                       ifelse(age_group == 'YA' & block %in% c(3, 7, 11), 3,
                                              ifelse(age_group == 'YA' & block %in% c(4, 8, 12), 4,
                                                     ifelse(age_group == 'OA' & block %in% c(1, 4, 7), 1,
                                                            ifelse(age_group == 'OA' & block %in% c(2, 5, 8), 2,
                                                                   ifelse(age_group == 'OA' & block %in% c(3, 6, 9), 3, NA))))))))


# figure: n-back HR by block, averaged over runs ----------------------

# make a copy of the data for figures
summary_nb_HR_fig <- summary_nb_HR

# overage HR for each condition over runs for each participant
summary_nb_HR_fig <- summary_nb_HR_fig %>%
  # make group uppercase for figure
  mutate(group = str_to_title(group)) %>%
  group_by(label_subject, group, age_group, block_recoded) %>%
  summarize(niHR = mean(mean_niHR, na.rm = TRUE),
            niHR_corr = mean(niHR_corr, na.rm = TRUE))

# set factor levels for plotting
summary_nb_HR_fig$age_group[summary_nb_HR_fig$age_group == 'YA'] <- 'Younger'
summary_nb_HR_fig$age_group[summary_nb_HR_fig$age_group == 'OA'] <- 'Older'
summary_nb_HR_fig$age_group <- factor(summary_nb_HR_fig$age_group, levels = c('Younger', 'Older'))
summary_nb_HR_fig$group <- factor(summary_nb_HR_fig$group, levels = c('Handgrip', 'Control'))
#summary_nb_HR_fig$condition <- factor(summary_nb_HR_fig$condition, levels = c('0-back', '1-back', '2-back', '3-back'))
summary_nb_HR_fig$block_recoded <- factor(summary_nb_HR_fig$block_recoded, levels = c(1, 2, 3, 4))

# create figure
fig_nback_HR <- ggplot(data = summary_nb_HR_fig, 
                       aes(x = block_recoded, y = niHR_corr, 
                           colour = group, fill = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3),
             alpha = 0.2) +
  stat_summary(geom = 'crossbar', width = 0.25,
               position = position_dodge(width = 0.3),
               alpha = 0.3) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.3)) +
  facet_wrap(~age_group) +
  scale_colour_manual(values = cols_group) +
  scale_fill_manual(values = cols_group) +
  labs(x = 'n-back block, relative to handgrip offset', 
       y = 'Heart rate (beats per minute)\nrelative to baseline',
       group = '', colour = '', fill = '') +
  theme_pubr() + theme_font_pub +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# summarize HR during nback, averaging over runs --------------------------

summary_nb_HR_avg <- summary_nb_HR %>%
  group_by(label_subject, group, age_group, block_recoded) %>%
  summarize(niHR_corr = mean(niHR_corr, na.rm = TRUE))
summary_nb_HR_avg[summary_nb_HR_avg == 'NaN'] <- NA


# ANOVA: HR during nback --------------------------------------------------

# set factor for ANOVA
summary_nb_HR_avg$label_subject <- factor(summary_nb_HR_avg$label_subject)
summary_nb_HR_avg$group <- factor(summary_nb_HR_avg$group, levels = c('handgrip', 'control'))
summary_nb_HR_avg$age_group <- factor(summary_nb_HR_avg$age_group, levels = c('YA', 'OA'))

# rmANOVA testing effects of block_recoded, group and their interactions on HR, for YA
aov.nb_HR_YA <- aov_ez(id = 'label_subject', 
                    dv = 'niHR_corr', 
                    data = summary_nb_HR_avg %>% filter(age_group == 'YA'), 
                    within = 'block_recoded',
                    between = c('group'),
                    type = 3,
                    include_aov = TRUE,
                    factorize = TRUE,
                    anova_table = list(es = "pes"))

aov.nb_HR_OA <- aov_ez(id = 'label_subject', 
                       dv = 'niHR_corr', 
                       data = summary_nb_HR_avg %>% filter(age_group == 'OA'), 
                       within = 'block_recoded',
                       between = c('group'),
                       type = 3,
                       include_aov = TRUE,
                       factorize = TRUE,
                       anova_table = list(es = "pes"))


# planned pairwise comparisons --------------------------------------------

# return estimated marginal means
emm.nb_HR_YA <- emmeans(aov.nb_HR_YA, 
                        ~ group | block_recoded)
emm.nb_HR_OA <- emmeans(aov.nb_HR_OA, 
                        ~ group | block_recoded)

# pairwise comparisons
contrasts.nb_HR_YA <- pairs(emm.nb_HR_YA,
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

contrasts.nb_HR_OA <- pairs(emm.nb_HR_OA,
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
contrasts.nb_HR <-
  as.data.frame(
    rbind(
      contrasts.nb_HR_YA,
      contrasts.nb_HR_OA
    )
  )

