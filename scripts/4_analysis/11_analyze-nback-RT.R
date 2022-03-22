# this script visualizes & analyzes auditory n-back reaction time data
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# organize n-back RT data -------------------------------------------------

# (accuracy script already restricted included responses to those occurring within 2.25s of stim onset)

# make NA response times outside mean +/- 2.5SD of reaction times for target trials, 
# for each participant
RT_thresh <- data_nback %>%
  filter(target == 1) %>%
  group_by(label_subject) %>%
  summarize(mean_RT = mean(response_time, na.rm = TRUE),
            sd_RT = sd(response_time, na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(RT_thresh_lower = mean_RT - 2.5*sd_RT,
         RT_thresh_upper = mean_RT + 2.5*sd_RT) %>%
  select(label_subject, RT_thresh_lower, RT_thresh_upper)

data_nback <- data_nback %>%
  left_join(RT_thresh, by = 'label_subject') %>%
  rowwise() %>%
  mutate(response_time = ifelse(is.na(response_time), NA,
                                ifelse(response_time > RT_thresh_lower & response_time < RT_thresh_upper, response_time, NA))) %>%
  select(-RT_thresh_lower, -RT_thresh_upper)


# summarize n-back RT data ------------------------------------------------

# (compute mean RT for each participant)

summary_nb_RT <- data_nback %>%
  
  # exclude practice trials, include target trials only
  filter(!run == 0,
         target == 1) %>%
  rowwise() %>%
  group_by(label_subject, condition) %>%
  
  # compute mean accuracy for each participant, by condition
  summarize(mean_RT_target = mean(response_time, na.rm = TRUE)) %>%
  
  # bind subject information
  left_join(data_subjects %>% 
              select(label_subject, group, age_group),
            by = 'label_subject') %>%
  rowwise() %>%
  mutate(condition = str_c(condition, '-back', sep = ''))


# figure: n-back RTs, averaged over runs ----------------------------------

# make a copy of the data for figures
summary_nb_RT_fig <- summary_nb_RT

# set group labels for figure
summary_nb_RT_fig <- summary_nb_RT_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group))

# set factor levels for plotting
summary_nb_RT_fig$age_group[summary_nb_RT_fig$age_group == 'YA'] <- 'Younger'
summary_nb_RT_fig$age_group[summary_nb_RT_fig$age_group == 'OA'] <- 'Older'
summary_nb_RT_fig$age_group <- factor(summary_nb_RT_fig$age_group, levels = c('Younger', 'Older'))
summary_nb_RT_fig$group <- factor(summary_nb_RT_fig$group, levels = c('Handgrip', 'Control'))
summary_nb_RT_fig$condition <- factor(summary_nb_RT_fig$condition, levels = c('0-back', '1-back', '2-back', '3-back'))

# create figure
fig_nback_RT <- ggplot(data = summary_nb_RT_fig, 
                       aes(x = condition, y = mean_RT_target, colour = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3),
             alpha = 0.2) +
  stat_summary(geom = 'crossbar', width = 0.25,
               position = position_dodge(width = 0.3),
               alpha = 0.3) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.3)) +
  facet_wrap(~age_group) +
  scale_colour_manual(values = cols_group) +
  labs(x = 'Working memory load', y = 'Mean reaction time (seconds)', colour = '') +
  theme_pubr() + theme_rotatex + theme_font_pub


# create composite figure 3 -----------------------------------------------

figure3 <- ggarrange(
  fig_nback_acc,
  fig_nback_RT,
  nrow = 2, ncol = 1, 
  align = 'h', labels = c('A', 'B'),
  common.legend = TRUE,
  legend = 'right'
)
ggsave(here('figures', 'figure3_nback-acc-RT.png'), 
       figure3,
       device = 'png', dpi = fig_dpi, bg = 'white',
       width = 5, height = 8)
        # previouly width = 10, height = 5


# summary with descriptives -----------------------------------------------

descriptives_nback_RT <- summary_nb_RT %>%
  # average over participants
  group_by(group, age_group, condition) %>%
  summarize(RT_target_mean = mean(mean_RT_target, na.rm = TRUE),
            RT_target_SD = sd(mean_RT_target, na.rm = TRUE),
            n = n()) %>%
  rowwise() %>%
  mutate(age_group = ifelse(age_group == 'YA', 'Younger',
                            ifelse(age_group == 'OA', 'Older', NA)),
         group = str_to_title(group)) %>%
  select(`Age group` = age_group, 
         Condition = condition, 
         Group = group, 
         n = n,
         `Reaction time, mean` = RT_target_mean, 
         `Reaction time, SD` = RT_target_SD) %>%
  arrange(desc(`Age group`), Condition, desc(Group))


# normality check ---------------------------------------------------------

sw.nb_RT <- summary_nb_RT %>%
  group_by(age_group) %>%
  shapiro_test(mean_RT_target)


# permuted ANOVA: RT by group, condition ----------------------------------

# set up contrasts for model
summary_nb_RT$group <- factor(summary_nb_RT$group, levels = c('handgrip', 'control'))  # ordered for handgrip-control pairwise comparisons
summary_nb_RT$condition <- factor(summary_nb_RT$condition, levels = c('0-back', '1-back', '2-back', '3-back'))
summary_nb_RT$label_subject <- factor(summary_nb_RT$label_subject)

# permuted ANOVAs for each age group
aov.nb_RT_YA <- permuco::aovperm(mean_RT_target ~ group * condition + Error(label_subject/(condition)), 
                                 data = summary_nb_RT %>% filter(age_group == 'YA'), np = nperm_aov)
aov.nb_RT_OA <- permuco::aovperm(mean_RT_target ~ group * condition + Error(label_subject/(condition)), 
                                 data = summary_nb_RT %>% filter(age_group == 'OA'), np = nperm_aov)


# pairwise comparisons of RT by group, condition (YA) ---------------------

# (using a bootstrap procedure,
# based on: https://www.r-bloggers.com/2019/05/bootstraping-follow-up-contrasts-for-within-subject-anovas-2/)

### make data wide
input_wide <- summary_nb_RT %>%
  filter(age_group == 'YA') %>%
  select(-age_group) %>%
  pivot_wider(names_from = condition, names_prefix = 'condition_', values_from = mean_RT_target)

input <- input_wide %>%
  select(label_subject, group)

# add matrix M with within-subjects factors and levels
input$M <- input_wide %>%
  select(-label_subject, -group) %>%
  as.matrix()

### fit regular model
# lhs is M representing all within-subjects factors and levels
# rhs includes only between-subjects factors
fit_mixed <- aov(M ~ group, input)

### define contrasts of interest
# create list of within-subjects factors and their levels
# this list must correspond to order of multivariate column in data
rm_levels_YA <- list(condition = c('0-back', '1-back', '2-back', '3-back'))

# function to return estimates of pairwise differences
# for each contrast of interest
# in this case, comparing groups for each condition
condition_group_contrasts_YA <- function(mod) {
  rg <- ref_grid(mod, mult.levs = rm_levels_YA)
  em_ <- emmeans(rg, ~ condition * group)
  c_ <- contrast(em_, 'pairwise', by = 'condition')
  # extract estimates
  est_names <- c('0-back: handgrip - control',
                 '1-back: handgrip - control',
                 '2-back: handgrip - control',
                 '3-back: handgrip - control')
  est_values <- summary(c_)$estimate
  names(est_values) <- est_names
  return(est_values)
}

# get bootstrapped estimates of pairwise differences
condition_group_results_YA <- Boot(fit_mixed, condition_group_contrasts_YA, R = nboot)

# save results as dataframe
contrasts.nb_RT_YA <- summary(condition_group_results_YA) %>%
  as.data.frame() %>%
  cbind(
    # get bootstrap confidence intervals
    confint(condition_group_results_YA, type = 'perc')
  ) %>%
  cbind(
    # get bootstrap p-values
    p.value = boot_pvalues(condition_group_results_YA)
  ) %>%
  # format results for table
  mutate(`Age group` = 'Younger',
         Contrast = 'Handgrip - Control') %>%
  mutate(rname = rownames(summary(condition_group_results_YA))) %>%
  rowwise() %>%
  mutate(Condition = str_split(rname, ':')[[1]][1],
         `95% CI` = str_c(round(`2.5 %`, 3), ' - ', round(`97.5 %`, 3), sep = '')) %>%
  select(`Age group`,
         Contrast,
         Load = Condition,
         `Bootstrapped estimate` = bootMed, 
         `95% CI`,
         `Bootstrapped p` = p.value)

# adjust p-values based on bonferroni correction
contrasts.nb_RT_YA$`Bootstrapped p, adj` <- p.adjust(contrasts.nb_RT_YA$`Bootstrapped p`,
                                                     method = 'bonferroni', n = 4)


# pairwise comparisons of RT by group, condition (OA) ---------------------

### make data wide
input_wide <- summary_nb_RT %>%
  filter(age_group == 'OA') %>%
  select(-age_group) %>%
  pivot_wider(names_from = condition, names_prefix = 'condition_', values_from = mean_RT_target)

input <- input_wide %>%
  select(label_subject, group)

# add matrix M with within-subjects factors and levels
input$M <- input_wide %>%
  select(-label_subject, -group) %>%
  as.matrix()

### fit regular model
# lhs is M representing all within-subjects factors and levels
# rhs includes only between-subjects factors
fit_mixed <- aov(M ~ group, input)

### define contrasts of interest
# create list of within-subjects factors and their levels
# this list must correspond to order of multivariate column in data
rm_levels_OA <- list(condition = c('0-back', '1-back', '2-back'))

# function to return estimates of pairwise differences
# for each contrast of interest
# in this case, comparing groups for each condition
condition_group_contrasts_OA <- function(mod) {
  rg <- ref_grid(mod, mult.levs = rm_levels_OA)
  em_ <- emmeans(rg, ~ condition * group)
  c_ <- contrast(em_, 'pairwise', by = 'condition')
  # extract estimates
  est_names <- c('0-back: handgrip - control',
                 '1-back: handgrip - control',
                 '2-back: handgrip - control')
  est_values <- summary(c_)$estimate
  names(est_values) <- est_names
  return(est_values)
}

# get bootstrapped estimates of pairwise differences
condition_group_results_OA <- Boot(fit_mixed, condition_group_contrasts_OA, R = nboot)

# save results as dataframe
contrasts.nb_RT_OA <- summary(condition_group_results_OA) %>%
  as.data.frame() %>%
  cbind(
    # get bootstrap confidence intervals
    confint(condition_group_results_OA, type = 'perc')
  ) %>%
  cbind(
    # get bootstrap p-values
    p.value = boot_pvalues(condition_group_results_OA)
  ) %>%
  # format results for table
  mutate(`Age group` = 'Older',
         Contrast = 'Handgrip - Control') %>%
  mutate(rname = rownames(summary(condition_group_results_OA))) %>%
  rowwise() %>%
  mutate(Condition = str_split(rname, ':')[[1]][1],
         `95% CI` = str_c(round(`2.5 %`, 3), ' - ', round(`97.5 %`, 3), sep = '')) %>%
  select(`Age group`,
         Contrast,
         Load = Condition,
         `Bootstrapped estimate` = bootMed, 
         `95% CI`,
         `Bootstrapped p` = p.value)

# adjust p-values based on bonferroni correction
contrasts.nb_RT_OA$`Bootstrapped p, adj` <- p.adjust(contrasts.nb_RT_OA$`Bootstrapped p`,
                                                     method = 'bonferroni', n = 4)


# join pairwise comparison tables
contrasts.nb_RT <- as.data.frame(
  rbind(
    contrasts.nb_RT_YA,
    contrasts.nb_RT_OA
  )
)

rm(contrasts.nb_RT_YA, contrasts.nb_RT_OA,
   condition_group_results_YA, condition_group_results_OA,
   input_wide, input, fit_mixed)
