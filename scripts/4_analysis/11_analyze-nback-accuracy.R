# this script visualizes & analyzes auditory n-back accuracy data
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# organize n-back accuracy data -------------------------------------------

# restrict included responses to those which occurred with 2.25s of stimulus onset
data_nback$response[data_nback$response_time > 2.25] <- NA
data_nback$response_time[data_nback$response_time > 2.25] <- NA

# add an accuracy variable (1=correct, 0=incorrect)
data_nback <- data_nback %>% 
  rowwise() %>%
  # recode NA responses as 0
  mutate(response = ifelse(is.na(response), 0, response)) %>%
  rowwise() %>%
  # response is accurate if target & response match
  mutate(accuracy = ifelse(target == response, 1, 0)) %>%
  rowwise() %>%
  left_join(data_subjects %>% 
              select(label_subject, group, age_group),
            by = 'label_subject')


# summarize n-back accuracy data ------------------------------------------

# (compute mean accuracy for each participant)

summary_nb_acc <- data_nback %>%
  
  # exclude practice trials, include target trials only
  filter(!run == 0,
         target == 1) %>%
  rowwise() %>%
  group_by(label_subject, condition) %>%
  
  # compute mean accuracy for each participant, by condition
  summarize(n_targets = n(),
            mean_acc = sum(response, na.rm = TRUE)/n_targets) %>%
  
  # bind subject information
  left_join(data_subjects %>% 
              select(label_subject, group, age_group),
            by = 'label_subject') %>%
  select(label_subject, group, age_group, condition, mean_acc) %>%
  rowwise() %>%
  mutate(condition = str_c(condition, '-back', sep = ''))


# figure: n-back accuracy, averaged over runs -----------------------------

# make a copy of the data for plotting 
summary_nb_acc_fig <- summary_nb_acc

# set group labels for figure
summary_nb_acc_fig <- summary_nb_acc_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group))

# set factor levels for plotting
summary_nb_acc_fig$age_group[summary_nb_acc_fig$age_group == 'YA'] <- 'Younger'
summary_nb_acc_fig$age_group[summary_nb_acc_fig$age_group == 'OA'] <- 'Older'
summary_nb_acc_fig$age_group <- factor(summary_nb_acc_fig$age_group, levels = c('Younger', 'Older'))
summary_nb_acc_fig$group <- factor(summary_nb_acc_fig$group, levels = c('Handgrip', 'Control'))
summary_nb_acc_fig$condition <- factor(summary_nb_acc_fig$condition, levels = c('0-back', '1-back', '2-back', '3-back'))

# create figure
fig_nback_acc <- ggplot(data = summary_nb_acc_fig, 
                        aes(x = condition, y = mean_acc, 
                            colour = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3),
             alpha = 0.2) +
  stat_summary(geom = 'crossbar', width = 0.25,
               position = position_dodge(width = 0.3),
               alpha = 0.3) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.3)) +
  facet_wrap(~age_group) +
  scale_colour_manual(values = cols_group) +
  labs(x = 'Working memory load', y = 'Accuracy (%)', colour = '') +
  theme_pubr() + theme_rotatex + theme_font_pub


# summary with descriptives -----------------------------------------------

descriptives_nback_acc <- summary_nb_acc %>%
  # average over participants
  group_by(group, age_group, condition) %>%
  summarize(accuracy_mean = mean(mean_acc, na.rm = TRUE),
            accuracy_SD = sd(mean_acc, na.rm = TRUE),
            n = n()) %>%
  rowwise() %>%
  mutate(age_group = ifelse(age_group == 'YA', 'Younger',
                            ifelse(age_group == 'OA', 'Older', NA)),
         group = str_to_title(group)) %>%
  select(`Age group` = age_group, 
         Condition = condition, 
         Group = group, 
         n = n,
         `Accuracy, mean` = accuracy_mean, 
         `Accuracy, SD` = accuracy_SD) %>%
  arrange(desc(`Age group`), Condition, desc(Group))


# normality check ---------------------------------------------------------

sw.nb_acc <- summary_nb_acc %>%
  group_by(age_group) %>%
  shapiro_test(mean_acc)


# permuted ANOVA: accuracy by group, condition ----------------------------

# set up contrasts for model
summary_nb_acc$group <- factor(summary_nb_acc$group, levels = c('handgrip', 'control'))  # ordered for handgrip-control pairwise comparisons
summary_nb_acc$condition <- factor(summary_nb_acc$condition, levels = c('0-back', '1-back', '2-back', '3-back'))
summary_nb_acc$label_subject <- factor(summary_nb_acc$label_subject)

# permuted ANOVAs for each age group
aov.nb_acc_YA <- permuco::aovperm(mean_acc ~ group * condition + Error(label_subject/(condition)), 
                                  data = summary_nb_acc %>% filter(age_group == 'YA'), np = 10000)
aov.nb_acc_OA <- permuco::aovperm(mean_acc ~ group * condition + Error(label_subject/(condition)), 
                                  data = summary_nb_acc %>% filter(age_group == 'OA'), np = 10000)


# pairwise comparisons of accuracy by group, condition (YA) ---------------

# (using a bootstrap procedure,
# based on: https://www.r-bloggers.com/2019/05/bootstraping-follow-up-contrasts-for-within-subject-anovas-2/)

### make data wide
input_wide <- summary_nb_acc %>%
  filter(age_group == 'YA') %>%
  select(-age_group) %>%
  pivot_wider(names_from = condition, names_prefix = 'condition_', values_from = mean_acc)

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
contrasts.nb_acc_YA <- summary(condition_group_results_YA) %>%
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
contrasts.nb_acc_YA$`Bootstrapped p, adj` <- p.adjust(contrasts.nb_acc_YA$`Bootstrapped p`,
                                                     method = 'bonferroni', n = 4)


# pairwise comparisons of accuracy by group, condition (OA) ---------------

### make data wide
input_wide <- summary_nb_acc %>%
  filter(age_group == 'OA') %>%
  select(-age_group) %>%
  pivot_wider(names_from = condition, names_prefix = 'condition_', values_from = mean_acc)

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
contrasts.nb_acc_OA <- summary(condition_group_results_OA) %>%
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
contrasts.nb_acc_OA$`Bootstrapped p, adj` <- p.adjust(contrasts.nb_acc_OA$`Bootstrapped p`,
                                                     method = 'bonferroni', n = 3)


# join pairwise comparison tables
contrasts.nb_acc <- as.data.frame(
  rbind(
    contrasts.nb_acc_YA,
    contrasts.nb_acc_OA
  )
)

rm(contrasts.nb_acc_YA, contrasts.nb_acc_OA,
   condition_group_results_YA, condition_group_results_OA,
   input_wide, input, fit_mixed)


# analyze test-retest reliability across runs -----------------------------

# organize data for ICC calculations
icc_nb_acc <- data_nback %>%
  
  # exclude practice trials, include target trials only
  filter(!run == 0,
         target == 1) %>%
  rowwise() %>%
  group_by(label_subject, condition, run) %>%
  
  # compute mean accuracy for each participant, by condition
  summarize(n_targets = n(),
            mean_acc = sum(response, na.rm = TRUE)/n_targets) %>%
  
  # bind subject information
  left_join(data_subjects %>% 
              select(label_subject, group, age_group),
            by = 'label_subject') %>%
  select(label_subject, group, age_group, condition, run, mean_acc) %>%
  rowwise() %>%
  mutate(condition = str_c(condition, '-back', sep = ''))
icc_nb_acc[icc_nb_acc == 'NaN'] <- NA

# split by WM load
# and reshape data for ICC calculation
icc_nb_acc_0 <- icc_nb_acc %>%
  filter(condition == '0-back') %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'mean_acc') %>%
  ungroup() %>%
  select(run1, run2, run3)

icc_nb_acc_1 <- icc_nb_acc %>%
  filter(condition == '1-back') %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'mean_acc') %>%
  ungroup() %>%
  select(run1, run2, run3)

icc_nb_acc_2 <- icc_nb_acc %>%
  filter(condition == '2-back') %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'mean_acc') %>%
  ungroup() %>%
  select(run1, run2, run3)

icc_nb_acc_3 <- icc_nb_acc %>%
  filter(condition == '3-back') %>%
  pivot_wider(names_from = 'run', 
              names_prefix = 'run', 
              values_from = 'mean_acc') %>%
  ungroup() %>%
  select(run1, run2, run3)

# calculate ICCs: rest
icc_nb_acc_0_estimate <- irr::icc(icc_nb_acc_0,
                                  type = "agreement",
                                  model = "twoway",
                                  unit = "average")

icc_nb_acc_1_estimate <- irr::icc(icc_nb_acc_1,
                                  type = "agreement",
                                  model = "twoway",
                                  unit = "average")

icc_nb_acc_2_estimate <- irr::icc(icc_nb_acc_2,
                                  type = "agreement",
                                  model = "twoway",
                                  unit = "average")

icc_nb_acc_3_estimate <- irr::icc(icc_nb_acc_3,
                                  type = "agreement",
                                  model = "twoway",
                                  unit = "average")


# arrange tables containing ICC estimates and F-test results
icc_nb_acc_results <- as.data.frame(
  cbind(c('0-back', '1-back', '2-back', '3-back'),
        rbind(extract_icc_results(icc_nb_acc_0_estimate),
              extract_icc_results(icc_nb_acc_1_estimate),
              extract_icc_results(icc_nb_acc_2_estimate),
              extract_icc_results(icc_nb_acc_3_estimate))
  ))
names(icc_nb_acc_results) <- c('Load', 'ICC', '95% CI', 'F (df1, df2)', 'p')

