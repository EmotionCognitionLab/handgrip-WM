# this script visualizes & analyzes salivary alpha amylase data
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# relabel sAA data and make long ------------------------------------------

data_sAA <- data_subjects %>%
  # relabel sAA samples: pre-handgrip1, post-handgrip1
  select(label_subject, group, age_group, 
         pre_handgrip1 = sAA_pre, 
         post_handgrip1 = sAA_post) %>%
  # convert from wide to long
  pivot_longer(cols = pre_handgrip1:post_handgrip1, 
               names_to = 'sample', 
               values_to = 'sAA') %>%
  filter(sample %in% c('pre_handgrip1', 'post_handgrip1')) 


# remove sAA outliers -----------------------------------------------------

# make outliers NA, for each age group separately
data_sAA_YA <- data_sAA %>%
  filter(age_group == 'YA') 
rmout_sAA_YA <- rm_outliers_mad(data_sAA_YA$sAA)  
data_sAA_YA$sAA <- rmout_sAA_YA[[1]]

data_sAA_OA <- data_sAA %>%
  filter(age_group == 'OA') 
rmout_sAA_OA <- rm_outliers_mad(data_sAA_OA$sAA)  
data_sAA_OA$sAA <- rmout_sAA_OA[[1]]

# join data from each age group back together
data_sAA <- as.data.frame(
  rbind(data_sAA_YA, data_sAA_OA)
)


# figure: sAA before & after handgrip -------------------------------------

# set factor levels for plotting
data_sAA <- data_sAA %>%
  mutate(sample_renamed = ifelse(sample == 'pre_handgrip1', 'Pre',
                                 ifelse(sample == 'post_handgrip1', 'Post', NA)),
         group = str_to_title(group))

data_sAA$age_group[data_sAA$age_group == 'YA'] <- 'Younger'
data_sAA$age_group[data_sAA$age_group == 'OA'] <- 'Older'
data_sAA$age_group <- factor(data_sAA$age_group, levels = c('Younger', 'Older'))
data_sAA$group <- factor(data_sAA$group, levels = c('Handgrip', 'Control'))
data_sAA$sample <- factor(data_sAA$sample, levels = c('pre_handgrip1', 'post_handgrip1'))
data_sAA$sample_renamed <- factor(data_sAA$sample_renamed, levels = c('Pre', 'Post'))

# create figure
fig_handgrip_sAA <- ggplot(data = data_sAA,
                           aes(x = sample_renamed, y = sAA, 
                               colour = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3),
             alpha = 0.2) +
  stat_summary(geom = 'crossbar', width = 0.25,
               position = position_dodge(width = 0.3),
               alpha = 0.3) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.3)) +
  scale_colour_manual(values = cols_group) +
  labs(x = '', y = 'Salivary alpha amylase (U/mL)', colour = '') +
  facet_wrap(~age_group) +
  theme_pubr() + theme_font_pub


# normality check ---------------------------------------------------------

sw.sAA <- data_sAA %>%
  shapiro_test(sAA)


# model: sAA pre vs. post handgrip ----------------------------------------

# set up contrasts for model
data_sAA$group <- factor(data_sAA$group, levels = c('Handgrip', 'Control')) 
data_sAA$age_group <- factor(data_sAA$age_group, levels = c('Younger', 'Older'))
data_sAA$label_subject <- factor(data_sAA$label_subject)
data_sAA$sample <- factor(data_sAA$sample, levels = c('pre_handgrip1', 'post_handgrip1'))

aov.hg_sAA <- permuco::aovperm(sAA ~ sample * group * age_group + Error(label_subject/sample), 
                               data = data_sAA, 
                               np = nperm_aov)


# pairwise comparisons: sAA by group, age group ---------------------------

# (using a bootstrap procedure,
# based on: https://www.r-bloggers.com/2019/05/bootstraping-follow-up-contrasts-for-within-subject-anovas-2/)

### make data wide
input_wide <- data_sAA %>%
  select(-sample_renamed) %>%
  pivot_wider(names_from = sample, values_from = sAA) %>%
  filter(!is.na(pre_handgrip1) & !is.na(post_handgrip1))

input <- input_wide %>%
  select(label_subject, group, age_group)

# add matrix M with within-subjects factors and levels
input$M <- input_wide %>%
  select(-label_subject, -group, -age_group) %>%
  as.matrix()

### fit regular model
# lhs is M representing all within-subjects factors and levels
# rhs includes only between-subjects factors
fit_mixed <- aov(M ~ group * age_group, input)

### define contrasts of interest
# create list of within-subjects factors and their levels
# this list must correspond to order of multivariate column in data
rm_levels <- list(condition = c('pre_handgrip1', 'post_handgrip1'))

# function to return estimates of pairwise differences
# for each contrast of interest
# in this case, comparing levels by time, for each group and age group
group_time_contrasts <- function(mod) {
  rg <- ref_grid(mod, mult.levs = rm_levels)
  em_ <- emmeans(rg, ~ condition * group * age_group)
  c_ <- pairs(em_, by = c('group', 'age_group'), 
              reverse = TRUE)  # reverse to get post-pre contrast
  # extract estimates
  est_names <- c('Handgrip: Younger: post - pre',
                 'Control: Younger: post - pre',
                 'Handgrip: Older: post - pre',
                 'Control: Older: post - pre')
  est_values <- summary(c_)$estimate
  names(est_values) <- est_names
  return(est_values)
}

# get bootstrapped estimates of pairwise differences
group_time_results <- Boot(fit_mixed, group_time_contrasts, R = nboot)

# save results as dataframe
contrasts.hg_sAA <- summary(group_time_results) %>%
  as.data.frame() %>%
  cbind(
    # get bootstrap confidence intervals
    confint(group_time_results, type = 'perc')
  ) %>%
  cbind(
    # get bootstrap p-values
    p.value = boot_pvalues(group_time_results)
  ) %>%
  # format results for table
  mutate(Contrast = 'Post-handgrip #1 - Pre-handgrip #1') %>%
  mutate(rname = rownames(summary(group_time_results))) %>%
  rowwise() %>%
  mutate(Group = str_split(rname, ': ')[[1]][1],
         `Age group` = str_split(rname, ': ')[[1]][2],
         `95% CI` = str_c(round(`2.5 %`, 3), ' - ', round(`97.5 %`, 3), sep = '')) %>%
  select(`Age group`,
         Group,
         Contrast,
         `Bootstrapped estimate` = bootMed, 
         `95% CI`,
         `Bootstrapped p` = p.value)

# adjust p-values based on bonferroni correction
contrasts.hg_sAA$`Bootstrapped p, adj` <- p.adjust(contrasts.hg_sAA$`Bootstrapped p`,
                                                   method = 'bonferroni', 
                                                   n = nrow(contrasts.hg_sAA))

rm(group_time_results,
   input_wide, input, fit_mixed)


# transform data to wide --------------------------------------------------

data_sAA_wide <- data_sAA %>%
  select(-sample_renamed) %>%
  pivot_wider(names_from = sample, 
              values_from = sAA) %>%
  rowwise() %>%
  mutate(sAA_chg = (post_handgrip1 - pre_handgrip1)) %>%
  # bind handgrip strength data
  left_join(summary_hg_arousal %>% 
              select(label_subject, EMG_diff), by = 'label_subject')


# normality check ---------------------------------------------------------

sw.sAAchg <- data_sAA_wide %>%
  shapiro_test(sAA_chg)

sw.EMG_diff <- data_sAA_wide %>%
  shapiro_test(EMG_diff)

