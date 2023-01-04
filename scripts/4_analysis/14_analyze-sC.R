# this script visualizes & analyzes salivary cortisol data
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# relabel sC data and make long -------------------------------------------

data_sC <- data_subjects %>%
  # relabel samples
  select(label_subject, group, age_group, 
         baseline = sC_pre,
         post_allruns = sC_post) %>%
  # convert from wide to long
  pivot_longer(cols = baseline:post_allruns, 
               names_to = 'sample', 
               values_to = 'sC') %>%
  filter(sample %in% c('baseline', 'post_allruns'))


# remove sC outliers ------------------------------------------------------

# remove outliers for each age group separately
data_sC_YA <- data_sC %>%
  filter(age_group == 'YA') 
rmout_sC_YA <- rm_outliers_mad(data_sC_YA$sC)  
data_sC_YA$sC <- rmout_sC_YA[[1]]

data_sC_OA <- data_sC %>%
  filter(age_group == 'OA') 
rmout_sC_OA <- rm_outliers_mad(data_sC_OA$sC)  
data_sC_OA$sC <- rmout_sC_OA[[1]]

# join data from each age group back together
data_sC <- as.data.frame(
  rbind(data_sC_YA, data_sC_OA)
)


# figure: sC before & after experiment ------------------------------------

# set factor levels for plotting
data_sC <- data_sC %>%
  mutate(sample_renamed = ifelse(sample == 'baseline', 'Before',
                                 ifelse(sample == 'post_allruns', 'After', NA)),
         group = str_to_title(group))
data_sC$age_group[data_sC$age_group == 'YA'] <- 'Younger'
data_sC$age_group[data_sC$age_group == 'OA'] <- 'Older'
data_sC$age_group <- factor(data_sC$age_group, levels = c('Younger', 'Older'))
data_sC$group <- factor(data_sC$group, levels = c('Handgrip', 'Control'))
data_sC$sample <- factor(data_sC$sample, levels = c('baseline', 'post_allruns'))
data_sC$sample_renamed <- factor(data_sC$sample_renamed, levels = c('Before', 'After'))

# create figure
fig_handgrip_sC <- ggplot(data = data_sC, 
                           aes(x = sample_renamed, y = sC, 
                               colour = group, group = group)) +
  geom_point(position = position_dodge(width = 0.3),
             alpha = 0.2) +
  stat_summary(geom = 'crossbar', width = 0.25,
               position = position_dodge(width = 0.3),
               alpha = 0.3) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.3)) +
  scale_colour_manual(values = cols_group) +
  labs(x = '', y = 'Salivary cortisol (micrograms/dL)', colour = '') +
  facet_wrap(~age_group) +
  theme_pubr() + theme_font_pub


# normality check ---------------------------------------------------------

sw.sC <- data_sC %>%
#  group_by(age_group) %>%
  shapiro_test(sC)


# model: sAA before & after experiment ------------------------------------

# set up contrasts for model
data_sC$group <- factor(data_sC$group, levels = c('Handgrip', 'Control')) 
data_sC$age_group <- factor(data_sC$age_group, levels = c('Younger', 'Older'))
data_sC$label_subject <- factor(data_sC$label_subject)
data_sC$sample <- factor(data_sC$sample, levels = c('baseline', 'post_allruns'))

aov.hg_sC <- permuco::aovperm(sC ~ sample * group * age_group + Error(label_subject/sample), 
                               data = data_sC, np = nperm_aov)


# pairwise comparisons: sAA by group, age group ---------------------------

# (using a bootstrap procedure,
# based on: https://www.r-bloggers.com/2019/05/bootstraping-follow-up-contrasts-for-within-subject-anovas-2/)

### make data wide
input_wide <- data_sC %>%
  select(-sample_renamed) %>%
  pivot_wider(names_from = sample, values_from = sC) %>%
  filter(!is.na(baseline) & !is.na(post_allruns))

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
rm_levels <- list(condition = c('baseline', 'post_allruns'))

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
contrasts.hg_sC <- summary(group_time_results) %>%
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
  mutate(Contrast = 'Post-tasks - Pre-baseline') %>%
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
contrasts.hg_sC$`Bootstrapped p, adj` <- p.adjust(contrasts.hg_sC$`Bootstrapped p`,
                                                   method = 'bonferroni', 
                                                   n = nrow(contrasts.hg_sC))

rm(group_time_results,
   input_wide, input, fit_mixed)

# create composite figure for salivary assays -----------------------------

figure4 <- ggarrange(
  
  fig_handgrip_sAA + 
    labs(subtitle = 'A. Salivary alpha amylase,\npre- vs. post-handgrip run #1') +
    theme(plot.subtitle = element_text(size = 11, face = 'bold')),
  fig_handgrip_sC +
    labs(subtitle = 'B. Salivary cortisol,\nbefore vs. after all tasks') +
    theme(plot.subtitle = element_text(size = 11, face = 'bold')),
  
  nrow = 2, ncol = 1,
  align = 'h',
  common.legend = TRUE, 
  legend = 'right'
)

ggsave(here('figures', 'figure5_handgrip-saliva.png'), 
       figure4, 
       device = 'png', dpi = fig_dpi, bg = 'white',
       width = 5, height = 8)
