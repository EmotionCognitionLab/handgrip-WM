# this script visualizes & analyzes EMG data during handgrip
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# figure: EMG during handgrip, by run -------------------------------------

# recreate event column and select only relevant columns
summary_hg_EMG <- summary_hg_EMG %>%
  select(label_subject, group, age_group, run, hemi, round, 
         rest = rest_emg_corr, squeeze = squeeze_emg_corr) %>%
  pivot_longer(cols = rest:squeeze, names_to = 'event', values_to = 'emg_corr') 

# average EMG values over handgrip runs
summary_hg_EMG_fig <- summary_hg_EMG %>%
  group_by(label_subject, age_group, group, event, round) %>%
  summarize(emg_corr = mean(emg_corr))

# add 'part' variable combining event and round
summary_hg_EMG_fig <- summary_hg_EMG_fig %>%
  rowwise() %>%
  mutate(part = str_c(event, round, sep = ''))

# format group for plotting
summary_hg_EMG_fig <- summary_hg_EMG_fig %>%
  rowwise() %>%
  mutate(group = str_to_title(group))

# set factor levels for figure
summary_hg_EMG_fig$group <- factor(summary_hg_EMG_fig$group, levels = c('Handgrip', 'Control'))
summary_hg_EMG_fig$age_group[summary_hg_EMG_fig$age_group == 'YA'] <- 'Younger'
summary_hg_EMG_fig$age_group[summary_hg_EMG_fig$age_group == 'OA'] <- 'Older'
summary_hg_EMG_fig$age_group <- factor(summary_hg_EMG_fig$age_group, levels = c('Younger', 'Older'))
summary_hg_EMG_fig$part <- factor(summary_hg_EMG_fig$part, levels = c('squeeze1', 'rest1', 'squeeze2', 'rest2', 'squeeze3', 'rest3', 'squeeze4', 'rest4'))

# create figure
fig_handgrip_EMG_binned <- ggplot(data = summary_hg_EMG_fig, 
                                  aes(x = part, y = emg_corr, 
                                      colour = factor(group), 
                                      group = factor(group))) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, 
                                             dodge.width = 0.2), 
             alpha = 0.2) +
  stat_summary(position = position_dodge(width = 0.2)) +
  stat_summary(geom = 'line',
               position = position_dodge(width = 0.2)) + 
  scale_colour_manual(values = cols_group) +
  labs(x = '', y = 'mean integrated EMG signal, \nlog-transformed & relative to baseline', 
       colour = '') +
  facet_wrap(~age_group) +
  theme_pubr() +
  theme(legend.position = 'right') +
  theme_rotatex + theme_font_pub

ggsave(here('figures', 'figureS1_handgrip-EMG.png'), 
       fig_handgrip_EMG_binned, 
       device = 'png', dpi = fig_dpi, bg = 'white',
       width = 10, height = 5)


# ANOVA: EMG during handgrip ----------------------------------------------

summary_hg_EMG_avg <- summary_hg_EMG %>%
  group_by(label_subject, age_group, group, event) %>%
  summarize(emg_corr = mean(emg_corr, na.rm = TRUE))

# set factor levels for ANOVA
summary_hg_EMG_avg$group <- factor(summary_hg_EMG_avg$group, levels = c('handgrip', 'control'))
summary_hg_EMG_avg$age_group <- factor(summary_hg_EMG_avg$age_group, levels = c('YA', 'OA'))
summary_hg_EMG_avg$event <- factor(summary_hg_EMG_avg$event, levels = c('squeeze', 'rest'))
summary_hg_EMG_avg$label_subject <- factor(summary_hg_EMG_avg$label_subject)

# rmANOVA testing effects of event, group, age group, and their interactions on EMG
aov.hg_EMG <- aov_ez(id = 'label_subject',
                     dv = 'emg_corr',
                     data = summary_hg_EMG_avg,
                     within = 'event',
                     between = c('group', 'age_group'),
                     type = 3,
                     anova_table = list(es = "pes"),
                     factorize = TRUE)


# post-hoc comparisons: EMG during handgrip -------------------------------

# return estimated marginal means
emm.hg_EMG <- emmeans(aov.hg_EMG, 
                      ~ event | age_group * group)

# pairwise comparisons of EMMs with bonferroni correction
contrasts.hg_EMG <- emm.hg_EMG %>%
  contrast('pairwise') %>%
  summary(by = NULL, adjust = 'bonferroni') %>%
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
  arrange(desc(`Age group`), desc(Group))
contrasts.hg_EMG$p <- fix_pval_table(contrasts.hg_EMG$p)
