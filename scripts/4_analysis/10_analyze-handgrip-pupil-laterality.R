# this script visualizes & analyzes pupil diameter data during handgrip, by laterality
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# re-load pupil diameter data with laterality included --------------------

data_ET_handgrip_lat <- fread(here('results', 'SAWM_handgrip_pupil_2021-10-20.csv')) 


# apply same exclusions as in previous analyses ---------------------------

data_ET_handgrip_lat <- data_ET_handgrip_lat %>%
  filter(!label_subject %in% subs_exclude_all)


# clean pupil data (same approach as in 06_analyze-handgrip-pupil.R) --------

# (only step that differs is we do not average over eyes)

data_ET_handgrip_lat <- data_ET_handgrip_lat %>%
  
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
  
  # handle NaNs from completely missing data
  mutate(pupil_corr = ifelse(pupil_corr == 'NaN', NA, pupil_corr)) %>%
  
  # bind subject info
  left_join(data_subjects %>%
              select(label_subject, group, age_group, grip), 
            by = 'label_subject') %>%
  
  # select only relevant columns
  select(label_subject, group, age_group, grip, which_eye = hemi, run, event, round, pupil_corr)


# bind data about laterality of grip during handgrip ----------------------

data_ET_handgrip_lat <- data_ET_handgrip_lat %>%
  left_join(grip_table,
            by = c('grip', 'run', 'round')
  ) %>%
  rename(which_hand = grip_side)


# summarize over runs and rounds of handgrip ------------------------------

summary_ET_handgrip_lat <- data_ET_handgrip_lat %>%
  group_by(label_subject, group, age_group, event, which_eye, which_hand) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE))
summary_ET_handgrip_lat[summary_ET_handgrip_lat == 'NaN'] <- NA


# define contra- and ipsilateral ------------------------------------------

summary_ET_handgrip_lat <- summary_ET_handgrip_lat %>% 
  rowwise() %>%
  mutate(laterality = ifelse(which_eye == which_hand, 'Ipsilateral', 'Contralateral'))


# average over right/left to get single ipsi/contra estimate per p --------

summary_ET_handgrip_lat <- summary_ET_handgrip_lat %>%
  group_by(label_subject, group, age_group, event, laterality) %>%
  summarize(pupil_corr = mean(pupil_corr, na.rm = TRUE))


# figure: right/left pupil diameter by right/left squeeze -----------------

# update factor labels and levels for figure
summary_ET_handgrip_lat_fig <- summary_ET_handgrip_lat %>%
  rowwise() %>%
  mutate(group = str_to_title(group))
summary_ET_handgrip_lat_fig$age_group[summary_ET_handgrip_lat_fig$age_group == 'OA'] <- 'Older'
summary_ET_handgrip_lat_fig$age_group[summary_ET_handgrip_lat_fig$age_group == 'YA'] <- 'Younger'
summary_ET_handgrip_lat_fig$group <- factor(summary_ET_handgrip_lat_fig$group,
                                        levels = c('Handgrip', 'Control'))
summary_ET_handgrip_lat_fig$age_group <- factor(summary_ET_handgrip_lat_fig$age_group,
                                            levels = c('Younger', 'Older'))
summary_ET_handgrip_lat_fig$laterality <- factor(summary_ET_handgrip_lat_fig$laterality,
                                            levels = c('Ipsilateral', 'Contralateral'))


# create figure
figure3 <- ggarrange(
  
  ggplot(data = summary_ET_handgrip_lat_fig %>% 
           filter(event == 'squeeze',
                  age_group == 'Younger'),
         aes(x = laterality, y = pupil_corr,
             colour = group, group = group)) +
    geom_point(alpha = 0.2) +
    stat_summary(geom = 'crossbar', 
                 width = 0.25,
                 alpha = 0.3) +
    stat_summary(geom = 'line') +
    scale_colour_manual(values = cols_group) +
    coord_cartesian(ylim = c(-3.4, 4.4)) +
    facet_wrap(~group) +
    labs(x = 'Laterality of pupil\nrelative to squeezing hand', 
         y = 'Pupil diameter (au)', 
         colour = '',
         subtitle = 'Younger') +
    theme_pubr() + theme_rotatex + theme_font_pub + 
    theme(strip.text = element_blank(),
          plot.subtitle = element_text(face = 'bold')),
  
  ggplot(data = summary_ET_handgrip_lat_fig %>% 
           filter(event == 'squeeze',
                  age_group == 'Older'),
         aes(x = laterality, y = pupil_corr,
             colour = group, group = group)) +
    geom_point(alpha = 0.2) +
    stat_summary(geom = 'crossbar', 
                 width = 0.25,
                 alpha = 0.3) +
    stat_summary(geom = 'line') +
    scale_colour_manual(values = cols_group) +
    coord_cartesian(ylim = c(-3.4, 4.4)) +
    labs(x = 'Laterality of pupil\nrelative to squeezing hand', 
         y = 'Pupil diameter (au)', 
         colour = '',
         subtitle = 'Older') +
    facet_wrap(~group) +
    theme_pubr() + theme_rotatex + theme_font_pub + 
    theme(strip.text = element_blank(),
          plot.subtitle = element_text(face = 'bold')),
  
  nrow = 1, ncol = 2,
  common.legend = TRUE, legend = 'right'
)

ggsave(here('figures', 'figure3_handgrip-laterality.png'), 
       figure3,
       device = 'png', dpi = fig_dpi, bg = 'white',
       width = 8, height = 6)


# ANOVA: pupil laterality during squeeze phase of handgrip ----------------

# set factor levels for ANOVA
summary_ET_handgrip_lat$group <- factor(summary_ET_handgrip_lat$group, levels = c('handgrip', 'control'))
summary_ET_handgrip_lat$age_group <- factor(summary_ET_handgrip_lat$age_group, levels = c('YA', 'OA'))
summary_ET_handgrip_lat$laterality <- factor(summary_ET_handgrip_lat$laterality, levels = c('Ipsilateral', 'Contralateral'))
summary_ET_handgrip_lat$label_subject <- factor(summary_ET_handgrip_lat$label_subject)

# rmANOVA testing effects of event, group, age group, and their interactions on pupil
aov.hg_pupil_laterality <- aov_ez(id = 'label_subject', 
                       dv = 'pupil_corr', 
                       data = summary_ET_handgrip_lat %>% 
                         filter(event == 'squeeze'), 
                       within = 'laterality',
                       between = c('group', 'age_group'),
                       type = 3, 
                       anova_table = list(es = "pes"),
                       include_aov = TRUE)


# planned pairwise comparisons --------------------------------------------

# return estimated marginal means
emm.hg_pupil_laterality <- emmeans(aov.hg_pupil_laterality,
                        ~laterality | age_group * group)

# pairwise comparisons with bonferroni correction
contrasts.hg_pupil_laterality <- emm.hg_pupil_laterality %>%
  contrast('pairwise') %>%
  summary(by = NULL, 
          adjust = 'bonferroni') %>%
  as.data.frame() %>%
  mutate(Contrast = 'Ipsilateral - Contralateral') %>%
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
