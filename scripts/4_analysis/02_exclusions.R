# this script performs exclusions
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# in this script, 2 steps are performed:

# (1) analyze EMG data from the handgrip data
# to identify any participants in the handgrip group
# who did not have higher EMG signal during squeeze vs. rest
# (these participants are excluded from all analysis)

# (2) identify participants to exclude from all & some analyses
# applies them to the data



# prepare left/right grip information for EMG analysis --------------------

grip_table <- as.data.frame(
  rbind(
    # if `grip` == 1, participant gripped with left hand first in first block
    data.frame(grip = rep(1, 12),
               run = c(rep(1, 4), rep(2, 4), rep(3, 4)),
               round = rep(c(1, 2, 3, 4), 3),
               grip_side = c('left', 'right', 'left', 'right', 
                             'right', 'left', 'right', 'left', 
                             'left', 'right', 'left', 'right')),
    # if `grip` == 2, participant gripped with right hand first in first block
    data.frame(grip = rep(2, 12),
               run = c(rep(1, 4), rep(2, 4), rep(3, 4)),
               round = rep(c(1, 2, 3, 4), 3),
               grip_side = c('right', 'left', 'right', 'left', 
                             'left', 'right', 'left', 'right', 
                             'right', 'left', 'right', 'left'))
  ))


# summarize EMG during handgrip -------------------------------------------

summary_hg_EMG <- left_join(
  
  # subset EMG data from rest
  data_EMG_handgrip %>%
    filter(event == 'rest') %>%
    select(label_subject, run, hemi, round, rest_emg = mean_emg),
  
  # subset EMG data from squeeze
  data_EMG_handgrip %>%
    filter(event == 'squeeze') %>%
    select(label_subject, run, hemi, round, squeeze_emg = mean_emg),
  
  # join rest and squeeze EMG data
  by = c('label_subject', 'run', 'hemi', 'round')
  
) %>%
  
  # bind age group, group & grip data
  left_join(data_subjects %>% 
              select(label_subject, age_group, group, grip)) %>%
  left_join(grip_table, by = c('grip', 'run', 'round')) %>%
  
  # filter to only include side where where squeeze happened for each round
  filter(hemi == grip_side) %>%
  
  # bind mean baseline EMG values
  left_join(data_EMG_baseline %>%
              select(label_subject, hemi, baseline_emg = mean_emg),
            by = c('label_subject', 'hemi')) %>%
  
  # baseline correct squeeze/rest EMG values
  rowwise() %>%
  mutate(squeeze_emg_corr = log(squeeze_emg) - log(baseline_emg),
         rest_emg_corr = log(rest_emg) - log(baseline_emg))


# identify exclusions based on EMG ----------------------------------------

# (identify handgrip-group participants where mean EMG signal for squeeze 
# was not greater than that for rest)

subs_exclude_EMG <- summary_hg_EMG %>%
  # average over runs and rounds
  group_by(label_subject, group, age_group) %>%
  summarize(squeeze_emg_corr = mean(squeeze_emg_corr),
            rest_emg_corr = mean(rest_emg_corr)) %>%
  
  # for each participant, compute difference between squeeze and rest for each round
  rowwise() %>%
  mutate(emg_squeezerest_diff = squeeze_emg_corr - rest_emg_corr) %>%
  
  # return only those in handgrip group with mean difference < 0
  filter(group == 'handgrip',
         emg_squeezerest_diff < 0)
  
subs_exclude_EMG <- subs_exclude_EMG$label_subject


# add exclusion information to participant data ---------------------------

if (internal == 0) {
  data_subjects <- data_subjects %>%
    rowwise() %>%
    mutate(exclude_overall = ifelse(label_subject %in% subs_exclude_EMG, 1, exclude_overall))
} else if (internal == 1) {
  data_subjects <- data_subjects %>%
    rowwise() %>%
    mutate(exclude_overall = ifelse(label_subject %in% subs_exclude_EMG, 1, exclude_overall),
           ifexclude_reason = ifelse(label_subject %in% subs_exclude_EMG, 'did_not_squeeze_EMG', ifexclude_reason))
}

# list of participants to exclude -----------------------------------------

# IDs included from all analyses include:
# (a) those who did not comply with pre-study requirements
# (b) those who did not squeeze during handgrip based on EMG
subs_exclude_all <- data_subjects$label_subject[data_subjects$exclude_overall == 1]

# save demographic data from those excluded
data_exclude_all <- data_subjects %>%
  filter(label_subject %in% subs_exclude_all)

# these are excluded ONLY from analyses of n-back
subs_exclude_nback <- data_subjects$label_subject[data_subjects$exclude_nback == 1]

# save demographic data from those excluded from n-back
data_exclude_nback <- data_subjects %>%
  filter(label_subject %in% subs_exclude_nback)


# apply exclusions to data ------------------------------------------------

# (first, make a copy of the data)
data_subjects_all <- data_subjects

data_subjects <- data_subjects %>%
  filter(!label_subject %in% subs_exclude_all)

data_qc_ECG <- data_qc_ECG %>%
  filter(!label_subject %in% subs_exclude_all)

data_EMG_handgrip <- data_EMG_handgrip %>%
  filter(!label_subject %in% subs_exclude_all)
data_ET_baseline <- data_ET_baseline %>%
  filter(!label_subject %in% subs_exclude_all)

data_ET_baseline <- data_ET_baseline %>%
  filter(!label_subject %in% subs_exclude_all)
data_ET_handgrip <- data_ET_handgrip %>%
  filter(!label_subject %in% subs_exclude_all)
data_ET_nback <- data_ET_nback %>%
  filter(!label_subject %in% subs_exclude_all)
data_ET_nback_ts <- data_ET_nback_ts %>%
  filter(!label_subject %in% subs_exclude_all)

data_HR_baseline <- data_HR_baseline %>%
  filter(!label_subject %in% subs_exclude_all)
data_HR_handgrip <- data_HR_handgrip %>%
  filter(!label_subject %in% subs_exclude_all)
data_HR_nback <- data_HR_nback %>%
  filter(!label_subject %in% subs_exclude_all)

data_aSKNA_baseline<- data_aSKNA_baseline %>%
  filter(!label_subject %in% subs_exclude_all)
data_aSKNA_handgrip <- data_aSKNA_handgrip %>%
  filter(!label_subject %in% subs_exclude_all)
data_aSKNA_nback <- data_aSKNA_nback %>%
  filter(!label_subject %in% subs_exclude_all)

# for n-back data, additional exclusions applied
data_nback <- data_nback %>%
  filter(!label_subject %in% c(subs_exclude_all, subs_exclude_nback))
data_ET_nback <- data_ET_nback %>%
  filter(!label_subject %in% c(subs_exclude_all, subs_exclude_nback))
data_HR_nback <- data_HR_nback %>%
  filter(!label_subject %in% c(subs_exclude_all, subs_exclude_nback))

data_aSKNA_nback <- data_aSKNA_nback %>%
  filter(!label_subject %in% c(subs_exclude_all, subs_exclude_nback))
data_ET_nback_ts <- data_ET_nback_ts %>%
  filter(!label_subject %in% c(subs_exclude_all, subs_exclude_nback))


# summarize ECG segment exclusions ----------------------------------------

summary_qc_ECG <- data_qc_ECG %>%
  left_join(data_subjects %>% select(label_subject, age_group, group),
            by = 'label_subject') %>%
  group_by(age_group, task) %>%
  summarize(n_segments_excl = sum(qc_rating_ecg == 0),
            frac_segments_excl = sum(qc_rating_ecg == 0)/nrow(data_qc_ECG)) %>%
  rowwise() %>%
  mutate(`N (%) segments excluded` = str_c(n_segments_excl, ' (', round(frac_segments_excl, 3), '%)'),
         task = str_to_title(task)) %>%
  arrange(desc(age_group)) %>%
  rowwise() %>%
  mutate(`Age group` = ifelse(age_group == 'YA', 'Younger',
                              ifelse(age_group == 'OA', 'Older', NA))) %>%
  select(`Age group`, Task = task, `N (%) segments excluded`)


# perform exclusions of ECG segments in HR and aSKNA data
# (excluded segments get HR and aSKNA values = NA)
data_HR_baseline <- data_HR_baseline %>%
  left_join(data_qc_ECG %>% 
              filter(task == 'baseline') %>%
              select(label_subject, qc_rating_ecg),
            by = 'label_subject') %>%
  rowwise() %>%
  mutate(mean_niHR = ifelse(qc_rating_ecg == 0, NA, mean_niHR)) %>%
  select(-qc_rating_ecg)

data_aSKNA_baseline <- data_aSKNA_baseline %>%
  left_join(data_qc_ECG %>% 
              filter(task == 'baseline') %>%
              select(label_subject, qc_rating_ecg),
            by = 'label_subject') %>%
  rowwise() %>%
  mutate(mean_iSKNA = ifelse(qc_rating_ecg == 0, NA, mean_iSKNA)) %>%
  select(-qc_rating_ecg)

data_HR_handgrip <- data_HR_handgrip %>%
  left_join(data_qc_ECG %>%
              filter(task == 'handgrip') %>%
              select(label_subject, run, qc_rating_ecg),
            by = c('run', 'label_subject')) %>%
  rowwise() %>%
  mutate(mean_niHR = ifelse(qc_rating_ecg == 0, NA, mean_niHR)) %>%
  select(-qc_rating_ecg)

data_aSKNA_handgrip <- data_aSKNA_handgrip %>%
  left_join(data_qc_ECG %>%
              filter(task == 'handgrip') %>%
              select(label_subject, run, qc_rating_ecg),
            by = c('run', 'label_subject')) %>%
  rowwise() %>%
  mutate(mean_iSKNA = ifelse(qc_rating_ecg == 0, NA, mean_iSKNA)) %>%
  select(-qc_rating_ecg)

data_HR_nback <- data_HR_nback %>%
  left_join(data_qc_ECG %>%
              filter(task == 'nback') %>%
              select(label_subject, run, qc_rating_ecg),
            by = c('run', 'label_subject')) %>%
  rowwise() %>%
  mutate(mean_niHR = ifelse(qc_rating_ecg == 0, NA, mean_niHR)) %>%
  select(-qc_rating_ecg)
  
data_aSKNA_nback <- data_aSKNA_nback %>%
  left_join(data_qc_ECG %>%
              filter(task == 'nback') %>%
              select(label_subject, run, qc_rating_ecg),
            by = c('run', 'label_subject')) %>%
  rowwise() %>%
  mutate(mean_iSKNA = ifelse(qc_rating_ecg == 0, NA, mean_iSKNA)) %>%
  select(-qc_rating_ecg)


# summarize segments not analyzed in physionet ----------------------------

summary_physionet_notanalyzed <- data_physionet_notanalyzed %>%
  select(V1) %>%
  rowwise() %>%
  mutate(filename = str_split(V1, ': ')[[1]][2]) %>%
  mutate(label_subject = str_split(filename, '_')[[1]][1],
         task = str_split(str_split(filename, '_')[[1]][2], 'task-')[[1]][2]) %>%
  mutate(run = ifelse(task == 'baseline', NA, 
                      ifelse(task %in% c('handgrip', 'nback'), 
                             as.numeric(str_split(filename, '_run-')[[1]][2]), NA))) %>%
  select(label_subject, task, run) %>%
  mutate(analysis_failed_physionet = 1)
