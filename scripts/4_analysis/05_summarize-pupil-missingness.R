# this script summarizes pupil diameter during baseline
# and summarizes pupil data missingness
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# organize baseline eyetracking data --------------------------------------

# (these values are used for baseline-correcting pupil diameter data 
# throughout the experiment)

pupil_norm_baseline <- data_ET_baseline %>%
  # apply missingness threshold
  mutate(pupil_mean = ifelse(frac_missing > thresh, NA, pupil_mean)) %>%
  select(label_subject, hemi, pupil_baseline = pupil_mean)


# record tallies on missingness in baseline eyetracking data --------------

ET_missing_baseline <- data_ET_baseline %>%
  
  # apply event-wise missingness threshold
  mutate(pupil_missing = ifelse(frac_missing > thresh, 1, 0)) %>%
  
  # compute missingness by participant
  group_by(label_subject) %>%
  summarize(n_segments_total = n(),
            n_segments_missing = sum(pupil_missing)) %>%
  
  # bind subject info
  # using a RIGHT join because we want to identify the few participants
  # that have overall eyetracking data,
  # but did not have any valid handgrip data and are not included in the summary above
  right_join(data_subjects %>%
               filter(has_eyetracking == 1) %>%
               select(label_subject, age_group, group),
             by = 'label_subject') %>%
  
  # now we handle participants who had no valid baseline data
  # for these participants, account for missing events in their tally
  mutate(n_segments_missing = ifelse(n_segments_total == 2, n_segments_missing,
                                     ifelse(!n_segments_total == 2, n_segments_missing + (2-n_segments_total), NA)),
         n_segments_total = ifelse(n_segments_total == 2, n_segments_total,
                                   ifelse(!n_segments_total == 2, 2, NA)))

# identify number of participants with no valid handgrip data
n_missing_ET_baseline <- ET_missing_baseline %>% filter(is.na(n_segments_total)) %>% nrow()

# summarize missingness across participants, by event
ET_missing_baseline_summary <- ET_missing_baseline %>%
  # if n_segments_total == NA, then those participants had no valid baseline data (accounted for below)
  filter(!is.na(n_segments_total)) %>%
  summarize(n_segments_total = sum(n_segments_total),
            n_segments_missing = sum(n_segments_missing)) %>%
  # account for particpiants with no valid handgrip data
  # by adding to tally of missingness (2 segments per missing ID = 2 eyes x 1 baseline)
  rowwise() %>%
  mutate(n_segments_total = n_segments_total + (n_missing_ET_baseline*2),
         n_segments_missing = n_segments_missing + (n_missing_ET_baseline*2)) %>%
  # compute fraction of missingness for each event and format for reporting in manuscript
  mutate(frac_segments_missing = n_segments_missing/n_segments_total,
         `N (%) missing data segments` = str_c(n_segments_missing, ' (', round(frac_segments_missing, digits = 2), '%)', sep = ''),
         `Part of experiment` = 'Baseline',
         Event = 'Baseline') %>%
  select(`N (%) missing data segments`)


# store IDs with missing baseline data
# (they get excluded at the bottom)
subs_ET_missing_baseline <- ET_missing_baseline$label_subject[ET_missing_baseline$n_segments_missing == 2 | is.na(ET_missing_baseline$n_segments_missing)]


# record tallies on missingness in handgrip eyetracking data --------------

# first, exclude participants missing baseline pupil data
data_ET_handgrip <- data_ET_handgrip %>%
  filter(!label_subject %in% subs_ET_missing_baseline)

ET_missing_handgrip <- data_ET_handgrip %>%
  
  # don't include initial rest data
  filter(!event == 'initial_rest') %>%
  
  # apply event-wise missingness threshold
  mutate(pupil_missing = ifelse(frac_missing > thresh, 1, 0)) %>%
  
  # merge event and round
  mutate(part = str_c(event, round, sep = '')) %>%
  
  # compute missingness by participant
  group_by(label_subject, part) %>%
  summarize(n_segments_total = n(),
            n_segments_missing = sum(pupil_missing)) %>%
  
  # bind subject info
  # using a RIGHT join because we want to identify the few participants
  # that have overall eyetracking data,
  # but did not have any valid handgrip data and are not included in the summary above
  right_join(data_subjects %>%
               filter(has_eyetracking == 1) %>%
               select(label_subject, age_group, group),
             by = 'label_subject') %>%
  
  # now we handle participants who had some runs with no valid handgrip data
  # for these participants, account for missing events in their tally
  mutate(n_segments_missing = ifelse(n_segments_total == 6, n_segments_missing,
                                     ifelse(!n_segments_total == 6, n_segments_missing + (6-n_segments_total), NA)),
         n_segments_total = ifelse(n_segments_total == 6, n_segments_total,
                                   ifelse(!n_segments_total == 6, 6, NA)))

# identify number of participants with no valid handgrip data
n_missing_ET_handgrip <- ET_missing_handgrip %>% filter(is.na(n_segments_total)) %>% nrow()

# summarize missingness across participants, by event
ET_missing_handgrip_summary <- ET_missing_handgrip %>%
  # if part == NA, then those participants had no valid handgrip data (accounted for below)
  filter(!is.na(part)) %>%
  group_by(part) %>%
  summarize(n_segments_total = sum(n_segments_total),
            n_segments_missing = sum(n_segments_missing)) %>%
  # account for particpiants with no valid handgrip data
  # by adding to tally of missingness (6 segments per missing ID = 2 eyes x 3 handgrip runs)
  rowwise() %>%
  mutate(n_segments_total = n_segments_total + (n_missing_ET_handgrip*6),
         n_segments_missing = n_segments_missing + (n_missing_ET_handgrip*6)) %>%
  # compute fracion of missingness for each event and format for reporting in manuscript
  mutate(frac_segments_missing = n_segments_missing/n_segments_total,
         `N (%) excluded` = str_c(n_segments_missing, ' (', round(frac_segments_missing, digits = 2), '%)', sep = ''),
         `Part of experiment` = 'Handgrip') %>%
  select(Event = part,
         `N (%) excluded`) %>%
  mutate(Event = str_to_title(Event))


# record tallies on missingness in n-back fixation data --------------------

ET_missing_nbfix <- data_ET_nback %>%
  
  # include participants missing baseline data segments in nbfix tally
  filter(!label_subject %in% subs_ET_missing_baseline) %>%

  # exclude practice run
  filter(!run == 0) %>%
  
  # include only initial fixation periods
  filter(event == 'initial_fixation') %>%
  
  # apply segment-wise missingness threshold
  mutate(pupil_missing = ifelse(frac_missing > thresh, 1, 0)) %>%
  
  # bind age group data
  left_join(data_subjects %>%
              select(label_subject, age_group),
            by = 'label_subject') %>%
  
  # recode block to reflect position wrt handgrip offset
  rowwise() %>%
  mutate(block_recoded = ifelse(age_group == 'YA' & block %in% c(1, 5, 9), 1, 
                                ifelse(age_group == 'YA' & block %in% c(2, 6, 10), 2,
                                       ifelse(age_group == 'YA' & block %in% c(3, 7, 11), 3,
                                              ifelse(age_group == 'YA' & block %in% c(4, 8, 12), 4,
                                                     ifelse(age_group == 'OA' & block %in% c(1, 4, 7), 1,
                                                            ifelse(age_group == 'OA' & block %in% c(2, 5, 8), 2,
                                                                   ifelse(age_group == 'OA' & block %in% c(3, 6, 9), 3, NA)))))))) %>%
  
  # compute missingness by participant
  group_by(label_subject, block_recoded) %>%
  summarize(n_segments_total = n(),
            n_segments_missing = sum(pupil_missing)) %>%
  
  # bind subject info
  # using a RIGHT join because we want to identify the few participants
  # that have overall eyetracking data,
  # but did not have any valid n-back fixation data and are not included in the summary above
  right_join(data_subjects %>%
               filter(has_eyetracking == 1) %>%
               select(label_subject, age_group, group),
             by = 'label_subject') %>%

  # now we handle participants who had some runs with no valid n-back fixation data
  # for these participants, account for missing events in their tally
  mutate(n_segments_missing = ifelse(n_segments_total == 6, n_segments_missing,
                                     ifelse(!n_segments_total == 6, n_segments_missing + (6-n_segments_total), NA)),
         n_segments_total = ifelse(n_segments_total == 6, n_segments_total,
                                   ifelse(!n_segments_total == 6, 6, NA)))
  
# identify number of participants with no valid n-back fixation data
n_missing_ET_nbfix <- ET_missing_nbfix %>% filter(is.na(n_segments_total)) %>% nrow()

# summarize missingness across participants, by event
ET_missing_nbfix_summary <- ET_missing_nbfix %>%
  # if n_segments_total == NA, then those participants had no valid n-back fixation data (accounted for below)
  filter(!is.na(n_segments_total)) %>%
  group_by(block_recoded) %>%
  summarize(n_segments_total = sum(n_segments_total),
            n_segments_missing = sum(n_segments_missing)) %>%
  # account for participants with no valid n-back fixation data
  # by adding to tally of missingness (6 segments per missing ID = 2 eyes x 3 n-back runs)
  rowwise() %>%
  mutate(n_segments_total = n_segments_total + (n_missing_ET_nbfix*6),
         n_segments_missing = n_segments_missing + (n_missing_ET_nbfix*6)) %>%
  # compute fraction of missingness for each event and format for reporting in manuscript
  mutate(frac_segments_missing = n_segments_missing/n_segments_total,
         `N (%) excluded` = str_c(n_segments_missing, ' (', round(frac_segments_missing, digits = 2), '%)', sep = ''),
         Event = str_c('Pre-block fixation: block ', block_recoded, ' relative to handgrip offset', sep = '')) %>%
  select(Event,
         `N (%) excluded`)


# record tallies on missingness in n-back trial data ----------------------

ET_missing_nbtrials <- data_ET_nback %>%
  
  # exclude practice run
  filter(!run == 0) %>%
  
  # include only initial fixation periods
  filter(event == 'trial') %>%
  
  # apply segment-wise missingness threshold
  mutate(pupil_missing = ifelse(frac_missing > thresh, 1, 0)) %>%
  
  # join trial data (condition, accuracy, RT)
  left_join(data_nback %>%
              select(label_subject, run, block, trial, condition),
            by = c('label_subject', 'run', 'block', 'trial')) %>%
  
  # compute missingness by participant
  group_by(label_subject, condition) %>%
  summarize(n_segments_total = n(),
            n_segments_missing = sum(pupil_missing)) %>%
  
  # bind subject info
  # using a RIGHT join because we want to identify the few participants
  # that have overall eyetracking data,
  # but did not have any valid n-back trial data and are not included in the summary above
  right_join(data_subjects %>%
               filter(has_eyetracking == 1) %>%
               select(label_subject, age_group, group),
             by = 'label_subject') %>%
  
  # now we handle participants who had some runs with no valid n-back fixation data
  # for these participants, account for missing events in their tally
  # (segments per bin = 2 eyes x 3 n-back runs x 18 trials per run = 108)
  mutate(n_segments_missing = ifelse(n_segments_total == 108, n_segments_missing,
                                     ifelse(!n_segments_total == 108, n_segments_missing + (108-n_segments_total), NA)),
         n_segments_total = ifelse(n_segments_total == 108, n_segments_total,
                                   ifelse(!n_segments_total == 108, 108, NA)))

# identify number of participants with no valid n-back fixation data
n_missing_ET_nbtrials <- ET_missing_nbtrials %>% filter(is.na(n_segments_total)) %>% nrow()

# summarize missingness across participants, by event
ET_missing_nbtrials_summary <- ET_missing_nbtrials %>%
  # if n_segments_total == NA, then those participants had no valid n-back fixation data (accounted for below)
  filter(!is.na(n_segments_total)) %>%
  group_by(condition) %>%
  summarize(n_segments_total = sum(n_segments_total),
            n_segments_missing = sum(n_segments_missing)) %>%
  # account for participants with no valid n-back fixation data
  # by adding to tally of missingness (108 segments per missing ID = 2 eyes x 3 n-back runs x 18 trials per run)
  rowwise() %>%
  mutate(n_segments_total = n_segments_total + (n_missing_ET_nbfix*108),
         n_segments_missing = n_segments_missing + (n_missing_ET_nbfix*108)) %>%
  # compute fraction of missingness for each event and format for reporting in manuscript
  mutate(frac_segments_missing = n_segments_missing/n_segments_total,
         `N (%) excluded` = str_c(n_segments_missing, ' (', round(frac_segments_missing, digits = 2), '%)', sep = ''),
         Event = str_c('Trials: ', condition, '-back', sep = '')) %>%
  select(Event,
         `N (%) excluded`)
