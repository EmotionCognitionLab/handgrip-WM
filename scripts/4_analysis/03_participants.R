# this script identifies participants to exclude
# from all & some analyses
# and applies them to the data
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# table 1: sample characteristics -----------------------------------------

if (internal == 1) {
  
  table1 <- data_subjects %>%
    group_by(age_group, group) %>%
    summarize(N = n(),
              age_mean = mean(age),
              age_sd = sd(age),
              age_min = min(age),
              age_max = max(age),
              
              n_female = sum(gender == 'female'),
              pct_female = sum(gender == 'female')/N,
              
              edu_mean = mean(edu),
              edu_sd = sd(edu),
              edu_max = max(edu),
              edu_min = min(edu),
              
              mmse_mean = mean(MMSE, na.rm = TRUE),
              mmse_sd = sd(MMSE, na.rm = TRUE),
              mmse_min = min(MMSE, na.rm = TRUE),
              mmse_max = max(MMSE, na.rm = TRUE)
    ) %>%
    rowwise() %>%
    mutate(mmse_mean = ifelse(mmse_mean == 'NaN', NA, mmse_mean),
           mmse_min = ifelse(mmse_min == Inf, NA, mmse_min),
           mmse_max = ifelse(mmse_max == Inf, NA, mmse_max)) %>%
    mutate(`Age, mean (SD)` = paste(printnum(age_mean), ' (', printnum(age_sd), ')', sep = ''),
           `Age, range` = paste(round(age_min, 0), '-', round(age_max, 0), sep = ''),
           `N (%) female` = paste(round(n_female, 0), ' (', printnum(pct_female), ')', sep = ''),
           `Edu, mean (SD)` = paste(printnum(edu_mean), ' (', printnum(edu_sd), ')', sep = ''),
           `Edu, range` = paste(round(edu_min, 0), '-', round(edu_max, 0), sep = ''),
           `MMSE, mean (SD)` = ifelse(is.na(mmse_mean), NA, paste(printnum(mmse_mean), ' (', printnum(mmse_sd), ')', sep = '')),
           `MMSE, range` = ifelse(is.na(mmse_mean), NA, paste(round(mmse_min, 0), '-', round(mmse_max, 0), sep = ''))) %>%
    arrange(desc(age_group), desc(group)) %>%
    mutate(age_group = ifelse(age_group == 'YA', 'Younger',
                              ifelse(age_group == 'OA', 'Older', NA)),
           group = ifelse(group == 'handgrip', 'Handgrip',
                          ifelse(group == 'control', 'Control', NA))) %>%
    select(`Age group`= age_group, `Group` = group, N, 
           `Age, mean (SD)`, `Age, range`, `N (%) female`,
           `Edu, mean (SD)`, `Edu, range`, `MMSE, mean (SD)`, `MMSE, range`)
  
}

if (internal == 0) {
  
  table1 <- data_subjects %>%
    group_by(age_group, group) %>%
    summarize(N = n(),
              
              n_female = sum(gender == 'female'),
              pct_female = sum(gender == 'female')/N,
              
              mmse_mean = mean(MMSE, na.rm = TRUE),
              mmse_sd = sd(MMSE, na.rm = TRUE),
              mmse_min = min(MMSE, na.rm = TRUE),
              mmse_max = max(MMSE, na.rm = TRUE)
    ) %>%
    rowwise() %>%
    mutate(mmse_mean = ifelse(mmse_mean == 'NaN', NA, mmse_mean),
           mmse_min = ifelse(mmse_min == Inf, NA, mmse_min),
           mmse_max = ifelse(mmse_max == Inf, NA, mmse_max)) %>%
    mutate(`N (%) female` = paste(round(n_female, 0), ' (', printnum(pct_female), ')', sep = ''),
           `MMSE, mean (SD)` = ifelse(is.na(mmse_mean), NA, paste(printnum(mmse_mean), ' (', printnum(mmse_sd), ')', sep = '')),
           `MMSE, range` = ifelse(is.na(mmse_mean), NA, paste(round(mmse_min, 0), '-', round(mmse_max, 0), sep = ''))) %>%
    arrange(desc(age_group), desc(group)) %>%
    mutate(age_group = ifelse(age_group == 'YA', 'Younger',
                              ifelse(age_group == 'OA', 'Older', NA)),
           group = ifelse(group == 'handgrip', 'Handgrip',
                          ifelse(group == 'control', 'Control', NA))) %>%
    select(`Age group`= age_group, `Group` = group, N, 
           `N (%) female`,
           `MMSE, mean (SD)`, `MMSE, range`)
  
}

