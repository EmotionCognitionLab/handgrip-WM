# this script summarizes neuECG metrics
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(stringr)
library(dplyr)
library(data.table)
library(lubridate)


# list files containing r peaks -------------------------------------------

iSKNA_files <- list.files(here('data', 'derivatives', 'SAWM_iSKNA_output_2021-10-19'), 
                       pattern = '_iSKNA.csv', full.names = TRUE)

# subset files for each task
iSKNA_files_baseline <- iSKNA_files[str_detect(iSKNA_files, 'baseline')]
iSKNA_files_handgrip <- iSKNA_files[str_detect(iSKNA_files, 'handgrip')]
iSKNA_files_nback <- iSKNA_files[str_detect(iSKNA_files, 'nback')]


# set output filenames ----------------------------------------------------

out_file_baseline_neuECG <- here('results', paste('SAWM_baseline_neuECG_', today(), '.csv', sep = ''))
out_file_handgrip_neuECG <- here('results', paste('SAWM_handgrip_neuECG_', today(), '.csv', sep = ''))
out_file_nback_neuECG <- here('results', paste('SAWM_nback_neuECG_', today(), '.csv', sep = ''))

  
# loop over baseline files & summarize iSKNA ------------------------------

data_iSKNA_baseline <- data.frame()

for (ii in 1:length(iSKNA_files_baseline)) {
  
  ###### extract subject & task information from filename
  
  filename_full <- iSKNA_files_baseline[ii]
  temp <- str_split(filename_full, '/')[[1]]
  filename <- temp[length(temp)]
  
  subject <- str_split(filename, '_')[[1]][1]
  task <- 'baseline'
  message(paste('###### now processing data for: ', subject, ' / ', task, '...', sep = ''))
  
  ###### load file containing iSKNA timeseries
  
  data_iSKNA <- fread(filename_full)
  
  ###### summarize iSKNA
  
  data_iSKNA <- data_iSKNA %>%
    summarize(sig_length = max(time) - min(time),
              mean_iSKNA = mean(iSKNA),
              median_iSKNA = median(iSKNA),
              min_iSKNA = min(iSKNA),
              max_iSKNA = max(iSKNA),
              sd_iSKNA = sd(iSKNA),
              var_iSKNA = var(iSKNA)) %>%
    mutate(label_subject = subject,
           task = task,
           .before = sig_length)
  
  ###### bind to composite data
  
  data_iSKNA_baseline <- as.data.frame(
    rbind(data_iSKNA_baseline, data_iSKNA)
  )
  rm(data_iSKNA)
  
  message('all done!')
}

###### save data

write.csv(data_iSKNA_baseline, out_file_baseline_neuECG, 
          row.names = FALSE, quote = FALSE)
rm(data_iSKNA_baseline)


# loop over handgrip files & summarize iSKNA ------------------------------

data_iSKNA_handgrip <- data.frame()

for (ii in 1:length(iSKNA_files_handgrip)) {
  
  ###### extract subject & task information from filename
  
  filename_full <- iSKNA_files_handgrip[ii]
  temp <- str_split(filename_full, '/')[[1]]
  filename <- temp[length(temp)]
  
  subject <- str_split(filename, '_')[[1]][1]
  task <- str_remove_all(str_split(filename, 'task-')[[1]][2], '_iSKNA.csv')
  message(paste('###### now processing data for: ', subject, ' / ', task, '...', sep = ''))
  
  ###### load file containing iSKNA timeseries
  
  data_iSKNA <- fread(filename_full)
  
  ###### summarize iSKNA
  
  data_iSKNA <- data_iSKNA %>%
    group_by(event, round) %>%
    summarize(sig_length = max(time) - min(time),
              mean_iSKNA = mean(iSKNA),
              median_iSKNA = median(iSKNA),
              min_iSKNA = min(iSKNA),
              max_iSKNA = max(iSKNA),
              sd_iSKNA = sd(iSKNA),
              var_iSKNA = var(iSKNA)) %>%
    mutate(label_subject = subject,
           task = task,
           .before = event) %>%
    rowwise() %>%
    # make run number a numeric column
    mutate(run = as.numeric(str_split(task, 'handgrip_run-')[[1]][2]),
           .before = event) %>%
    select(-task)
  
  ###### bind to composite data
  
  data_iSKNA_handgrip <- as.data.frame(
    rbind(data_iSKNA_handgrip, data_iSKNA)
  )
  rm(data_iSKNA)
  
  message('all done!')
}

###### save data

write.csv(data_iSKNA_handgrip, out_file_handgrip_neuECG, 
          row.names = FALSE, quote = FALSE)
rm(data_iSKNA_handgrip)


# loop over nback files & summarize iSKNA ---------------------------------

data_iSKNA_nback <- data.frame()

for (ii in 1:length(iSKNA_files_nback)) {
  
  ###### extract subject & task information from filename
  
  filename_full <- iSKNA_files_nback[ii]
  temp <- str_split(filename_full, '/')[[1]]
  filename <- temp[length(temp)]
  
  subject <- str_split(filename, '_')[[1]][1]
  task <- str_remove_all(str_split(filename, 'task-')[[1]][2], '_iSKNA.csv')
  message(paste('###### now processing data for: ', subject, ' / ', task, '...', sep = ''))
  
  ###### load file containing iSKNA timeseries
  
  data_iSKNA <- fread(filename_full)
  
  ###### summarize iSKNA
  
  data_iSKNA <- data_iSKNA %>%
    group_by(event, block) %>%
    summarize(sig_length = max(time) - min(time),
              mean_iSKNA = mean(iSKNA),
              median_iSKNA = median(iSKNA),
              min_iSKNA = min(iSKNA),
              max_iSKNA = max(iSKNA),
              sd_iSKNA = sd(iSKNA),
              var_iSKNA = var(iSKNA)) %>%
    mutate(label_subject = subject,
           task = task,
           .before = event) %>%
    rowwise() %>%
    # make run number a numeric column
    mutate(run = as.numeric(str_split(task, 'nback_run-')[[1]][2]),
           .before = event) %>%
    select(-task)
  
  ###### bind to composite data
  
  data_iSKNA_nback <- as.data.frame(
    rbind(data_iSKNA_nback, data_iSKNA)
  )
  rm(data_iSKNA)
  
  message('all done!')
}

###### save data

write.csv(data_iSKNA_nback, out_file_nback_neuECG, 
          row.names = FALSE, quote = FALSE)
rm(data_iSKNA_nback)

