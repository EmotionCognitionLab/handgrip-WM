# this script aggregates EMG data for the handgrip task
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(stringr)
library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(fuzzyjoin)


# list physio handgrip files ----------------------------------------------

files_physio <- list.files(here('data', 'rawdata'), pattern = 'physio.tsv.gz',
                       recursive = TRUE, full.names = TRUE)

# subset to include baseline & handgrip files
# (need baseline for baseline correction)
files_baseline <- files_physio[str_detect(files_physio, 'baseline')]
files_handgrip <- files_physio[str_detect(files_physio, 'handgrip')]


# set output filenames ----------------------------------------------------

out_file_baseline_EMG <- here('results', paste('SAWM_baseline_EMG_', today(), '.csv', sep = ''))
out_file_handgrip_EMG <- here('results', paste('SAWM_handgrip_EMG_', today(), '.csv', sep = ''))


# loop over baseline files & extract EMG metrics --------------------------

data_EMG_baseline <- data.frame()

for (ii in 1:length(files_baseline)) {
  
  ###### extract subject & task information from filename
  
  filename <- str_split(files_baseline[ii], '/')[[1]][length(str_split(files_baseline[ii], '/')[[1]])]
  subject <- str_split(filename, '_')[[1]][1]
  task <-  'baseline'
  message(paste('###### processing data for: ', subject, ' / ', task, '...', sep = ''))
  
  ###### load & organize data
  
  data <- fread(files_baseline[ii])
  
  # add time vector
  data$time <- seq(from = 0, to = (nrow(data)/2000)-(1/2000), by = 1/2000)
  
  # remove baseline data beyond 240s
  data <- data %>%
    filter(time < 240)
  
  ###### calculate summary stats for left and right integrated EMG signal
  
  data_subject <- data %>%
    select(time, 
           left = emg_int_left, 
           right = emg_int_right) %>%
    mutate(task = 'baseline') %>%
    pivot_longer(cols = left:right, 
                 names_to = 'hemi', 
                 values_to = 'emg_int') %>%
    group_by(task, hemi) %>%
    summarize(sig_length = max(time) - min(time),
              mean_emg = mean(emg_int),
              median_emg = median(emg_int),
              min_emg = min(emg_int),
              max_emg = max(emg_int),
              sd_emg = sd(emg_int),
              var_emg = var(emg_int)) %>%
    mutate(label_subject = subject, 
           .before = task)
  
  ###### join with composite data
  
  data_EMG_baseline <- as.data.frame(
    rbind(data_EMG_baseline, data_subject)
  )
  rm(data_subject)
  
}

###### save data
write.csv(data_EMG_baseline, file = out_file_baseline_EMG,
          row.names = FALSE, quote = FALSE)


# loop over handgrip files & extract EMG metrics ----------------------------

data_EMG_handgrip <- data.frame()

for (ii in 1:length(files_handgrip)) {
  
  ###### extract subject & task information from filename
  
  filename <- str_split(files_handgrip[ii], '/')[[1]][length(str_split(files_handgrip[ii], '/')[[1]])]
  subject <- str_split(filename, '_')[[1]][1]
  task <- str_c('handgrip_', str_split(filename, '_')[[1]][3], sep = '')
  message(paste('###### processing data for: ', subject, ' / ', task, '...', sep = ''))
  
  ###### load & organize data
  
  data <- fread(files_handgrip[ii])
  
  # add time vector
  data$time <- seq(from = 0, to = (nrow(data)/2000)-(1/2000), by = 1/2000)
  
  # remove handgrip data beyond 202s
  data <- data %>%
    filter(time < 202)
  
  ###### calculate summary stats for left and right integrated EMG signal
  
  data_subject <- data %>%
    select(time, event, round,
           left = emg_int_left, 
           right = emg_int_right) %>%
    pivot_longer(cols = left:right, 
                 names_to = 'hemi', 
                 values_to = 'emg_int') %>%
    group_by(event, round, hemi) %>%
    summarize(sig_length = max(time) - min(time),
              mean_emg = mean(emg_int),
              median_emg = median(emg_int),
              min_emg = min(emg_int),
              max_emg = max(emg_int),
              sd_emg = sd(emg_int),
              var_emg = var(emg_int)) %>%
    mutate(label_subject = subject, 
           task = task,
           .before = event) %>%
    rowwise() %>%
    # make run number a numeric column
    mutate(run = as.numeric(str_split(task, 'handgrip_run-')[[1]][2]),
           .before = event) %>%
    select(-task)
  
  ###### join with composite data
  
  data_EMG_handgrip <- as.data.frame(
    rbind(data_EMG_handgrip, data_subject)
  )
  rm(data_subject)
  
}

###### save data
write.csv(data_EMG_handgrip, file = out_file_handgrip_EMG,
          row.names = FALSE, quote = FALSE)
