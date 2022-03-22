# this script extracts the niHR timeseries and HRV metrics
# using the RHRV package, for baseline/handgrip/nback tasks
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(stringr)
library(dplyr)
library(data.table)
library(RHRV)
library(lubridate)
library(ggplot2)
library(fuzzyjoin)


# list files containing r peaks -------------------------------------------

annot_files <- list.files(here('data', 'derivatives', 'SAWM_HRV_output_2021-10-19', 'Annotation'), 
                       pattern = '_annot.csv', full.names = TRUE)

# subset files for each task
rr_files_baseline <- annot_files[str_detect(annot_files, 'baseline')]
rr_files_handgrip <- annot_files[str_detect(annot_files, 'handgrip')]
rr_files_nback <- annot_files[str_detect(annot_files, 'nback')]


# set output filenames ----------------------------------------------------

out_file_baseline_niHR <- here('results', paste('SAWM_baseline_niHR_', today(), '.csv', sep = ''))
out_file_handgrip_niHR <- here('results', paste('SAWM_handgrip_niHR_', today(), '.csv', sep = ''))
out_file_nback_niHR <- here('results', paste('SAWM_nback_niHR_', today(), '.csv', sep = ''))

  
# loop over baseline files & analyze niHR/HRV -----------------------------

data_niHR_baseline <- data.frame()

for (ii in 1:length(rr_files_baseline)) {
  
  ###### extract subject & task information from filename
  
  filename_full <- rr_files_baseline[ii]
  temp <- str_split(filename_full, '/')[[1]]
  filename <- temp[length(temp)]
  
  subject <- str_split(filename, '_')[[1]][1]
  task <- 'baseline'
  message(paste('###### now processing data from: ', subject, ' / ', task, '...', sep = ''))
  
  # load file containing time & sample indices of r-peaks
  data_rr <- fread(filename_full)
  
  ###### prepare data for HRV analysis
  
  # create empty HRVData structure
  hrv.data <- CreateHRVData()
  #hrv.data <- SetVerbose(hrv.data, TRUE)
  
  # import data containing heartbeat positions to HRVData structure
  hrv.data <- LoadBeatVector(hrv.data, data_rr$time, scale = 1)
  
  # build RR and instantaneous heart rate time series (niHR)
  hrv.data <- BuildNIHR(hrv.data)
  
  # filtering operation to eliminate outliers/spurious points in the niHR time series
  # here only using automatic removal of spurious points
  hrv.data <- FilterNIHR(hrv.data)
  
  # interpolate niHR series to generate a uniformly sampled HR series
  # here using the default interpolation, 4Hz
  hrv.data <- InterpolateNIHR(hrv.data, freqhr = 4)
  
  ###### create niHR dataframe and bind event information
  
  time <- seq(from = 0, to = length(hrv.data$HR)/4, by = (1/4))
  time <- time[1:(length(time)-1)]
  data_subject <- data.frame(time = time, niHR = hrv.data$HR) %>%
    mutate(label_subject = subject, task = task, .before = time)
  
  ###### summarize niHR data
  
  data_subject <- data_subject %>%
    group_by(task) %>%
    summarize(sig_length = max(time),
              mean_niHR = mean(niHR),
              median_niHR = median(niHR),
              min_niHR = min(niHR),
              max_niHR = max(niHR),
              sd_niHR = sd(niHR),
              var_niHR = var(niHR)) %>%
    mutate(label_subject = subject, .before = task)
  
  ###### bind to composite data
  
  data_niHR_baseline <- as.data.frame(
    rbind(data_niHR_baseline, data_subject)
  )
  rm(data_subject)
  
  message('all done!')
}

###### save data

write.csv(data_niHR_baseline, out_file_baseline_niHR, 
          row.names = FALSE, quote = FALSE)
rm(data_niHR_baseline)


# loop over handgrip files & analyze niHR/HRV -----------------------------

data_niHR_handgrip <- data.frame()

for (ii in 1:length(rr_files_handgrip)) {
  
  ###### extract subject & task information from filename
  
  filename_full <- rr_files_handgrip[ii]
  temp <- str_split(filename_full, '/')[[1]]
  filename <- temp[length(temp)]
  
  subject <- str_split(filename, '_')[[1]][1]
  task <- str_remove_all(str_split(filename, 'task-')[[1]][2], '_annot.csv')
  message(paste('###### now processing data from: ', subject, ' / ', task, '...', sep = ''))
  
  ###### load physio data file for timing info
  
  # build name of original physio file
  filename_physio <- here('data', 'rawdata', subject, paste(subject, '_task-', task, '_physio.tsv.gz', sep = ''))
  data_physio <- fread(filename_physio)
  data_physio$index <- 1:nrow(data_physio)
  time_vec <- seq(from = 0, to = nrow(data_physio)/2000, by = 1/2000)
  data_physio$time <- time_vec[1:length(time_vec)-1]
  
  # summarize timing of physio data
  data_physio <- data_physio %>%
    group_by(event, round) %>%
    summarize(time_start = min(time),
              time_end = max(time))
  
  ###### prepare data for HRV analysis
  
  # load file containing time & sample indices of r-peaks
  data_rr <- fread(filename_full)
  
  # create empty HRVData structure
  hrv.data <- CreateHRVData()
  #hrv.data <- SetVerbose(hrv.data, TRUE)
  
  # import data containing heartbeat positions to HRVData structure
  hrv.data <- LoadBeatVector(hrv.data, data_rr$time, scale = 1)
  
  # build RR and instantaneous heart rate time series (niHR)
  hrv.data <- BuildNIHR(hrv.data)
  
  # filtering operation to eliminate outliers/spurious points in the niHR time series
  # here only using automatic removal of spurious points
  hrv.data <- FilterNIHR(hrv.data)
  
  # interpolate niHR series to generate a uniformly sampled HR series
  # here using the default interpolation, 4Hz
  hrv.data <- InterpolateNIHR(hrv.data, freqhr = 4)
  
  ###### create niHR dataframe and bind event 
  
  time <- seq(from = 0, to = length(hrv.data$HR)/4, by = (1/4))
  time <- time[1:(length(time)-1)]
  data_subject <- data.frame(time = time, niHR = hrv.data$HR) %>%
    mutate(label_subject = subject, task = task, .before = time)
  
  # identify event and round using physio timing
  data_subject <- fuzzy_left_join(data_subject, data_physio,
                  by = c("time" = "time_start",
                         "time" = "time_end"),
                  match_fun = list(`>=`, `<=`)) %>%
    select(-c(time_start, time_end))
  
  ###### summarize niHR data
  
  data_subject <- data_subject %>%
    group_by(event, round) %>%
    summarize(sig_length = max(time) - min(time),
              mean_niHR = mean(niHR),
              median_niHR = median(niHR),
              min_niHR = min(niHR),
              max_niHR = max(niHR),
              sd_niHR = sd(niHR),
              var_niHR = var(niHR)) %>%
    mutate(label_subject = subject, 
           task = task, 
           .before = event) %>%
    rowwise() %>%
    # make run number a numeric column
    mutate(run = as.numeric(str_split(task, 'handgrip_run-')[[1]][2]),
           .before = event) %>%
    select(-task)
  
  ###### bind to composite data
  
  data_niHR_handgrip <- as.data.frame(
    rbind(data_niHR_handgrip, data_subject)
  )
  
  message('all done!')
  
}

###### save data

write.csv(data_niHR_handgrip, out_file_handgrip_niHR, 
          row.names = FALSE, quote = FALSE)
rm(data_niHR_handgrip)


# loop over nback files & analyze niHR/HRV --------------------------------

data_niHR_nback <- data.frame()

for (ii in 1:length(rr_files_nback)) {
  
  ###### extract subject & task information from filename
  
  filename_full <- rr_files_nback[ii]
  temp <- str_split(filename_full, '/')[[1]]
  filename <- temp[length(temp)]
  
  subject <- str_split(filename, '_')[[1]][1]
  task <- str_remove_all(str_split(filename, 'task-')[[1]][2], '_annot.csv')
  message(paste('###### now processing data from: ', subject, ' / ', task, '...', sep = ''))
  
  ###### load physio data file for timing info
  
  # build name of original physio file
  filename_physio <- here('data', 'rawdata', subject, paste(subject, '_task-', task, '_physio.tsv.gz', sep = ''))
  data_physio <- fread(filename_physio)
  data_physio$index <- 1:nrow(data_physio)
  time_vec <- seq(from = 0, to = nrow(data_physio)/2000, by = 1/2000)
  data_physio$time <- time_vec[1:length(time_vec)-1]
  
  # summarize timing of physio data
  data_physio <- data_physio %>%
    group_by(event, block) %>%
    summarize(time_start = min(time),
              time_end = max(time))

  ###### prepare data for HRV analysis
  
  # load file containing time & sample indices of r-peaks
  data_rr <- fread(filename_full)
  
  # create empty HRVData structure
  hrv.data <- CreateHRVData()
  #hrv.data <- SetVerbose(hrv.data, TRUE)
  
  # import data containing heartbeat positions to HRVData structure
  hrv.data <- LoadBeatVector(hrv.data, data_rr$time, scale = 1)
  
  # build RR and instantaneous heart rate time series (niHR)
  hrv.data <- BuildNIHR(hrv.data)
  
  # filtering operation to eliminate outliers/spurious points in the niHR time series
  # here only using automatic removal of spurious points
  hrv.data <- FilterNIHR(hrv.data)
  
  # interpolate niHR series to generate a uniformly sampled HR series
  # here using the default interpolation, 4Hz
  hrv.data <- InterpolateNIHR(hrv.data, freqhr = 4)
  
  ###### create niHR dataframe and bind event information
  
  time <- seq(from = 0, to = length(hrv.data$HR)/4, by = (1/4))
  time <- time[1:(length(time)-1)]
  data_subject <- data.frame(time = time, niHR = hrv.data$HR) %>%
    mutate(subject = subject, task = task, .before = time)
  
  # identify event and round using physio timing
  data_subject <- fuzzy_left_join(data_subject, data_physio,
                                  by = c("time" = "time_start",
                                         "time" = "time_end"),
                                  match_fun = list(`>=`, `<=`)) %>%
    select(-c(time_start, time_end))
  
  ###### summarize niHR data
  
  data_subject <- data_subject %>%
    group_by(event, block) %>%
    summarize(sig_length = max(time) - min(time),
              mean_niHR = mean(niHR),
              median_niHR = median(niHR),
              min_niHR = min(niHR),
              max_niHR = max(niHR),
              sd_niHR = sd(niHR),
              var_niHR = var(niHR)) %>%
    mutate(label_subject = subject, 
           task = task, 
           .before = event) %>%
    rowwise() %>%
    # make run number a numeric column
    mutate(run = as.numeric(str_split(task, 'nback_run-')[[1]][2]),
           .before = event) %>%
    select(-task)
  
  ###### bind to composite data
  
  data_niHR_nback <- as.data.frame(
    rbind(data_niHR_nback, data_subject)
  )
  
  message('all done!')
  
}

###### save data

write.csv(data_niHR_nback, out_file_nback_niHR, 
          row.names = FALSE, quote = FALSE)
rm(data_niHR_nback)

