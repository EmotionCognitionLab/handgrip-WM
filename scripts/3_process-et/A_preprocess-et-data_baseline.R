# this script preprocesses and compiles preprocessed eyetracking data
# from the initial 4-minute baseline task
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(gazer)
library(saccades)  # make sure you have installed version from github, not CRAN
library(lubridate)
library(zoo)


# parameters --------------------------------------------------------------

hz <- 60    # freq at which to resample raw data
                   # (due to sometimes inconsistent sampling rate in raw data)

lambda <- 6  # a parameter for tuning the saccade detection - 
            # it specifies which multiple of the standard deviation of the 
            # velocity distribution should be used as the detection threshold
            
            # the larger the lambda, the higher the threshold and thus less 
            # blinks detected

blink_fillback <- 100 # ms to extend NAs from blinks backward in time

blink_fillforward <- 100 # ms to extend NAs from blinks forward in time

moving_avg_window <- 10 # moving average window for smoothing of pupil trace

sides <- c('left', 'right')


# function to interpolate pupil data --------------------------------------

interpolate_pupil_data <- function(data, start, end, hz) {
  
  # this function interpolate pupil data to a new frequency
  
  ### arguments: 
  # data: dataframe containing at least time, x, y, pupil columns
  # start: start time for this event, in seconds
  # end: end time for this event, in seconds
  # hz: sampling rate at which to interpolate data
  
  # resample time, trial, pupil, x and y vectors
  resamp_pupil <- approx(data$time, data$pupil,
                         xout = seq(from = start, to = end, by = 1/hz), 
                         na.rm = FALSE)
  resamp_x <- approx(data$time, data$x,
                     xout = seq(from = start, to = end, by = 1/hz),
                     na.rm = FALSE)
  resamp_y <- approx(data$time, data$y,
                     xout = seq(from = start, to = end, by = 1/hz),
                     na.rm = FALSE)
  
  # resample event vector
  events <- data.frame(trial_code = 1:nlevels(as.factor(data$trial)), 
                       trial = levels(as.factor(data$trial)))
  resamp_event <- approx(x = data$time, y = as.factor(data$trial), 
                         xout = seq(from = start, to = end, by = 1/hz))$y
  resamp_event <- data.frame(trial_code = resamp_event) %>%
    rowwise() %>% 
    left_join(events, by = 'trial_code')
  
  # replace NAs in event vector with most recent non-NA prior
  resamp_event$trial_noNA <- na.locf(resamp_event$trial)
  
  # arrange and return resampled data
  data_resamp <- data.frame(
    time = resamp_pupil$x, 
    trial_code = resamp_event$trial_code, 
    trial = resamp_event$trial_noNA,
    x = resamp_x$y, y = resamp_y$y, pupil = resamp_pupil$y
  )
  return(data_resamp)
  
}


# function to preprocess pupil data (baseline) ----------------------------

preprocess_pupil_data_baseline <- function(filename, subject, description) {
  
  print(paste("preprocessing pupil data for ", subject, ' ', description, sep=''))
  
  ## initialize results storage
  results <- vector(mode = 'list', length = 2)
  data_ET <- data.frame()
  stats_ET <- data.frame()
  
  ### load data and add subject column
  data <- fread(filename, sep = "\t", na.strings = 'n/a') %>%
    mutate(trial = description) %>%
    mutate(subject = subject)
  
  ### loop over eyes
  
  for (jj in 1:2) {
    
    ### prepare data ###
    # store relevant column names for this side
    pupil_col <- paste('diameter_', sides[jj], sep = '')
    x_col <- paste('pos_x_', sides[jj], sep = '')
    y_col <- paste('pos_y_', sides[jj], sep = '')
    
    # make non-confident pupil values NA
    data[data$pupil_confidence == 0, pupil_col] <- NA
    
    # prepare data for gazeR processing
    data_gazer <- make_gazer(data, subject = 'subject', 
                             trial = 'trial', time = 'time', 
                             x = x_col, y = y_col, 
                             pupil = pupil_col) %>%
      select(subject, trial, time, x, y, pupil)
    
    # interpolate to account for inconsistent sampling rate
    # baseline is always same length for participants (240s)
    data_gazer <- interpolate_pupil_data(data_gazer, 0, max(data_gazer$time), hz) %>%
      mutate(subject = subject, 
             trial = 'baseline', 
             .before = time) %>%
      select(-trial_code)
    
    ### deblinking ###
    # identifying and removing data during blinks, and interpolating data during
    # the blink
    # instead of calling blink_detect(), we run source code with different parameters
    # for detect.fixations() to try to get better blink detection
    blinks <- detect.fixations(data_gazer, lambda = lambda,
                               smooth.coordinates = TRUE,
                               smooth.saccades = FALSE) %>%
     filter(event == "blink") %>%
     tidyr::gather(key = "startend",
                   value = "time", start:end) %>%
      select(-x, -y)
    
    # merge blink data with prepared data
    # s.t. pupil values during blinks are marked as NA
    data_gazer <- merge(data_gazer, blinks,
                       by = c('trial', 'time'), all = TRUE) %>%
      mutate(pupil = ifelse(!is.na(event), NA, pupil)) %>%
      select(-mad.x, -mad.y, -peak.vy, -peak.vx, -dur)  # remove columns not using

    # interpolation during blinks (100 ms before and after as well)
    data_gazer <- data_gazer %>%
      mutate(extendpupil = extend_blinks(pupil, 
                                         fillback = blink_fillback, 
                                         fillforward = blink_fillforward,
                                         hz = hz))
    
    ### smoothing & interpolation ###
    # linear interpolation, smoothing pupil trace with moving average of 10
    data_gazer <- smooth_interpolate_pupil(data_gazer, 
                                           pupil = "pupil", 
                                           extendpupil = "extendpupil", 
                                           extendblinks = TRUE, 
                                           step.first = "interp", 
                                           filter="moving", 
                                           maxgap = Inf, 
                                           type = "linear", 
                                           hz = hz, 
                                           n = moving_avg_window)
    
    ### compute stats ###
    pupil_stats <- data_gazer %>%
      group_by(trial) %>%
      summarize(n_obs = n(),
                frac_missing = sum(is.na(extendpupil)) / n_obs,  # percentage of missing (NA) values after blink detection & extension
                pupil_mean = ifelse(frac_missing == 1, NA, mean(pup_interp, na.rm = TRUE)),
                pupil_sd = ifelse(frac_missing == 1, NA, sd(pup_interp, na.rm = TRUE)),
                pupil_min = ifelse(frac_missing == 1, NA, min(pup_interp, na.rm = TRUE)),
                pupil_max = ifelse(frac_missing == 1, NA, max(pup_interp, na.rm = TRUE))) %>%
      select(-n_obs) %>%
      rename(task = trial) %>%
      mutate(label_subject = subject, 
             .before = task) %>%
      mutate(hemi = sides[jj], .after = task) %>%
      select(label_subject, task, hemi, pupil_mean:pupil_max, frac_missing)

    ### format timeseries data ###
    data_gazer <- data_gazer %>%
      rename(task = trial) %>%
      mutate(hemi = sides[jj]) %>%
      select(label_subject = subject, task, time, hemi, pup_interp)
    
    ### save summary stats and timeseries data ###
    data_ET <- as.data.frame(
      rbind(data_ET, data_gazer)
    )
    stats_ET <- as.data.frame(
      rbind(stats_ET, pupil_stats)
    )
    
  }
  
  ### return list containing preprocessed data and stats ###
  results[[1]] <- data_ET
  results[[2]] <- stats_ET
  return(results)
  
}


# preprocess and compile baseline data ------------------------------------

files_baseline <- list.files(here('data', 'rawdata'), pattern = 'task-baseline_et.tsv.gz', recursive = TRUE)

data_baseline <- data.frame()
stats_baseline <- data.frame()

for (ii in 1:length(files_baseline)) {
  
  ### extract subject label
  subject <- str_split(files_baseline[ii], '/')[[1]][1]
  
  tryCatch(
    expr = {
      ### preprocess data
      results_baseline <- preprocess_pupil_data_baseline(here('data', 'rawdata', files_baseline[ii]), subject, 'baseline')
      
      ### store results
      data_baseline <- as.data.frame(
        rbind(data_baseline, results_baseline[[1]])
      )
      stats_baseline <- as.data.frame(
        rbind(stats_baseline, results_baseline[[2]])
      )
      rm(results_baseline)
    },
    error = function(e){
      message(paste('caught an error for ', files_baseline[ii], '! moving on...', sep = ''))
    }
  )
  
}

# save file
write.csv(stats_baseline, here('results', paste('SAWM_baseline_pupil_', today(), '.csv', sep = '')), 
                              row.names = FALSE, quote = FALSE)

rm(data_baseline, stats_baseline, files_baseline, subject, ii)
