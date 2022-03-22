# this script preprocesses and compiles preprocessed eyetracking data
# from handgrip task runs
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


# function to preprocess pupil data (handgrip) ----------------------------

preprocess_pupil_data_handgrip <- function(filename, subject, description) {
  
  print(paste("preprocessing pupil data for ", subject, ' ', description, sep=''))
  
  ## initialize results storage
  results <- vector(mode = 'list', length = 2)
  data_ET <- data.frame()
  stats_ET <- data.frame()
  
  ### load data and add subject column
  data <- fread(filename, sep = "\t") %>%
    rowwise() %>%
    # create 'part' from event, also to avoid conflict after blink detection
    mutate(part = ifelse(event == 'initial_rest', 'initial_rest',
                          ifelse(event %in% c('squeeze', 'rest'), str_c(event, round, sep = ''), NA))) %>%
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
                             trial = 'part', time = 'time', 
                             x = x_col, y = y_col, 
                             pupil = pupil_col) %>%
      select(subject, trial, time, x, y, pupil)
    
    # INTERPOLATE to account for sampling rate not always consistent
    data_gazer <- interpolate_pupil_data(data_gazer, 0, max(data_gazer$time), hz) %>%
      rename(part = trial) %>%
      mutate(subject = subject, trial = description, .before = time)
    
    ### deblinking ###
    # identifying and removing data during blinks, and interpolating data during
    # the blink
    # instead of calling blink_detect(), we run source code with different parameters
    # for detect.fixations() to try to get better blink detection
    blinks <- detect.fixations(data_gazer %>% select(-part), lambda = lambda,
                               smooth.coordinates = TRUE,
                               smooth.saccades = FALSE) %>%
      filter(event == "blink") %>%
      tidyr::gather(data=., key="startend", value="time", start:end) %>%
      select(-x, -y)
    
    # merge blink data with prepared data
    # s.t. pupil values during blinks are marked as NA
    data_gazer <- merge(data_gazer, blinks %>% select(-trial),
                        by = 'time', all = TRUE) %>%
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
    
    ### rename part & trial
    data_gazer <- data_gazer %>%
      select(-trial, -trial_code)
    
    ### compute stats ###
    pupil_stats <- data_gazer %>%
      select(-c(event, startend)) %>%
      group_by(part) %>%
      summarize(n_obs = n(),
                frac_missing = sum(is.na(extendpupil)) / n_obs,  # percentage of missing (NA) values after blink detection & extension
                pupil_mean = ifelse(frac_missing == 1, NA, mean(pup_interp, na.rm = TRUE)),
                pupil_sd = ifelse(frac_missing == 1, NA, sd(pup_interp, na.rm = TRUE)),
                pupil_min = ifelse(frac_missing == 1, NA, min(pup_interp, na.rm = TRUE)),
                pupil_max = ifelse(frac_missing == 1, NA, max(pup_interp, na.rm = TRUE))) %>%
      select(-n_obs) %>%
      mutate(task = description, .before = part) %>%
      mutate(label_subject = subject, .before = task) %>%
      mutate(hemi = sides[jj], .after = part) %>%
      # split part into event and round, as in input data
      rowwise() %>%
      mutate(event = ifelse(part == 'initial_rest', 'initial_rest',
                            ifelse(str_detect(part, 'squeeze'), 'squeeze',
                                   ifelse(str_detect(part, 'rest'), 'rest', NA))),
             round = ifelse(part == 'initial_rest', 0,
                            ifelse(str_detect(part, 'squeeze'), as.numeric(str_split(part, 'squeeze')[[1]][2]),
                                   ifelse(str_detect(part, 'rest'), as.numeric(str_split(part, 'rest')[[1]][2]), NA)))) %>%
     # add column with run number (built from task description) %>%
      mutate(run = as.integer(str_split(task, 'handgrip_run-')[[1]][2])) %>%
      select(label_subject, run, event, round, hemi, pupil_mean:pupil_max, frac_missing)
    
    # add column indicating hemi & join with composite dataframe
    data_gazer <- data_gazer %>%
      mutate(label_subject = subject, .before = part) %>%
      mutate(task = description, .before = part) %>%
      mutate(hemi = sides[jj], .after = part) %>%
      # split part into event and round, as in input data
      rowwise() %>%
      mutate(event = ifelse(part == 'initial_rest', 'initial_rest',
                            ifelse(str_detect(part, 'squeeze'), 'squeeze',
                                   ifelse(str_detect(part, 'rest'), 'rest', NA))),
             round = ifelse(part == 'initial_rest', 0,
                            ifelse(str_detect(part, 'squeeze'), as.numeric(str_split(part, 'squeeze')[[1]][2]),
                                   ifelse(str_detect(part, 'rest'), as.numeric(str_split(part, 'rest')[[1]][2]), NA)))) %>%
      select(-part) %>%
      # add column with run number (built from task description)
      mutate(run = as.integer(str_split(task, 'handgrip_run-')[[1]][2])) %>%
      select(label_subject, run, event, round, time, hemi, pup_interp)
    
    ### save summary stats and timeseries data ###
    data_ET <- as.data.frame(
      rbind(data_ET, data_gazer)
    )
    stats_ET <- as.data.frame(
      rbind(stats_ET, pupil_stats)
    )
    
  }
  
  ### return list containing preprocessed data and stats
  results[[1]] <- data_ET
  results[[2]] <- stats_ET
  return(results)
  
}


# preprocess and compile handgrip data ------------------------------------

### run1
files_hg1 <- list.files(here('data', 'rawdata'), pattern = 'task-handgrip_run-01_et.tsv.gz', recursive = TRUE)

data_hg1 <- data.frame()
stats_hg1 <- data.frame()

for (ii in 1:length(files_hg1)) {
  
  ### extract subject label
  subject <- str_split(files_hg1[ii], '/')[[1]][1]
  
  tryCatch(
    expr = {
      ### preprocess data
      results_hg1 <- preprocess_pupil_data_handgrip(here('data', 'rawdata', files_hg1[ii]), subject, 'handgrip_run-01')
      
      ### store data & stats
      data_hg1 <- as.data.frame(
        rbind(data_hg1, results_hg1[[1]])
      )
      stats_hg1 <- as.data.frame(
        rbind(stats_hg1, results_hg1[[2]])
      )
      rm(results_hg1)
    },
    error = function(e){
      message(paste('caught an error for ', files_hg1[ii], '! moving on...', sep = ''))
    }
  )
  
}

### run2
files_hg2 <- list.files(here('data', 'rawdata'), pattern = 'task-handgrip_run-02_et.tsv', recursive = TRUE)

data_hg2 <- data.frame()
stats_hg2 <- data.frame()

for (ii in 1:length(files_hg2)) {
  
  ### extract subject label
  subject <- str_split(files_hg2[ii], '/')[[1]][1]
  
  tryCatch(
    expr = {
      ### preprocess data
      results_hg2 <- preprocess_pupil_data_handgrip(here('data', 'rawdata', files_hg2[ii]), subject, 'handgrip_run-02')
      
      ### store data & stats
      data_hg2 <- as.data.frame(
        rbind(data_hg2, results_hg2[[1]])
      )
      stats_hg2 <- as.data.frame(
        rbind(stats_hg2, results_hg2[[2]])
      )
      rm(results_hg2)
    },
    error = function(e){
      message(paste('caught an error for ', files_hg2[ii], '! moving on...', sep = ''))
    }
  )
  
}

### run3
files_hg3 <- list.files(here('data', 'rawdata'), pattern = 'task-handgrip_run-03_et.tsv', recursive = TRUE)

data_hg3 <- data.frame()
stats_hg3 <- data.frame()

for (ii in 1:length(files_hg3)) {
  
  ### extract subject label
  subject <- str_split(files_hg3[ii], '/')[[1]][1]
  
  tryCatch(
    expr = {
      ### preprocess data
      results_hg3 <- preprocess_pupil_data_handgrip(here('data', 'rawdata', files_hg3[ii]), subject, 'handgrip_run-03')
      
      ### store data & stats
      data_hg3 <- as.data.frame(
        rbind(data_hg3, results_hg3[[1]])
      )
      stats_hg3 <- as.data.frame(
        rbind(stats_hg3, results_hg3[[2]])
      )
      rm(results_hg3)
    },
    error = function(e){
      message(paste('caught an error for ', files_hg3[ii], '! moving on...', sep = ''))
    }
  )
  
}


# save handgrip data ------------------------------------------------------

# bind data across runs
data_hg <- as.data.frame(
  rbind(data_hg1, data_hg2, data_hg3)
)
stats_hg <- as.data.frame(
  rbind(stats_hg1, stats_hg2, stats_hg3)
)

write.csv(data_hg, here('results', paste('SAWM_handgrip_pupil_timeseries_', today(), '.csv', sep = '')), 
          row.names = FALSE, quote = FALSE)
rm(data_hg, data_hg1, data_hg2, data_hg3)
write.csv(stats_hg, here('results', paste('SAWM_handgrip_pupil_', today(), '.csv', sep = '')), 
          row.names = FALSE, quote = FALSE)
rm(stats_hg, stats_hg1, stats_hg2, stats_hg3)

