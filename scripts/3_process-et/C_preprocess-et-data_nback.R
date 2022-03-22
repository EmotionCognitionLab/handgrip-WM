# this script preprocesses and compiles preprocessed eyetracking data
# from n-back task runs
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


# function to preprocess pupil data (n-back) ------------------------------

preprocess_pupil_data_nback <- function(filename, subject, description) {
  
  print(paste("preprocessing pupil data for ", subject, ' ', description, sep=''))
  
  ## initialize results storage
  results <- vector(mode = 'list', length = 2)
  data_ET <- data.frame()
  stats_ET <- data.frame()
  
  ### load data and add subject column
  data <- fread(filename, sep = '\t') %>%
    mutate(part = ifelse(event %in% c('initial_fixation', 'instruction', 'rest'), str_c(event, '_block', block, sep = ''),
                         ifelse(event == 'trial', str_c(event, '_block', block, '_trial', trial, sep = ''), NA)),
           .after = trial) %>%
    select(-c(event, block, trial)) %>%
    mutate(subject = subject)
  
  ### loop over eyes
  
  for (jj in 1:2) {
    
    #### prepare data ###
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
    
    # interpolate to account for sampling rate not always consistent
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
    
    ### rename part & trial, add task / subject / hemi columns
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
      rowwise() %>%
      # split part into event, block and trial, as in input data
      mutate(event = str_split(part, '_block')[[1]][1], 
             .after = part) %>%
      mutate(block = ifelse(event == 'trial', as.integer(str_split(str_split(part, '_block')[[1]][2], '_trial')[[1]][1]),
                            ifelse(event %in% c('initial_fixation', 'instruction', 'rest'), as.integer(str_split(part, '_block')[[1]][2]), NA)),
             trial = ifelse(event == 'trial', as.integer(str_split(str_split(part, '_block')[[1]][2], '_trial')[[1]][2]),
                            ifelse(event %in% c('initial_fixation', 'instruction', 'rest'), NA, NA)),
             .after = event) %>%
      # add column with run number (built from task description)
      mutate(run = as.integer(str_split(task, 'nback_run-')[[1]][2])) %>%
      select(label_subject, run, event, block, trial, hemi, pupil_mean:pupil_max, frac_missing)
    
    ### format timeseries data ###
    data_gazer <- data_gazer %>%
      mutate(task = description, .before = part) %>%
      mutate(hemi = sides[jj], .after = part) %>%
      rowwise() %>%
      # split part into event, block and trial, as in input data
      mutate(event = str_split(part, '_block')[[1]][1], 
             .after = part) %>%
      mutate(block = ifelse(event == 'trial', as.integer(str_split(str_split(part, '_block')[[1]][2], '_trial')[[1]][1]),
                            ifelse(event %in% c('initial_fixation', 'instruction', 'rest'), as.integer(str_split(part, '_block')[[1]][2]), NA)),
             trial = ifelse(event == 'trial', as.integer(str_split(str_split(part, '_block')[[1]][2], '_trial')[[1]][2]),
                            ifelse(event %in% c('initial_fixation', 'instruction', 'rest'), NA, NA)),
             .after = event) %>%
      # add column with run number (built from task description)
      mutate(run = as.integer(str_split(task, 'nback_run-')[[1]][2])) %>%
      select(label_subject = subject, run, event, block, trial, time, hemi, pup_interp) %>%
      arrange(time)
    
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


# preprocess and compile n-back data --------------------------------------

### run0
files_nb0 <- list.files(here('data', 'rawdata'), pattern = 'task-nback_run-00_et.tsv.gz', recursive = TRUE)

data_nb0 <- data.frame()
stats_nb0 <- data.frame()

for (ii in 1:length(files_nb0)) {
  
  ### extract subject label
  subject <- str_split(files_nb0[ii], '/')[[1]][1]
  
  tryCatch(
    expr = {
      ### preprocess data
      results_nb0 <- preprocess_pupil_data_nback(here('data', 'rawdata', files_nb0[ii]), subject, 'nback_run-00')
      
      ### store data & stats
      data_nb0 <- as.data.frame(
        rbind(data_nb0, results_nb0[[1]])
      )
      stats_nb0 <- as.data.frame(
        rbind(stats_nb0, results_nb0[[2]])
      )
      rm(results_nb0)
    },
    error = function(e){
      message(paste('caught an error for ', files_nb0[ii], '! moving on...', sep = ''))
    }
  )
}

### run1
files_nb1 <- list.files(here('data', 'rawdata'), pattern = 'task-nback_run-01_et.tsv.gz', recursive = TRUE)

data_nb1 <- data.frame()
stats_nb1 <- data.frame()

for (ii in 1:length(files_nb1)) {
  
  ### extract subject label
  subject <- str_split(files_nb1[ii], '/')[[1]][1]
  
  tryCatch(
    expr = {
      ### preprocess data
      results_nb1 <- preprocess_pupil_data_nback(here('data', 'rawdata', files_nb1[ii]), subject, 'nback_run-01')
      
      ### store data & stats
      data_nb1 <- as.data.frame(
        rbind(data_nb1, results_nb1[[1]])
      )
      stats_nb1 <- as.data.frame(
        rbind(stats_nb1, results_nb1[[2]])
      )
      rm(results_nb1)
    },
    error = function(e){
      message(paste('caught an error for ', files_nb1[ii], '! moving on...', sep = ''))
    }
  )
}

### run2
files_nb2 <- list.files(here('data', 'rawdata'), pattern = 'task-nback_run-02_et.tsv.gz', recursive = TRUE)

data_nb2 <- data.frame()
stats_nb2 <- data.frame()

for (ii in 1:length(files_nb2)) {
  
  ### extract subject label
  subject <- str_split(files_nb2[ii], '/')[[1]][1]
  
  tryCatch(
    expr = {
      ### preprocess data
      results_nb2 <- preprocess_pupil_data_nback(here('data', 'rawdata', files_nb2[ii]), subject, 'nback_run-02')
      
      ### store data & stats
      data_nb2 <- as.data.frame(
        rbind(data_nb2, results_nb2[[1]])
      )
      stats_nb2 <- as.data.frame(
        rbind(stats_nb2, results_nb2[[2]])
      )
      rm(results_nb2)
    },
    error = function(e){
      message(paste('caught an error for ', files_nb2[ii], '! moving on...', sep = ''))
    }
  )
}


### run3
files_nb3 <- list.files(here('data', 'rawdata'), pattern = 'task-nback_run-03_et.tsv.gz', recursive = TRUE)

data_nb3 <- data.frame()
stats_nb3 <- data.frame()

for (ii in 1:length(files_nb3)) {
  
  ### extract subject label
  subject <- str_split(files_nb3[ii], '/')[[1]][1]
  
  tryCatch(
    expr = {
      ### preprocess data
      results_nb3 <- preprocess_pupil_data_nback(here('data', 'rawdata', files_nb3[ii]), subject, 'nback_run-03')
      
      ### store data & stats
      data_nb3 <- as.data.frame(
        rbind(data_nb3, results_nb3[[1]])
      )
      stats_nb3 <- as.data.frame(
        rbind(stats_nb3, results_nb3[[2]])
      )
      rm(results_nb3)
    },
    error = function(e){
      message(paste('caught an error for ', files_nb3[ii], '! moving on...', sep = ''))
    }
  )
}


# save nback data ---------------------------------------------------------

# bind data across runs
data_nb <- as.data.frame(
  rbind(data_nb0, data_nb1, data_nb2, data_nb3)
)
stats_nb <- as.data.frame(
  rbind(stats_nb0, stats_nb1, stats_nb2, stats_nb3)
)

write.csv(data_nb, here('results', paste('SAWM_nback_pupil_timeseries_', today(), '.csv', sep = '')), 
          row.names = FALSE, quote = FALSE)
rm(data_nb0, data_nb1, data_nb2, data_nb3, data_nb)
write.csv(stats_nb, here('results', paste('SAWM_nback_pupil_', today(), '.csv', sep = '')), 
          row.names = FALSE, quote = FALSE)
rm(stats_nb0, stats_nb1, stats_nb2, stats_nb3, stats_nb)