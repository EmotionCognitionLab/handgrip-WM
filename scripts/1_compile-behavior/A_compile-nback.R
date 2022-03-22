# this script compiles n-back behavioral data into a single file
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# setup -------------------------------------------------------------------

rm(list = ls())
library(here)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)


# get list of subject subdirectories --------------------------------------

dirs <- dir(here('data', 'rawdata'),
            pattern = 'sub-',
            full.names = FALSE, recursive = FALSE)


# compile n-back data -----------------------------------------------------

# loop over subject subdirectories & gather n-back data
data_nback <- data.frame()

for (ii in 1:length(dirs)) {
  
  # identify n-back files for this subject 
  files_nback <- list.files(here('data', 'rawdata', dirs[ii]), 
                            pattern = 'nback')
  files_nback <- files_nback[str_detect(files_nback, 'beh.tsv')]
  
  # loop over n-back files and load data
  data_subject_nback <- data.frame()
  for (jj in 1:length(files_nback)) {
    
    # identify run number
    run <- as.numeric(
      str_split(
        str_split(files_nback[jj], '_')[[1]][3],
        'run-')[[1]][2]
    )
    # load data
    data <- fread(here('data', 'rawdata', dirs[ii], files_nback[jj]),
                  na.strings = 'n/a') %>%
      mutate(run = run, .before = trial)
    # bind data to composite dataframe for this subject
    data_subject_nback <- as.data.frame(
      rbind(data_subject_nback, data)
    )
    rm(data)
  }
  
  # add subject label
  data_subject_nback <- data_subject_nback %>%
    mutate(label_subject = dirs[ii], .before = run)
  
  # bind with composite dataframe for all subjects
  data_nback <- as.data.frame(
    rbind(data_nback, data_subject_nback)
  )
  
}


# save n-back data --------------------------------------------------------

if (!dir.exists(here('results'))) {
  dir.create(here('results'))
}

write.csv(data_nback, here('results', paste('SAWM_nback_beh_', today(), '.csv', sep = '')),
          quote = FALSE, row.names = FALSE)

