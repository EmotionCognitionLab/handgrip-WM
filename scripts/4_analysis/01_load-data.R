# load aggregated data files 
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# NOTE: in order for this script to run, you need to replace the filenames in
# lines 43-69 with the correct names from your own runs of the preprocessing scripts
# this should entail updating the dates at the end of the filenames
# you ALSO need to update the directory name in line 39


# participant data --------------------------------------------------------

if (internal == 0) {
  data_subjects <- fread(here('data', 'rawdata', 'participants.tsv'),
                         na.strings = 'n/a')
} else if (internal == 1) {
  data_subjects <- fread(here('data', 'rawdata', 'participants_internal.tsv'), 
                         na.strings = 'n/a')
}


# ECG manual QC data ------------------------------------------------------

data_qc_ECG <- fread(here('data', 'rawdata', 'phenotype', 'ecg_manual_qc.tsv'),
                     na.strings = 'n/a')

data_qc_ECG_baseline <- data_qc_ECG %>%
  filter(task == 'baseline')
data_qc_ECG_handgrip <- data_qc_ECG %>%
  filter(task == 'handgrip')
data_qc_ECG_nback <- data_qc_ECG %>%
  filter(task == 'nback')


# data on ECG segments not analyzed with PhysioNet ------------------------

data_physionet_notanalyzed <- fread(here('data', 'derivatives', 
                                         'SAWM_HRV_output_2021-10-19', 
                                         'AnalysisError.txt'))


# physio data -------------------------------------------------------------

# EMG
data_EMG_baseline <- fread(here('results', 'SAWM_baseline_EMG_2021-10-19.csv'))
data_EMG_handgrip <- fread(here('results', 'SAWM_handgrip_EMG_2021-10-19.csv'))

# heart rate
data_HR_baseline <- fread(here('results', 'SAWM_baseline_niHR_2021-10-19.csv'))
data_HR_handgrip <- fread(here('results', 'SAWM_handgrip_niHR_2021-10-19.csv'))
data_HR_nback <- fread(here('results', 'SAWM_nback_niHR_2021-10-19.csv'))

# neuECG
data_aSKNA_baseline <- fread(here('results', 'SAWM_baseline_neuECG_2021-10-21.csv'))
data_aSKNA_handgrip <- fread(here('results', 'SAWM_handgrip_neuECG_2021-10-21.csv'))
data_aSKNA_nback <- fread(here('results', 'SAWM_nback_neuECG_2021-10-21.csv'))

# eyetracking
data_ET_baseline <- fread(here('results', 'SAWM_baseline_pupil_2021-10-20.csv')) 
data_ET_handgrip <- fread(here('results', 'SAWM_handgrip_pupil_2021-10-20.csv')) 
data_ET_nback <- fread(here('results', 'SAWM_nback_pupil_2021-10-20.csv')) 
data_ET_nback_ts <- fread(here('results', 'SAWM_nback_pupil_timeseries_2021-10-20.csv'))


# behavioral data ---------------------------------------------------------

# n-back
data_nback <- fread(here('results', 'SAWM_nback_beh_2021-10-19.csv'))