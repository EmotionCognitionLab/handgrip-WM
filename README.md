## Isometric handgrip exercise speeds working memory responses in younger and older adults

Analysis and manuscript code for: Bachman, S.L., Attanti, S., Mather, M. (2022). [Isometric handgrip exercise speeds working memory responses in younger and older adults](https://doi.org/10.31234/osf.io/2bpn3).

Data for this project are available on [Zenodo](LINK).

For questions, contact [sbachman@usc.edu](mailto:sbachman@usc.edu).

## contents

- `functions/`: MATLAB functions used for data preprocessing
- `handgrip-WM_analysis.Rmd`: main statistical analysis document that calls relevant scripts
- `scripts/`: scripts used for data preprocessing and statistical analysis

## dependencies
- MATLAB, including the Statistics and Machine Learning Toolbox
- R
- R packages needed for preprocessing (run `scripts/0_install-scripts.R` to install)
- (R packages needed for statistical analysis are installed when knitting `handgrip-WM_analysis.Rmd`)
- PhysioNet Cardiovascular Signal Toolbox ([link](https://github.com/cliffordlab/PhysioNet-Cardiovascular-Signal-Toolbox); for compatibility with the scripts, install into a subdirectory `toolboxes`)

*Preprocessing and analysis for Bachman et al. (2022) were performed using R Version 4.0.4, MATLAB Version R2021b, using a linux kernel with version 5.13.0-7614. See preprint for the specific package versions that were used.*

## instructions for reproducing analyses in Bachman et al., 2022

### prepare project directory

- clone this directory
- initialize an R project in the project directory
- create a subdirectory named `data` in the project directory

### download data

- download the dataset from Zenodo (link above)
- unzip `rawdata.zip` so that `rawdata` is a subdirectory of `data`

### compile behavioral data

- in R, run `scripts/1_compile-behavior/A_compile-nback.R` to compile n-back task data

### preprocess ECG & EMG data

- in MATLAB, run `scripts/2_process-physio/A_find_beats_ECG.m` to identify r-peaks in ECG data
	- *beforehand*: update `dir_root` to location of the project directory on your machine

- in R, run `scripts/2_process-physio/B_analyze-niHR-HRV.R` to generate heart rate time series
	- *beforehand*: update line 22 with the name of the directory in `data/derivatives` containing physionet output (you should just need to update the date in the directory name)
	
- in MATLAB, run `scripts/2_process-physio/C_analyze_neuECG.m` to apply the neuECG method to ECG data
	- *beforehand*: update `dir_root` to location of the project directory on your machine
	
- in R, run `scripts/2_process-physio/D_summarize-neuECG.R` to aggregate neuECG results
  - *beforehand*: update line 18 with the name of the directory in `data/derivatives` containing neuECG output (you should just need to update the date in the directory name)

- in R, run `scripts/2_process-physio/E_summarize-EMG.R` to aggregate EMG metrics

### preprocess eyetracking data

- in R, run `scripts/3_process-et/A_preprocess-et-data_baseline.R` to preprocess baseline eyetracking data
- in R, run `scripts/3_process-et/B_preprocess-et-data_handgrip.R` to preprocess handgrip eyetracking data
- in R, run `scripts/3_process-et/C_preprocess-et-data_nback.R` to preprocess n-back eyetracking data

### statistical analysis for manuscript

- update the file `scripts/4-analysis/01_load-data.R` with correct names of results files you created in the steps above
- knit the document `handgrip-WM_analysis.Rmd` to html
