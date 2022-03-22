% this script performs neuECG analyses
% to calculate aSKNA from ECG data
% for the handgrip-WM project
% written by shelby bachman, sbachman@usc.edu

%% setup

clear all
close all
clc

%% directories

dir_root =          '/home/shelby/work/projects/handgrip-WM';
dir_data =          [dir_root, filesep, 'data', filesep, 'rawdata'];
dir_deriv =         [dir_root, filesep, 'data', filesep, 'derivatives'];
if ~exist([dir_deriv, filesep, 'SAWM_iSKNA_output_', datestr(date, 'yyyy-mm-dd')], 'dir')
    mkdir([dir_deriv, filesep, 'SAWM_iSKNA_output_', datestr(date, 'yyyy-mm-dd')])
end
dir_results =       [dir_root, filesep, 'results'];
dir_functions =     [dir_root, filesep, 'functions'];
dir_toolboxes =     [dir_root, filesep, 'toolboxes'];

addpath(genpath(dir_functions))
addpath(genpath(dir_toolboxes))

%% set input & output filenames

ext.in =                      '*_physio.tsv.gz'; 
filename_out_overall =       [dir_results, filesep, 'SAWM_neuECG_', datestr(date, 'yyyy-mm-dd'), '.csv'];

%% set log filename

filename_log =      [dir_root, filesep, 'logs', filesep, 'analyze_neuECG_log_', datestr(date, 'yyyy-mm-dd'), '.txt'];

%% tsv file import options - baseline

opts = delimitedTextImportOptions("NumVariables", 1);

% Specify range and delimiter
opts.DataLines = [2, Inf];
opts.Delimiter = "\t";

% Specify column names and types
opts.VariableNames = ["ecg"];
opts.SelectedVariableNames = ["ecg"];
opts.VariableTypes = ["double"];

% Specify file level properties
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

%% tsv file import options - handgrip

opts_hg = delimitedTextImportOptions("NumVariables", 8);

% Specify range and delimiter
opts_hg.DataLines = [2, Inf];
opts_hg.Delimiter = "\t";

% Specify column names and types
opts_hg.VariableNames = ["ecg", "Var2", "Var3", "Var4", "Var5", "Var6", "event", "round"];
opts_hg.SelectedVariableNames = ["ecg", "event", "round"];
opts_hg.VariableTypes = ["double", "string", "string", "string", "string", "string", "categorical", "double"];

% Specify file level properties
opts_hg.ExtraColumnsRule = "ignore";
opts_hg.EmptyLineRule = "read";

%% tsv file import options - nback

opts_nb = delimitedTextImportOptions("NumVariables", 8);

% Specify range and delimiter
opts_nb.DataLines = [2, Inf];
opts_nb.Delimiter = "\t";

% Specify column names and types
opts_nb.VariableNames = ["ecg", "Var2", "Var3", "Var4", "Var5", "Var6", "event", "block"];
opts_nb.SelectedVariableNames = ["ecg", "event", "block"];
opts_nb.VariableTypes = ["double", "string", "string", "string", "string", "string", "categorical", "double"];

% Specify file level properties
opts_nb.ExtraColumnsRule = "ignore";
opts_nb.EmptyLineRule = "read";


%% subjects to exclude

subs_to_exclude = {'sub-002', 'sub-010', 'sub-028', 'sub-037', ...
                 'sub-058', 'sub-101', 'sub-120', ...
                 'sub-124', 'sub-146', 'sub-147'};

%% signal processing parameters and create filters

%%% structure for parameters
P =                             [];

%%% parameters across signals
P.Fs =                          2000;                                       % original sampling rate is 2000 Hz                   

%%% SNA parameters (ref: Kusayama et al., 2020) 
% initial highpass filter -- yields SKNA
% note: article recommends bandpass filter 500-1000Hz
% but 1000Hz is our nyquist frequency, so highpass filter with cutoff 500Hz is equivalent
P.filter.sna.type =             'high';                                     % bandpass filter
P.filter.sna.fc =               500;                                        % cutoff freq: 500Hz
P.filter.sna.n_coeffs =         2 * (P.Fs / P.filter.sna.fc);               % number of filter coefficients
P.filter.sna.order =            P.filter.sna.n_coeffs - 1;                  % filter order - NOTE it gets increased by one during script (see line 73)
P.filter.sna.Wn =               P.filter.sna.fc ./ (P.Fs/2);                % cutoff freq in rad
P.filter.sna.b =                fir1(P.filter.sna.order, P.filter.sna.Wn, P.filter.sna.type);  % create filter
P.filter.sna.order =            P.filter.sna.order + 1;                     % this accounts for the increase that happens automatically

% integration -- yields iSKNA
P.integrate.sna.type =          'leaky';                % leaky integrator
P.integrate.sna.tau =           0.1;                    % time constant: 100ms

% averaging - yields aSKNA
P.average.sna.window =          0.1;                    % 10s window

% burst analysis
P.burst.binwidth =              0.0005;                 % binwidth for histogram
P.burst.maxcomponents =         4;                      % max number of gaussians to fit
P.burst.windowlength =          0.1;                    % data window length for burst analysis

%% list subject subdirectories (*_physio.tsv.gz)

cd(dir_data)
sub_list = dir('sub-*');

%% loop through subject subdirectories

%%% initialize table for storing % iSKNA outliers
table_neuECG = table();

for ii = 1:length(sub_list)
    
    %%% extract subject label
    subject = sub_list(ii).name;
    
    %%% skip if no physio data
    if any(strcmp(subject, subs_to_exclude))
        disp(['no physio data for ', subject, '! moving on...'])
        continue
    else
        disp(['now analyzing iSKNA for ', subject, '! ...'])
    end
                 
    %%% get list of physio files for this subject
    files_physio = dir([dir_data, filesep, subject, filesep, ext.in]);
    
    %%% initialize table and variables for this subject
    task = cell([1 length(files_physio)]);
    neuECG = []; iSKNA_composite = [];
    neuECG_pct_outliers = nan([1 length(files_physio)]);
    iSKNA_mean = nan([1 length(files_physio)]);
    
    %%% loop over physio files, preprocess data & save
    for jj = 1:length(files_physio)
        
        %%% initialize tables for storing iSKNA
        table_iSKNA = table();

        % extract task name
        task{jj} = files_physio(jj).name(14:end-14);
        
        % gunzip file
        gzipped = [dir_data, filesep, subject, filesep, files_physio(jj).name];
        gunzipped = [dir_data, filesep, subject, filesep, files_physio(jj).name(1:end-3)];
        gunzip(gzipped)
        
        % load physio filename
        % (import options depend on task)
        data = readtable(gunzipped, opts_hg);

        if strcmp(task{jj}, 'baseline')
            data = readtable(gunzipped, opts);
        elseif strcmp(task{jj}, 'handgrip_run-01') || strcmp(task{jj}, 'handgrip_run-02') || strcmp(task{jj}, 'handgrip_run-03')
            data = readtable(gunzipped, opts_hg);
        elseif strcmp(task{jj}, 'nback_run-00') || strcmp(task{jj}, 'nback_run-01') || strcmp(task{jj}, 'nback_run-02') || strcmp(task{jj}, 'nback_run-03')
            data = readtable(gunzipped, opts_nb);
        end
        
        % add time column
        data.time = (0:(1/P.Fs):(height(data)-1)/P.Fs)';
        
        % remove gunzipped file
        delete(gunzipped)
        
        %%%%%%%%%%% SYMPATHETIC TONE (neuECG) ANALYSIS %%%%%%%%%
        
        % isolate SKNA using high-pass filter
        neuECG(jj).SKNA = filtfilt(P.filter.sna.b, 1, data.ecg);

        % convert from millivolts to microvolts
        neuECG(jj).SKNA = neuECG(jj).SKNA .* 1000;

        % full-wave rectify signal
        neuECG(jj).SKNA_rect = abs(neuECG(jj).SKNA);

        % integrate rectified signal using leaky integrator (yields iSKNA)
        neuECG(jj).iSKNA = filter_leakyintegrator(neuECG(jj).SKNA_rect, P.Fs, P.integrate.sna.tau);
        
        % compute aSKNA, averaged over 10-s time window
        %[neuECG(jj).aSKNA, ~] = calculate_aSKNA(neuECG(jj).iSKNA, P.Fs, P.average.sna.window);
        
        %%%%%% store iSKNA in derivative file
        filename_out = [dir_deriv, filesep, 'SAWM_iSKNA_output_', datestr(date, 'yyyy-mm-dd'), filesep, ...
            subject, '_task-', task{jj}, '_iSKNA.csv'];

        % create table, column names depend on task
        if strcmp(task{jj}, 'baseline')
            iSKNA_subject = table(repmat(subject, [length(neuECG(jj).iSKNA) 1]), ...
            repmat(task{jj}, [length(neuECG(jj).iSKNA) 1]), ...
            data.time, neuECG(jj).iSKNA);
            iSKNA_subject.Properties.VariableNames = {'subject', 'task', 'time', 'iSKNA'};
        elseif strcmp(task{jj}, 'handgrip_run-01') || strcmp(task{jj}, 'handgrip_run-02') || strcmp(task{jj}, 'handgrip_run-03')
            iSKNA_subject = table(repmat(subject, [length(neuECG(jj).iSKNA) 1]), ...
            repmat(task{jj}, [length(neuECG(jj).iSKNA) 1]), ...
            data.time, data.event, data.round, neuECG(jj).iSKNA);
            iSKNA_subject.Properties.VariableNames = {'subject', 'task', 'time', 'event', 'round', 'iSKNA'};
        elseif strcmp(task{jj}, 'nback_run-00') || strcmp(task{jj}, 'nback_run-01') || strcmp(task{jj}, 'nback_run-02') || strcmp(task{jj}, 'nback_run-03')
            iSKNA_subject = table(repmat(subject, [length(neuECG(jj).iSKNA) 1]), ...
            repmat(task{jj}, [length(neuECG(jj).iSKNA) 1]), ...
            data.time, data.event, data.block, neuECG(jj).iSKNA);
            iSKNA_subject.Properties.VariableNames = {'subject', 'task', 'time', 'event', 'block', 'iSKNA'};
        end

        % write iSKNA timeseries to file
        table_iSKNA = vertcat(table_iSKNA, iSKNA_subject);
        writetable(table_iSKNA, filename_out)
        
        % store the average iSKNA across the entire period
        iSKNA_mean(jj) = mean(neuECG(jj).iSKNA);

        % append to composite iSKNA
        iSKNA_composite = [iSKNA_composite; neuECG(jj).iSKNA];
                
    end
    
     %%% using composite iSKNA, compute percent outliers
     
        % using the mad-median rule to identify outliers
        % i.e. outlier defined as being more than 3 scaled MAD from the median
        iSKNA_is_outlier = isoutlier(iSKNA_composite);
        iSKNA_frac_outliers = sum(iSKNA_is_outlier)/length(iSKNA_composite);

    %%% compile overall results for this subject and bind to composite table
    table_subject = table(repmat(subject, [length(files_physio) 1]), task', ...
        repmat(iSKNA_frac_outliers, [length(files_physio) 1]), iSKNA_mean');
    table_subject.Properties.VariableNames = {'subject', 'task', 'iSKNA_frac_outliers', 'iSKNA_mean'};
    table_neuECG = vertcat(table_neuECG, table_subject);

    
    % remove irrelevant variables
    clear table_subject subject task iSKNA_frac_outliers iSKNA_mean
    
end

%% save results
writetable(table_neuECG, filename_out_overall)
