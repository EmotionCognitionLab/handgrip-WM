% this script preprocesses ecg data 
% and calculates HRV metrics for all tasks using physionet
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

% create derivatives subdirectory if it does not exist
if ~exist(dir_deriv, 'dir')
    mkdir(dir_deriv)
end

dir_results =       [dir_root, filesep, 'results'];
% create results subdirectory if it is does not exist
if ~exist(dir_deriv, 'results')
    mkdir(dir_results)
end

dir_functions =     [dir_root, filesep, 'functions'];
dir_toolboxes =     [dir_root, filesep, 'toolboxes'];

addpath(genpath(dir_functions))
addpath(genpath(dir_toolboxes))

%% set input & output filenames

ext.in =            '*_physio.tsv.gz'; 
filename_out =      [dir_results, filesep, 'SAWM_physio_', datestr(date, 'yyyy-mm-dd'), '.csv'];

%% create log directory & set log filename

if ~exist([dir_root, filesep, 'logs'], 'dir')
    mkdir([dir_root, filesep, 'logs'])
end

filename_log =      [dir_root, filesep, 'logs', filesep, 'preprocess_physio_log_', datestr(date, 'yyyy-mm-dd'), '.txt'];

%% tsv file import options

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

%% subjects to exclude

subs_to_exclude = {'sub-002', 'sub-010', 'sub-028', 'sub-037', ...
                 'sub-058', 'sub-101', 'sub-120', ...
                 'sub-124', 'sub-146', 'sub-147'};

%% signal processing parameters and create filters

%%% structure for parameters
P =                             [];

%%% parameters across signals
P.Fs =                          2000;                                       % original sampling rate is 2000 Hz                   

%% HRV parameters

P.HRV_resultsLabels =           {'t_start','t_end','NNmean','NNmedian','NNmode','NNvariance','NNskew','NNkurt','NNiqr',...
                                'SDNN','RMSSD','pnn50','btsdet','avgsqi','tdflag','ulf','vlf','lf','hf','lfhf','ttlpwr',...
                                'fdflag','ac','dc','SD1','SD2','SD1SD2','SampEn','ApEn'};

%%% initialize HRV parameters & result storage
cd(dir_deriv)
HRVparams =                     InitializeHRVparams('SAWM');       % initialize HRV parameters
HRVparams.output.ann_format =   'csv';                                     
HRVparams.writedata =           ['SAWM_HRV_output_', datestr(date, 'yyyy-mm-dd')];
dir_rr =                        [dir_deriv, filesep, HRVparams.writedata, filesep, 'Annotation']; % directory containing R peak annotations

%%% global HRV parameters (do not vary by phase)
HRVparams.PeakDetect.windows =  10;         % adjust window length for jqrs (avoids problem with sqijw having NaNs)
HRVparams.RejectionThreshold = 1;           % bc sqi.LowQualityThreshold is set at 0         
HRVparams.sqi.LowQualityThreshold = 0;      % necessary in order to calculate HRV for short segments (most have low sqi)

%% list subject subdirectories (*_physio.tsv.gz)

cd(dir_data)
sub_list = dir('sub-*');

%% loop through subject subdirectories

cd(dir_deriv)

%%% initialize table across subjects
table_physio = table();

for ii = 1:length(sub_list)
    
    %%% extract subject label
    subject = sub_list(ii).name;
    
    %%% skip if no physio data
    if any(strcmp(subject, subs_to_exclude))
        disp(['no physio data for ', subject, '! moving on...'])
        continue
    else
        disp(['now preprocessing physio data for ', subject, '! ...'])
    end
                 
    %%% get list of physio files for this subject
    files_physio = dir([dir_data, filesep, subject, filesep, ext.in]);
    
    %%% initialize table for this subject
    task = cell([1 length(files_physio)]);
    HR_mean = nan([1 length(files_physio)]);
    sig_length = nan([1 length(files_physio)]);
    
    %%% loop over physio files, preprocess data & save
    for jj = 1:length(files_physio)
        
        % extract phase name
        task{jj} = files_physio(jj).name(14:end-14);

        % gunzip file
        gzipped = [dir_data, filesep, subject, filesep, files_physio(jj).name];
        gunzipped = [dir_data, filesep, subject, filesep, files_physio(jj).name(1:end-3)];
        gunzip(gzipped)
        
        % load physio filename
        data = readtable(gunzipped, opts);
        
        % add time column
        data.time = (0:(1/P.Fs):(height(data)-1)/P.Fs)';

        % store segment length in seconds
        sig_length(jj) = data.time(end);
        
        % remove gunzipped file
        delete(gunzipped)

        
        % set HRV analysis parameters for this signal
        physionet_subID = files_physio(jj).name(1:end-14);
        HRVparams.Fs =                  P.Fs;
        HRVparams.windowlength =        sig_length(jj);     % make window length the length of the signal (by default it is 300s)
        HRVparams.sd.segmentlength =    HRVparams.windowlength;             % for SDANN and SDNNI analyses                    
        HRVparams.MSE.on =              0;
        HRVparams.DFA.on =              0;
        HRVparams.HRT.on =              0;
        HRVparams.Entropy.on =          0;
        HRVparams.freq.on =             1;
        HRVparams.freq.method =         'lomb';

        try
            
            % perform time & frequency domain analysis (lomb method)  
            [res, resFileName] = Main_HRV_Analysis(data.ecg, [], 'ECGWaveform', HRVparams, physionet_subID);

            % read r peak annotations             
            file_annot = [dir_rr, filesep, physionet_subID, '.jqrs.csv'];
            r_data = readtable(file_annot);
            r_data = r_data.Var1';
            r_data = table(data.time(r_data), r_data');
            r_data.Properties.VariableNames = {'time', 'index'};
            writetable(r_data, [dir_rr, filesep, physionet_subID, '_annot.csv']);

            % calculate mean heart rate
            HR_mean(jj) = 60./mean(diff(r_data.time));
            
        catch
           
            % open log file
            fid = fopen(filename_log,'a+');

            % write raw data filename to log file
            fprintf(fid, '%s\n', ['error processing HRV for ', physionet_subID]);

            % close file
            fclose(fid);
            
            % fill values as NaN
            HR_mean(jj) = NaN;
            
        end
        
    end
    
    table_subject = table(repmat(subject, [length(files_physio) 1]), task', sig_length', HR_mean');
    table_subject.Properties.VariableNames = {'subject', 'task', 'HR_mean', 'sig_length'};
    table_physio = vertcat(table_physio, table_subject);
    
end

% save table to file
writetable(table_physio, filename_out);
    
%% remove irrelevant directories which were created

% delete annotation files other than jqrs & only for one modality
% (e.g. we keep only .sqijw, comparison of jqrs wrt wqrs detection)
cd(dir_rr)
delete('*.hea')        % remove header files
delete('*.sqijs.csv')  % remove files with comparison of jqrs and sqrs
delete('*.sqrs.csv')   % remove sqrs annotations
delete('*.wqrs.csv')   % remove wqrs annotations
delete('*fft.jqrs.csv')
delete('*burg.jqrs.csv')
delete('*fft.sqijw.csv')    

% delete empty dir created by physionet
cd(dir_deriv)
rmdir('HRV_Output', 's')

