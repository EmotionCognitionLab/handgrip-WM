function [aSKNA_data, aSKNA_time] = calculate_aSKNA(iSKNA_data, Fs, window)

    % this function computes aSKNA

    %%% inputs:
    % iSKNA_data = rectified and integrated SKNA
    % Fs = sampling frequency, in Hertz
    % window = time window over which to calculate aSKNA, in seconds

    %%% outputs
    % aSKNA_data = iSKNA averaged over time window of interest
    % aSKNA_time = aSKNA time vector

    % number of time intervals over which to average
    signal_length = length(iSKNA_data)/Fs;              % signal length in seconds
    n_windows = floor(signal_length/window);            % full windows in signal
    signal_length_analysis = n_windows*window;          % signal length included in analysis in seconds
    time_windows = 0:window:signal_length_analysis;     % timestamps of window starts in seconds
    sample_windows = (time_windows*Fs)+1;               % sample indices of window starts
    n_samples_per_interval = window .* Fs;              % samples per window

    % loop through time windows and take average
    for ii = 1:n_windows
        indices = int32(sample_windows(ii):(sample_windows(ii+1)-1));
        this_segment = iSKNA_data(indices);             
        time_window(ii) = (sample_windows(ii)-1)/Fs;    % window start in seconds
        aSKNA_data(ii) = sum(this_segment)/n_samples_per_interval;  % take average in this window
    end
    
    % timestamps of window starts
    aSKNA_time = time_windows(1:end-1);
    

end