function data_integrated = filter_leakyintegrator(data, Fs, time_constant)
% this function performs leaky integration

%%% inputs:
% data = signal
% Fs = sampling frequency of signal in Hertz
% time_constant = time constant in seconds

%%% outputs: 
% data_integrated = integrated signal

%%% apply filter
intDecay = exp(-(1/(Fs * time_constant)));
intGain = 1 - intDecay;
data_integrated = filter(intGain, [1 -intDecay], data);

% rationale for final step: Y = filter(B, A, X)
% B = intGain
% A = [1 -intDecay]
% X = SIGNAL

end