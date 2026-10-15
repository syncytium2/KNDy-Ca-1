% filtfilt filter ROIs 
function fdata = filterData( data, Fsampling, Niir, Fstop )
% data = nsamples x ncells
% Fs = 10; % sampling rate
% Niir = 7; % order
% Fst = 1; % stop frequency
iir = designfilt('lowpassiir','FilterOrder',Niir,...
    'HalfPowerFrequency',Fstop,'SampleRate',Fsampling);
fdata = filtfilt(iir, data);
end