% uses hardcoded solution to extracting frame timing
% this could fail if acq accessories change

function [ time, interval ] = bfTime( metadata )
% input is the original metadata from stack read!
% return an array containing the time values for each frame
% OLD - returns interval in seconds from olympus ETS/VSI files
% olympus data imported with bioformats 'openETSui'
% original metadata contains timing information
% ome metadata version does not appear to contain timing info
% but does contain frame count!
% data points to the cell array imported by openETSui

%metadata = data{1, 2}; % using original metadata for time data
%key0 = 'SizeT';
%nframes = str2double( metadata.get(key0) );
%nframes = 12000;
%omemeta = data{1,4}; % ome generated metadata easiest to access for nframes
%nframes = omemeta.getPixelsSizeT(0).getValue();
nframes = metadataNFrames( metadata );

% fixing so this works with both old and new data
% get the first two entries
strNframes = num2str(nframes);
% need to pad zeros to match digits of max frame
formatValueString = append( 'Value #%0',num2str(length(strNframes)),'d');
formatUnitString = append( 'Units #%0',num2str(length(strNframes)),'d');

% the time of the first frame relative to the start of acq
valKey1 = sprintf(formatValueString,1);%'Value #00001'; 
mkey1 = metadata.get(valKey1);
unitsKey1 = sprintf(formatUnitString,1);
units = metadata.get(unitsKey1);
dk=1;
for k=2:9 % search for the next key with the same units as the first key-value
    unitsKey_k=sprintf(formatUnitString,k);
    units_k = metadata.get(unitsKey_k);
    if strcmp(units,units_k)
        % found it!
        dk=k-1;
        break; % k=inf;
    end
end        

valKey2 = sprintf(formatValueString,dk+1);%'Value #00002'; % was 8 for older acq format
mkey2 = metadata.get(valKey2);

endTimeNumber = 1 + (nframes - 1) * dk ; %nframes; % was 1 + (nframes - 1) * 7 ;
endTimeKey = append('Value #',num2str(endTimeNumber) ); 
endTime = str2double( metadata.get(endTimeKey) );

%fprintf('failed to pull nframes from metadata: %s = %s\n', key0, metadata.get(key0))
fprintf('1st point %s = %s\n', valKey1, mkey1)
fprintf('2nd point %s = %s\n', valKey2, mkey2)
fprintf('end time %s = %s ; %8.3f\n', endTimeKey, metadata.get(endTimeKey),endTime)

% use hardcoded solution to extracting frame timing
% this could fail if acq accessories change
% frame timing is slightly imperfect...storing timing for each frame
% NEED ROBUST WAY TO PREDICT WHERE TIMING IS STORED!
time = zeros(1,nframes);
for i=1:nframes
    increment = (dk * (i-1)) +1; %i; % now there is no increment! was (7 * (i-1)) +1;
    key = sprintf(formatValueString,increment);
    value = metadata.get(key);
    numvalue = str2double(value);
    %fprintf('%g : %s = %s %g\n', i, key, value, numvalue);
    time(1,i) = numvalue;
end
interval = mean(diff(time)); %str2double(mkey2) - str2double(mkey1);
fprintf('mean interval: %g msec\n', interval)

% from the bfmatlab website
% https://docs.openmicroscopy.org/bio-formats/5.7.1/developers/matlab-dev.html?highlight=keyset#ome-metadata

%To print out all of the metadata key/value pairs for the first series:
%  metadataKeys = metadata.keySet().iterator();
%  for i=1:metadata.size()
%    key = metadataKeys.nextElement();
%    value = metadata.get(key);
%    if strfind(value,'12000')>0
%        fprintf('%g : %s = %s\n', i, key, value)
%    end
% %    if findstr('SizeT',key)
% %        fprintf('%g : %s = %s\n', i, key, value)
% %    end
%  end
end