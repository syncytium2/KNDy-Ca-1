% get the number of frames from original metadata
function numframes = metadataNFrames( metadata )
% metadata is the original metadata obtained from the vsi/ets
% bioformats import in matlab, see openETSuiProbe
key = 'External file volume'; % why this works is unimportant?
value = metadata.get(key); 
% this should be a string like 
% '(0, 0, 0, 0, 0, 512, 512, 12000, 1, 1)'
% now its '(0, 0, 0, 0, 512, 512, 12000, 1, 1)'
disp(append('nframes: ',value))
A = sscanf(value, '(%g, %g, %g, %g, %g, %g, %g, %g, %g, %g)');
%numframes = A(end-1); % was 8);
% 2024 was: numframes = max(A(end-2:end));
index=find(A>1,1,'last');
numframes = A(index);

% version 2
% key = 'Series 0 Name'; % should be TD 488, name of illumination settting
% namekey = metadata.get(key);
% timekey = append( namekey, ' SizeT');
% numframes = metadata.get(timekey);

end
