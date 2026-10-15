%resize the time dimension of an image stack
function [resize, resizeT] = resizeZ( stack, time, df )
% x-y-nframes stack
% f number of frames to average, 
% output will be x by y by nframes/f
height = size( stack, 1);
width = size( stack, 2);
nframes = size( stack, 3);
newframes = floor(nframes/df);
resize = zeros( height, width, newframes, 'single'); %'uint16');
disp('starting time resize...');
tic
for f = 1:newframes
    fe = f * df;
    fs = 1 + fe - df;
    resize( :, :, f ) = mean( stack( :, :, fs:fe ), 3 );
    resizeT(f)=mean(time(fs:fe));
end
toc
end