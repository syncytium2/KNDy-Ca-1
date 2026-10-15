% crop in x-y plane: get crop from max/sd projection, crop stack
function [ cstack ] = cropStackRect( stack, rectin )
% cstack is the cropped stack
% rect, rectout are the coordinates selected by the user
maxpro = projectMax( stack );
maxpro = histeq( maxpro );

rect = round(rectin);
height = rect(4); % size( series1{1,1}, 1 );
width = rect(3); % size( series1{1,1}, 2 );
rs = rect(1); % row start and end
re = rs + width-1;
cs = rect(2); % column start and end
ce = cs + height-1;
frames = size( stack, 3 );
cstack = zeros(height, width, frames, 'uint16');
tic
%for p = 1 : frames
     cstack(:,:,:) = stack( cs:ce, rs:re, : ); 
%end
toc
CropMaxProj = projectMax( cstack );
CropMaxProj = histeq( CropMaxProj );

figure
montage( { maxpro, CropMaxProj } );
clim('auto');
end