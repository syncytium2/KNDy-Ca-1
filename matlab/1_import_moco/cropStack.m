% crop in x-y plane: get crop from max/sd projection, crop stack
function [ cstack, rectout ] = cropStack( stack )
% cstack is the cropped stack
% J are the coordinates selected by the user
%maxpro = projectMax( stack );
[meanp,maxp,stdp]=projMeanMaxSTD(stack);
%maxpro = histeq( maxpro );
minp = min(meanp,[],'all');
img = (meanp-minp)/(max(meanp,[],'all')-minp);
[ croppedImage, rectout ] = imcrop( img ); % J [ xstart, ystart, width, height ]
rect = round(rectout);
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
minp = min(CropMaxProj,[],'all');
img2 = (CropMaxProj-minp)/(max(CropMaxProj,[],'all')-minp);

figure
montage( { img, img2 } );
clim('auto');
end