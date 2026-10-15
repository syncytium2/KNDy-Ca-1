function im1 = stackViewer(ax, st1, nave, frame, hROI, minmax)
% st1 st2 are image stacks (x,y,t)
% nave is the number of frames to average
% frame is the image to show
% hROI is a ROI structure created with interface

nFrames = size(st1,3);
ts = frame;
tend = frame+nave;
if(tend>nFrames)
    tend=nFrames;
end

im1 = mean(st1(:,:,ts:tend),3); % average nave frames

imshow( im1, 'Parent', ax )
%clim(ax,'auto')
clim( ax, minmax )
if ~isempty(hROI)
    h = drawROI( hROI, ax );
end
mystr = append('Frame: ', num2str(frame)); %sprintf('Frame %i out of %i', frame, nFrames, 'fontweight', 'bold', 'fontsize', 14 );
title( ax, mystr );
drawnow;

end