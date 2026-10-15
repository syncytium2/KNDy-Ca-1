function S = getROIvalues( image, h )
% return values for ROIs, deadzones, and penumbra
% h contains handles for ROI polygons, more crucially the masks!!!
% image is the image
% S contains a value for the mean of ROI, pen and pensub

nROIs = size( h, 2 ); % number of ROIs
for j = 1 : nROIs % loop over each ROI, loop over frames is within

    % binary image %% code structure stolen from image analyst (again)
    roiImage = h(j).mask.ROI;
    deadzoneImage = h(j).mask.deadzone;
    penumbraImage = h(j).mask.penumbra;

    % https://www.mathworks.com/matlabcentral/answers/1727170-how-to-make-a-two-dimensional-mask-act-on-a-three-dimensional-array
    sliceROI = image + 0 ./ roiImage; % all NaNs outside the ROI
    slicePen = image + 0 ./ penumbraImage + 0 ./ ~deadzoneImage; % all NaNs except the penumbral ring
    % NaN everything OUTSIDE the penumbra, NaN everything INSIDE the deadzone

    S.ROI = mean( sliceROI, [1 2], 'omitnan' );
    S.penumbra = mean( slicePen, [1 2], 'omitnan' );

    S.pensub = S.ROI - S.penumbra;

end % ends loop over ROIs

end