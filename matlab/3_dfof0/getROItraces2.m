function ROIstats = getROItraces2( imStack, h, fracPenumbra )
% replaces roiStats3D !
% h contains handles for ROI polygons, more crucially the masks!!!
% imstack is the raw data stack
% fracPenumbra is the fraction of the penumbral signal to subtract, 0.7

imStack = single( imStack );
nROIs = size( h, 2 ); % number of ROIs
for j = 1 : nROIs % loop over each ROI, loop over frames is within
    disp(append('roistats, ROI:',num2str(j)));
    tic
    % binary image %% code structure stolen from image analyst (again)
    roiImage = h(j).mask.ROI;
    deadzoneImage = h(j).mask.deadzone;
    penumbraImage = h(j).mask.penumbra;

    % Calculate the area, in pixels, that they drew.
    numPixROI = sum( roiImage(:) );
    numPixDead = sum( deadzoneImage(:) );
    numPixPen = sum( penumbraImage(:) );

    % Another way to calculate it that takes fractional pixels into account.
    numberOfPixels2 = bwarea( roiImage );

    % https://www.mathworks.com/matlabcentral/answers/1727170-how-to-make-a-two-dimensional-mask-act-on-a-three-dimensional-array
    sliceROI = imStack + 0 ./ roiImage; % all NaNs outside the ROI
    slicePen = imStack + 0 ./ penumbraImage + 0 ./ ~deadzoneImage; % all NaNs except the penumbral ring
    % NaN everything OUTSIDE the penumbra, NaN everything INSIDE the deadzone

    ROI(j).ROI.meanZ = squeeze( mean( sliceROI, [1 2], 'omitnan' ) );
    ROI(j).penumbra.meanZ = squeeze( mean( slicePen, [1 2], 'omitnan' ) );

    pensub = squeeze( ROI(j).ROI.meanZ - fracPenumbra * ROI(j).penumbra.meanZ );

    ROI(j).pensub = pensub;

    ROI(j).ROI.area1 = numPixROI;
    ROI(j).penumbra.area1 = numPixPen;
    ROI(j).ROI.area2 = numberOfPixels2;
    toc
end % ends loop over ROIs

% add an additional entry to ROIstats
% full frame average
FFA = mean(imStack, [1 2]); % mean over x-y
ROIstats.ROI = ROI;
ROIstats.FFA.raw = squeeze(FFA);
ROIstats.nROIs = nROIs;

disp(append(' roi2stats3d: ', num2str(toc) ) );
end