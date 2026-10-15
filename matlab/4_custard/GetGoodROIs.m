% distance between ROIs
function [goodROIs,Z] = GetGoodROIs( hROI, minDistance )
indices=[];
goodROIs=[];
row=[];
col=[];
thresh=minDistance; % units are microns, the size of one pixel is 1.3 microns
% thresh=13 is about a cell body distance
nROIs = length(hROI.combo);
index=1;
for ii=1:nROIs
%    for jj=ii:nROIs
        X(index,:) = hROI.combo(ii).xy;
        index=index+1;
end
d=pdist(X); % pairwise distance between each ROI
Z=squareform(d);
ROIs = true(1,nROIs); % all ROIs could be good ROIs
temp_row=nan(1,nROIs);
for iROI=1:nROIs-1
    temp_row(iROI+1:nROIs) = Z(iROI,iROI+1:nROIs);
    ROIs(temp_row < thresh)=false;
    temp_row(:)=nan;
end
goodROIs = find(ROIs==true);


