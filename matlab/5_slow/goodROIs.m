% distance between ROIs
function [ goodROIs, Z, row, col, indices ] = goodROIs( hROI, minDistance )
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
ROIs = true(1,nROIs);
temp_row=nan(1,nROIs);
for iROI=1:nROIs-1
    temp_row(iROI+1:nROIs) = Z(iROI,iROI+1:nROIs);
    indices = find( temp_row < thresh );
    ROIs(indices)=false;
    temp_row(:)=nan;
end
goodROIs = find(ROIs==true);
    % indices = find( Z > thresh );
    % [row,col] = ind2sub(size(Z),indices);
    % goodROIs = unique(row);

