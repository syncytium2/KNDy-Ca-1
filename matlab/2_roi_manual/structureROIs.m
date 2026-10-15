%% structure ROIs derived from previous ROIs
function h = structureROIs( hROI, fax, im, r, offset, pen, bonus )
% hROI is a set of ROI handles
% test first ROI
%r=10;
%offset = 4;
if nargin<7
    bonus = 0;
end
nROIs = size(hROI,2);

for i=1:nROIs
    M =  mean( hROI(i).ROI ); %hROI(i).handles.ROI.Position   ) ; % get the center of the round ROI
    % revise the ROI
    h(i) = structuredROIax( M(1), M(2), im, fax, r, offset, pen, bonus );
    text( fax, M(1), M(2), num2str(i), "HorizontalAlignment","center" )
end

end