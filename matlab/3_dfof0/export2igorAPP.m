function export2igorAPP( S, fname )
% fname is a full path name, or saves to current folder
% custom function to store processed ROIs
% to be read by Igor (or MATLAB)
% S is the structure returned by roi2stats3D and processROIs 
 % sometimes called "baseStats"

ncells = size(S.baseStats.ROI,2);
npoints = size( S.baseStats.ROI(1).dFoF0.ROI, 1);

pen = zeros( npoints, ncells );
roi = zeros( npoints, ncells );
pensub = zeros( npoints, ncells );
roidf = zeros( npoints, ncells );
pensubdf = zeros( npoints, ncells );
for i=1:ncells
    roi( :, i ) = S.baseStats.ROI(i).ROI.meanZ(:);
    pen( :, i ) = S.baseStats.ROI(i).penumbra.meanZ(:);
    pensub( :, i ) = S.baseStats.ROI(i).pensub; 

    roidf( :, i ) = S.baseStats.ROI(i).dFoF0.ROI(:);
    pensubdf( :, i ) = S.baseStats.ROI(i).dFoF0.pensub(:);
end
FFAdFoF0 = S.baseStats.FFA.dFoF0;
timing = S.timing;
save(fname, 'timing', 'roidf', 'pensubdf','FFAdFoF0', 'roi', 'pen', 'pensub' );
end