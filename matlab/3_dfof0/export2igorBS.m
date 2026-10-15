function export2igorAPP( BS, timing, fname )
% fname is a full path name, or saves to current folder
% custom function to store processed ROIs
% to be read by Igor (or MATLAB)
% S is the structure returned by roi2stats3D and processROIs 
 % sometimes called "baseStats"

ncells = size(BS.ROI,2);
npoints = size( BS.ROI(1).dFoF0.ROI, 1);

pen = zeros( npoints, ncells );
roi = zeros( npoints, ncells );
pensub = zeros( npoints, ncells );
roidf = zeros( npoints, ncells );
pensubdf = zeros( npoints, ncells );
for i=1:ncells
    roi( :, i ) = BS.ROI(i).ROI.meanZ(:);
    pen( :, i ) = BS.ROI(i).penumbra.meanZ(:);
    pensub( :, i ) = BS.ROI(i).pensub; 

    roidf( :, i ) = BS.ROI(i).dFoF0.ROI(:);
    pensubdf( :, i ) = BS.ROI(i).dFoF0.pensub(:);
end
FFAdFoF0 = []; % BS.FFA.dFoF0;
save(fname, 'timing', 'roidf', 'pensubdf','FFAdFoF0', 'roi', 'pen', 'pensub' );
end