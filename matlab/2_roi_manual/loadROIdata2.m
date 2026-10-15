% import data from *_Igor.mat files (contains ROI data)
% returns penumbra subtracted dF/F0!
function ROIdataStruct = loadROIdata2( path ) % use full path now! localpath )

% get the list of file names
%upath = userpath; % default user directory "MATLAB" contains data directory "ROI data"
if ismac
    ext = '*_Igor*.mat';
elseif isunix
    % Code to run on Linux platform
elseif ispc
    ext = '\*_Igor*.mat';
else
    disp('Platform not supported')
end
dpath = append(path,ext); % code for ROI stacks output to Igor
ROIdirStruct = dir(dpath);
nROIstacks = size(ROIdirStruct,1);
for i=1:nROIstacks
     ROIdataName = ROIdirStruct(i).name;
     ROIdataStruct(i).name = ROIdataName(1:end-9); % removes '_Igor.mat' from file name
     fullpathname = append(ROIdirStruct(i).folder, '/', ROIdirStruct(i).name);

     S = load( fullpathname ); %, 'pensubdf' ); %ROIdataName, 'pensubdf');
% variables stored in struct/file 'timing', 'roidf', 'pensubdf','FFAdFoF0', 'roi', 'pen', 'pensub'

     ROIdataStruct(i).data = S.pensubdf;
     % ROIstack = cell2mat(struct2cell(data));
     % n = size(ROIstack,1);
     nel = max(size(S.pensubdf)); % get the number of time points
     if isfield(S,'timing')
         ROIdataStruct(i).xdata = S.timing(1:nel);
         dx = mean(diff(S.timing));
         if dx>1
             disp('xdata in msec...converting to sec');
             ROIdataStruct(i).xdata = S.timing(1:nel)/1000;
         end
     else
         dx=0.05;
         % fake the xdata
         ROIdataStruct(i).xdata = 0:dx:((nel-1)*dx);
         disp(append('*** ', ROIdataName,': no xdata, faked interval of 0.05s ',num2str(nel),' psdf: ',num2str(size(S.pensubdf))));
     end
    if isfield(S,'FFAdFoF0')
        ROIdataStruct(i).FFA = S.FFAdFoF0;
    else
        ROIdataStruct(i).FFA = NaN;
    end
end

end