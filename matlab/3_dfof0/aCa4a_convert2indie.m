% after aCa4_ROI_dFoF0, convert to individiual files for ease of use
% makes much smaller files
% add spikes and exp to individual files!!

first_file=0; % 75;

% get dir
path = uigetdir('C:\Users\defazio\Documents\data\complete');
savepath = uigetdir(['C:\Users\defazio\Documents\data\processed\individual']);

% get list of files in dir
codekey = 'complete.mat';
if ismac
    ext = append('\*',codekey); %complete2.mat';
    the_slash='/';
elseif isunix
    % Code to run on Linux platform
elseif ispc
    ext = append('\*',codekey); %complete.mat';
    the_slash='\';
else
    disp('Platform not supported')
end
dpath = append(path,ext); % set the path with wildcard
completeDir = dir(dpath);
nfiles = size(completeDir,1);

% loop over list
t1=10; %seconds
t2=inf;
t3=inf;
t4=200; %seconds

fracPen = 0.7;

set(0,'DefaultFigureWindowStyle','docked')
targetField = 'baseStats';
first=1;
if first_file~=0
    first=first_file;
end
for i=first:nfiles
    % load file complete
    filename = append(completeDir(i).folder, the_slash, completeDir(i).name);
    datecode_exp = '\d{8}[a-z]?_?\d*';
    name = regexp(completeDir(i).name,datecode_exp,'match','once');
    disp(append('fn: ',filename, ' name: ', name))

    m = matfile(filename,"Writable",false);
    mvars = who(m);
    if ismember( targetField, mvars ) % make sure mat file event detection!
        baseStats = m.baseStats;
        ROIdata = baseStats.combo.ROI;
        hROI=m.hROI;
        projections=m.projections;
        timing=m.timing;
        fname = fullfile(savepath,[name,'.mat']);

        if isfile(fname) % update the old file with new ROIs, etc
            disp('file already exists! updating ROIdata and purging analysis')
            m_dest = matfile(fname,"Writable",true);
            m_dest.ROIdata = ROIdata;
            m_dest.hROI = hROI;
            m_dest.projections = projections;
            m_dest.timing = timing;
            m_dest.S_event = []; % delete previous analysis
            m_dest.humanLog = [];
            disp(append(fname,'updated indie file!'))            

        else
            save(fname,"ROIdata","hROI","projections","timing","name")
            disp(append(fname,' making new indie file!'))
        end
    else
        disp('no baseStats')
    end
end
