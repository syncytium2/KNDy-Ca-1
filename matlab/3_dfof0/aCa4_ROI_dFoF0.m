

% deprecated! XXX RUN COMBINE ROIS FIRST!!!!!!!!!!!!!!!

first_file=0; %45; % 34; % 0;

% 20240722 hard coded to use S.hROI.DFoF0!
projection_type = 'DFoF0';
stack_type = 'Smoco';

% get dir
path = uigetdir('C:\Users\defazio\Documents\data\complete');

% get list of files in dir
codekey = 'complete*.mat';
if ismac
    ext = append('/*',codekey); %complete2.mat';
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
% settings chagned from t1=10, t4=200 20251024
t1=1; %seconds
t2=inf;
t3=inf;
t4=20; %seconds

fracPen = 0.7;

set(0,'DefaultFigureWindowStyle','docked')
first=1;
if first_file~=0
    first=first_file;
end
for i=first:nfiles
    % load file complete
    filename = append(completeDir(i).folder, the_slash, completeDir(i).name);
    exp = '\d{8}[a-z]?_?\d*';
    name = regexp(completeDir(i).name,exp,'match','once');
    disp(append(num2str(i),' fn: ',filename, ' name: ', name))

    m = matfile(filename,"Writable",true);
    S = loadFlatStackOpt( m, stack_type ); %loadComplete3( filename );

    S.tn = [t1,t2,t3,t4];

    if isfield(S,'hROI')

        % S.baseStats.mean = getROItraces2( S.(stack_type).stack, S.hROI.mean, fracPen ); % uses "raw" (cropped, smoothed etc)
        % S.baseStats.max = getROItraces2( S.(stack_type).stack, S.hROI.max, fracPen ); % uses "raw" (cropped, smoothed etc)
        % S.baseStats.std = getROItraces2( S.(stack_type).stack, S.hROI.std, fracPen ); % uses "raw" (cropped, smoothed etc)
        S.baseStats.combo = getROItraces2( S.(stack_type).stack, S.hROI.combo, fracPen ); % uses "raw" (cropped, smoothed etc)

        dx = mean(diff(S.timing));
        if dx>5
            disp('timing data in msec...converting to sec');
            S.timing = S.timing/1000;
        end
        tn_frames = S.tn / mean(diff(S.timing)); % 911

        % S.baseStats.mean = processDFoF0( S.baseStats.mean, tn_frames(1), tn_frames(2), tn_frames(4) );% , minminBF );
        % S.baseStats.max = processDFoF0( S.baseStats.max, tn_frames(1), tn_frames(2), tn_frames(4) );% , minminBF );
        % S.baseStats.std = processDFoF0( S.baseStats.std, tn_frames(1), tn_frames(2), tn_frames(4) );% , minminBF );
        S.baseStats.combo = processDFoF0( S.baseStats.combo, tn_frames(1), tn_frames(2), tn_frames(4) );% , minminBF );

        % fname = append(name, '_igorMean');
        % export2igorBS(S.baseStats.mean,S.timing,fname)
        % fname = append(name, '_igorMax');
        % export2igorBS(S.baseStats.max,S.timing,fname)
        % fname = append(name, '_igorSTD');
        % export2igorBS(S.baseStats.std,S.timing,fname)
        fname = append(name, '_igorCombo');
        export2igorBS(S.baseStats.combo,S.timing,fname)
        %save(filename,"-struct","S")
        % update matfile
        m.baseStats = S.baseStats;
    else
        disp(append('file lacks hROI! import failed: ', filename))
    end
    clear S
end