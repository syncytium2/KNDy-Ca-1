%%
% 
% RUN CONVERT TO INDIE after dFoF0! aCa4a_convert2indie
%
% choose a file or 0 for all files in directory
path=uigetdir("C:\Users\defazio\Documents\data\processed\individual");
%path="C:\Users\defazio\Documents\data\processed\individual";
%path="\\umms-smoenter-win.turbo.storage.umich.edu\umms-smoenter\defazio turbo\data\processed\individual";
% if ismac()
%     path="/Users/tonydefazio/Documents/data/processed/individual";
% end

first = 75; %74; % 64; % to process a single file, enter the file number
fast_batch_name='custard';
% what data from ROIdata are we analyzing
field = 'dFoF0';
% minSep for goodROIS
minSep=5; % 

%% custard and filter settings for FAST

% set up the runs
data_category = 'fast'; % where to store in S_event detection struct
data_sources = { "pensub", "ROI" }; % penumbra subtracted or straight ROI, dF/F0
threshold_types={"SD","diffSD","MLspike_sigmaest","CaImAn"}; % ,'Med','Strict','ML'};
threshold_factors=[2,3,4,5];

n_data_sources=length(data_sources);

nRuns = length(data_sources) * length(threshold_types) * length(threshold_factors);
iRun=0;
analyses = cell(nRuns,1);
for iDS=1:length(data_sources)
    for iTT=1:length(threshold_types)
        for iTF=1:length(threshold_factors)
            iRun = iRun + 1;
            a_par = append( data_sources{iDS}, ...
                '_', threshold_types{iTT}, ...
                '_', num2str(threshold_factors(iTF)) );

            analyses{iRun} = a_par;
            analysis_pars.(a_par).iRun = iRun;
            analysis_pars.(a_par).data_source = data_sources{iDS};
            analysis_pars.(a_par).forceThresh = -1; % -1 triggers autothreshold
            analysis_pars.(a_par).threshold_type = threshold_types{iTT}; 
            analysis_pars.(a_par).threshold_factor = threshold_factors(iTF); 
            analysis_pars.(a_par).FWHMfraction = 0.5; % seconds
            analysis_pars.(a_par).psig = 0.05; % slope factor, significance
            analysis_pars.(a_par).win = 0.5; % seconds
            analysis_pars.(a_par).minWidth = 0.4; % seconds
        end
    end
end

%% loop over files!

% set up the loop over files in directory
files = dir(fullfile(path,'*.mat'));
nFiles=length(files);

if first ~= 0
    %nFiles = 1;
    last=nFiles; % first;
else
    first=1;
    last=nFiles;
end

expr = '\d{8}[a-z]?_?\d*'; % full date code
expr2 = '\d{8}\d*'; % first 8 char

for iFile=first:last
    tic

    m = matfile( fullfile( files(iFile).folder, files(iFile).name ), 'Writable', true );
    disp(append(num2str(iFile),' ',files(iFile).name,' mname:',m.name))

    ROIdata = m.ROIdata;
    timing = m.timing;
    hROI = m.hROI;

    mvars = who(m);
    if ismember( "S_event", mvars ) % make sure mat file has event detection!
        S_event = m.S_event;
    else
        S_event=[];
    end

    mvars = who(m);

    [ goodROIs.ROIs, ~ ] = GetGoodROIs( hROI, minSep ); % only uses combo
    goodROIs.minSep = minSep;
    m.goodROIs = goodROIs;

    str = append("ROIs: ",num2str(length(hROI.combo)) , ...
        " good: ",num2str(length(goodROIs.ROIs)) );
    disp(str)

    % seconds to points
    dt=mean(diff(timing));
    nROIs = length(ROIdata); % process all the data, let Gxd sort it out later
    iROI=1;
    subfield = data_sources{1};
    nPoints = length( ROIdata(iROI).(field).(subfield) ); % nPoints x 1

    % load the fluo_matrix, nROIs x nPoints
    % to fix moco crash, have to round nPoints to easy number
    % forgot to fix length of timing
    % check timing is same length
    nPoints2 = length(timing);
    if nPoints~=nPoints2
        nPoints=min(nPoints,nPoints2);
        timing2 = timing(1:nPoints);
        timing=timing2;
    end
    fluo_matrix = nan( nRuns, nROIs, nPoints );
    %fluo_matrix = nan(nROIs,nPoints);
    for iROI=1:nROIs % load the fluo_matrix
        for iRun=1:nRuns
            a_par = analyses{iRun};
            subfield = analysis_pars.(a_par).data_source; 
            data = squeeze( ROIdata(iROI).(field).(subfield) ); % nPoints x 1
        end
        % need only one copy of data
        fluo_matrix( iROI, 1:nPoints ) = data(1:nPoints);
    end
    %%
    events = custardWrapper3( timing, fluo_matrix, analysis_pars, analyses ); % , detailed );
    % append detection to S_event
    S_event.(data_category).(fast_batch_name) = events;
    m.S_event = S_event;
    disp(append('custard run: ',num2str(toc)))

end % loop over files!