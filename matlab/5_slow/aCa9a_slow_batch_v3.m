% autothreshold!

% read array from file
%loc = "C:\Users\defazio\Documents\data\processed\individual\20250925_233.mat";
%load(loc)

% choose a file or 0 for all files in directory
path=uigetdir("Z:\defazio turbo\data\processed\individual");
%path="C:\Users\defazio\Documents\data\processed\individual";
if ismac()
    path="/Users/tonydefazio/Documents/data/processed/individual";
end

first = 0; % 0; % 64; % to process a single file, enter the file number


%% custard and filter settings for slow
data_sources = { 'pensub', 'ROI'}; % penumbra subtracted or straight ROI, dF/F0
filters={'slow_movmin','slow_filtfilt','slow_prctFlt'}; % ,'Med','Strict','ML'};
% version 3 pulls autothresholds from custard analysis 
auto_threshold_sources={"MLspike_sigmaest"};
auto_threshold_factors=[2,3,4,5];

analysis_pars.data_sources = data_sources;
analysis_pars.filters = filters;

%% THESE SETTINGS ARE COMMON ACROSS ALL RUNS!
overall_win = 1; % seconds
overall_psig = 0.025; % significance
overall_FWHM = 0.5; % second
overall_forceThresh = 0.01; % dF/F0
overall_minWidth = 1; % second

% movmin 
min_t = 5; % seconds

% filtfilt
Niir = 3; % order
Fst = 0.2; % stop frequency

% prctfilt
window_seconds = 2.5;
step = 1; %samples % seconds, x units
perc = 5; % percentile, 5th is classic CaImAn
mode = 0;

%% /\/\/\/\/\/\/\ SETTINGS FOR ALL RUNS ABOVE! /\/\/\/\/\/\/\/\/\/\

% movmin settings
iRun = 1;
iFilter = 1;
ds = 1;
a_par = append( filters{iFilter}, '_', data_sources{ds} );
analyses{iRun} = a_par;
analysis_pars.(a_par).iFilter=iFilter;
analysis_pars.(a_par).iDataset=ds;
analysis_pars.(a_par).forceThresh = overall_forceThresh; % signal units % app.cEZ_ForceThreshEditField.Value;
analysis_pars.(a_par).FWHMfraction = overall_FWHM; % fraction of max % app.cEZ_FWHMfractionEditField.Value;
analysis_pars.(a_par).psig = overall_psig; % significance % app.cEZ_SigThreshEditField.Value;
analysis_pars.(a_par).win = overall_win; % seconds % app.cEZ_WinEditField.Value;
analysis_pars.(a_par).minWidth = overall_minWidth; % seconds % app.cEZ_MinWidthEditField.Value;
%
analysis_pars.(a_par).window = min_t; % movmin, seconds

% movmin settings, 2
iRun = iRun+1;
iFilter=1;
ds=2;
a_par = append( filters{iFilter}, '_', data_sources{ds} );
analyses{iRun} = a_par;
analysis_pars.(a_par).iFilter=iFilter;
analysis_pars.(a_par).iDataset=ds;
analysis_pars.(a_par).forceThresh = overall_forceThresh; % signal units % app.cEZ_ForceThreshEditField.Value;
analysis_pars.(a_par).FWHMfraction = overall_FWHM; % fraction of max % app.cEZ_FWHMfractionEditField.Value;
analysis_pars.(a_par).psig = overall_psig; % significance % app.cEZ_SigThreshEditField.Value;
analysis_pars.(a_par).win = overall_win; % seconds % app.cEZ_WinEditField.Value;
analysis_pars.(a_par).minWidth = overall_minWidth; % seconds % app.cEZ_MinWidthEditField.Value;
%
analysis_pars.(a_par).window = min_t; % movmin, seconds

% filtfilt settings:
iRun = iRun+1;
iFilter=2;
ds=1;
a_par = append( filters{iFilter}, '_', data_sources{ds} );
analyses{iRun} = a_par;
analysis_pars.(a_par).iFilter=iFilter;
analysis_pars.(a_par).iDataset=ds;
analysis_pars.(a_par).forceThresh = overall_forceThresh; % signal units % app.cEZ_ForceThreshEditField.Value;
analysis_pars.(a_par).FWHMfraction = overall_FWHM; % fraction of max % app.cEZ_FWHMfractionEditField.Value;
analysis_pars.(a_par).psig = overall_psig; % significance % app.cEZ_SigThreshEditField.Value;
analysis_pars.(a_par).win = overall_win; % seconds % app.cEZ_WinEditField.Value;
analysis_pars.(a_par).minWidth = overall_minWidth; % seconds % app.cEZ_MinWidthEditField.Value;
%
analysis_pars.(a_par).Fst = Fst;
analysis_pars.(a_par).Niir = Niir;

% filtfilt settings: 2
iRun = iRun+1;
iFilter=2;
ds=2;
a_par = append( filters{iFilter}, '_', data_sources{ds} );
analyses{iRun} = a_par;
analysis_pars.(a_par).iFilter=iFilter;
analysis_pars.(a_par).iDataset=ds;
analysis_pars.(a_par).forceThresh = overall_forceThresh; % signal units % app.cEZ_ForceThreshEditField.Value;
analysis_pars.(a_par).FWHMfraction = overall_FWHM; % fraction of max % app.cEZ_FWHMfractionEditField.Value;
analysis_pars.(a_par).psig = overall_psig; % significance % app.cEZ_SigThreshEditField.Value;
analysis_pars.(a_par).win = overall_win; % seconds % app.cEZ_WinEditField.Value;
analysis_pars.(a_par).minWidth = overall_minWidth; % seconds % app.cEZ_MinWidthEditField.Value;
%
analysis_pars.(a_par).Fst = Fst;
analysis_pars.(a_par).Niir = Niir;

% prctfilt settings
iRun = iRun+1;
iFilter=3;
ds=1;
a_par = append( filters{iFilter}, '_', data_sources{ds} );
analyses{iRun} = a_par;
analysis_pars.(a_par).iFilter=iFilter;
analysis_pars.(a_par).iDataset=ds;
analysis_pars.(a_par).forceThresh = overall_forceThresh; % signal units % app.cEZ_ForceThreshEditField.Value;
analysis_pars.(a_par).FWHMfraction = overall_FWHM; % fraction of max % app.cEZ_FWHMfractionEditField.Value;
analysis_pars.(a_par).psig = overall_psig; % significance % app.cEZ_SigThreshEditField.Value;
analysis_pars.(a_par).win = overall_win; % seconds % app.cEZ_WinEditField.Value;
analysis_pars.(a_par).minWidth = overall_minWidth; % seconds % app.cEZ_MinWidthEditField.Value;
%
analysis_pars.(a_par).window = window_seconds;
analysis_pars.(a_par).step = step;
analysis_pars.(a_par).perc = perc;
analysis_pars.(a_par).mode = mode;
prctfiltFilter=iFilter;

% prctfilt settings 2
iRun = iRun+1;
iFilter=3;
ds=2;
a_par = append( filters{iFilter}, '_', data_sources{ds} );
analyses{iRun} = a_par;
analysis_pars.(a_par).iFilter=iFilter;
analysis_pars.(a_par).iDataset=ds;
analysis_pars.(a_par).forceThresh = overall_forceThresh; % signal units % app.cEZ_ForceThreshEditField.Value;
analysis_pars.(a_par).FWHMfraction = overall_FWHM; % fraction of max % app.cEZ_FWHMfractionEditField.Value;
analysis_pars.(a_par).psig = overall_psig; % significance % app.cEZ_SigThreshEditField.Value;
analysis_pars.(a_par).win = overall_win; % seconds % app.cEZ_WinEditField.Value;
analysis_pars.(a_par).minWidth = overall_minWidth; % seconds % app.cEZ_MinWidthEditField.Value;
%
analysis_pars.(a_par).window = window_seconds;
analysis_pars.(a_par).step = step;
analysis_pars.(a_par).perc = perc;
analysis_pars.(a_par).mode = mode;
%prctfiltFilter=iFilter;

nRuns = iRun;
nFilters = iFilter;

% what data from ROIdata are we analyzing
field = 'dFoF0';
subfield = 'ROI'; % could choose pensub for penumbra subtracted data

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
    disp(files(iFile).name)
    m = matfile( fullfile( files(iFile).folder, files(iFile).name ), 'Writable', true );

    ROIdata = m.ROIdata;
    timing = m.timing;
    goodROIs = m.goodROIs;
    S_event = m.S_event;

    % seconds to points
    dt = mean( diff( timing ) );
    k = round( min_t / dt ); % convert to points FOR MOVMIN FILTER
    win = round( window_seconds / dt ); %samples FOR PRCTFILT
    Fs = 1/dt; % sampling rate FOR FILTFILT

    nROIs = length(ROIdata); % process all the data, let Gxd sort it out later

    % load the fluo_matrix, nROIs x nPoints
    nPoints = length( timing );
    nPoints2 = length( ROIdata(1).dFoF0.ROI );
    if nPoints~=nPoints2
        nPoints=min(nPoints,nPoints2);
        m.timing_old = timing;
        timing=timing(1:nPoints);
        m.timing=timing;
        disp(append(m.name,' updated timing! check moco code for lack of update'))
    end
    fluo_matrix = nan( nFilters, nROIs, nPoints );

    for iROI=1:nROIs % load the fluo_matrix
        for iRun=1:nRuns
            a_par = analyses{iRun}; %  = a_par;
            iFilter = analysis_pars.(a_par).iFilter;
            ds = analysis_pars.(a_par).iDataset; % =ds;
            subfield = data_sources{ds};
            data = squeeze( ROIdata(iROI).(field).(subfield)(1:nPoints) ); % nPoints x 1
            filter = filters{iFilter};
            switch(filter)
                case filters{1}
                    fdata = movmin( data, k );
                case filters{2}
                    fdata = filterData( data, Fs, Niir, Fst )'; % custom Function calling filtfilt
                case filters{3}
                    fdata = prctfilt( data', perc, win, step, mode )'; % returns 1xT
            end
            fluo_matrix( iRun, iROI, : ) = fdata;
        end
    end
    %% actual custard analysis happens once for each filter and dataset
    events = [];
    events = custardWrapper3( timing, fluo_matrix, analysis_pars, analyses ); % , detailed );
    S_event.slow = events; % this stores original custard analysis
    S_event.slow.data = fluo_matrix;

    %% NOW RUN THROUGH THE VARIOUS THRESHOLDS using the same custard data
    i_run=0;
    n_data_sources=length(data_sources);
    n_filters = length(filters);
    n_threshold_factors = length(auto_threshold_factors);
    field_names=cell(n_data_sources*n_filters*n_threshold_factors,1);

    for i_ds=1:n_data_sources
        for i_filter=1: n_filters % filters in the slow custard run
            for i_factors=1: n_threshold_factors
                threshold_source=append(data_sources{i_ds},'_',auto_threshold_sources{1},'_',num2str(auto_threshold_factors(i_factors)));

                % pull the thresholds (one for each ROI), already factored!
                thresholds = S_event.fast.custard.(threshold_source)(1).custard_params.thresholds;
                nROIs=length(thresholds);

                slow_field_name = append( filters{i_filter}, '_', data_sources{i_ds} );

                new_field_name = append( data_sources{i_ds},'_', ...
                    filters{i_filter}, '_', ...
                    num2str(auto_threshold_factors(i_factors)));
                i_run=i_run+1;
                field_names{i_run}=new_field_name;

                nROIs2 = length( S_event.slow.( slow_field_name ) );
                if nROIs2 ~= nROIs
                    disp("houston we have a problem loading slow")
                end

                for iROI=1:nROIs
                    custard_in = S_event.slow.(slow_field_name)(iROI).custard;
                    custard_params = S_event.slow.(slow_field_name)(iROI).custard_params;
                    % store all the peaks whether they are too small or too narrow!
                    peak = custard_in.peakAll; % =peak;
                    peakAmp = custard_in.peakAmpAll; % =peakAmp;
                    peakLoc = custard_in.peakLocAll; % =peakLoc;
                    peakWidth = custard_in.peakWidthAll; % =peakWidth;
                    tXriseIndex = custard_in.t50riseIndexAll; % =tXriseIndex;
                    tXfallIndex = custard_in.t50fallIndexAll; % =tXfallIndex;
                    XAmp = custard_in.halfAmpAll; % =XAmp;
                    peakBase = custard_in.peakBaseAll; %  = peakBase;
                    peakBaseLine = custard_in.peakBaseLineAll; % = peakBaseLine;
                    peakBaseLineX = custard_in.peakBaseLineXAll; % = peakBaseLineX;

                    % get rid of small and narrow peaks!
                    minAmp = thresholds(iROI);
                    indices = find( peakAmp>minAmp & peakWidth >= overall_minWidth );

                    custardOutput.peak=peak(indices);
                    custardOutput.peakAmp=peakAmp(indices);
                    custardOutput.peakLoc=peakLoc(indices);
                    custardOutput.peakWidth=peakWidth(indices);
                    custardOutput.t50riseIndex=tXriseIndex(indices);
                    custardOutput.t50fallIndex=tXfallIndex(indices);
                    custardOutput.halfAmp=XAmp(indices);
                    custardOutput.peakBase = peakBase(indices); % = upBase(ii);
                    custardOutput.peakBaseLine = peakBaseLine(indices,:); % = upBaseLine(ii,:);
                    custardOutput.peakBaseLineX = peakBaseLineX(indices,:); % = upLoc(ii)-nBase : upLoc(ii)-1;

                    % custard output data structure
                    events.(new_field_name)(iROI).pks = custardOutput.peak;
                    events.(new_field_name)(iROI).locs = ( custardOutput.peakLoc-1 ) * dt;
                    events.(new_field_name)(iROI).w = custardOutput.peakWidth * dt;
                    events.(new_field_name)(iROI).p = custardOutput.peakAmp;

                    wxPk = zeros( length(custardOutput.peak), 2 );
                    wxPk(:,1) = (custardOutput.t50riseIndex-1) * dt;
                    wxPk(:,2) = (custardOutput.t50fallIndex-1) * dt;
                    events.(new_field_name)(iROI).wxPk = wxPk;
                    events.(new_field_name)(iROI).custard = custardOutput;
                    events.(new_field_name)(iROI).custard_params = custard_params;
                    events.(new_field_name)(iROI).run_date = datetime;
                    events.(new_field_name)(iROI).revised_threshold=thresholds(iROI);
                end % iROIs

            end % filters
        end % factors
    end % data sources
    %% append revised event detection
    for i_field=1:length(field_names)
        S_event.slow.(field_names{i_field}) = events.(field_names{i_field});
    end % loop over fields
    %%

    m.S_event = S_event;
    disp(append('custard run: ',num2str(toc)))
end % loop over files!