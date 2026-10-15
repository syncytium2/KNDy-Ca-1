function events = custardWrapper3( timing, fluo_matrix, params_in, run_params_fields, detailed )
% version 3 adapted for multiple runs on the same data

% expects params in seconds, converts to points based on timing 

% params_in contains multiple sets of custard parameters
% all inputs in real units, converted to points below
% timing 1 x npoints
% fluo nROIs x npoints
% params_in, detailed structure with params for multiple runs
% detailed, optional flag for showing details of custard algorithm
    % params_name = run_params_fields{iRun}; % names of the param sets to
    % run
    % custard_params = params_in.(params_name); % param sets
    % forceThresh = custard_params.forceThresh; % dF/F0, 
    % set to -1 for autothresh
    % tXrise = custard_params.FWHMfraction; % fraction of peak for width
    % psig = custard_params.psig; % ttest sig for test windows
    % win = custard_params.win; % seconds, duration of test windows
    % minWidth = custard_params.minWidth;

events.start=datetime;

detailFlag=false;
if nargin==5
    detailFlag=detailed;
end

% handle multiple sets of custard params: EZ, Medium, Strict
nRuns = length(run_params_fields); 

% handle run based changes in data
if ndims(fluo_matrix)==3
    [nRuns2,nROIs,nPoints]=size(fluo_matrix);
    runMatrixFlag=true;
    %validate runs
    if nRuns2~=nRuns
        runMatrixFlag=false;
        disp('custardWrapper: nRuns must match first dimension of fluo_matrix')
    end
else
    [nROIs,nPoints]=size(fluo_matrix);
    runMatrixFlag=false;
end

for iRun = 1:nRuns % loop over the analysis parameter sets

    params_name = run_params_fields{iRun};
    custard_params = params_in.(params_name);

    forceThresh = custard_params.forceThresh; % dF/F0, -1 for autothresh
    % if forceThresh==-1 % future version might enable this
    %     threshold_type= custard_params.threshold_type;
    %     threshold_factor= custard_params.threshold_factor;
    % end
    tXrise = custard_params.FWHMfraction; % fraction of peak for width
    psig = custard_params.psig; % ttest sig for test windows
    win = custard_params.win; % seconds, duration of test windows
    minWidth = custard_params.minWidth; % seconds, minimum event width

    noFig=true;
    if isfield(params_in,'showPlots') && params_in.showPlots == 1
        noFig = false;
    end

    dt = mean( diff( timing ) );
     minWidthPoints = floor( minWidth/dt );
    npnts = round( win/dt );

    silent=1;
    parstor=nan(nROIs,nPoints);
    for iROI=1:nROIs
        if runMatrixFlag
            parstor(iROI,:)=squeeze( fluo_matrix( iRun, iROI, : ) ); % iRun is the root loop
        else
            parstor(iROI,:) = squeeze( fluo_matrix( iROI, : ) );
        end
    end
    thresholds=nan(nROIs,1);
    parfor iROI=1:nROIs
    % for iROI=1:nROIs
        if forceThresh == 0
            % noise_estimate = TDnoise2( squeeze(parstor(iROI,:)), dt, threshold_type );
            % minHeight = noise_estimate * threshold_factor;
            minHeight=0;
        else
            minHeight = forceThresh;
        end
        thresholds(iROI) = minHeight;
        custardOutput(iROI) = TDcustard2( npnts, squeeze(parstor(iROI,:)), psig, minHeight, minWidthPoints, tXrise, noFig, detailFlag, silent );
    end
    % add the thresholds for each ROI to the output of the event detection
    custard_params.thresholds = thresholds;

    events.params = params_in;
    events.analyses = run_params_fields;
    if ~isempty(custardOutput)
        % fast output data structure
        for iROI=1:nROIs
            events.(params_name)(iROI).pks = custardOutput(iROI).peak; 
            events.(params_name)(iROI).locs = ( custardOutput(iROI).peakLoc-1 ) * dt; 
            events.(params_name)(iROI).w = custardOutput(iROI).peakWidth * dt;
            events.(params_name)(iROI).p = custardOutput(iROI).peakAmp;

            wxPk = zeros( length(custardOutput(iROI).peak), 2 );
            wxPk(:,1) = (custardOutput(iROI).t50riseIndex-1) * dt;
            wxPk(:,2) = (custardOutput(iROI).t50fallIndex-1) * dt;
            events.(params_name)(iROI).wxPk = wxPk;
            events.(params_name)(iROI).custard = custardOutput(iROI);
            events.(params_name)(iROI).custard_params = custard_params;
            events.(params_name)(iROI).run_date = datetime;
        end
    else
        for iROI=1:nROIs
            events.(params_name)(iROI).pks = [];
            events.(params_name)(iROI).locs = [];
            events.(params_name)(iROI).w = [];
            events.(params_name)(iROI).p = [];
            events.(params_name)(iROI).wxPk = [];
            events.(params_name)(iROI).custard = custardOutput;
            events.(params_name)(iROI).custard_params = custard_params;
            events.(params_name)(iROI).run_date = datetime;
        end
    end
end % runs: loop over multiple custard runs!
events.data = []; % fluo_matrix;
events.timing = []; % timing;

events.end=datetime;