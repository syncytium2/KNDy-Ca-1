function events = custardWrapper( timing, fluo_matrix, params_in, detailed )
% params_in contains multiple sets of custard parameters
% timing 1 x npoints
% fluo nROIs x npoints
% params_in, detailed structure with params for multiple runs
% detailed, flag for showing details of custard algorithm

detailFlag=false;
if nargin==4
    detailFlag=detailed;
end

% handle multiple sets of custard params: EZ, Medium, Strict
fn = fieldnames(params_in);
run_params_fields = { fn{ ~strcmp(fn,'showPlots') } };
nRuns = length(fn)-1; % last field is showPlots
for iRun = 1:nRuns % loop over the analysis parameter sets

    params_name = run_params_fields{iRun};
    custard_params = params_in.(params_name);
    % if ~isfield(custard_params,'forceThresh') % outdated needs cleanup 911
    %     forceThresh = 0.0075; % dF/F0
    %     tXrise = 0.5; % fraction of peak for width
    %     psig = 0.2; % ttest sig for test windows
    %     win = 0.2; % seconds, duration of test windows
    %     minWidth = 0.2; % fast_params.minwidth; % seconds
    %     disp('*** TDcustard, using default parameters ***')
    % else
        forceThresh = custard_params.forceThresh; % dF/F0
        tXrise = custard_params.FWHMfraction; % fraction of peak for width
        psig = custard_params.psig; % ttest sig for test windows
        win = custard_params.win; % seconds, duration of test windows
        minWidth = custard_params.minWidth;
    %end

    noFig=true;
    if params_in.showPlots == 1
        noFig = false;
    end

    dt = mean( diff( timing ) );
    % if forceThresh == 0 % untested 911
    %     %SD = std(fluo); % standard deviation
    %     rollStd = movstd( fluo, roll/dt ); % rolling standard deviation
    %     P = prctile( rollStd, prcthresh );
    %     minHeight = Pfactor * P;
    % else
        minHeight = forceThresh;
    %end
    %fast_params.minHeight = minHeight;

    minWidthPoints = floor( minWidth/dt );
    npnts = round( win/dt );
    % parallelizable!
    for iROI=1:size(fluo_matrix,1)
        fluo = fluo_matrix( iROI, : );
        custardOutput(iROI) = TDcustard2( npnts, fluo, psig, minHeight, minWidthPoints, tXrise, noFig, detailFlag );
    end

    % % format of custard output
    % custardOutput.upLoc=upLoc; % index of up
    % custardOutput.upAmp=upAmp; % relative amp of up
    % custardOutput.up=up; % value of up
    % custardOutput.dnLoc=dnLoc; % index of down
    % custardOutput.dnAmp=dnAmp; % relative amp of dn
    % custardOutput.dn=dn; % value of dn
    % custardOutput.p=p;
    % % get rid of narrow peaks!
    % custardOutput.peak=peak(peakWidth>=minWidth);
    % custardOutput.peakAmp=peakAmp(peakWidth>=minWidth);
    % custardOutput.peakLoc=peakLoc(peakWidth>=minWidth);
    % custardOutput.peakWidth=peakWidth(peakWidth>=minWidth);
    % custardOutput.t50riseIndex=t50riseIndex(peakWidth>=minWidth);
    % custardOutput.t50fallIndex=t50fallIndex(peakWidth>=minWidth);
    % custardOutput.halfAmp=halfAmp(peakWidth>=minWidth);

    if ~isempty(custardOutput)
        % w = round( minWidth/dt );
        % mvmin = movmin( fluo, [w,w] );
        % events.movThresh = minHeight + mvmin;

        % fast output data structure
        events.data = fluo_matrix;
        events.timing = timing;
        events.(params_name).pks = custardOutput.peak; % pks(ii); % peaks
        locs = custardOutput.peakLoc;
        events.(params_name).locs = ( locs-1 ) * dt; % locs(ii); % zero-based absolute location of peaks
        w = custardOutput.peakWidth;
        events.(params_name).w =  w * dt; % w(ii); % width using rise and fall times

        events.(params_name).p = custardOutput.peakAmp; % p(ii); % prominence, relative peak
        %events.wxPk(jj,:) = wxPk(ii,:); % rise and fall times
        wxPk = zeros( length(custardOutput.peak), 2 );
        wxPk(:,1) = (custardOutput.t50riseIndex-1) * dt; % wxPk(ii,:); % rise and fall times
        wxPk(:,2) = (custardOutput.t50fallIndex-1) * dt; % wxPk(ii,:); % rise and fall times
        events.(params_name).wxPk = wxPk;
        events.(params_name).custard = custardOutput;

        % custard.t50 = tXrise;
        % custard.psig = psig;
        % custard.win = win;
        % custard.npnts = npnts;

        events.(params_name).custard_params = custard_params;
    else
        % w = round( minWidth/dt );
        % mvmin = movmin( fluo, [w,w] );
        % events.movThresh = minHeight + mvmin;
        events.data = fluo_matrix;
        events.timing = timing;
        events.(params_name).pks = []; %custardOutput.peak; % pks(ii); % peaks
        events.(params_name).locs = []; % ( custardOutput.peakLoc-1 ) * dt; % locs(ii); % absolute location of peaks
        events.(params_name).w = []; % custardOutput.peakWidth * dt; % w(ii); % width using rise and fall times
        events.(params_name).p = []; % custardOutput.peakAmp; % p(ii); % prominence, relative peak
        events.(params_name).wxPk = []; % wxPk;
        events.(params_name).custard = custardOutput;

        % custard.t50 = tXrise;
        % custard.psig = psig;
        % custard.win = win;
        % custard.npnts = npnts;

        events.(params_name).custard_params = custard_params;
    end

end % loop over multiple custard runs!