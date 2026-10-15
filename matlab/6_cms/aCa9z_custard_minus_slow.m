set(0,'DefaultFigureWindowStyle','docked');
load_matfile =   true; %false; %
if load_matfile
    path="Z:\defazio turbo\data\processed\individual"; 
    %uigetdir("Z:\defazio turbo\data\processed\individual");
    files = dir(fullfile(path,'*.mat'));
    nFiles=length(files);
else
    nFiles=1;
end

do_plots = false;%true; % 
first = 0; % 26; %16; % 64; %  0; % to process a single file, enter the file number
%nFiles = 0;
last = 0; % inf;
threshold_flag = 0;
max_gap_for_deletion = 2; % seconds,
% gaps between custard and slow custard events less than this value will
% lead to deletion of the custard event (removal of slow from custard)
%%
% CHOOSE THE CUSTARD DATA SET(s)
field_names = { "pks", "locs", "w", "p" }; % parameters to store
data_sources = { "pensub", "ROI" };
threshold_methods = { "SD", "diffSD", "MLspike_sigmaest", "CaImAn_Sn" };
threshold_factors = [2,3,4,5];
n_ds = length(data_sources);
n_tm = length(threshold_methods);
n_tf = length(threshold_factors);
n_runs = n_ds * n_tm * n_tf;
% CHOOSE!
i_ds = 2; % ROI
i_tm = 3; % sigmaest
i_tf = 1; %2; % 3

custard_data = append( ...
    data_sources{i_ds}, "_", ...
    threshold_methods{i_tm}, "_", ...
    num2str(threshold_factors(i_tf)));

% CHOOSE THE SLOW DATA SET(s)
event_type = 'slow';
analysis_type = "custard";
slow_methods = { "slow_prctFlt", "slow_movmin", "slow_filtfilt" };
% CHOOSE!
i_sm = 3; % note i_tf is the same as fast
%
i_slow_run = 4; % this should not be hard coded!
% this is only used in the plot routine

slow_custard_data = append( data_sources{i_ds}, "_", ...
    slow_methods{i_sm}, "_", ...
    num2str( threshold_factors(i_tf) ) );

%% LOOP THROUGH THE FILES
if first ~= 0
    if isinf(last)
        last=nFiles;
    else
        last=first;
    end
else
    first=1;
    last=nFiles;
end
f = uifigure;
d = uiprogressdlg( f, 'Title', 'batch file progress');
fileCount = 0;

targetField='S_event';
for iFile=first:last
    d.Value = (iFile-1) / last;
    if load_matfile
        m = matfile( fullfile( files(iFile).folder, files(iFile).name ), 'Writable', true );
        mvars = who(m);
        if ismember( targetField, mvars ) % S_event... make sure mat file has event detection!
            S_event = m.S_event;
            name = m.name;
            ROIdata = m.ROIdata;
            timing = m.timing;
        else
            disp("no event detection in matfile")
            return
        end
    end

    fileCount = fileCount+1;
    d.Message = ['processing ',name];

    %% set up the runs
    n_runs = length( threshold_factors );
    CMS = struct([]);
    custard_minus_slow_template = struct;
    for i_f=1:length(field_names)
        custard_minus_slow_template.(field_names{i_f}) = nan(1,1);
    end
    custard_minus_slow_template.min_gaps = NaN;
    custard_minus_slow_template.minIdx = NaN;

    for i_run=1:n_runs

        custard_data = append( ...
            data_sources{i_ds}, "_", ...
            threshold_methods{i_tm}, "_", ...
            num2str(threshold_factors(i_run)));

        slow_custard_data = append( data_sources{i_ds}, "_", ...
            slow_methods{i_sm}, "_", ...
            num2str( threshold_factors(i_run) ) );

        % gather detection results
        custard = S_event.fast.custard.(custard_data);
        n_ROIs = length(custard);
        total_widths_removed=0;
        for i_ROI=1:n_ROIs
            % check that there are no short custard events, width bug 20260420
            locs = custard(i_ROI).locs;
            pks = custard(i_ROI).pks;
            widths = custard(i_ROI).w;
            p = custard(i_ROI).p;
            wxPk = custard(i_ROI).wxPk;

            %
            % threshold the data if available
            if threshold_flag
                if isfield(thresholds,"amplitude") && ~isempty(thresholds.amplitude)
                    big_indices = find( amps>=thresholds.amplitude );
                    if ~isempty(big_indices)
                        locs = locs(big_indices);
                        pks = pks(big_indices);
                        widths = widths(big_indices);
                        p = p(big_indices);
                        wxPk = wxPk(big_indices,:);
                    else
                        locs=[];
                        amps=[];
                        widths=[];
                        p=[];wxPk=[];
                    end
                    %disp(append(num2str(iROI),' thresholded amplitude: ',num2str(n_before),' after: ',num2str(length(locs))))
                else

                end % if there's an amplitude threshold
                n_before2 = length(locs);
                if isfield(thresholds,"width") && ~isempty(thresholds.width)
                    big_indices = find( widths>=thresholds.width );
                    if ~isempty(big_indices)
                        locs = locs(big_indices);
                        pks = pks(big_indices);
                        widths = widths(big_indices);
                        p = p(big_indices);
                        wxPk = wxPk(big_indices,:);
                    else
                        locs=[];
                        amps=[];
                        widths=[];
                        p=[];wxPk=[];
                    end
                    n_locs = length(locs);
                    n_removed = n_before2 - n_locs;
                    total_widths_removed = total_widths_removed + n_removed;
                    if n_removed>0
                        disp( append( ...
                            num2str(i_ROI), ...
                            ' thresholded width: ',num2str(n_before2), ...
                            ' after: ',num2str(n_locs), ...
                            ' removed: ',num2str(n_removed) ) )
                    end
                else

                end % if there's an width threshold
            end % if we're thresholding data
            custard(i_ROI).locs = locs;
            custard(i_ROI).pks = pks;
            custard(i_ROI).w = widths;
            custard(i_ROI).p = p;
            custard(i_ROI).wxPk = wxPk;
        end
        S_event.fast.custard.(custard_data)=custard;
        %m.S_event = S_event;
        %disp('updated S_event!')
        %
        %
        slow = S_event.slow.(slow_custard_data);

        n_ROIs2 = length(slow);
        if n_ROIs ~= n_ROIs2
            disp("big problems! nROIs don't match!")
            return
        end

        % CMS custard minus slow
        custard_minus_slow = custard_minus_slow_template;
        % store the names of the data sets
        CMS(i_run).custard_data = custard_data;
        CMS(i_run).slow_custard_data = slow_custard_data;

        % subtract slow events from custard
        T = cell(n_ROIs,1);
        for i_ROI = 1:n_ROIs
            n_locs_slow = length(slow(i_ROI).locs);
            if n_locs_slow > 0
                n_locs_custard = length(custard(i_ROI).locs);
                if n_locs_custard > 0

                    locs_custard = custard(i_ROI).locs;
                    locs_slow = nan(n_locs_custard,1);
                    locs_slow(1:n_locs_slow) = slow(i_ROI).locs;
                    locs_custard_copy = locs_custard;

                    % find the closests!
                    % stolen from Adam Danz https://www.mathworks.com/matlabcentral/answers/637835-finding-elements-of-one-vector-that-are-closest-to-elements-of-another
                    % locs_slow should be smaller

                    % all the same length so data set can be exported as table
                    minIdx = nan(n_locs_custard,1);
                    min_gaps = nan(n_locs_custard,1);
                    locs_slow_copy = locs_slow(~isnan(locs_slow));
                    for i_loc = 1:n_locs_slow
                        diffs = abs( locs_slow_copy(:) - locs_custard_copy(:).');
                        [ min_gaps(i_loc), minIdx(i_loc) ] = min(diffs(i_loc,:));
                        if min_gaps(i_loc) < max_gap_for_deletion
                            locs_custard_copy(minIdx(i_loc)) = NaN;
                        end
                        if all(isnan(locs_custard_copy))
                            break
                        end
                    end
                    % Confirm at all values are unique
                    %assert(numel(unique(minIdx))==numel(minIdx), 'Index values are not unique.')

                    % Show pairs
                    min_indices = minIdx(~isnan(minIdx));
                    % T{i_ROI} = table(locs_slow(:), locs_custard(min_indices)', locs_slow(:)-locs_custard(min_indices)', min_gaps(:), minIdx(:), ...
                    %     'VariableNames', {'slow','x_custard','diff','abs(diff)','xMinIndex'});

                    % create a new database

                    locs_fast=nan(n_locs_slow,1);
                    pks_fast=nan(n_locs_slow,1);
                    custard_minus_slow = struct;
                    for i_f=1:length(field_names)
                        custard_minus_slow.(field_names{i_f}) = nan(n_locs_custard,1);
                    end
                    custard_minus_slow.min_gaps = min_gaps;
                    custard_minus_slow.minIdx = minIdx;

                    i_fast=0;
                    for i_loc = 1:length(locs_custard)
                        kill_idx = find( minIdx == i_loc );
                        kill=false; % default is do not include event
                        if ~isempty(kill_idx) && any(min_gaps(kill_idx) < max_gap_for_deletion)
                            kill = true; % the gap is ok, killed custard event!
                        end
                        if ~kill % if not kill, store in new database
                            i_fast=i_fast+1;
                            locs_fast(i_fast)=locs_custard(i_loc);
                            pks_fast(i_fast)=custard(i_ROI).pks(i_loc);
                            for i_f=1:length(field_names)
                                custard_minus_slow.(field_names{i_f})(i_fast)=custard(i_ROI).(field_names{i_f})(i_loc);
                            end
                        else
                            % disp(append("subtracted ",num2str(i_loc), "gap: "))
                        end
                    end

                    CMS(i_run).ROI(i_ROI) = custard_minus_slow;

                    if do_plots
                        figure( "Name", append( name, "_", num2str(i_ROI) ) )
                        plot( timing, ROIdata(i_ROI).dFoF0.(data_sources{i_ds}), 'k', 'DisplayName', data_sources{i_ds} );
                        hold on
                        plot( timing, squeeze(S_event.slow.data(i_slow_run,i_ROI,:)), 'r', 'DisplayName', 'slow' )
                        plot( locs_custard, custard(i_ROI).pks, 'bo', 'MarkerFacecolor', 'b', 'DisplayName', 'custard detection' )
                        plot( locs_slow(1:length(slow(i_ROI).pks)), slow(i_ROI).pks, 'go', 'MarkerFacecolor','g','DisplayName', 'slow detection' )
                        plot( locs_fast,pks_fast, 'co', 'MarkerFacecolor', 'c', 'DisplayName', 'fast' )
                        legend
                    end

                else
                    CMS(i_run).ROI(i_ROI) = custard_minus_slow_template;
                    disp(append('i_run: ', num2str(i_run),...
                        ' i_ROI: ', num2str(i_ROI), ...
                        ' n_locs_custard: ',num2str(n_locs_custard)))
                end
            else
                CMS(i_run).ROI(i_ROI) = custard_minus_slow_template;
                disp(append('i_run: ', num2str(i_run),...
                    ' i_ROI: ', num2str(i_ROI), ...
                    ' n_locs_slow: ',num2str(n_locs_slow)))
            end

        end % loop over i_ROI
    end % loop over runs
    %%
    if load_matfile
        m.CMS = CMS; % write to the matfile, separate from S_event
    end
end % loop over iFile
close(d)
close(f)
