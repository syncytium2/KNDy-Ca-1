% v4 trims duration to ~20 minutes,
%  backwards from treatment time
%  forwards after treatment delay
% production straight to table with all four data sets, hardcoded to _4
% 20260406 updated to export filtfilt in addition to prctfilt and movmin

version_suffix = "4v4";  % bumping this is the single point of change for the next version
p = if2_paths();

% make the ROI summary table
varNames={'run_id','group_idx','group_id','mouse_id','slice_idx','slice_id','ROI','treatment','treatment_idx','amp_median','width_median','freq','n_events','duration'};
varTypes={'string','int8',     'string',  'int8',    'int8',     'string', 'int8','string',   'int8',         'double',    'double',      'double','int16','double'};
tbl = table('Size',[1,length(varNames)],'VariableTypes',varTypes,'VariableNames',varNames);
set(0,'DefaultFigureWindowStyle','docked');
% must run detection in batch mode (not event confirmation app)
% creates the array of region data tables from S_event

path = p.event_archive;
%path="C:\Users\defazio\Documents\data\processed\individual";
files = dir(fullfile(path,'*.mat'));
nFiles=length(files);
%%
first = 0; % 26; %16; % 64; %  0; % to process a single file, enter the file number
run_mode = "";  % MUST SET: "long_window_20" or "short_window_5"
switch run_mode
    case "long_window_20"
        baseline_window_max = 20;
        baseline_slice_min  = 12;
        treatment_window    = 20;
        solution_delay      = 2;
        slice_filter        = "none";
        suppress_hik        = false;
    case "short_window_5"
        baseline_window_max = 5;
        baseline_slice_min  = 12;
        treatment_window    = 5;
        solution_delay      = 2;
        slice_filter        = "senktide";
        suppress_hik        = true;
    otherwise
        error('aCa5z: set run_mode to "long_window_20" or "short_window_5"');
end

treatments = {'senktide', 'TTX' }; % case SHOULD not matter
%treatment_focus = "TTX"; % "senktide"; % "sb222200"; % just match partial to avoid typos
groups = {'DI','OVX','MALE','ORX'}; % DEFINED, should not be hardcoded but out of time
analytes = {'frequency','amplitude','width'};

%% get the group info from excel indiegroups file
% indiegroups table contains experimental group information
% direct read from excel
%fn="C:\Users\defazio\Documents\data\indiegroups_db.xlsx";

fn = p.xls;
opt=detectImportOptions(fn);
opt.DataRange='A2';
opt.VariableNamesRange='A1';
shts=sheetnames(fn);
indiegroups_sheet_name = "indiegroups";
mlspike_fail_tbl_name = "fail_tbl";

% get the sheet with the experiment metadata
indie_index = find(strcmp(indiegroups_sheet_name,shts)); % i=1; % assumes relevant sheet is the first one!
opt = detectImportOptions(fn,'Sheet',indie_index);
opt.DataRange = 'A2';
opt.VariableNamesRange = 'A1';
indiegroups = readtable( fn, opt, 'Sheet', shts(indie_index) );

dategroup_varname = "date";
experiment_id_varname = "experiment_id";
treat_varname = "treat";
exclude_varname = "exclude";
varname_check = {dategroup_varname,experiment_id_varname,treat_varname,exclude_varname};
nChecks = length(varname_check);
varname_index = nan(1,nChecks);
% match the relavant column names
indie_varnames = indiegroups.Properties.VariableNames;
for ii=1:nChecks
    index = find(strcmp(varname_check{ii},indie_varnames));
    if index
        varname_index(ii)=index;
    else
        disp(append('failed to find',varname_check{ii},' !'))
        return
    end
end

%% CMS data base: four rows, representing four runs
% row 3 is sigmaest_4 minus slow_4
FAST_run_index = 3;
data_sources = {"ROI", "pensub"};
%% MASTER LOOP

if first ~= 0
    nFiles = 1;
    last=first;
else
    first=1;
    last=nFiles;
end
maxSpikes=10000;

targetField='S_event';

expr = '\d{8}[a-z]?_?\d*'; % full date code
expr2 = '\d{8}\d*'; % first 8 char

for i_ds = 1:numel(data_sources)
ds = data_sources{i_ds};
SLOW_run    = append(ds, "_slow_filtfilt_4");
MLspike_run = append("MLspike_prctfilt_", ds);
custard_run = append(ds, "_MLspike_sigmaest_4");

FAST_tbl = tbl; SLOW_tbl = tbl; MLspike_tbl = tbl; custard_tbl = tbl;
FAST_idx = 0; SLOW_idx = 0; MLspike_idx = 0; custard_idx = 0;

f = uifigure;
d = uiprogressdlg(f, 'Title', append('batch file progress — ', char(ds)));

fileCount = 0;

%%
slice_idx = 0;
for iFile=first:last
    d.Value = (iFile-1) / last;
    %m = matfile( fullfile( files(iFile).folder, files(iFile).name ), 'Writable', true );
    clear S_event CMS timing name
    S = load(fullfile(files(iFile).folder, files(iFile).name), ...
             'S_event', 'CMS', 'timing', 'name');
    S_event = S.S_event;
    CMS     = S.CMS;
    timing  = S.timing;
    name    = S.name;
    clear S
    %mvars = who(m);
    %if ismember( targetField, mvars ) % S_event... make sure mat file has event detection!
    %experiment = m.exp;
    %slice_id = m.name;
    experiment = if2_load_exp_timing(name);
    slice_id = name;
    % check for input errors between indiegroups table and matfile
    idx = find( strcmpi( indiegroups.experiment_id, slice_id ), 1, 'first' );
    if isempty(idx)
        disp(append('mismatch experiment_id: ',slice_id))
        return
    end
    excluded = indiegroups.exclude(idx);

    if excluded~=1

        fileCount = fileCount+1;
        d.Message = ['processing ',slice_id];
        % find the matching data in indiegroups

        % process the experiment to get the timing
        maxExp = length( experiment );
        iRegion=0;
        short_flag=false;
        for ii=1:maxExp
            if ~isempty(experiment(ii).start_end)
                iRegion=iRegion+1;
                if iRegion==1
                    exp_delay=0;
                else
                    exp_delay=solution_delay;
                end
                if isinf(experiment(ii).start_end(2))
                    exp_end = max(timing)/60;
                else
                    exp_end = experiment(ii).start_end(2);
                end
            end
        end
        maxRegion = iRegion;
        if maxRegion >= 1
            has_senktide = maxRegion >= 2 && startsWith(strtrim(string(experiment(2).name)), "se", 'IgnoreCase', true);
            if experiment(1).start_end(2) < baseline_slice_min
                disp(append("EXCLUDED: ", slice_id, " baseline too short (", ...
                    num2str(experiment(1).start_end(2)), " min)"))
                continue
            end
            switch slice_filter
                case "senktide"
                    if ~has_senktide
                        if maxRegion >= 2
                            treat1_name = strtrim(string(experiment(2).name));
                        else
                            treat1_name = "<none>";
                        end
                        disp(append("EXCLUDED: ", slice_id, " treat1 is ", treat1_name, ", not senktide"))
                        continue
                    end
            end
            % load the data for export
            % disp('loading...')
            % S_event = m.S_event;
            % CMS = m.CMS;
            % timing = m.timing;
            % name = m.name;

            %% get the indiegroups data
            slice_id = name;
            slice_idx = slice_idx + 1;
            idx = find( strcmpi( indiegroups.experiment_id, slice_id ), 1, 'first' );
            if isempty(idx)
                disp(append('mismatch experiment_id: ',mouse_id))
                pause
                return
            end
            % pull the unique mouse_id number from indiegroups excel file
            mouse_id = num2str(indiegroups.mouse_id(idx)); % arr{mouse_id_row,data_col};
            group_id = string( indiegroups.group_id{idx} );
            group_idx = find( contains( groups, group_id ), 1);

            disp(append('loaded: ',num2str(iFile),' fn: ',slice_id, ' name: ', mouse_id))
            % gather the four sets of data

            custard_data = S_event.fast.custard.(custard_run);
            SLOW_data = S_event.slow.(SLOW_run);
            MLspike_data =  S_event.fast.MLspike.(MLspike_run);
            FAST_data = CMS(FAST_run_index).(char(ds));
            % make the name of the FAST run
            FAST_run = append( ...
                CMS(FAST_run_index).(append(char(ds), '_custard_data')), ...
                '_minus_', ...
                CMS(FAST_run_index).(append(char(ds), '_slow_custard_data')));

            % set up the times
            baseline_dur = experiment(1).start_end(2) - experiment(1).start_end(1);
            if baseline_dur < baseline_window_max
                baseline_dur = experiment(1).start_end(2);
            else
                baseline_dur = baseline_window_max;
            end
            baseline_start = (experiment(1).start_end(2) - baseline_dur) * 60;
            baseline_end = experiment(1).start_end(2) * 60; % locs in seconds, exp in minutes
            baseline_dur = baseline_end - baseline_start;

            tw = repmat(struct('t_start', 0, 't_end', 0, 't_dur', 0, 'skip', true), 1, maxRegion);
            for ii = 2:maxRegion
                t_name    = strtrim(string(experiment(ii).name));
                is_hik    = contains(t_name, "hi", 'IgnoreCase', true);
                t_end_raw = experiment(ii).start_end(2);
                if isinf(t_end_raw), t_end_raw = max(timing)/60; end
                if is_hik && ~suppress_hik
                    ts = experiment(ii).start_end(1) * 60;
                    te = t_end_raw * 60;
                    tw(ii).t_start = ts;  tw(ii).t_end = te;
                    tw(ii).t_dur   = te - ts;  tw(ii).skip = false;
                elseif ~is_hik
                    ts_min = experiment(ii).start_end(1) + solution_delay;
                    te_min = t_end_raw;
                    if (te_min - ts_min) > treatment_window
                        te_min = ts_min + treatment_window;
                    end
                    ts = ts_min * 60;  te = te_min * 60;
                    tw(ii).t_start = ts;  tw(ii).t_end = te;
                    tw(ii).t_dur   = te - ts;  tw(ii).skip = false;
                end
            end

            %%
            nROIs = length(FAST_data); % same for all!
            freq = nan(nROIs,1);
            amp = nan(nROIs,1);
            width = nan(nROIs,1);

            for iROI = 1:nROIs
                % CMS FAST
                locs = FAST_data(iROI).locs;
                baseline_indices = find( locs > baseline_start & locs < baseline_end );
                baseline_n_events = length(baseline_indices);
                if baseline_n_events>0
                    baseline_frequency = length( baseline_indices ) / baseline_dur; % total freq
                    baseline_amp = median( FAST_data(iROI).p( baseline_indices ) );
                    baseline_width = median( FAST_data(iROI).w( baseline_indices ) );
                else
                    baseline_frequency = 0;
                    baseline_amp = nan;
                    baseline_width = nan;
                end
                %
                FAST_idx = FAST_idx + 1;
                FAST_tbl.run_id(FAST_idx) = FAST_run;
                FAST_tbl.group_idx(FAST_idx) = group_idx;
                FAST_tbl.group_id(FAST_idx) = group_id;
                FAST_tbl.mouse_id(FAST_idx) = str2num(mouse_id);
                FAST_tbl.slice_id(FAST_idx) = slice_id;
                FAST_tbl.slice_idx(FAST_idx) = slice_idx;
                FAST_tbl.ROI(FAST_idx) = iROI;
                FAST_tbl.treatment(FAST_idx) = "baseline";
                FAST_tbl.treatment_idx(FAST_idx) = 1;
                FAST_tbl.amp_median(FAST_idx) = baseline_amp;
                FAST_tbl.width_median(FAST_idx) = baseline_width;
                FAST_tbl.freq(FAST_idx) = baseline_frequency;
                FAST_tbl.n_events(FAST_idx) = baseline_n_events;
                FAST_tbl.duration(FAST_idx) = baseline_dur;
                %FAST_tbl.interval_median(FAST_idx) = nan;
                %
                for ii = 2:maxRegion
                    if tw(ii).skip, continue; end
                    t_indices = find(locs > tw(ii).t_start & locs < tw(ii).t_end);
                    t_n = length(t_indices);
                    if t_n > 0
                        t_freq  = t_n / tw(ii).t_dur;
                        t_amp   = median(FAST_data(iROI).p(t_indices));
                        t_width = median(FAST_data(iROI).w(t_indices));
                    else
                        t_freq = 0; t_amp = nan; t_width = nan;
                    end
                    FAST_idx = FAST_idx + 1;
                    FAST_tbl.run_id(FAST_idx)        = FAST_run;
                    FAST_tbl.group_idx(FAST_idx)     = group_idx;
                    FAST_tbl.group_id(FAST_idx)      = group_id;
                    FAST_tbl.mouse_id(FAST_idx)      = str2num(mouse_id);
                    FAST_tbl.slice_id(FAST_idx)      = slice_id;
                    FAST_tbl.slice_idx(FAST_idx)     = slice_idx;
                    FAST_tbl.ROI(FAST_idx)           = iROI;
                    FAST_tbl.treatment(FAST_idx)     = experiment(ii).name;
                    FAST_tbl.treatment_idx(FAST_idx) = ii;
                    FAST_tbl.amp_median(FAST_idx)    = t_amp;
                    FAST_tbl.width_median(FAST_idx)  = t_width;
                    FAST_tbl.freq(FAST_idx)          = t_freq;
                    FAST_tbl.n_events(FAST_idx)      = t_n;
                    FAST_tbl.duration(FAST_idx)      = tw(ii).t_dur;
                end

                % SLOW
                locs = SLOW_data(iROI).locs;
                baseline_indices = find( locs > baseline_start & locs < baseline_end );
                baseline_n_events = length(baseline_indices);
                if baseline_n_events>0
                    baseline_frequency = length( baseline_indices ) / baseline_dur; % total freq
                    baseline_amp = median( SLOW_data(iROI).p( baseline_indices ) );
                    baseline_width = median( SLOW_data(iROI).w( baseline_indices ) );
                else
                    baseline_frequency = 0;
                    baseline_amp = nan;
                    baseline_width = nan;
                end
                % slow
                SLOW_idx = SLOW_idx + 1;
                SLOW_tbl.run_id(SLOW_idx) = SLOW_run;
                SLOW_tbl.group_idx(SLOW_idx) = group_idx;
                SLOW_tbl.group_id(SLOW_idx) = group_id;
                SLOW_tbl.mouse_id(SLOW_idx) = str2num(mouse_id);
                SLOW_tbl.slice_id(SLOW_idx) = slice_id;
                SLOW_tbl.slice_idx(SLOW_idx) = slice_idx;
                SLOW_tbl.ROI(SLOW_idx) = iROI;
                SLOW_tbl.treatment(SLOW_idx) = "baseline";
                SLOW_tbl.treatment_idx(SLOW_idx) = 1;
                SLOW_tbl.amp_median(SLOW_idx) = baseline_amp;
                SLOW_tbl.width_median(SLOW_idx) = baseline_width;
                SLOW_tbl.freq(SLOW_idx) = baseline_frequency;
                SLOW_tbl.n_events(SLOW_idx) = baseline_n_events;
                SLOW_tbl.duration(SLOW_idx) = baseline_dur;
                %SLOW_tbl.interval_median(SLOW_idx) = nan;
                %
                for ii = 2:maxRegion
                    if tw(ii).skip, continue; end
                    t_indices = find(locs > tw(ii).t_start & locs < tw(ii).t_end);
                    t_n = length(t_indices);
                    if t_n > 0
                        t_freq  = t_n / tw(ii).t_dur;
                        t_amp   = median(SLOW_data(iROI).p(t_indices));
                        t_width = median(SLOW_data(iROI).w(t_indices));
                    else
                        t_freq = 0; t_amp = nan; t_width = nan;
                    end
                    SLOW_idx = SLOW_idx + 1;
                    SLOW_tbl.run_id(SLOW_idx)        = SLOW_run;
                    SLOW_tbl.group_idx(SLOW_idx)     = group_idx;
                    SLOW_tbl.group_id(SLOW_idx)      = group_id;
                    SLOW_tbl.mouse_id(SLOW_idx)      = str2num(mouse_id);
                    SLOW_tbl.slice_id(SLOW_idx)      = slice_id;
                    SLOW_tbl.slice_idx(SLOW_idx)     = slice_idx;
                    SLOW_tbl.ROI(SLOW_idx)           = iROI;
                    SLOW_tbl.treatment(SLOW_idx)     = experiment(ii).name;
                    SLOW_tbl.treatment_idx(SLOW_idx) = ii;
                    SLOW_tbl.amp_median(SLOW_idx)    = t_amp;
                    SLOW_tbl.width_median(SLOW_idx)  = t_width;
                    SLOW_tbl.freq(SLOW_idx)          = t_freq;
                    SLOW_tbl.n_events(SLOW_idx)      = t_n;
                    SLOW_tbl.duration(SLOW_idx)      = tw(ii).t_dur;
                end

                % MLspike
                locs = MLspike_data(iROI).locs;
                baseline_indices = find( locs > baseline_start & locs < baseline_end );
                baseline_n_events = length(baseline_indices);
                if baseline_n_events>0
                    baseline_frequency = length( baseline_indices ) / baseline_dur; % total freq
                    baseline_amp = median( MLspike_data(iROI).p( baseline_indices ) );
                    baseline_width = median( MLspike_data(iROI).w( baseline_indices ) );
                else
                    baseline_frequency = 0;
                    baseline_amp = nan;
                    baseline_width = nan;
                end
                MLspike_idx = MLspike_idx + 1;
                MLspike_tbl.run_id(MLspike_idx) = MLspike_run;
                MLspike_tbl.group_idx(MLspike_idx) = group_idx;
                MLspike_tbl.group_id(MLspike_idx) = group_id;
                MLspike_tbl.mouse_id(MLspike_idx) = str2num(mouse_id);
                MLspike_tbl.slice_id(MLspike_idx) = slice_id;
                MLspike_tbl.slice_idx(MLspike_idx) = slice_idx;
                MLspike_tbl.ROI(MLspike_idx) = iROI;
                MLspike_tbl.treatment(MLspike_idx) = "baseline";
                MLspike_tbl.treatment_idx(MLspike_idx) = 1;
                MLspike_tbl.amp_median(MLspike_idx) = baseline_amp;
                MLspike_tbl.width_median(MLspike_idx) = baseline_width;
                MLspike_tbl.freq(MLspike_idx) = baseline_frequency;
                MLspike_tbl.n_events(MLspike_idx) = baseline_n_events;
                MLspike_tbl.duration(MLspike_idx) = baseline_dur;
                %MLspike_tbl.interval_median(MLspike_idx) = nan;
                %
                for ii = 2:maxRegion
                    if tw(ii).skip, continue; end
                    t_indices = find(locs > tw(ii).t_start & locs < tw(ii).t_end);
                    t_n = length(t_indices);
                    if t_n > 0
                        t_freq  = t_n / tw(ii).t_dur;
                        t_amp   = median(MLspike_data(iROI).p(t_indices));
                        t_width = median(MLspike_data(iROI).w(t_indices));
                    else
                        t_freq = 0; t_amp = nan; t_width = nan;
                    end
                    MLspike_idx = MLspike_idx + 1;
                    MLspike_tbl.run_id(MLspike_idx)        = MLspike_run;
                    MLspike_tbl.group_idx(MLspike_idx)     = group_idx;
                    MLspike_tbl.group_id(MLspike_idx)      = group_id;
                    MLspike_tbl.mouse_id(MLspike_idx)      = str2num(mouse_id);
                    MLspike_tbl.slice_id(MLspike_idx)      = slice_id;
                    MLspike_tbl.slice_idx(MLspike_idx)     = slice_idx;
                    MLspike_tbl.ROI(MLspike_idx)           = iROI;
                    MLspike_tbl.treatment(MLspike_idx)     = experiment(ii).name;
                    MLspike_tbl.treatment_idx(MLspike_idx) = ii;
                    MLspike_tbl.amp_median(MLspike_idx)    = t_amp;
                    MLspike_tbl.width_median(MLspike_idx)  = t_width;
                    MLspike_tbl.freq(MLspike_idx)          = t_freq;
                    MLspike_tbl.n_events(MLspike_idx)      = t_n;
                    MLspike_tbl.duration(MLspike_idx)      = tw(ii).t_dur;
                end
                % custard
                locs = custard_data(iROI).locs;
                baseline_indices = find( locs > baseline_start & locs < baseline_end );
                baseline_n_events = length(baseline_indices);
                if baseline_n_events>0
                    baseline_frequency = length( baseline_indices ) / baseline_dur; % total freq
                    baseline_amp = median( custard_data(iROI).p( baseline_indices ) );
                    baseline_width = median( custard_data(iROI).w( baseline_indices ) );
                else
                    baseline_frequency = 0;
                    baseline_amp = nan;
                    baseline_width = nan;
                end
                custard_idx = custard_idx + 1;
                custard_tbl.run_id(custard_idx) = custard_run;
                custard_tbl.group_idx(custard_idx) = group_idx;
                custard_tbl.group_id(custard_idx) = group_id;
                custard_tbl.mouse_id(custard_idx) = str2num(mouse_id);
                custard_tbl.slice_id(custard_idx) = slice_id;
                custard_tbl.slice_idx(custard_idx) = slice_idx;
                custard_tbl.ROI(custard_idx) = iROI;
                custard_tbl.treatment(custard_idx) = "baseline";
                custard_tbl.treatment_idx(custard_idx) = 1;
                custard_tbl.amp_median(custard_idx) = baseline_amp;
                custard_tbl.width_median(custard_idx) = baseline_width;
                custard_tbl.freq(custard_idx) = baseline_frequency;
                custard_tbl.n_events(custard_idx) = baseline_n_events;
                custard_tbl.duration(custard_idx) = baseline_dur;
                %custard_tbl.interval_median(custard_idx) = nan;
                %
                for ii = 2:maxRegion
                    if tw(ii).skip, continue; end
                    t_indices = find(locs > tw(ii).t_start & locs < tw(ii).t_end);
                    t_n = length(t_indices);
                    if t_n > 0
                        t_freq  = t_n / tw(ii).t_dur;
                        t_amp   = median(custard_data(iROI).p(t_indices));
                        t_width = median(custard_data(iROI).w(t_indices));
                    else
                        t_freq = 0; t_amp = nan; t_width = nan;
                    end
                    custard_idx = custard_idx + 1;
                    custard_tbl.run_id(custard_idx)        = custard_run;
                    custard_tbl.group_idx(custard_idx)     = group_idx;
                    custard_tbl.group_id(custard_idx)      = group_id;
                    custard_tbl.mouse_id(custard_idx)      = str2num(mouse_id);
                    custard_tbl.slice_id(custard_idx)      = slice_id;
                    custard_tbl.slice_idx(custard_idx)     = slice_idx;
                    custard_tbl.ROI(custard_idx)           = iROI;
                    custard_tbl.treatment(custard_idx)     = experiment(ii).name;
                    custard_tbl.treatment_idx(custard_idx) = ii;
                    custard_tbl.amp_median(custard_idx)    = t_amp;
                    custard_tbl.width_median(custard_idx)  = t_width;
                    custard_tbl.freq(custard_idx)          = t_freq;
                    custard_tbl.n_events(custard_idx)      = t_n;
                    custard_tbl.duration(custard_idx)      = tw(ii).t_dur;
                end
            end
        end

    else
        disp(append(slice_id, ' EXCLUDED experiment: ',slice_id)); % ,' looking for: ',treatment_focus))
    end
    % else
    %     disp(["missing event detection: ",slice_id])
    % end % if detection is present

end % loop over iFile

close(d)
close(f)
%%
% %d.Message="Saving data table...";
% save the matlab file for group data analyis
date_folder = datestr(now, 'yyyy-mm-dd');
date_stamp  = datestr(now, 'yyyymmdd');
save_path = fullfile(p.data, '2R', date_folder, append(char(run_mode), '_', char(ds)));
if ~exist(save_path, 'dir'), mkdir(save_path); end
writetable(FAST_tbl,    fullfile(save_path, append(char(run_mode), '_', char(ds), '_FAST_',    date_stamp, '.csv')))
writetable(SLOW_tbl,    fullfile(save_path, append(char(run_mode), '_', char(ds), '_SLOW_',    date_stamp, '.csv')))
writetable(MLspike_tbl, fullfile(save_path, append(char(run_mode), '_', char(ds), '_MLspike_', date_stamp, '.csv')))
writetable(custard_tbl, fullfile(save_path, append(char(run_mode), '_', char(ds), '_custard_', date_stamp, '.csv')))

end % i_ds

