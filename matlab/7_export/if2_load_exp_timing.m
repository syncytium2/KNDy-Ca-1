function exp_struct = if2_load_exp_timing(slice_id)
%if2_load_exp_timing  Load exp timing from the authoritative exp_timing spreadsheet.
%
%   exp_struct = if2_load_exp_timing(slice_id)
%
%   Reads the exp_timing sheet from indiegroups_db4.xlsx (via if2_paths),
%   finds the row matching experiment_id == slice_id, and returns an exp
%   struct array compatible with the batch script consumers (aCa99, aCa5z).
%
%   Output struct array fields (one element per region):
%     .name      — string, region name ('baseline' or drug name from sheet)
%     .start_end — [start end] in minutes
%
%   Region 1 is always baseline. Subsequent elements are treatments, in
%   order, stopping at the first NaN-timing treatment pair.
%
%   Errors (hard stop):
%     - No row found for slice_id in exp_timing
%     - baseline_start or baseline_end is NaN (row not populated)

    p = if2_paths();
    xlsPath = p.xls;

    % read sheet with explicit column types — prevents readtable from
    % inferring all-empty columns (e.g. treat4_*) as cell/char instead of double.
    % VariableNamesRange/DataRange must be passed into detectImportOptions directly
    % (not set as properties after) so that setvartype can resolve column names.
    opts = detectImportOptions(xlsPath, 'Sheet', 'exp_timing', ...
        'VariableNamesRange', 'A1', 'DataRange', 'A2');
    opts = setvartype(opts, ...
        {'baseline_start','baseline_end', ...
         'treat1_start','treat1_end', ...
         'treat2_start','treat2_end', ...
         'treat3_start','treat3_end', ...
         'treat4_start','treat4_end'}, 'double');
    opts = setvartype(opts, ...
        {'treat1_name','treat2_name','treat3_name','treat4_name'}, 'char');
    T = readtable(xlsPath, opts, 'Sheet', 'exp_timing');

    % find row for this slice
    if iscell(T.experiment_id)
        row_idx = find(strcmpi(T.experiment_id, slice_id), 1, 'first');
    else
        row_idx = find(strcmpi(string(T.experiment_id), slice_id), 1, 'first');
    end

    if isempty(row_idx)
        error('exp_timing: no row for slice %s', slice_id);
    end

    row = T(row_idx, :);

    % validate baseline is populated
    if isnan(row.baseline_start) || isnan(row.baseline_end)
        error('exp_timing: baseline timing is NaN for slice %s (row not populated)', slice_id);
    end

    % build baseline element
    exp_struct(1).name      = 'baseline';
    exp_struct(1).start_end = [row.baseline_start, row.baseline_end];

    % treatment columns, in order; stop at first fully-NaN pair
    treat_names  = {'treat1_name',  'treat2_name',  'treat3_name',  'treat4_name'};
    treat_starts = {'treat1_start', 'treat2_start', 'treat3_start', 'treat4_start'};
    treat_ends   = {'treat1_end',   'treat2_end',   'treat3_end',   'treat4_end'};

    for k = 1:4
        tstart = row.(treat_starts{k});
        tend   = row.(treat_ends{k});

        if isnan(tstart) && isnan(tend)
            break;  % no more treatments
        end

        tname_raw = row.(treat_names{k});
        if iscell(tname_raw)
            tname = tname_raw{1};
        elseif isstring(tname_raw) || ischar(tname_raw)
            tname = char(tname_raw);
        else
            tname = '';
        end

        idx = k + 1;
        exp_struct(idx).name      = tname;  %#ok<AGROW>
        exp_struct(idx).start_end = [tstart, tend];  %#ok<AGROW>
    end
end
