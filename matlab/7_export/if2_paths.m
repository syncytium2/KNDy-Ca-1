function p = if2_paths(version)
%if2_paths  Canonical paths for the interface2 repo. Cross-platform.
%
%   p = if2_paths(version) returns a struct with fields:
%     .home       — user home dir
%     .data       — IF2_DATA_ROOT, or ~/University of Michigan Dropbox/Richard DeFazio/data
%     .sts        — <data>/STS/STS_<version>              (RateViewer input)
%     .aps        — <data>/APs                            (APViewer input)
%     .xls        — <data>/indiegroups_db4.xlsx           (slice metadata)
%     .archive    — <data>/processed_archive              (locked read-only batch output)
%     .pdfOut     — <data>/rateviewer output              (RateViewer output)
%     .apsOut     — <data>/aps_recon                      (APViewer output)
%     .csvOut     — IF2_CSVOUT_ROOT/04-data, or ~/University of Michigan Dropbox/Richard DeFazio/lme-data/04-data
%     .version    — version string, e.g. '4v2'
%     .filePrefix — e.g. 'STS_4v2_'
%
%   Both data roots are configurable via env vars (useful for CI or
%   non-standard installs):
%     IF2_DATA_ROOT    — overrides the default Dropbox data path
%     IF2_CSVOUT_ROOT  — overrides the default Dropbox lme-data path
%
%   Single source of truth for all interface2 scripts. If a path
%   moves or the OS conventions change, edit HERE only — no caller
%   should ever reach for getenv('HOME') or getenv('USERPROFILE')
%   directly.
%
%   Usage:
%     p = if2_paths();          % defaults to '4v2'
%     p = if2_paths('4v3');
%     dataDir = p.sts;
%     xlsPath = p.xls;

    if nargin < 1 || isempty(version); version = '4v2'; end

    if ispc
        p.home = getenv('USERPROFILE');
    else
        p.home = getenv('HOME');
    end
    % Data root: configurable via IF2_DATA_ROOT env var.
    % Defaults to U-M Dropbox (Dropbox folder name has spaces — works on
    % both Windows and Mac since getenv USERPROFILE/HOME resolves correctly).
    dataRoot = getenv('IF2_DATA_ROOT');
    if isempty(dataRoot)
        dataRoot = fullfile(p.home, 'University of Michigan Dropbox', ...
                            'Richard DeFazio', 'data');
    end
    p.data = dataRoot;
    p.sts        = fullfile(p.data, 'STS', sprintf('STS_%s', version));
    p.version    = version;
    p.filePrefix = sprintf('STS_%s_', version);
    p.aps    = fullfile(p.data, 'APs');
    p.xls     = fullfile(p.data, 'indiegroups_db4.xlsx');
    p.archive       = fullfile(p.data, 'processed_archive');
    p.event_archive = fullfile(p.data, 'processed_archive', 'event_archive');
    p.pdfOut  = fullfile(p.data, 'rateviewer output');
    p.apsOut = fullfile(p.data, 'aps_recon');
    csvOutRoot = getenv('IF2_CSVOUT_ROOT');
    if isempty(csvOutRoot)
        csvOutRoot = fullfile(p.home, 'University of Michigan Dropbox', ...
                              'Richard DeFazio', 'lme-data');
    end
    p.csvOut = fullfile(csvOutRoot, '04-data');
end
