function Smoco = td_moco2( Yraw, bound, grid_size, maxShift, binWidth, rigid, n_iter, shifts_method, saveY )
% modified from normCorr demo.m
% | Parameter name | Description |
% |----------------|-------------|
% | ```d1,d2,d3``` | dimensions of field of view |
% | ```grid_size``` | size of non-overlapping portion of each patch the grid in each direction (x-y-z)|
% | ```overlap_pre```| size of overlapping region in each direction before upsampling  |
% | ```mot_uf```    | upsampling factor for smoothing and refinement of motion field |
% | ```overlap_post ``` | size of overlapping region in each direction after upsampling |
% | ```max_shift``` | maximum allowed shift for rigid translation |
% | ```max_dev``` | maximum deviation of each patch from estimated rigid translation |
% | ```upd_template``` | update the template online after registering some frames |
% | ```bin_width``` | length of bin over which the registered frames are averaged to update the template |
% | ```init_batch``` | number of frames to be taken for computing initial template |
% | ```iter``` | number of times to go over the dataset |
% | ```output_type``` | type of output registered file |
% | ```phase_flag``` | flag for using phase correlation |
% | ```correct_bidir``` | check for offset due to bidirectional scanning (default: true) |

% creates a shit ton of variables, clean up?
% save the input settings
settings.bound=bound;
settings.grid_size=grid_size;
settings.maxShift=maxShift;
settings.binWidth=binWidth;
settings.rigid=rigid;
settings.iter=n_iter;
settings.saveY=saveY;
settings.shifts_method=shifts_method; %      ' % method to apply shifts ('FFT','cubic','linear')

settings.version='20250805'; % todays version, yesterdays milk left on the counter overnight

[ d1, d2, T ] = size( Yraw );
Y = prepStack4SingleGauss(Yraw);
% removes baseline, normalizes

disp('moco: imfilter complete')
%bound = 0;
if rigid == 1
    % MOTION CORRECTION PARAMS: first try out rigid motion correction
    % exclude boundaries due to high pass filtering effects
    options = NoRMCorreSetParms( ...
        'd1',d1-bound, ...
        'd2',d2-bound, ...
        'bin_width',binWidth, ...       % | ```bin_width```     | length of bin over which the registered frames are averaged to update the template |
        'max_shift', maxShift, ...      % | ```max_shift```     | maximum allowed shift for rigid translation |
        'iter', n_iter, ...                  % | ```iter```          | number of times to go over the dataset |
        'init_batch', binWidth, ...     % | ```init_batch``` | number of frames to be taken for computing initial template |
        'correct_bidir',false);         % | ```correct_bidir``` | check for offset due to bidirectional scanning (default: true) |

    % MOTION CORRECTION: register using the high pass filtered data and apply shifts to original data
    try
        tic; [ M1, shifts, template ] = normcorre_batch( Y( bound/2+1:end-bound/2, bound/2+1:end-bound/2, : ), options ); toc % register filtered data
        [ cM1, mM1, vM1 ] = motion_metrics( M1, options.max_shift );
        clear M1
    catch
        disp('CAUGHT THE ERROR! td_moco2 - rigid - Smoco returned empty!')
        Smoco.template = []; % template;
        Smoco.options = []; % options;
        Smoco.shifts = []; % shifts;
        Smoco.T = []; % T;
        Smoco.cY = []; % cY;
        Smoco.cM1 = []; % cM1;
        Smoco.cMr = []; % cMr;
        Smoco.cYraw = []; % cYraw;

        if saveY==1 % requires massive memory!
            Smoco.Y = []; % Y;
            %S(i).normStack = Ynorm;
        end

        Smoco.stack = []; % Mr;
        return
    end
else
    % now try non-rigid motion correction (also in parallel)
    options = NoRMCorreSetParms( ...
        'd1',size(Y,1), ...
        'd2',size(Y,2), ...
        'grid_size', [ grid_size, grid_size ], ...        % | ```grid_size``` | size of non-overlapping portion of each patch the grid in each direction (x-y-z)|
        'mot_uf', 4, ...                % | ```mot_uf```    | upsampling factor for smoothing and refinement of motion field |
        'bin_width', binWidth, ...      % | ```bin_width``` | length of bin over which the registered frames are averaged to update the template |
        'max_shift', maxShift, ...      % | ```max_shift``` | maximum allowed shift for rigid translation |
        'max_dev', 3, ...               % | ```max_dev```   | maximum deviation of each patch from estimated rigid translation |
        'us_fac', 50, ...
        'iter', n_iter, ...
        'init_batch', binWidth, ...     % | ```init_batch``` | number of frames to be taken for computing initial template |
        'correct_bidir',false ...       % | ```correct_bidir``` | check for offset due to bidirectional scanning (default: true) |
        );
    try
        tic; [ M1, shifts, template, options ] = normcorre_batch( Y, options ); toc
    catch 
        Smoco.options = options;
        disp('normcorre_batch: caught the error!')
        return
    end
    try
        [ cM1, mM1, vM1 ] = motion_metrics( M1, options.max_shift );
        clear M1
    catch 
        disp('caught the error!')
    end

end

% exclude boundaries due to high pass filtering effects

% SHOW YOUR WORK: compute metrics
[ cY, mY, vY ] = motion_metrics( Y( bound/2+1:end-bound/2, bound/2+1:end-bound/2, : ), options.max_shift );
if saveY==1 % requires massive memory!
    Smoco.Y = Y;
end
clear Y

% apply shifts on the whole movie
tic; Mr = apply_shifts( Yraw, shifts, options, bound/2, bound/2 ); toc % apply shifts to full dataset
[ cYraw, mYraw, vYraw ] = motion_metrics( Yraw, options.max_shift );
clear Yraw



[ cMr, mMr, vMr ] = motion_metrics( Mr, options.max_shift );
Smoco.stack = Mr;
clear Mr

% return rigid shifts and metrics

Smoco.template = template;
Smoco.options = options;
Smoco.shifts = squeeze( cat( 3, shifts(:).shifts ) );
Smoco.T = T;
Smoco.cY = cY;
Smoco.cM1 = cM1;
Smoco.cMr = cMr;
Smoco.cYraw = cYraw;
Smoco.settings = settings;

end