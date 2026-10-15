% master
% open dataset
% run openETS, extracts image stack into memory
% get two files, baseline and treatment
disp("reading ETS and VSI...")
% rbase and rtreat raw data files, nbase ntreat meaningful names
[ baseRaw, basetiming, baseName, baseETS ] = openETSui(); % puts image stack into 'stack'
[ treatRaw, treattiming, treatName, treatETS ] = openETSui(); % puts image stack into 'stack'
disp("ETS read into memory...")
%% cuts off first 10s, 100 frames with fast transient
disp("trimming started...")
% base and treat are image stacks from previous step
base = trimArray( baseRaw, 3, 1, 100 );
treat = trimArray( treatRaw, 3, 1, 100 );
disp("trimming completed...")

%% clean stack
disp('cleaning ')
% oBase is base outliers, pmBase is projectMaxBase
[ baseOutliers, baseProjMax ] = findOutliersMAX( base, 9 ); % 4.5 x std threshold
%%
base = replaceOutliers( base, baseOutliers );
%%
[ treatOutliers, treatProjMax ] = findOutliersMAX( treat, 9 ); % 4.5 x std threshold
%%
treat = replaceOutliers( treat, treatOutliers );
disp("cleaning completed")

%% OPTIONAL gaussian spatial smoothing (xy plane filter for each frame)
disp("spatGauss")
%stack = spatGauss( stack, 0.5, 5 );
disp("spatGauss completed")

%% run tdResize, bin stack, 4x4 again seems fine!
disp('resize, 4x4 binning')
bin = 2;
base = tdResize( base, bin );
treat = tdResize( treat, bin );
disp("spatial resize completed")

%% run cropstack - project max, cut as small as possible
disp('cropstack')
[ base, rect ] = cropStack( base ); % user must right click and crop
[ treat ] = cropStackRect( treat, rect ); % applies same crop as base
disp("crop completed")

%% OPTIONAL resize in time
disp('downsampling in time')
%zst = cst4x4; % 
base = resizeZ( base, 2 ); % downsample by 10 frames
treat = resizeZ( treat, 2 ); % downsample by 10 frames
disp("downsampling completed")

%% dF/F0
disp('dFoF0')
t1=20; %was 50 for low activity 10s?, frames, 0.1 sec / frame right now!
t2=inf; % frames, should be 500s but MinBarF is really slow! 
t4=200; % frames to define a prominent event for slope fill
[ baseDFoF0, baseMinBF ] = deltaFoverFzero( base, t1, t2, t4 ); % includes t1 and t2 steps above!
[ treatDFoF0, treatMinBF ] = deltaFoverFzero( treat, t1, t2, t4 ); % includes t1 and t2 steps above!disp("dFoF0 completed")
% store the analyzed data!
%createH5( dFoF0, name, '/1' );

%% OPTIONAL smooth stack
%sDFoF0 = SGtStack( dFoF0, 3, 9 );

%% MOVIE TIME!
disp('make a movie')
movieTime( treatDFoF0 );

%% MOVIE TIME!
disp('make a movie')
movieTime( treatDFoF0 );
%%
tmj = projectMaxAnalysis( treatDFoF0, treatMinBF, treat );
%%
bmj = projectMaxAnalysis( baseDFoF0, baseMinBF, base );

%%
[ meanDFoF0, maxDFoF0, stdDFoF0 ] = projMeanMaxSTD(treatDFoF0);
[ meanStack, maxStack, stdStack ] = projMeanMaxSTD(treat);

%%
save 2021111c21metadata.mat -v7.3