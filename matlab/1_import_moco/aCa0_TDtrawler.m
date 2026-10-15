%% DOES NOT NAME FILES RELIABLY!

%% ZERO FOR NON-RIGID MOCO
rigid = 0; % set to zero for non-rigid
crop = 0;

minFrames=2999;
target_binWidth = 150;
force_frame_count = 1; %33450; % 33450; % prime numbers crash the moco
force_resize = 0; % checking if changing size of frame helps with crash
% moco settings
bounds = 10; % was 20 also does not fix array bounds crash
gridSize = 75; % 100; % non-rigid only
maxShift = 20; % was 50 forever; does not fix 
shifts_method = 'fft'; % 'cubic'; %      ' % method to apply shifts ('FFT','cubic','linear'), cubic does not save the day
n_iter = 3;
%Index in position 2 exceeds array bounds. Index must not exceed 53.
%Error in cell2mat_ov (line 24)
% I{i,j,k}(1+(xx_s(i)-extended_grid(1)):end-(extended_grid(2)-xx_f(i)),1+(yy_s(j)-extended_grid(3)):end-(extended_grid(4)-yy_f(j)),1+(zz_s(k)-extended_grid(5)):end-(extended_grid(6)-zz_f(k)));

% file settings
first=1; % straight to for-loop, no conditional, use 1 to start at the first file
%%
%Folder='Z:\defazio turbo\data\raw\20241210';%\20240930';
Folder='\\umms-smoenter-win.turbo.storage.umich.edu\umms-smoenter\defazio turbo\data\raw\to do';
%https://www.mathworks.com/matlabcentral/answers/373870-subfolders-file-search-and-allocation#answer_297121
%Folder='C:\Users\defazio\Documents\data\raw';
SaveFolder = '\\umms-smoenter-win.turbo.storage.umich.edu\umms-smoenter\defazio turbo\data\complete';
%SaveFolder="C:\Users\defazio\Documents\data\complete";
Folder = uigetdir(Folder);
SaveFolder = uigetdir(SaveFolder);
FileList = dir(fullfile(Folder, '**', '*.ets'));
completed = dir(fullfile(SaveFolder,'**','*complete.mat'));
comp={completed.name};

S=[];
%%
% loop over file list
for iFile=first:size(FileList,1)
    %% convert to matlab
    fullpath = fullfile(FileList(iFile).folder,FileList(iFile).name);
    datec_exp = '\d{8}';

    % need to check if there are multiple matches!!
    datec = regexp(fullpath,datec_exp,'match','once');
    exp_exp = '_?\d{2,3}_';
    
    % need to check if there are multiple matches!!
    expc = regexp(fullpath,exp_exp,'match','once');
    basename = [datec,'_',expc(2:end-1)];

    filename = append( basename, 'complete.mat');
    index=find(strcmp(filename,comp)==true, 1);
    if isempty(index)
        disp(['importing ',filename, ' as ', fullpath])
        if isempty(S) || ~strcmp(S.ets,fullpath)
            S=openETSbatch(fullpath);
        else
            disp('S is already loaded, might want to double check!')
        end
        sz=size(S.stack);
        if sz(3)>minFrames
            if sz(1)*sz(2) > 512*512 % resize!
                % if not smaller than 512x512, run tdResize, bin stack, 4x4 again seems fine!
                disp('resize, 4x4 binning')
                bin = 4;
                S.stack = tdResize( S.stack, bin );
                disp("spatial resize completed")
            end
            %% clean stack
            thresh = 7;
            % oBase is base outliers, pmBase is projectMaxBase
            [ baseOutliers, baseProjMax ] = findOutliersMAXb( S.stack, thresh ); % 4.5 x std threshold
            S.clean.cleanStack = replaceOutliersb( S.stack, baseOutliers );
            %% clean projections for review
            [smean,smax,sSTD]=projMeanMaxSTD(S.clean.cleanStack);
            S.projections.clean.mean=smean;
            S.projections.clean.max=smax;
            S.projections.clean.std=sSTD;
            %% run cropstack - project max, cut as small as possible
            if crop
                disp('cropstack')
                [ S.clean.cleanStack, rect ] = cropStack( S.clean.cleanStack ); % user must right click and crop
                %[ treat ] = cropStackRect( treat, rect ); % applies same crop as base
                disp("crop completed")
            end
            %% MOCO
            if force_frame_count>0
                % force frame count to be divisible by target_binwidth
                sz=size(S.clean.cleanStack);
                nBins = floor( sz(3)/target_binWidth );
                nFrames2 = nBins * target_binWidth;
                if force_resize>0
                    sz(1)=sz(1)-force_resize;
                    sz(2)=sz(2)-force_resize;
                end
                S.clean.cleanStack=resize(S.clean.cleanStack,[sz(1),sz(2),nFrames2]);
                timing = S.timing;
                S.old_timing = timing;
                S.timing = resize(timing,nFrames2);
            end
            
            nframes = size(S.clean.cleanStack,3);
            % if (force_frame_count>0) && (nframes~=force_frame_count)
            %     disp('force frame count failed!')
            % end

            proj = MeanMaxSTD( S.clean.cleanStack, 'all' );
            S.projections.raw.mean = proj{1,1};
            S.projections.raw.max = proj{1,2};
            S.projections.raw.std = proj{1,3};
            % run moco if not already present
            Smoco.stack = [];

            % get binsize, bestDivsior of frame number
            if force_frame_count>0
                binWidth = target_binWidth;
            else
                binWidth  = bestDivisor(nframes,target_binWidth);
            end
            saveY = 0; % saves the filtered stack, memory hog!
            % % fix binwidth
            % if binWidth < 100
            %     %disp( append( 'bestdivisor failed ', num2str(nframes), ' best: ', num2str(binWidth) ) )
            %     binWidth = target_binWidth;
            % end

            %% TD_MOCO! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            Smoco = td_moco2( ...
                S.clean.cleanStack, ...
                bounds, gridSize, maxShift, binWidth, rigid, n_iter, shifts_method, ...
                saveY );
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            % if smoco present, 
            if isfield(Smoco,'stack') && ~isempty(Smoco.stack) % Smoco is empty if moco fails
                % update structure
                opts.bounds=bounds;
                opts.gridSize=gridSize;
                opts.maxShift=maxShift;
                opts.binWidth=binWidth;
                opts.iter = n_iter;
                opts.rigid=rigid;
                Smoco.opts=opts;
                S.Smoco = Smoco;
                clear Smoco
                % do the projections
                proj = MeanMaxSTD( S.Smoco.stack, 'all' );
                S.projections.moco.mean = proj{1,1};
                S.projections.moco.max = proj{1,2};
                S.projections.moco.std = proj{1,3};

                % Full frame dFoF0 for projections
                t1=10; %was 50 for low activity 10s?, frames, 0.1 sec / frame right now!
                t2=inf; % frames, should be 500s but MinBarF is really slow!
                t4=200; % frames to define a prominent event for slope fill
                [ S.FF.DFoF0, ~ ] = deltaFoverFzeroEmbedded( S.Smoco.stack, t1, t2, t4 ); % includes t1 and t2 steps above!
                proj = MeanMaxSTD( S.FF.DFoF0, 'all' );
                S.projections.DFoF0.mean  = proj{1,1};
                S.projections.DFoF0.max = proj{1,2};
                S.projections.DFoF0.std = proj{1,3};

                % generate confirmation images
                if rigid==0
                    moco_type="_flex";
                else
                    moco_type="_rigid";
                end
                filename=append(basename,moco_type,"_",num2str(n_iter));
                confirm_fig = figure('Name',filename);
                mocoConfirm( confirm_fig, S.clean.cleanStack, S.Smoco.stack, filename )
                %% save
                filename = append( basename, 'complete', moco_type,"_",num2str(n_iter), '.mat');
                fullpath = fullfile(SaveFolder,filename);
                save(fullpath, "-struct", "S", "-v7.3")

            else
                disp(['MOCO failed! ',filename])
            end

        else
            disp(['not processed: too short ',filename])
        end
    else
        disp(['already saved ',filename, ' as ', fullfile(completed(index).folder,completed(index).name)])
    end

end % loop over file list
