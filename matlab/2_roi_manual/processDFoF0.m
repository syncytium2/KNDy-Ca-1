function ROIstats = processDFoF0(ROIstats, t1, t2, t4 ) %, minminBF)
% based on processROIs2!
% note all functions within are using frames not time!

    % t1 = 50;
    % t2 = inf;
    % t4 = 2000; 
    % dt is time step "interval"
    
    %n = size( ROIstats, 2 ); % number of ROIs
    nROIs = ROIstats.nROIs; % ROIstats(n).nROIs;

    nframes = max( size( ROIstats.ROI(1).ROI.meanZ ) ); 
    % max to maintain compatibility with previous versions, now should be nx1
    stack = zeros( 2, nROIs+1, nframes );
    
    % load stack array with raw and trace data (trace is pen subtracted)
    % both will be processed by the deltaFoverFzero function
    % here stack is like a movie where each "pixel" is an ROI time course
    for i=1:nROIs % loop over ROIs
        stack( 1, i, : ) = ROIstats.ROI(i).ROI.meanZ;
        stack( 2, i, : ) = ROIstats.ROI(i).pensub;
    end
    stack(1,nROIs+1,:) = ROIstats.FFA.raw;
    stack(2,nROIs+1,:) = 1;

    [ dFoF0, minBF ] = deltaFoverFzeroEmbedded( stack, t1, t2, t4 ); %, minminBF );
    
    % load the dFoF0 data into the ROIstats structure and return
    
    for i=1:nROIs % loop over ROIs to store the processed data
        ROIstats.ROI(i).dFoF0.ROI = squeeze( dFoF0( 1, i, : ) );
        ROIstats.ROI(i).dFoF0.pensub = squeeze( dFoF0( 2, i, : ) );
        ROIstats.ROI(i).dFoF0.ROIminBF = squeeze( minBF( 1, i, : ) );
        ROIstats.ROI(i).dFoF0.minBF = squeeze( minBF( 2, i, : ) );
    end
    % full frame average, treat the expensive camera 
    %   like a single photodiode
    ROIstats.FFA.dFoF0 = squeeze( dFoF0(1,nROIs+1,:) );

end