function analytes = analyzeROIs2( ROIdata, roll, minwidth, ...
    Pfactor, prcthresh )

%fdataIIR = filter(iir,data);
%fdataIIRff = filtfilt(iir,data);

% P factor factor of 10th percentile rolling SD for prominence threshold
% prcthresh=10;
nFiles = size( ROIdata, 2 );
for iFile = 1 : nFiles
    name = ROIdata(iFile).name;
    if isfield(ROIdata(iFile),'fdata')
        ROIstack = ROIdata(iFile).fdata;
    else
        ROIstack = ROIdata(iFile).data;
        disp("analyzeROIs2: WARNING! NO PRE-FILTERED DATA!")
    end% pensubdf = ROIdataStruct(iFile).data;
    % ROIstack = cell2mat(struct2cell(pensubdf));
    nROI = size(ROIstack,2);
    analytes(iFile).name = name;
    xdata = ROIdata(iFile).xdata;
    dx = mean( diff(xdata) ); % interval in seconds
    nx = max(size(xdata));
    %npoints = max( size( xdata ) );
    for iROI = 1 : nROI
        
        % trace = ROIstack(:,iROI);
        % smth = boxcar( trace , smooth );
        smth = ROIstack(:,iROI);
        SD = std(smth); 

        rollStd = movstd( smth, roll ); % rolling standaRDDEVIATION
        P = prctile(rollStd,prcthresh);

        %minwidth = 0.05; % time (seconds), now using Fs sampling freq
        minHeight = Pfactor * P;
        widthRef = 'halfprom'; %'halfheight'; %
        minPeakProm = minHeight; % Pfactor * P; % dF/F0 units
        %minwidth_points = minwidth * Fs;
        
        [pks,locs,w,p,wxPk] = findpeaksTD(smth,'MinPeakWidth',minwidth/dx, ...
            'Annotate','extents','WidthReference',widthRef,'MinPeakProminence',minPeakProm);
        % wxPk contains the start and end of the width reference, FWHM
        % w is widths
        % p is prominence for each peak
% insert t50 code here!

        nevents = size( pks, 1 );

        xs = locs - fix(w); % time window to find 50% risetime
        xe = locs;% + fix(w); % time of the prominence

        xs( xs<1 ) = 1; % anything  <1 is now 1

        crossings = NaN(nevents,1);
        crosslev = NaN(nevents,1);

        for ie = 1:nevents
            %BLWx = xdata( xs(ie) : xe(ie) );
            BLW = smth( xs(ie) : xe(ie) ); % "bilevel waveform"

            P50 = pks(ie) - p(ie) * 0.5;
            c = find( BLW < P50, 1, 'last' ); % find the index of the last value in BLW < P50
% c is 1x0 if no level crossing
            if size(c,1)==0
                cx = xdata( xs(ie) );
                P50 = 0.5 * pks( ie );
                disp(append('* failed to find crossing: file: ',...
                    num2str(iFile),' ROI: ',num2str(iROI),...
                    ' event: ',num2str(ie),num2str(P50),num2str(xe(ie))))
            else
                pos = xs(ie) + c;
                if pos>nx
                    pos= nx;
                    disp(append('* faked t50: file: ',...
                    num2str(iFile),' ROI: ',num2str(iROI),...
                    ' event: ',num2str(ie),num2str(P50),num2str(xe(ie))))
                end
                cx = xdata( pos );
            end

% plot(BLWx, BLW)
% scatter(cx,P50,'filled')
            if size(cx,2)~=1
                disp('problema')
            end
            crossings(ie,1) = cx; %BLWx(c); %cx; %locs(ie) - ( w(ie) - c );
            crosslev(ie,1) = P50; % midlev;
        end
        if size(pks,1)<1
            disp(append('no peaks; file: ',num2str(iFile),'; ROI: ',num2str(iROI), ...
                '; percentile SD: ',num2str(P),'; minheight: ',num2str(minHeight)))
        end
        analytes(iFile).ROI(iROI).peaks = pks;
        analytes(iFile).ROI(iROI).locs = locs;
        analytes(iFile).ROI(iROI).times = xdata( locs );% must use xdata!!! * dx;
        analytes(iFile).ROI(iROI).width = w * dx;
        analytes(iFile).ROI(iROI).prominence = p;
        analytes(iFile).ROI(iROI).crossings = crossings;% * dx;
        analytes(iFile).ROI(iROI).crosslev = crosslev;        
        analytes(iFile).ROI(iROI).SD = SD;
        analytes(iFile).ROI(iROI).P = P;
        analytes(iFile).ROI(iROI).minHeight = minHeight;
        analytes(iFile).ROI(iROI).wxPk = wxPk; % t50 rise and fall in interpolated points (not time!)
    end % loop over ROI
    analytes(iFile).fdata = ROIstack; % filtered data!
end % loop over files
end
        