% TD custard
% quick version
function custardOutput = TDcustard2 (n, signal, sigthresh, minAmp, minWidth, tXrise, noFig, detailFlag, silent )
% n, number of points to test for a change
% signal, 1xnpnts
% sigthresh, significance to call an up
% minAmp, to be a peak the relative amplitude needs to exceed this
%detailFlag=true;
nBase = n;
silent=0;
if nargin==9
    silent=silent;
else
    silent=1;
end
if detailFlag==true
    dFig = figure('Name','custardDetail');
    ax = axes(dFig);
    plot(ax,signal,'k'); hold(ax,'on')
    xlabel(ax,'points')
    ylabel(ax,'\DeltaF/F0')
    upColor='b'; % ups are blue
    dnColor='r'; % downs are red
    peakColor='m';
end

win = 5 * minWidth; % 10; % how far to look for rise and fall, should be settable, not hard coded

p=nan(size(signal));
upLoc=[];
upAmp=[];
iUp=0; iDn=0;
npnts=length(signal);

% find all the ups and downs
for ii=nBase+1:npnts-n
    base = signal(ii-nBase:ii-1);
    test = signal(ii:ii+n-1);
    [h,p(ii)] = ttest2( base, test );
    if ( p(ii) < sigthresh ) %&& ( amp > minAmp )
        amp = mean(test) - mean(base);
        if amp > 0 % minAmp % we have an up
            iUp = iUp+1;
            upLoc( iUp ) = ii;
            upAmp( iUp ) = amp;
            upTest( iUp, : ) = test;
            upBaseLine( iUp, : ) = base;
            upBase( iUp ) = mean(base);
            %[ maxpeak, ipeak ] = max( test );
            up( iUp ) = signal(ii); % maxpeak;
            %upLoc( iUp ) = ii; % + ipeak - 1;
        else % we might have a down
            if amp < 0
                iDn = iDn + 1;
                dnLoc( iDn ) = ii;
                dnAmp( iDn ) = amp;
                %[ minNadir, iNadir ] = min( test );
                dn( iDn ) = signal(ii); % minNadir;
                %dnLoc( iDn ) = ii + iNadir - 1;
            end
        end
    end
end

% if detailFlag
%     msize=11;
%     plot(ax,upLoc,up,'o', 'DisplayName','up', ...
%         'MarkerFaceColor',upColor, ...
%         'MarkerEdgeColor',upColor ,'MarkerSize',msize)
%     msize=5;
%     plot(ax,dnLoc,dn,'o', 'DisplayName','down', ...
%         'MarkerFaceColor',dnColor, ...
%         'MarkerEdgeColor',dnColor ,'MarkerSize',msize)
% end

% get peaks from ups and dns
% handle consecutive ups
peak=[];
iPeak = 0;
ii=1;
base = upBase(ii);
maxGap=2;
while ii < length(upLoc)
    iPeak=iPeak+1;
    peakBase(iPeak) = upBase(ii);
    peakBaseLine(iPeak,:) = upBaseLine(ii,:);
    peakBaseLineX(iPeak,:) = upLoc(ii)-nBase : upLoc(ii)-1;
    % get the next downs
    next_dnLocs = dnLoc( dnLoc > upLoc(ii) ); % please use the first one!
    if isempty(next_dnLocs)
        next_dnLocs = inf;
    end
    % get the next ups
    next_upLocs = upLoc( upLoc >= upLoc(ii) & upLoc < next_dnLocs(1) ); % includes current point
    if ~isempty( next_upLocs ) && length(next_upLocs)>1 % if empty go to the next peak
        % get the gaps
        gaps = diff( next_upLocs )-1; % adjacent points have no gap
        iGap = 1;
        while iGap <= length(gaps) && gaps( iGap ) < maxGap % move on up the ups to get to the last up
            iGap=iGap+1;
        end
        ii=ii+iGap-1;
    else
        % placeholder
    end 
    peak(iPeak) = up(ii); % max(upTest(ii,:)); % up(ii);
    peakAmp(iPeak) = peak(iPeak) - peakBase(iPeak);
    peakLoc(iPeak) = upLoc(ii);    
    ii=ii+1;
end

% advance the peak to the local max
iPeak=0;
while iPeak < length(peak)-1
    iPeak=iPeak+1;

    % find the next up or down
    iUp = find( upLoc > peakLoc(iPeak), 1, 'first' );
    iDn = find( dnLoc > peakLoc(iPeak), 1, 'first' );
    if ~isempty(iUp)&&~isempty(iDn)
        iRange = min( [peakLoc(iPeak)+maxGap, upLoc(iUp), dnLoc(iDn)],[], 'omitmissing' );
    else
        if ~isempty(iUp)
            iRange=upLoc(iUp)-1; % stop search before the next up
        else
            if ~isempty(iDn)
                iRange=dnLoc(iDn);
            else
                % both are empty! no ups or downs
                iRange=peakLoc(iPeak)+maxGap; % [];
            end
        end
    end

    if ~isempty(iRange)
        % [localMax, iLocalMax] = max( signal( peakLoc(iPeak) : iRange ) );
        % above is original code, changed 20250714 TD
        % need to check for a peak before up-up peak
        % 20260407 on smoothed data this results in a revised peak 
        % if a max is within 10s before peak_loc
        [localMax, iLocalMax] = max( signal( peakBaseLineX(iPeak,1) : iRange ) );
        % new version
        %[localMax, iLocalMax] = max( signal( peakLoc(iPeak) - minWidth : iRange ) );        

        peakLoc(iPeak) = peakBaseLineX(iPeak,1) + iLocalMax - 1;
        peak(iPeak) = localMax;
    else
        % don't update the peak!
    end
    peakAmp(iPeak) = peak(iPeak) - peakBase(iPeak);
end

% % plot local peak
% if detailFlag
%     msize=15;
%     plot(ax,peakLoc,signal(peakLoc),'ok','DisplayName','local peak','MarkerSize',msize)
% end

% peak FWHM
if ~isempty(peak)
    tXriseIndex=zeros(1,length(peak));
    tXfallIndex=zeros(1,length(peak));
    XAmp=zeros(1,length(peak));

    for ii=1:length(peak)

        XAmp(ii) = peakBase(ii) + tXrise * peakAmp(ii);
        if win >= peakLoc(ii) 
            % if we don't have enough points before the peak
            tXsearch = signal( 1: peakLoc(ii));
            offset = 0;
        else
            % we have enough points to search the whole window
            tXsearch = signal( peakLoc(ii)-win : peakLoc(ii) );
            offset=peakLoc(ii)-win-1;
        end

        temp=find( tXsearch < XAmp(ii), 1, "last");
        if ~isempty(temp)
            tXriseIndex(ii) = temp + offset;
        else
            [M,I]=min(tXsearch);
            tXriseIndex(ii) = peakLoc(ii)-I; % 1;
            if ~silent 
                disp('custard: no t50rise, used local minimum'); 
            end
        end

        if length(signal) < peakLoc(ii)+win
            % if we don't have points after peak to search the full window
            if ii<length(peakLoc)
                % use the next peak as the end of the window
                tXsearch = signal( peakLoc(ii) : peakLoc(ii+1) );
            else
                % use the end of the signal as the end of the window
                tXsearch = signal( peakLoc(ii) : end );
            end
        else
            if ii<length(peakLoc)
                tXsearch = signal( peakLoc(ii) : peakLoc(ii+1) );
            else
                tXsearch = signal( peakLoc(ii) :  peakLoc(ii)+win );
            end
        end

        test = find( tXsearch < XAmp(ii), 1, "first");

        if isempty(test) % did not find an obvious t50fall index

            oldFallIndex= tXriseIndex(ii) + minWidth - 1;% + peakLoc(ii);
            if ii<length(peakLoc)
                % get the last down location before the next peak
                nextDnLoc = find( dnLoc > peakLoc(ii) & dnLoc < peakLoc(ii+1), 1, 'last');
            else
                % or the last down location overall
                nextDnLoc = find( dnLoc > peakLoc(ii), 1, 'last');
            end
            txt='none';
            if isempty(nextDnLoc)
                % if there are no downs between this peak and the next?
                if ii<length(peakLoc)
                    txt='upLoc>peakLoc(ii) & upLoc<=peakLoc(ii+1)';
                    nextUpLoc = find( upLoc>peakLoc(ii) & upLoc<=peakLoc(ii+1),1,'last');
                    if isempty(nextUpLoc)
                        if ~silent 
                            disp('no downs, no ups! use next loc')
                        end
                        nextUpLoc = find(upLoc<=peakLoc(ii+1),1,'last');
                        if isempty(nextUpLoc)
                            disp('no upLoc between this peak and the next?')
                            nextUpLoc = peakLoc(ii+1);
                            txt='upLoc set to next peakLoc!';
                        else
                        txt='upLoc<=peakLoc(ii+1)';
                        end
                    end
                else
                    % we hit the end of the sweep, take the last up
                    nextUpLoc = find( upLoc>peakLoc(ii),1,'last');
                    txt='upLoc>peakLoc(ii)';
                    if isempty(nextUpLoc)
                        nextUpLoc=length(upLoc); % 911 need to handle this better
                        txt='WHOOPSIE!';
                    end
                end
                tXfallIndex(ii) = upLoc( nextUpLoc );
            else
                txt='upLoc > nextDnLoc!';
                nextUpLoc = find( upLoc > dnLoc(nextDnLoc), 1, 'first' );
                if isempty(nextUpLoc) % no more ups!
                    tXfallIndex(ii) = dnLoc(end);
                    txt=[txt,' no more ups! using last down'];
                else
                    tXfallIndex(ii) = upLoc( nextUpLoc );
                end
            end

            if ~silent
                disp(['no t50fall: ',txt,'. ',num2str(peakLoc(ii)),' window default old: ',num2str(oldFallIndex),' new: ',num2str(tXfallIndex(ii))])
            end

            else
            % we found an easy t50 fall!
            tXfallIndex(ii) = test + peakLoc(ii)-1;
        end

    end
    peakWidth=tXfallIndex-tXriseIndex;

    % handle other things?
    % min peak separation

    % store the output
    custardOutput.upLoc=upLoc; % index of up
    custardOutput.upAmp=upAmp; % relative amp of up
    custardOutput.up=up; % value of up
    custardOutput.dnLoc=dnLoc; % index of down
    custardOutput.dnAmp=dnAmp; % relative amp of dn
    custardOutput.dn=dn; % value of dn
    custardOutput.p=p; % significance!

    % store all the peaks whether they are too small or too narrow!
    custardOutput.peakAll=peak; 
    custardOutput.peakAmpAll=peakAmp;
    custardOutput.peakLocAll=peakLoc;
    custardOutput.peakWidthAll=peakWidth;
    custardOutput.t50riseIndexAll=tXriseIndex;
    custardOutput.t50fallIndexAll=tXfallIndex;
    custardOutput.halfAmpAll=XAmp;
    custardOutput.peakBaseAll = peakBase; 
    custardOutput.peakBaseLineAll = peakBaseLine; 
    custardOutput.peakBaseLineXAll = peakBaseLineX;

    % get rid of small and narrow peaks!
    indices = find( peakAmp>minAmp & peakWidth>=minWidth );
    custardOutput.peak=peak(indices);
    custardOutput.peakAmp=peakAmp(indices);
    custardOutput.peakLoc=peakLoc(indices);
    custardOutput.peakWidth=peakWidth(indices);
    custardOutput.t50riseIndex=tXriseIndex(indices);
    custardOutput.t50fallIndex=tXfallIndex(indices);
    custardOutput.halfAmp=XAmp(indices);
    custardOutput.peakBase = peakBase(indices); % = upBase(ii);
    custardOutput.peakBaseLine = peakBaseLine(indices,:); % = upBaseLine(ii,:);
    custardOutput.peakBaseLineX = peakBaseLineX(indices,:); % = upLoc(ii)-nBase : upLoc(ii)-1;

    % plot final peaks
    % if detailFlag
    %     msize=5;
    %     plot(ax,custardOutput.peakLoc,custardOutput.peak,'o','DisplayName','final peak', ...
    %         'MarkerFaceColor','g', ...
    %         'MarkerSize',msize)
    %     legend
    % end
    % 
    % if ~noFig
    %     [f,x]=ecdf(t2,custardOutput.peakWidth);
    %     plot(t2,x,f, ...
    %         'DisplayName','after', ...
    %         'LineWidth',1); hold on
    % end

else
    disp('no peaks in TDcustard')
    custardOutput=[];
end
