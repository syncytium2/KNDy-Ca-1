% TD custard
% quick version
function custardOutput = TDcustard(n,signal,sigthresh,minAmp,minWidth,tXrise,noFig,detailFlag)
% n, number of points to test for a change
% signal, 1xnpnts
% sigthresh, significance to call an up
% minAmp, to be a peak the relative amplitude needs to exceed this

nBase = n;

if detailFlag==true
    dFig = figure('Name','custardDetail');
    ax = axes(dFig);
    plot(ax,signal,'k'); hold(ax,'on')
    upColor='b'; % ups are blue
    dnColor='r'; % downs are red
    peakColor='g';
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
    amp = mean(test) - mean(base);
    [h,p(ii)] = ttest2( base, test );
    if ( p(ii) < sigthresh ) %&& ( amp > minAmp )
        if amp > minAmp % we have an up
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

if detailFlag
    msize=5;
    plot(ax,upLoc,up,'o', 'DisplayName','up', ...
        'MarkerFaceColor',upColor, ...
        'MarkerEdgeColor',upColor ,'MarkerSize',msize)
    msize=5;
    plot(ax,dnLoc,dn,'o', 'DisplayName','down', ...
        'MarkerFaceColor',dnColor, ...
        'MarkerEdgeColor',dnColor ,'MarkerSize',msize)
end

% clean up
% handle consecutive ups
%intervals = diff(upLoc); % diff uses x(2)-x(1)
peak=[];
iPeak = 0;
ii=1;
while ii < length(upLoc)-1
    iPeak=iPeak+1;
    peak(iPeak) = up(ii); % max(upTest(ii,:)); % up(ii);
    peakBase(iPeak) = upBase(ii);
    peakBaseLine(iPeak,:) = upBaseLine(ii,:);
    peakBaseLineX(iPeak,:) = upLoc(ii)-nBase:upLoc(ii)-1;
    
    % x=squeeze(peakBaseLineX(iPeak,:));
    % y=squeeze(peakBaseLine(iPeak,:));
    % npnts=length(x);
    % startOnly=true;
    % [baseCorr, baseLineFit] = removeLinearBaseline(x,y,npnts,startOnly);
    % peakBaseLineFit(iPeak,:) = baseLineFit;

    peakAmp(iPeak) = upAmp(ii);
    peakLoc(iPeak) = upLoc(ii);
    while (ii<length(upLoc)-1) && ( ( upLoc(ii+1) - upLoc(ii) ) < 2 ) %== 1 % adjacent ups!
        ii=ii+1;
        if peak(iPeak) < up(ii)
            [localMax, iLocalMax] = max( upTest(ii,:) );

            [ peak(iPeak), kk ] = max( [localMax, peak(iPeak)] ); % ,up(ii);

            peakAmp(iPeak) = peak(iPeak) - peakBase(iPeak); % upAmp(ii);
            if kk==1
                peakLoc(iPeak) = upLoc(ii) + iLocalMax-1;
            else
                peakLoc(iPeak) = upLoc(ii);
            end
        end
    end
    ii=ii+1;
end

if detailFlag
    msize=12;
    plot(ax,peakLoc,signal(peakLoc),'o','DisplayName','up-up peak', ...
        'MarkerFaceColor',peakColor, ...
        'MarkerEdgeColor',peakColor ,'MarkerSize',msize)
    % for ii=1:size(peakBaseLine,1)
    %     plot(ax,peakBaseLineX(ii,:),peakBaseLineFit(ii,:),'g')
    % end
end

% advance the peak to the local max
iPeak=0;
while iPeak < length(peak)-1
    iPeak=iPeak+1;
    % find the next up or down
    iUp = find( upLoc > peakLoc(iPeak), 1, 'first' );
    if isempty(iUp) % there are no ups after peakloc

    else

    end
    iDn = find( dnLoc > peakLoc(iPeak), 1, 'first' );
    if isempty(iDn) % there are no downs after peakloc

    else
        
    end
    if ~isempty(iUp)&&~isempty(iDn)
        iRange = min( upLoc(iUp), dnLoc(iDn), 'omitmissing' );
    else
        if ~isempty(iUp)
            iRange=upLoc(iUp);
        else
            if ~isempty(iDn)
                iRange=dnLoc(iDn);
            else
                % both are empty! no ups or downs
                iRange=[];
            end
        end
    end
    if ~isempty(iRange)
        [localMax, iLocalMax] = max( signal( peakLoc(iPeak) : iRange ) );
        peakLoc(iPeak) = peakLoc(iPeak) + iLocalMax - 1;
        peak(iPeak) = localMax;
    else
        % don't update the peak!
    end
    peakAmp(iPeak) = peak(iPeak) - peakBase(iPeak);
end

if detailFlag
    msize=12;
    plot(ax,peakLoc,signal(peakLoc),'ok','DisplayName','local peak','MarkerSize',msize)
end

% peak FWHM
if ~isempty(peak)
    tXriseIndex=zeros(length(peak),1);
    tXfallIndex=zeros(length(peak),1);
    XAmp=zeros(length(peak),1);

    for ii=1:length(peak)
        XAmp(ii) = peakBase(ii) + tXrise * peakAmp(ii);
        if win >= peakLoc(ii)
            tXsearch = signal( 1: peakLoc(ii));
            offset = 0;
        else
            tXsearch = signal( peakLoc(ii)-win : peakLoc(ii) );
            offset=peakLoc(ii)-win-1;
        end
        temp=find( tXsearch < XAmp(ii), 1, "last");
        if ~isempty(temp)
            tXriseIndex(ii) = temp + offset;
        else
            tXriseIndex(ii) = peakLoc(ii)-1;
            disp('fake tX rise')
        end
        if length(signal) < peakLoc(ii)+win
            if ii<length(peakLoc)
                tXsearch = signal( peakLoc(ii) : peakLoc(ii+1) );
            else
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
                nextDnLoc = find( dnLoc > peakLoc(ii) & dnLoc < peakLoc(ii+1), 1, 'last');
            else
                nextDnLoc = find( dnLoc > peakLoc(ii), 1, 'last');
            end
            txt='none';
            if isempty(nextDnLoc)
                % if there are no downs between this peak and the next?
                if ii<length(peakLoc)
                    txt='upLoc>peakLoc(ii) & upLoc<=peakLoc(ii+1)';
                    nextUpLoc = find( upLoc>peakLoc(ii) & upLoc<=peakLoc(ii+1),1,'last');
                    if isempty(nextUpLoc)
                        disp('no downs, no ups! use next loc')
                        nextUpLoc = find(upLoc<=peakLoc(ii+1),1,'last');
                        txt='upLoc<=peakLoc(ii+1)';

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

            else
                txt='upLoc > nextDnLoc';
                nextUpLoc = find( upLoc > dnLoc(nextDnLoc), 1, 'first' );
                if isempty(nextUpLoc) % no more ups!
                    nextUpLoc=length(upLoc); % 911 not sure this works
                end
            end
            tXfallIndex(ii) = upLoc( nextUpLoc );
            disp(['no t50fall: ',txt,'. ',num2str(peakLoc(ii)),' window default old: ',num2str(oldFallIndex),' new: ',num2str(tXfallIndex(ii))])
        else
            tXfallIndex(ii) = test + peakLoc(ii)-1;
        end

    end
    peakWidth=tXfallIndex-tXriseIndex;

    if detailFlag
        ccode='m+';msize=12;
        plot(ax,tXriseIndex,XAmp,ccode,'DisplayName','rise', ...
            'MarkerSize',msize)
        ccode='c+';msize=12;
        plot(ax,tXfallIndex,XAmp,ccode,'DisplayName','fall', ...
            'MarkerSize',msize)
    end

    %noFig=1;
    if ~noFig

        upColor='b'; % ups are blue
        dnColor='r'; % downs are red
        peakColor='g';
        figure('Name','custardTrouble')
        tiledlayout(1,2)
        t1=nexttile;
        plot(t1,signal,'k','DisplayName','fluo'); hold on
        msize=12;
        plot(t1,peakLoc,signal(peakLoc),'o','DisplayName','peak', ...
            'MarkerFaceColor',peakColor, ...
            'MarkerEdgeColor',peakColor ,'MarkerSize',msize)
        ccode='m+';msize=12;
        plot(t1,tXriseIndex,XAmp,ccode,'DisplayName','rise', ...
            'MarkerSize',msize)
        ccode='c+';msize=12;
        plot(t1,tXfallIndex,XAmp,ccode,'DisplayName','fall', ...
            'MarkerSize',msize)
        msize=5;
        plot(t1,upLoc,up,'o', 'DisplayName','up', ...
            'MarkerFaceColor',upColor, ...
            'MarkerEdgeColor',upColor ,'MarkerSize',msize)
        msize=5;
        plot(t1,dnLoc,dn,'o', 'DisplayName','down', ...
            'MarkerFaceColor',dnColor, ...
            'MarkerEdgeColor',dnColor ,'MarkerSize',msize)
        ii=1;
        legend(t1)
        xlim(t1,[peakLoc(ii)-win, peakLoc(ii)+win])
        xlabel('points')
        ylabel('\DeltaF/F0')
        t2=nexttile;
        [f,x]=ecdf(t2,peakWidth);
        plot(t2,x,f, ...
            'DisplayName','before', ...
            'LineWidth',1); hold on
        xlabel('width, points')
        ylabel('fraction')
    end

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

    % get rid of narrow peaks!
    custardOutput.peak=peak(peakWidth>=minWidth);
    custardOutput.peakAmp=peakAmp(peakWidth>=minWidth);
    custardOutput.peakLoc=peakLoc(peakWidth>=minWidth);
    custardOutput.peakWidth=peakWidth(peakWidth>=minWidth);
    custardOutput.t50riseIndex=tXriseIndex(peakWidth>=minWidth);
    custardOutput.t50fallIndex=tXfallIndex(peakWidth>=minWidth);
    custardOutput.halfAmp=XAmp(peakWidth>=minWidth);

    if detailFlag
        msize=12;
        plot(ax,custardOutput.peakLoc,signal(custardOutput.peakLoc),'x','DisplayName','final peak', ...
            'MarkerFaceColor','r', ...
            'MarkerEdgeColor','r' ,'MarkerSize',msize)
        legend
    end

    if ~noFig
        [f,x]=ecdf(t2,custardOutput.peakWidth);
        plot(t2,x,f, ...
            'DisplayName','after', ...
            'LineWidth',1); hold on
    end
else
    disp('no peaks in TDcustard')
    custardOutput=[];
end
