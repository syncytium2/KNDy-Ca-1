function [ax1,ax2] = plotEvents(t, ROIdata, analytes, iFile, iROI, pre, post)
% t is a tiledlayout
% ROIdata contains dF/F0 traces
% analytes contains detected event info
% ifile, iroi for choosing trace to display
% pre and post are the baseline and duration of the event display 

    dx=mean(diff(ROIdata(iFile).xdata));
    ax1 = nexttile(t);
    ax2 = nexttile(t);
    
    data = ROIdata(iFile).data(:,iROI);
    if isfield(ROIdata(iFile),'fdata')
        fdata = ROIdata(iFile).fdata(:,iROI); 
    else
        fdata = data;
    end
    xdata = ROIdata(iFile).xdata/60;
    plot(ax1, xdata, data, 'DisplayName', 'raw')
    hold(ax1,"on")

    plot(ax1, xdata, fdata, 'DisplayName', 'filtered')

    times = analytes(iFile).ROI(iROI).times/60;
    pks = analytes(iFile).ROI(iROI).peaks;
    t50pks = pks - 0.5 * analytes(iFile).ROI(iROI).prominence;
    t50locs = analytes(iFile).ROI(iROI).wxPk(:,1)*dx/60;
    scatter(ax1,times,pks,'o','filled', 'DisplayName', 'peaks')
    scatter(ax1,t50locs,t50pks,'o','filled', 'DisplayName', 't50')
    xs = min(xdata);
    xe = max(xdata);
    % plot 2xSD line
    SD = 2*analytes(iFile).ROI(iROI).SD;
    plot(ax1,[xs,xe],[SD,SD], 'DisplayName', '2xSD')
    % plot minheight
    mh = analytes(iFile).ROI(iROI).minHeight;
    plot(ax1,[xs,xe],[mh,mh], 'DisplayName', 'minheight')
    legend(ax1,'show')
    xlabel(ax1, 'time (min)')
    ylabel(ax1,'dF/F0')
    xlim(ax1,[xs,xe])

    if ~isempty(pks)
        locs = round( analytes(iFile).ROI(iROI).wxPk(:,1) ); % should be t50 rise in points
        events = getEvents( fdata, locs, round(pre/dx), round(post/dx) ); % pre and post in points using get events!!
        xpre = pre;
        xpost = post;
        xxx = -xpre:dx:xpost+dx;
        nevents = size(events,2);
        hold(ax2,"on")
        for i=1:size(events,1)
            plot(ax2, xxx(1:nevents), events(i,1:nevents))
        end
        xlim(ax2,[-pre,post])
        ylabel(ax2,'dF/F0')
        xlabel(ax2,'time (s)')
    end

end