function events = getEvents( data, locs, pre, post )
% data contains one traces
% locs contains a list of indices of peaks in "Data"
% pre and post are the durations in points before and after the loc
    nevents = size(locs,1);
    events = zeros(nevents,pre+post+1);
    for i = 1:nevents
        is = locs(i)-pre;
        if is <1
            is = 1;
        end
        ie = locs(i)+post;
        imax = size( data, 1 );
        if ie > imax
            ie = imax;
        end
        events(i,1:ie-is+1) = data( is : ie );
    end
end