function twoPairs = localFWHdelta1D2( xprofile, yprofile, profile, r )
twoPairs = zeros(2,2);
% each pair is based on FWHdelta, full width at half delta
% where delta = max - min of image subregion 
% defined by win and location of click

% img is a 2d image array; note smoothing in preprocessing
% xx, yy is the location of a click for an ROI
% win is the 0.5 search width for local max and min

% from imageanalyst
% https://www.mathworks.com/matlabcentral/answers/310113-how-to-find-out-full-width-at-half-maximum-from-this-graph
% Find the half max value.
%figure
%plot( xprofile, yprofile )
%plot3( xprofile, yprofile, profile)

% along the PROFILE
% data = profile;
% halfMax = ( min( data, [], 1 ) + max( data, [], 1 )) / 2;
% % Find where the data first drops below half the max.
% index1 = find( data >= halfMax, 1, 'first' );
% % Find where the data last rises above half the max.
% index2 = find( data >= halfMax, 1, 'last' );
% % trouble->shooting
% if abs(index2-index1)<2
%     index1 = index1 - 1;
%     index2 = index2 + 1;
% end
% 20240228 using find peaks
%plot(profile)
[ pks, locs, w, p, wx ] = findpeaksTD( profile, 'Annotate', 'extents' ); % mod to return indices of widths
[ M, I ] = max( pks );

% check for weird width
if isempty(w) || ( w(I)<0.75*r ) || ( w(I)>1.25*r )
    %display(append("width too small or big ", num2str(w(I))))
    index1 = floor(1 + 0.5*r);
    index2 = ceil(1 + 1.5*r);
else
    index1 = floor( wx( I, 1 ) );
    index2 = ceil( wx( I, 2 ) );
end

twoPairs(1,1) = xprofile(index1);
twoPairs(1,2) = yprofile(index1);
twoPairs(2,1) = xprofile(index2);
twoPairs(2,2) = yprofile(index2);
%twoPairs
end