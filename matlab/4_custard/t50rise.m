% t50 rise alignment
function [ t50rise, t50riseIndex ] = t50rise( x, y )
% y data, assumes data have been:
%   baseline zeroed and there's a single peak
% t50rise is the interpolated time of 50% rise
% t50riseIndex is the index of the last point below 50% rise

amplitude = max(y);
crossings = FindLevel( x, y, 0.5*amplitude ); % igor func from starstrider 
t50rise = crossings(1);
t50riseIndex = find( y > 0.5*amplitude, 1, 'first' );
t50riseIndex = t50riseIndex - 1;

% get the peak data point in y
% estimate amplitude. peak - baseline
% move backwards until data point is < 50% of the amplitude

