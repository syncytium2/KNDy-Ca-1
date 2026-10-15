% how to normalize each frame
function [nst, Smax] = normStack( st )
% assumes 3rd 
%Smean = squeeze(mean(st, [1 2]));

sz = size( st );

for i=1:sz(3)
    Smax(i) = single(1.1*max( st(:,:,i),[], "all")); % 1.1 to avoid saturation
    nst(:,:,i) = single(st(:,:,i)  / Smax(i));
end

%mn2 = mean(nst, [1 2]);
end