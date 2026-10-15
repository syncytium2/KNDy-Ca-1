function [meanp,maxp,stdp] = projMeanMaxSTD( stackin )
stackin = single(stackin); %double( stackin );
meanp = mean( stackin, 3 );
maxp = max( stackin, [], 3 );
stdp = std( stackin, 0, 3 );
end

