function [proj] = MeanMaxSTD( stackin, type )
% stackin is a single stack
% type is mean, max, or std
switch type
    case 'mean' 
        proj = mean( stackin, 3 );
    case 'max'
        proj = max( stackin, [], 3 );
    case 'std'
        stackin = single(stackin);
        proj = std( stackin, 0, 3 );
    case 'all'
        mean_proj = mean( stackin, 3 );
        max_proj = max( stackin, [], 3 );
        stackin = single(stackin);
        std_proj = std( stackin, 0, 3 );
        proj = { mean_proj, max_proj, std_proj };
    otherwise
        proj = [];
end
end

