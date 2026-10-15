function [out, Amax] = findOutliersMAX( A, factor )
% factor = 5 seems to work at first
tic
    
    Amax = projectMax(A);
    Amax = im2single(Amax);
    %sx = size( Amax, 1 );
    %sy = size( Amax, 2 );
    %sz = size( A, 3 );
%    out = zeros( sx, sy, 'logical' );
    maxmean = mean( Amax, 'all' );
    maxstd = std( Amax,0,'all');
%    out = isoutlier( double(A), 'percentile', [0 2] ); %, 'linear' );
    out = Amax > (maxmean + factor * maxstd);
%    out = isoutlier( double(Amax), 'movmedian', factor );

regmax = imregionalmax( Amax );
peaks = Amax .* regmax ;
SD = std( Amax, 0, "all" );
MN = mean( Amax, "all");
regmax2 = regmax;
regmax2( peaks<(MN+factor*SD) ) = 0;

t=tiledlayout(1,2);
nexttile
    imshowpair( Amax, out );
    clim('auto');

    nexttile
    imshow(Amax)
    clim('auto');
    disp(append('find outliers...', num2str(toc)));
end
