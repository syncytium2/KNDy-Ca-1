function [out, Amax] = findOutliersMAXb( A, factor )

Amax = projectMax(A);
Amax = im2single(Amax);

maxmean = mean( Amax, 'all' );
maxstd = std( Amax,0,'all');
out = Amax > (maxmean + factor * maxstd);

end
