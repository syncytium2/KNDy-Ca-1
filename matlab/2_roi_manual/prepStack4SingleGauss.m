function Y = prepStack4SingleGauss(Y)
% remove baseline trend, normalize 
    Y = single(Y);
    Y = subMinStack( Y );
    Y = normStack( Y );

    % gaussian filter below stolen from normCorr
    gSig = 3; %7; 
    gSiz = 3*gSig; 
    psf = fspecial('gaussian', round(2*gSiz), gSig);
    ind_nonzero = (psf(:)>=max(psf(:,1)));
    psf = psf-mean(psf(ind_nonzero));
    psf(~ind_nonzero) = 0;   % only use pixels within the center disk
    Y = imfilter( Y, psf, 'symmetric' );

end