function mov = movieTime( stack, cropfactor )
% 8 bit conversion
%[smin, smax] = minmax(stack);
sz=size(stack);
xs=cropfactor*sz(1);
ys=cropfactor*sz(2);
xe=sz(1)-xs;
ye=sz(2)-ys;
stack8b = dbl2uint8( stack(xs:xe,ys:ye,:) );
%[s8bmin, s8bmax] = minmax(stack8b);
% convert to multiframe array
MF8b = stack2MFarray( stack8b );
%[MF8bmin, MF8bmax] = minmax(MF8b);

% write movie
%movname = name;
%success = writevideo( MF8b, movname);
c = jet
mov = immovie( MF8b, c );

end