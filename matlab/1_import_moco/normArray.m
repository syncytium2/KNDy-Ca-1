function nA = normArray(A,bounds)
A=single(A);
sz=size(A);
%minA = min(A(bounds/2:sz(1)-bounds/2,bounds/2:sz(2)-bounds/2),[],"all");
%maxA = max(A(bounds/2:sz(1)-bounds/2,bounds/2:sz(2)-bounds/2),[],"all");
P = prctile( ...
    A(round(bounds/2):round(sz(1)-bounds/2), ...
    round(bounds/2):round(sz(2)-bounds/2)), ...
    [0.1,99.9],'all');

nA = (A-P(1))/(P(2)-P(1));
end