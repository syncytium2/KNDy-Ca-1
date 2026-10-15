% star strider's findlevel function from Igor
% modified to avoid crash if there aren't 3 points for interpolation
%https://www.mathworks.com/matlabcentral/answers/1770785-looking-for-a-way-to-copy-an-igor-pro-function-in-matlab
function xv = FindLevel(x,y,level)
    cxi = find(diff(sign(y-level)));
    for k = 1:numel(cxi)
        if cxi(k)==1 || abs( y( cxi(k)-1 ) - y( cxi(k) ) ) < 1e-6 % not enough different points
            idxrng(1)=cxi(k);
            idxrng(2)=cxi(k)+1;
        else
            idxrng = max(1,cxi(k)-1) : min(numel(x), cxi(k)+1);
        end
        xv(k) = interp1(y(idxrng), x(idxrng), level);
    end
end