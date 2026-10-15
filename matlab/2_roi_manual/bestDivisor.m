function bestDiv = bestDivisor(number,target)
% return the divisor of number closest to the target value
factor = 0.2;
if isprime(number)
    % need the largest remainder
    maxRem=-inf;
    for m = round((1-factor)*target):round((1+factor)*target)
        tempRem = rem(number,m);
        if tempRem > maxRem
            maxRem = tempRem;
            maxm = m;
        end
    end
    bestDiv = maxm;
    msg = [ 'PRIME! looking for max remainder: ', ...
        num2str(maxRem), ...
        ' max divisor: ', num2str(maxm), ...
        ];
    disp(msg)
else
    divs = divisors(number);
    [ ~, index ] = min( abs(divs-target), [], "all" );
    bestDiv = divs(index);
end
end