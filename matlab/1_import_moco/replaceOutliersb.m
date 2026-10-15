function cleaned = replaceOutliersb( A, outliers )
    % out is a logical array identifying outliers to limit search
    tic
    sx = size( A, 1 );
    sy = size( A, 2 );
    cleaned = A; %zeros( sx, sy, sz, 'uint16');
    meanround = zeros(1,size(A,3));
    for i = 1 : sx 
        for j = 1 : sy
            if outliers(i,j) == 1
                % suround mean from Eve
                x=i;
                y=j;
                offset = 1;
                sz=size(A);
                count=0;
                meanround(1,:)=0;
                for k = x-offset:1:x+offset
                    for l = y-offset:1:y+offset
                        if k<1
                            k=1;
                        end
                        if l<1
                            l=1;
                        end

                        if ~((k==x) && (l==y)) && ( k <= sz(1) ) && ( l <= sz(2) ) % this is wrong
                            count = count+1;
                            for m = 1:sz(3)
                                    meanround(1,m) = meanround(1,m) + A(k,l,m);
                            end
                        end
                    end
                end
                meanround = meanround/count; %replace outliers with the surrounding 8 pixels.
                cleaned( i, j, : ) = meanround; % uint16( vc );
            end
        end
    end
end
