function cleaned = replaceOutliers( A, outliers )
    % out is a logical array identifying outliers to limit search
    tic
    sx = size( A, 1 );
    sy = size( A, 2 );
    % sz = size( A, 3 );
    cleaned = A; %zeros( sx, sy, sz, 'uint16');

    meanround = zeros(1,size(A,3));
    for i = 1 : sx 
        for j = 1 : sy
            if outliers(i,j) == 1
                % v = double( A( i, j, : ) );
                % vc = filloutliers( v, 'linear' );
                % suround mean from Eve

                x=i;
                y=j;
                disp(append(" before x: ", num2str(i)," y: ", num2str(j)," val: ", num2str(A(x,y,1))));
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
%  ~((k==x)&&(l==y))
                        if ~((k==x) && (l==y)) && ( k <= sz(1) ) && ( l <= sz(2) ) % this is wrong
                            count = count+1;
                            for m = 1:sz(3)
                                    meanround(1,m) = meanround(1,m) + A(k,l,m);
                            end
                            disp(append("x: ", num2str(k)," y: ", num2str(l)," val: ", num2str(A(k,l,1))));
                        end
                    end
                end
                meanround = meanround/count; %replace outliers with the surrounding 8 pixels.
                cleaned( i, j, : ) = meanround; % uint16( vc );
                disp(append("after x: ", num2str(i)," y: ", num2str(j)," val: ", num2str(cleaned(i,j,1))," count ", num2str(count)));
                %plotMultiY(v,vc);
                %pause
            end
        end
    end
    disp(append('replace outliers...', num2str(toc)));
end
