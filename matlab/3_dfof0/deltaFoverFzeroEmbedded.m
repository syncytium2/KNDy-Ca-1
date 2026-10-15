% calculate dF over F0, checks minBarF for <= 0
function [dFoverF0,minBF] = deltaFoverFzeroEmbedded( stack, t1, t2, t4 ) %, minminBF )

% bar F "smoothing"
%disp('starting BarFofT');

BarF = BarFofTse;
    function BarF = BarFofTse %( stack, t1 )%; % s version uses single precision

        %t1 = 100;
        %bFofT = zeros( height, width, frames, 'double' );
        BarF = single(stack); % double( stack );
        frames = size(stack, 3);
        %barFofT = smooth( cropstack(:,:,p), t1 );
        %nframes = zeros( size(frames) - [0,0,1] );
        ts = floor(t1 / 2);
        te = ceil(frames - ts);

        %tic
        %temp = ones( height, width, ts );
        temp = mean( BarF( :, :, 1:ts ), 3 );
        for f = 1:ts
            BarF( :, :, f ) = temp;
        end
        temp = mean( BarF(:,:, te:frames), 3 );
        for f = te : frames
            BarF( :, :, f ) = temp;
        end

        for f = ts+1 : te
            fs = f - ts;
            fe = f + ts;
            BarF( :, :, f ) = mean( stack( :, :, fs:fe ), 3 );
        end
        %str = append( 'barFofT time: ', num2str(toc) );
        %disp( str );
    end

%disp('starting minBarF. Prepare to wait a long while...');
minBF = minBarF2e; %( BarF, t2, t4 );

    function minBF = minBarF2e %( barFofT, t2, t4 )
        % for each time point from the end,
        % look backwards in time over t2 to find the minimum barFofT
        % t2 = 500;
        [height,width,frames] = size( BarF );

        %tic

        % linear indexing
        pixels = height * width;

        sz = [ height width ];

        parstor = zeros(pixels,frames,'single');
        for p=1:pixels
            [h,w]=ind2sub(sz,p);
            parstor(p,:) = BarF(h,w,:);
        end

        parfor p = 1 : pixels
            %for p = 1 : pixels
            mPofT = parstor(p,:); %barFofT( h, w, : );
            mPofT = squeeze(mPofT); % = reshape( PofT, frames);

            i = frames; % start at the end

            % set the initial min and minloc to absolute min, should be near
            % the end of the sweep
            [prevmin, prevminloc] = min( mPofT );
            while i > 0

                is = i-t2;
                if(is<1)
                    is=1;
                end

                [ fmin, fminloc ] = min( mPofT(is:i) );
                fminloc = fminloc + is - 1; % adds the offset for indexing the subarray
                dx = i - fminloc; % distance to min, width of event roughly

                if ( dx > t4 ) && ( prevminloc > fminloc ) % ( minflag == true )  % use slope
                    slope_dx = prevminloc - fminloc;
                    slope_dy = prevmin - fmin;
                    slope = slope_dy / slope_dx;
                    slope_fill = fmin + linspace(0,slope_dx+1,slope_dx+1) * slope;
                    mPofT(fminloc:prevminloc) = slope_fill;

                else % do not use slope
                    mPofT(fminloc:i) = fmin;
                end
                if ( (prevminloc-fminloc) > 2*t4 )
                    prevmin = fmin;
                    prevminloc = fminloc;
                    %disp(append('updating prevmin: ',num2str(prevmin), ' , ', num2str(prevminloc)))
                end
                i = fminloc - 1; % this is a time saver, advances to the min value

            end % while loop

            parstor(p,:) = mPofT;

        end % for or parfor loop
        clear mPofT % no longer needed
        %disp('clear mPofT')

        minBF = zeros( height, width, frames, 'single');
        for p=1:pixels
            [h,w]=ind2sub(sz,p);
            minBF(h,w,:)=parstor(p,:);
        end

    end

singleStack = single( stack ); % double( stack );

dFoverF0 = singleStack - minBF;

dFoverF0 = dFoverF0 ./ minBF; % ./ means by element, otherwise need a scalar

end