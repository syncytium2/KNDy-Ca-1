% min bar F
function minBF = minBarF2( barFofT, t2, t4 ) 
% for each time point from the end, 
% look backwards in time over t2 to find the minimum barFofT
% t2 = 500;
height = size( barFofT, 1);
width = size( barFofT, 2);
frames = size(barFofT, 3);

tic
 
% linear indexing
pixels = height * width;

sz = [ height width ];

parstor = zeros(pixels,frames,'single');
for p=1:pixels
    [h,w]=ind2sub(sz,p);
    parstor(p,:)=barFofT(h,w,:);
end
barFofT = [];

parfor p = 1 : pixels
            
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
disp('clear mPofT')

minBF = zeros( height, width, frames);
for p=1:pixels
    [h,w]=ind2sub(sz,p);
    minBF(h,w,:)=parstor(p,:);
end

str = append('min bar f of t: ', num2str(toc) );
disp(str);

end
