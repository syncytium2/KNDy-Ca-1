function bFofT = BarFofTs2( stack, t1 )
%t1 = 100;
%bFofT = zeros( height, width, frames, 'double' );
bFofT = single(stack); % double( stack );
frames = size(stack, 3);
%barFofT = smooth( cropstack(:,:,p), t1 );
%nframes = zeros( size(frames) - [0,0,1] );
ts = t1 / 2;
te = frames - ts;

tic
%temp = ones( height, width, ts );
% temp = mean( bFofT( :, :, 1:ts ), 3 );
% for f = 1:ts
%     bFofT( :, :, f ) = temp;
% end
temp = mean( bFofT(:,:, te:frames), 3 );
for f = te : frames
    bFofT( :, :, f ) = temp;
end
% 20231201 need smoothing up to the beginning!
%for f = ts+1 : te
for f = 1 : te
    if f <= t1
        fs = 1;
        fe = 2*f - 1;
        if fe>10 % hard coded for fast transient
            adj=t1/10;
            fs=f-adj;
            fe=f+adj;
        end
    else
        fs = f - ts;
        fe = f + ts;
    end

  bFofT( :, :, f ) = mean( stack( :, :, fs:fe ), 3 );
end
str = append( 'barFofTs2 time: ', num2str(toc) );
disp( str );
end