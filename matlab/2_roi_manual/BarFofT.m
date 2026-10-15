function bFofT = BarFofT( stack, t1 )
%t1 = 100;
%bFofT = zeros( height, width, frames, 'double' );
bFofT = double( stack );
frames = size(stack, 3);
%barFofT = smooth( cropstack(:,:,p), t1 );
%nframes = zeros( size(frames) - [0,0,1] );
ts = t1 / 2;
te = frames - ts;

tic
%temp = ones( height, width, ts );
temp = mean( bFofT( :, :, 1:ts ), 3 );
for f = 1:ts
    bFofT( :, :, f ) = temp;
end
temp = mean( bFofT(:,:, te:frames), 3 );
for f = te : frames
    bFofT( :, :, f ) = temp;
end

for f = ts+1 : te
  fs = f - ts;
  fe = f + ts;
  bFofT( :, :, f ) = mean( stack( :, :, fs:fe ), 3 );
end
str = append( 'barFofT time: ', num2str(toc) );
disp( str );
end