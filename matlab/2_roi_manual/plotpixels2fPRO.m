% plot a pixel over time
function plotpixels2fPRO( f, s1, s2, s3, x, y )
frames = size( s1, 3 );
xx = linspace(1,frames,frames);
p1 = zeros( frames, 1 );
p1(:,1) = s1( x, y, : );
p2 = zeros( frames, 1 );
p2(:,1) = s2( x, y, : );
p3 = zeros( frames, 1 );
p3(:,1) = s3( x, y, : );

figure(f)
yyaxis left

plot(xx,p1)
ylabel(inputname(2))
%plot(xx,sgf)
yyaxis right
plot(xx, p2, xx, p3)
ylabel(append(inputname(3),' and ',inputname(4)))


end