% calculate dF over F0
function [dFoverF0,minBF] = deltaFoverFzero( stack, t1, t2, t4 ) %, minBarF )

% bar F "smoothing"
disp('starting BarFofT');
BarF = BarFofTs2( stack, t1 ); % s version uses single precision

disp('starting minBarF. Prepare to wait a long while...');
minBF = minBarF2( BarF, t2, t4 );

% F0 is minBF
% df is raw - minBF
%height = size( stack, 1);
%width = size( stack, 2);
%frames = size( stack, 3);

%dFoverF0 = zeros( height, width, frames );
dblstack = double( stack );

disp('starting dF/F0');

tic
dFoverF0 = dblstack - minBF;
disp(append(' F-F0: ', num2str(toc) ) );
tic
dFoverF0 = dFoverF0 ./ minBF; % ./ means by element, otherwise need a scalar
disp(append(' dF / F0: ', num2str(toc) ) );

%plot2axes( stack, BarF, minBF, dFoverF0, 1, 1 );
%plot2axes( stack, BarF, minBF, dFoverF0, 2, 1 );
%tic
%enhanced = histeq( dFoverF0);
%toc
%implay(dFoverF0);
end                 