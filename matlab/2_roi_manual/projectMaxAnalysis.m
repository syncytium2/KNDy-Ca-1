
function MaxProj = projectMaxAnalysis( stackY1, stackY2a, stackY2b )
% max projection, sd projection?
tic
MaxProj = max( stackY1, [], 3 );
%MaxProj2 = histeq( MaxProj );
df = figure; % target for data plot
imgf = figure;
mp = imshow( MaxProj );
clim( 'auto' );
dcm = datacursormode; % use cursor to id pixel for plot
dcm.Enable = 'on';
dcm.Updatefcn = {@getDataTipsTDPRO MaxProj stackY1 stackY2a stackY2b imgf df};
toc
end

%MeanProj = mean( stack1, 3);
%StdProj = std( single(stack1), 0, 3);

% StdProj = std( stack1, 0, 3 ); % this is not how std works!
% StdProj2 = histeq( StdProj );
% figure
% imshowpair( MaxProj2, StdProj2 );