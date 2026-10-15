function h2 = drawROI( h, axe )
% h is a handle structure containing AT LEAST .ROI, .deadzone and .penumbra
% these contain coordinates of the actual ROI, deadzone and penumbra 
% (NOT THE POLYGON HANDLES!)
% axe is axes handle

    msize = 0.1;
    lsize = 0.1;
    nROIs = size(h,2);
    for i = 1:nROIs
        h2(i).ROI = h(i).ROI; %h(i).handles.ROI.Position; % roi;
        h2(i).deadzone = h(i).deadzone; %handles.deadzone.Position; %dead;
        h2(i).penumbra = h(i).penumbra; %.handles.penumbra.Position; %pen;

        hROI = drawpolygon(axe,'Position', h2(i).ROI, 'FaceAlpha', 0, 'LineWidth', lsize, 'MarkerSize', msize, 'Tag', num2str(i) );
        hDead = drawpolygon(axe,'Position', h2(i).deadzone, 'LineWidth', lsize, 'MarkerSize', msize, 'Color', [1,1,1], 'Tag', num2str(i), FaceAlpha=0 );
        hPen = drawpolygon(axe,'Position', h2(i).penumbra, 'LineWidth', lsize, 'MarkerSize', msize,  'Color', [1,0,1], 'Tag', num2str(i), FaceAlpha=0 );

        h2(i).mask.ROI = createMask(hROI);
        h2(i).mask.deadzone = createMask(hDead);
        h2(i).mask.penumbra = createMask(hPen);

        h2(i).handles.ROI = hROI; %drawpolygon(axe,'Position', h2(i).ROI, 'FaceAlpha', 0, 'LineWidth', lsize, 'MarkerSize', msize );
        h2(i).handles.deadzone = hDead; %drawpolygon(axe,'Position', h2(i).deadzone, 'LineWidth', lsize, 'MarkerSize', msize, 'Color', [1,1,1], FaceAlpha=0 );
        h2(i).handles.penumbra = hPen; %drawpolygon(axe,'Position', h2(i).penumbra, 'LineWidth', lsize, 'MarkerSize', msize,  'Color', [1,0,1], FaceAlpha=0 );
        M =  mean( h2(i).ROI ); %hROI(i).handles.ROI.Position   ) ; % get the center of the round ROI
        text( axe, M(1), M(2), num2str(i), ...
            'HorizontalAlignment', 'center', ...
            'Tag', num2str(i), ...
            'Color', 'r', ...
            'Clipping','on' )
    end
end