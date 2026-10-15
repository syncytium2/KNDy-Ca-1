function h = structuredROIax( x, y, im, fax, r, deadzone, penumbra, expand )
% x, y position of click, im is an image (usually max projection), 
% fax is the target axes for drawing polygons,
% r is radius, offset the gap between ROI and deadzone, and deadzone and
% penumbra
% expand (pixels) expands/shrinks the FWHM based ROI, optional
if nargin<8
    expand = 0;
end

% creates ROI, deadzone, and penumbra coordinates
% returns in h: 3 sets of coords, polygon handles
%r = 20;
deadzoneOffset = deadzone;
penumbraOffset = penumbra;

h.xy = [ x, y ];
[ xxx, vp ] = ringCoordFromXY2( im, x, y, r );
h.ROI = xxx; % coordinates of ROI, 8x2

% flip x and y: WHY!?! 
xxx(:,1)=h.ROI(:,2);
xxx(:,2)=h.ROI(:,1);
h.vp = vp; % stores the profiles used to auto generate ROI

factor = sqrt(0.5); % conversion from hypoteneuse to catheti
% the order must correspond to what comes out of ringCoordFromXY2!!!
dilation =[-1,0; -factor,+factor; 0,1; factor,factor; 1,0; factor,-factor; 0, -1; -factor,-factor];

msize = 1; 
lsize = 1;

xxx =  xxx + expand * dilation; % 20240501 mod to enhance size of ROI independent of FWHM
h.ROI = xxx;
h.handles.ROI = drawpolygon(fax,'Position', xxx, 'FaceAlpha', 0, 'LineWidth', lsize, 'MarkerSize', msize );

h.mask.ROI = createMask(h.handles.ROI);
deadzone = xxx + deadzoneOffset * dilation;
h.deadzone = deadzone;
h.handles.deadzone = drawpolygon(fax,'Position', deadzone, 'LineWidth', lsize, 'MarkerSize', msize, 'Color', [1,1,1], FaceAlpha=0 );
h.mask.deadzone = createMask(h.handles.deadzone);
penumbra = deadzone + penumbraOffset * dilation;
h.penumbra = penumbra;
h.handles.penumbra = drawpolygon(fax,'Position', penumbra, 'LineWidth', lsize, 'MarkerSize', msize,  'Color', [1,0,1], FaceAlpha=0 );
h.mask.penumbra = createMask(h.handles.penumbra);

end