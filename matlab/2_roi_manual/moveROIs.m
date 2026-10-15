function h = moveROIs( dx, dy, h, axe )
% dx, dy is movement in pixels 
% h contains the ROI handles
% axe is the image axes handle

    msize = 1;
    lsize = 1;
    nROIs = size(h,2);
    for i = 1:nROIs

        h(i).ROI = h(i).ROI + [ dx, dy ]; %h(i).handles.ROI.Position; % roi;
        h(i).deadzone = h(i).deadzone + [ dx, dy ]; %handles.deadzone.Position; %dead;
        h(i).penumbra = h(i).penumbra + [ dx, dy ]; %.handles.penumbra.Position; %pen;

        movees = findobj( axe, 'Tag', num2str(i) );
        for k = 1:length(movees)
            switch class(movees(k))
                case 'matlab.graphics.primitive.Text'
                    movees(k).Position = movees(k).Position + [dx, dy, 0];
                case 'images.roi.Polygon'
                    movees(k).Position = movees(k).Position + [dx, dy ];
                otherwise
                    disp('*** cannot move this object', class(movees(k)))
            end
        end

        h(i).mask.ROI = createMask(h(i).handles.ROI);
        h(i).mask.deadzone = createMask(h(i).handles.deadzone);
        h(i).mask.penumbra = createMask(h(i).handles.penumbra);
 
    end
end
