function [x, y] = selectDataPoint(~, ax)
    roi = drawpoint(ax);
    x = roi.Position(1);
    y = roi.Position(2);
    %ax.Position
end