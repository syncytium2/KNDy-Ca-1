function txt = getDataTipsTDPRO(~, info, plane, stack1, stack2, stack3, ifig, dfig)
    %disp(info);
    %disp( append('testcallback: ', num2str(info.Position(1)), num2str(info.Position(2)) ) );
    y = info.Position(1);
    x = info.Position(2);
    %info.target
    %img = getimage(info.target);
    z = plane(x, y);
    txt = ['(' num2str(x) ', ' num2str(y) ', ' num2str(z) ')'];
    plotpixels2fPRO( dfig, stack1, stack2, stack3, x, y );
    figure(ifig)
end