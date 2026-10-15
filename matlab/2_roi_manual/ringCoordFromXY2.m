function [ eightPairs, vprofs ] = ringCoordFromXY2( im, X0, Y0, r )
% returns 8 pairs of coordinates based on the 
% Full Width Half delta (=max-min) of fluorescence peak
% roughly centered on the coordinates R0,C0

% : im is image 
% : R0, C0 row and column coords from mouse click
%  note that something is not right about row,column = x,y
%  and code required switching row and column to work!
% : r is the max search radius
% matlab (row,column) = (x,y) % origin is top left
[ Ypnts, Xpnts ] = size(im); % size of images is height (Y), width (X)
Xmin = X0 - r; % make sure we don't go off the image!
if(Xmin<1)
    Xmin = 1;
end
Xmax = X0 + r;
if(Xmax>Xpnts) % make sure we don't go off the image!
    Xmax = Xpnts;
end
Ymin = Y0 - r;
if(Ymin<1) % make sure we don't go off the image!
    Ymin = 1;
end
Ymax = Y0 + r;
if(Ymax>Ypnts) % make sure we don't go off the image!
    Ymax = Ypnts;
end
eightPairs = zeros(8,2);
% 20240227 horizontal! xxx vertical, v
Xcoord = [ Xmin, Xmax ];
Ycoord = [ Y0, Y0 ];

%imshow(im); clim('auto')
%hhair = drawcrosshair('Position',[X0,Y0]);
%hline = drawline('Position',[Xmin Y0; Xmax Y0]);
%improfile(im, Rcoord, Ccoord), grid on;

[ vXcoord, vYcoord, vprofile ] = improfile( im, Xcoord, Ycoord );
sz = size(vprofile,1);
vprofs = NaN(4,sz+1);
vprofs(1,1:sz) = vprofile(1:sz);

% local full width at half delta, delta is max-min in profile
twoPairs = localFWHdelta1D2( vXcoord, vYcoord, vprofile, r ); % r is min width
%hpoint1 = drawpoint('Position',[twoPairs(1,1) twoPairs(1,2)],'Color','r');
%hpoint2 = drawpoint('Position',twoPairs(2,:),'Color','g');

eightPairs(1,1) = twoPairs(1,2);
eightPairs(5,1) = twoPairs(2,2);
eightPairs(1,2) = twoPairs(1,1);
eightPairs(5,2) = twoPairs(2,1);

% horizontal, h
Xcoord = [ X0, X0 ];
Ycoord = [ Ymax, Ymin ];
[ vXcoord, vYcoord, vprofile ] = improfile( im, Xcoord, Ycoord );
sz = size(vprofile,1);
vprofs(2,1:sz) = vprofile(1:sz);
twoPairs = localFWHdelta1D2( vXcoord, vYcoord, vprofile, r );
%hpoint3 = drawpoint('Position',twoPairs(1,:),'Color','b');
%hpoint4 = drawpoint('Position',twoPairs(2,:),'Color','c');

eightPairs(3,1) = twoPairs(1,2);
eightPairs(7,1) = twoPairs(2,2);
eightPairs(3,2) = twoPairs(1,1);
eightPairs(7,2) = twoPairs(2,1);

% +45 degrees, p
Xcoord = [ Xmin, Xmax ];
Ycoord = [ Ymax, Ymin ];
[ vXcoord, vYcoord, vprofile ] = improfile( im, Xcoord, Ycoord );
sz = size(vprofile,1);
vprofs(3,1:sz) = vprofile(1:sz);
twoPairs = localFWHdelta1D2( vXcoord, vYcoord, vprofile, r );
%hpoint5 = drawpoint('Position',twoPairs(1,:),'Color','m');
%hpoint6 = drawpoint('Position',twoPairs(2,:),'Color','y');

eightPairs(2,1) = twoPairs(1,2);
eightPairs(6,1) = twoPairs(2,2);
eightPairs(2,2) = twoPairs(1,1);
eightPairs(6,2) = twoPairs(2,1);

% -45 degrees, m
Xcoord = [ Xmax, Xmin ];
Ycoord = [ Ymax, Ymin ];
[ vXcoord, vYcoord, vprofile ] = improfile( im, Xcoord, Ycoord );
sz = size(vprofile,1);
vprofs(4,1:sz) = vprofile(1:sz);
twoPairs = localFWHdelta1D2( vXcoord, vYcoord, vprofile, r );
%hpoint7 = drawpoint('Position',twoPairs(1,:),'Color','k');
%hpoint8 = drawpoint('Position',twoPairs(2,:),'Color','w');

eightPairs(4,1) = twoPairs(1,2);
eightPairs(8,1) = twoPairs(2,2);
eightPairs(4,2) = twoPairs(1,1);
eightPairs(8,2) = twoPairs(2,1);


end