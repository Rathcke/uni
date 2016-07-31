function [BPI] = backproj(PR, M, Filter)

THETA = 1:180/M:180;

% figure out how big our picture is going to be.
n = size(PR,1);
sideSize = n;

% filter the projections
if Filter;
  filtPR = projfilter(PR);
else
  filtPR = PR;
end

% convert THETA to radians
th = (pi/180)*THETA;
m = length(THETA); 

% set up the image
BPI = zeros(sideSize,sideSize);

% find the middle index of the projections
midindex = (n+1)/2;

% set up x and y matrices
x = 1:sideSize;
y = 1:sideSize;
[X,Y] = meshgrid(x,y);
xpr = X - (sideSize+1)/2;
ypr = Y - (sideSize+1)/2;

% loop over each projection
for i = 1:m
    % figure out which projections to add to which spots
    filPR = imrotate(filtPR, THETA(i), 'bilinear', 'crop');
    filtIndex = round(midindex + xpr*sin(th(i)) - ypr*cos(th(i)));

    % if we are "in bounds" then add the point
    BPIa = zeros(sideSize,sideSize);
    spota = find((filtIndex > 0) & (filtIndex <= n));
    newfiltIndex = filtIndex(spota);
    BPIa(spota) = filtPR(newfiltIndex(:),i);
    BPI = BPI + BPIa; 
end
BPI = imrotate(BPI, -90, 'bilinear', 'crop');