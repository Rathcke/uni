%% 2.c
H = @(t,s) (-2*t^2)/((2*s^2+2*t^2)*pi*(s^2+t^2));
X = -40:40;
Y10 = arrayfun(@(x) H(x,10) ,X);
Y20 = arrayfun(@(x) H(x,20) ,X);
Y30 = arrayfun(@(x) H(x,30) ,X);
subplot(1,3,1); plot(X,Y10); title('\sigma = 10');
subplot(1,3,2); plot(X,Y20); title('\sigma = 20');
subplot(1,3,3); plot(X,Y30); title('\sigma = 30');
%% 2.d
I = imread('sunflower.tiff');
sigmas = 15:2:35;
imgs = scaleSpace(I,sigmas);

[xb,yb,zb] = nMax(imgs, 20, 0);

negImgs = scaleSpace(255 - I,sigmas);
[xd,yd,zd] = nMax(negImgs,20, 255);
imshow(I);
for i = 1:20
  d = sigmas(zb(i))*2;
  rectangle('Position',[yb(i)-d/2 xb(i)-d/2 d d], 'Curvature', [1 1], 'EdgeColor','r');
  d = sigmas(zd(i))*2;
  rectangle('Position',[yd(i)-d/2 xd(i)-d/2 d d], 'Curvature', [1 1], 'EdgeColor','b');
end