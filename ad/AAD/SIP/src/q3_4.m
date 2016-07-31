I = imread('hand.tiff');
zeros(size(I, 1), size(I, 2), 100)
for i = 1:100

subplot(1, 1, 1);
    imshow(I);
end

function res = dsa(x, y, t)
    t/(sqrt(2*pi*s^2))*1/(2*pi*(s^2+t^2))*exp(-x/(2*
end