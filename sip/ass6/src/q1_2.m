I = zeros(77, 77);
x = size(I, 1);
y = size(I, 2);
for i = 0:2
    for j = 0:2
        I(x/2-0.5+i, y/2-0.5+j) = 255;
    end
end
I2 = shiftImg(I, 30, 30);
subplot(1, 2, 1);
    imagesc(I);
    title('Original image');
    colormap gray;
    axis image;
subplot(1, 2, 2);
    imagesc(I2);
    title('Image after translation');
    colormap gray;
    axis image;