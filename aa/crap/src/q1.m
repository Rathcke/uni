s = 3;
t = 4;
w = 64;
h = 64;
A = fspecial('gaussian', [w h], s);
AB = real(ifft2(scale(fft2(A), t, 0, 0)));
AB_direct = fspecial('gaussian', [w h], sqrt(s^2+t^2));
subplot(1, 2, 1);
    imagesc(AB);
    colormap gray;
    axis image;
    title('Using the scale function');
subplot(1, 2, 2);
    imagesc(AB_direct);
    colormap gray;
    axis image;
    title('Calculating it directly');
immse(AB, AB_direct)