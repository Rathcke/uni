function q2_3()
    I = imread('lena.tiff');
    I2 = addFunc(I, 50, 0.25, 0.25);
    subplot(2,2,1);
        imshow(I);
        title('Original image');
        colormap gray;
    subplot(2,2,2);
        imshow(I2);
        title('Image with added function');
        colormap gray;
    ps = 10*log10(fftshift(abs(fft2(I2)).^2));
    subplot(2,2,3);
        imagesc(ps);
        title('Power Spectrum of modified image');
        colormap gray;
    ps = fftshift(fft2(I2));
    ps(105, 105) = 0;
    ps(123, 123) = 0;
    subplot(2,2,4);
        imagesc(abs(real(ifft2(ps))));
        title('Image after removing cosine noise');
        colormap gray;
end

function res = addFunc(img, a, v, w)
    x = size(img, 1);
    y = size(img, 2);
    for i = 1:x
        for j = 1:y
            res(i, j) = img(i, j) + a*cos(v*i+w*j);
        end
    end
end