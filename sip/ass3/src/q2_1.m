function q2_1()
    I = imread('lena.tiff');    
    ps = 10*log10(fftshift(abs(fft2(I)).^2));
    subplot(1,2,1);
        imagesc(I);
        title('Original image');
        colormap gray;
    subplot(1,2,2);
        imagesc(ps);
        title('Power spectrum of the image');
        colormap gray;
end