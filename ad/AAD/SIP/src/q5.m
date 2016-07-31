clear all;

A = imread('lena.tif');
PSF = fspecial('gaussian', size(A), 4);

I = LSI(A,PSF);

[f, otf] = InvFilt(I, PSF);

figure
subplot(1,4,1)
imshow(I, [min(min(I)) max(max(I))]); title('LSI degraded image');
subplot(1,4,2)
imagesc(abs(PSF)); axis square; axis off; title('PSF');
subplot(1,4,3)
imagesc(abs(otf)); axis square; axis off;title('OTF');
subplot(1,4,4)
imagesc(f);axis square; axis off;title('Inverse filtering');

B = fft2(A); B = fftshift(B);
H = psf2otf(PSF, size(PSF)); H = fftshift(H);
g = ifft2(B.*H); g = abs(g);
[f, otf] = InvFilt(g, PSF);

figure
subplot(1,4,1)
imshow(g, [min(min(g)) max(max(g))]); title('Gauss blurred image');
subplot(1,4,2)
imagesc(abs(PSF)); axis square; axis off;title('PSF');
subplot(1,4,3)
imagesc(abs(otf)); axis square; axis off;title('OTF');
subplot(1,4,4)
imagesc(f);axis square; axis off;title('Inverse filtering');
