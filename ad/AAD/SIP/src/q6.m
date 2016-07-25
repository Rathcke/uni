clear all;
I = imread('lena.tif');I = double(I);
noise = 15.*randn(size(I));
PSF = fspecial('motion',21,11);
Blurred = imfilter(I,PSF,'circular');
BlurredNoisy = Blurred + noise; 

NSR = sum(noise(:).^2)/sum(I(:).^2); 

NP = abs(fftn(noise)).^2;
NPOW = sum(NP(:))/prod(size(noise));
NCORR = fftshift(real(ifftn(NP)));

IP = abs(fftn(I)).^2;
IPOW = sum(IP(:))/prod(size(I));
ICORR = fftshift(real(ifftn(IP)));

NSR = NPOW./IPOW;

subplot(1,3,1);imshow(BlurredNoisy,[min(min(BlurredNoisy)) max(max(BlurredNoisy))]);
title('Blurred noisy image');
subplot(1,3,2);imshow(deconvwnr(BlurredNoisy,PSF,NSR),[]);
title('Wiener filtered, scalar est.');
subplot(1,3,3);imshow(deconvwnr(BlurredNoisy,PSF,NCORR,ICORR),[]);
title('Wiener filtered, autocorrelated')
