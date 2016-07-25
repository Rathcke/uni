function res = LSI(A, PSF)
    B = fft2(A); B = fftshift(B);
    H = psf2otf(PSF, size(PSF)); H = fftshift(H);
    g = ifft2(B.*H); g = mat2gray(abs(g));
    
    res = imnoise(g, 'gaussian',0, 0.002);
end