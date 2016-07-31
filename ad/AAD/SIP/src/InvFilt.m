function [f OTF] = InvFilt(I, PSF)
OTF = fftshift(psf2otf(PSF)); 

G = fftshift(fft2(I));

indices = find(OTF > 1e-6);

F = zeros(size(G)); 
F(indices) = G(indices)./OTF(indices);
f = abs(ifft2(F));
end