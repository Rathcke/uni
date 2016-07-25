% The code below takes a matrix of projections and returns
% a matrix with filtered projections whose Fourier Transform
% is |w|.  By uncommenting the vairous filters, various aspects
% of the reconstructed picture will be emphasized or lost.

function FIL = projfilter(Image)

[L,C]=size(Image);

w = [-pi : (2*pi)/L : pi-(2*pi)/L];
Filt = abs(sin(w));

% Below we use the Fourier Slice Theorem to filter the image
for i = 1:C,
    IMG = fft(Image(:,i));
    FiltIMG = IMG.*Filt';
    FIL(:,i) = ifft(FiltIMG);
end
FIL = real(FIL);


