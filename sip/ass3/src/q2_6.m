function q2_6()
    I = imread('lena.tiff');
    for i = 0:2
        for j = 0:2
            A = deriveImg(I, i, j);
            subplot(3, 3, 3*i+j+1);
            imagesc(A);
            colormap gray;
            title(sprintf('With n = %d and m = %d', i, j));
        end
    end
end

function res = deriveImg(img, n, m)
    ft = fft2(img);
    x = size(ft, 1);
    y = size(ft, 2);
    for i = 1:x
        for j = 1:x
            ftd(i, j)= (1j * i).^n *(1j * j).^m * ft(i, j);
        end
    end
    res = real(ifft2(ftd));
end