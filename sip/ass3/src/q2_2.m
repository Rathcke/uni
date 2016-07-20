function q2_2()
    %q2_2a()
    %q2_2b()
    q2_2c()
end

function q2_2a()
    I = imread('lena.tiff');
    h = fspecial('gaussian');
    A = nestedConv(h, I);
    B = fourConv(h, I);
    subplot(1, 3, 1);
        imagesc(I);
        colormap gray;
        title('Original image');
    subplot(1, 3, 2);
        imagesc(A);
        colormap gray;
        title('Image after convolution');
    subplot(1, 3, 3);
        imagesc(B);
        colormap gray;
        title('Image after convolution using FFT');
end

function q2_2b()
    I = imread('lena.tiff');
    fst = [];
    snd = [];
    for i = 1:7
        h = fspecial('gaussian', [i*2+1 i*2+1]);
        tic;
        A = nestedConv(h, I);
        fst(i) = toc;
        tic
        B = fourConv(h, I);
        snd(i) = toc;
    end
    subplot(1, 1, 1);
        plot([3:2:15], fst, [3:2:15], snd);
        xlabel('Window size in n*n');
        ylabel('Computation time in seconds');
        title('Nested for-loop convolution vs. convolution using FFT');
end

function q2_2c()
    I = imread('lena.tiff');
    fst = [];
    snd = [];
    for i = 1:10
        h = fspecial('gaussian');
        I2 = imresize(I, i/10);
        tic;
        A = nestedConv(h, I2);
        fst(i) = toc;
        tic
        B = fourConv(h, I2);
        snd(i) = toc;
    end
    subplot(1, 1, 1);
        plot([0.1:0.1:1], fst, [0.1:0.1:1], snd);
        xlabel('Image size in scale = n/10');
        ylabel('Computation time in seconds');
        title('Nested for-loop convolution vs. convolution using FFT');
end

function res = nestedConv(ker, img)
    x = size(img, 1);
    y = size(img, 2);
    x2 = size(ker, 1);
    y2 = size(ker, 2);
    x3 = fix(x2/2);
    y3 = fix(y2/2);
    res = zeros(x, y);
    for i = 1:x
        for j = 1:y
            acc = 0;
            for k = -x3:x3
                for l = -y3:y3
                    if (i+k > 0 && i+k < x)
                        if (j+l > 0 && j+l < y)
                            acc = acc + ker(k+x3+1, l+y3+1) * img(i+k, j+l);
                        else
                            acc = acc + ker(k+x3+1, l+y3+1) * img(i+k, j-l);
                        end
                    elseif (j+l > 0 && j+l < y)
                        acc = acc + ker(k+x3+1, l+y3+1) * img(i-k, j+l);
                    else
                        acc = acc + ker(k+x3+1, l+y3+1) * img(i-k, j-l);
                    end
                end
            end
            res(i, j) = acc;
        end
    end
end

function res = fourConv(ker, img)
    cumSize = size(img) + size(ker) - 1;
    img(cumSize(1), cumSize(2)) = 0;
    ker(cumSize(1), cumSize(2)) = 0;
    res = ifft2(fft2(img).*fft2(ker));
end
