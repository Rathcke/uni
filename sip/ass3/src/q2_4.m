function q2_4()
    for i = 1:6
        res = scale((i-1)*3+1);
        subplot(2, 3, i);
            imshow(res);
            title(sprintf('With sigma = %d', (i-1)*3+1));
    end
end

function res = scale(arg)
    I = imread('lena.tiff');
    h = fspecial('gaussian', [arg arg], arg);
    res = imfilter(I, h);
end