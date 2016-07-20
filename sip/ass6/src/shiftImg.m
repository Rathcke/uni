function res = shiftImg(img, a, b)
    x = size(img, 1);
    y = size(img, 2);
    res = imtranslate(img, [a b]);
end