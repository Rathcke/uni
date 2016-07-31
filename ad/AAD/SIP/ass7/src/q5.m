I = imread('test_digits.bmp');
zero  = imread('zero.bmp');
one   = imread('one.bmp');
two   = imread('two.bmp');
three = imread('three.bmp');
four  = imread('four.bmp');
five  = imread('five.bmp');
six   = imread('six.bmp');
seven = imread('seven.bmp');
eight = imread('eight.bmp');
nine  = imread('nine.bmp');

hm = bwhitmiss(I, five, ~five);

subplot(1, 2, 1);
    imshow(I);
    title('Original image');
subplot(1, 2, 2);
    imshow(hm);
    title('Hit-or-miss operation');