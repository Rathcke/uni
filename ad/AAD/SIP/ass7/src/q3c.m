I = imread('blobs.png');

se1 = strel('diamond', 4);
se2 = strel('disk', 4);
se3 = strel('diamond', 5);
se4 = strel('disk', 5);
se5 = strel('diamond', 6);

I2 = imbothat(I, se1);
I3 = imbothat(I, se2);
I4 = imbothat(I, se3);
I5 = imbothat(I, se4);
I6 = imbothat(I, se5);

subplot(2, 3, 1);
    imshow(I);
    title('Original Image');
subplot(2, 3, 2);
    imshow(I2);
    title('9x9 diamond');
subplot(2, 3, 3);
    imshow(I3);
    title('9x9 disk');
subplot(2, 3, 4);
    imshow(I4);
    title('11x11 diamond');
subplot(2, 3, 5);
    imshow(I5);
    title('11x11 disk');
subplot(2, 3, 6);
    imshow(I6);
    title('13x13 diamond');