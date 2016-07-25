clear all;

I = imread('flowers.jpg');
I_orig = I;
subplot(3,3,1); imshow(I); title('Original');
%Grayscale
I = rgb2gray(I);
subplot(3,3,2); imshow(I); title('1: Grayscale');

%Blur /w gauss
blur = fspecial('gaussian', 11, 10);
I = I - imfilter(I, blur, 'replicate');
subplot(3,3,3); imshow(I); title('2: Removing blur');

%Edge detection
I_e = imerode(I, strel('diamond', 1));
I_d = imdilate(I, strel('diamond',1));
I = I_d - I_e;
subplot(3,3,4); imshow(I); title('3: Edge detection');

%Dilation followed by erosion
I = imdilate(I, strel('diamond', 2));
I = imerode(I, strel('disk', 2));
subplot(3,3,5); imshow(I); title('4: Dilation then erosion');

%Threshold
I = I > 4;
subplot(3,3,6); imshow(I); title('5: Thresholding');

%Island removal
I = imopen(I, strel('diamond',3));
subplot(3,3,7); imshow(I); title('6: Island removal');

%Filling
I = imdilate(I, strel('diamond',5));
I = imclose(I, strel('diamond', 3));
I = imerode(I, strel('diamond', 4));
subplot(3,3,8); imshow(I); title('7: Filling');

%Masking
subplot(3,3,9); k = imshow(I_orig); title('8: Masking'); set(k ,'AlphaData', I);
