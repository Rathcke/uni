%% 1.a
clear all;
bw = imread('text.png');
bw = imcrop(bw, [25 20 35 30]);

img  = [0 0 0 0 0 0 0;
        0 0 1 1 1 1 0;
        0 0 1 1 1 1 0;
        0 0 0 1 1 1 0;
        0 0 0 0 0 0 0;
        0 1 1 0 0 0 0;
        0 0 0 0 0 0 0];
%bw = img;

se1 = [1 1 1; 
       0 1 0; 
       0 1 0];
se1 = strel(se1);

se2 = [0 1; 
       1 0];
se2 = strel(se2);

se3 = [0 0 1]; 
se3 = strel(se3);

bw_out1a = imdilate(bw,se1);
bw_out1b = imdilate(bw,se1.reflect);
bw_out2 = imdilate(bw,se2);
bw_out3 = imdilate(bw,se3.reflect);

figure
subplot(1,3,1); 
imshow(bw); title('Original image');
subplot(1,3,2);
imshowpair(bw,bw_out1a); title('Mask 1 not reflected');
subplot(1,3,3);
imshowpair(bw,bw_out1b); title('Mask 1 reflected');


figure
subplot(1,4,1); 
imshow(bw); title('Original image');
subplot(1,4,2)
imshowpair(bw,bw_out1a); title('Mask 1');
subplot(1,4,3)
imshowpair(bw,bw_out2); title('Mask 2');
subplot(1,4,4)
%imshow(bw_out3); title('Mask 3');
imshowpair(bw,bw_out3); title('Mask 3');

%% 1.b 
clear all;

bw = imread('four.bmp');
bw = zeros(15,15);
bw(7,4:10) = 1;

se1 = [1 1 1; 
       0 1 0; 
       0 1 0];

se2 = [0 1;
       1 0];
  
se3 = [0 0 1];

bw_out1 = uint8(imdilate(imrotate(se1, 180),bw));
bw_out2 = uint8(imdilate(se2,bw));
bw_out3 = uint8(imdilate(imrotate(se3, 180),bw));

figure
subplot(1,4,1); 
imshow(bw); title('Original image');
subplot(1,4,2)
imagesc(bw_out1); title('Mask 1'); axis image; axis on;
%imshowpair(bw,bw_out1); title('Mask 1');
subplot(1,4,3)
imagesc(bw_out2); title('Mask 2'); axis image; axis on;
%imshowpair(bw,bw_out2); title('Mask 2');
subplot(1,4,4)
imagesc(bw_out3); title('Mask 3'); axis image; axis on;
%imshowpair(bw,bw_out3); title('Mask 3');
%% 1.c
clear all;

bw = imread('four.bmp');
bw = zeros(15,15);
bw(7,4:10) = 1;

se1 = [1 1 1; 
       0 1 0; 
       0 1 0];

se2 = [0 1; 
       1 0];
  
se3 = [0 0 1]; 

bw_out1 = imdilate(imrotate(se1, 180),bw,'full');
bw_out2 = imdilate(se2,bw,'full');
bw_out3 = imdilate(imrotate(se3, 180),bw,'full');

figure
subplot(1,4,1); 
imshow(bw); title('Original image');
subplot(1,4,2)
imagesc(bw_out1); title('Mask 1'); axis image;
%imshowpair(bw,bw_out1); title('Mask 1');
subplot(1,4,3)
imagesc(bw_out2); title('Mask 2'); axis image;
%imshowpair(bw,bw_out2); title('Mask 2');
subplot(1,4,4)
imagesc(bw_out3); title('Mask 3'); axis image;
%imshowpair(bw,bw_out3); title('Mask 3');
