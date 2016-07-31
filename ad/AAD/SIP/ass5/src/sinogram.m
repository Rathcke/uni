%% Originals
clear all;
S = imread('sinogram.png');
subplot(1,3,1); imshow(S); title('Original sinogram.png'); axis image;
S = imrotate(S, 90);
I = imread('box.png');
subplot(1,3,2); imshow(I); title('Original box.png'); axis image;
psrc = zeros(256);
psrc(50, 50) = 255;
subplot(1,3,3); imshow(psrc); title('Non central point'); axis image;

%% Sinograms
M = 180;
R1 = myradon(I, M);
R2 = myradon(psrc, M);

figure
subplot(1,2,1), 
imshow(R1,[]), title('box.png sinogram'), xlabel('M'), ylabel('N')
subplot(1,2,2), 
imshow(R2,[]), title('Non central point source sinogram'), xlabel('M'), ylabel('N')

%% Reconstruction non-filtered
G1 = backproj(R1, M, 0);
G2 = backproj(R2, M, 0);
G3 = backproj(S, M, 0);

figure
subplot(1,3,1)
imagesc(G1); axis image; axis off; title('Reconstructed box.png')
colormap gray;
subplot(1,3,2)
imagesc(G2); axis image; axis off; title('Reconstructed non central point')
colormap gray;
subplot(1,3,3)
imagesc(G3); axis image; axis off; title('Reconstructed sinogram.png')

%% Reconstruction filtered
G1 = backproj(R1, M, 1);
G2 = backproj(R2, M, 1);
G3 = backproj(S, M, 1);

figure
subplot(1,3,1)
imagesc(G1); axis image; axis off; title('Rec. filtered box.png')
colormap gray;
subplot(1,3,2)
imagesc(G2); axis image; axis off; title('Rec. filtered non central point')
colormap gray;
subplot(1,3,3)
imagesc(G3); axis image; axis off; title('Rec. filtered sinogram.png')

%% M-testing 1
figure
colormap(gray);
R = myradon(I, 2); 
B = backproj(R, 2, 1);
subplot(3,3,1); imagesc(B); axis image; axis off; title(sprintf('M = %d', 2));
R = myradon(I, 5); 
B = backproj(R, 5, 1);
subplot(3,3,2); imagesc(B); axis image; axis off; title(sprintf('M = %d', 5));
R = myradon(I, 10); 
B = backproj(R, 10, 1);
subplot(3,3,3); imagesc(B); axis image; axis off; title(sprintf('M = %d', 10));
for i = 1:5
    R = myradon(I, i*20); 
    B = backproj(R, i*20, 1);
    subplot(3,3,i+3); imagesc(B); axis image; axis off; title(sprintf('M = %d', i*20));
end
R = myradon(I, 180); 
B = backproj(R, 180, 1);
subplot(3,3,9); imagesc(B); axis image; axis off; title(sprintf('M = %d', 180));
%% M-testing 2
figure
colormap(gray);
R = S;
B = backproj(R, 2, 1);
subplot(3,3,1); imagesc(B); axis image; axis off; title(sprintf('M = %d', 2));
B = backproj(R, 5, 1);
subplot(3,3,2); imagesc(B); axis image; axis off; title(sprintf('M = %d', 5));
B = backproj(R, 10, 1);
subplot(3,3,3); imagesc(B); axis image; axis off; title(sprintf('M = %d', 10));
for i = 1:5
    B = backproj(R, i*20, 1);
    subplot(3,3,i+3); imagesc(B); axis image; axis off; title(sprintf('M = %d', i*20));
end
B = backproj(R, 180, 1);
subplot(3,3,9); imagesc(B); axis image; axis off; title(sprintf('M = %d', 180));