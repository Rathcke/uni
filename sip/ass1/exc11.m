A = randn(20, 20);
B = mat2gray(A);
im2bw(B);
subplot(2,2,1),imshow(B);
[x, y] = ginput(1);