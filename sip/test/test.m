A = imread('cell.tif');

figure
imshow(A);
colormap pink;

B = imread('onion.png');
x = size(B, 1);
y = size(B, 2);

for i = 1:x
    for j = 1:y      
        B(i,j,:) = B(i,j,:) - 20;
    end
end