function [ sinogram ] = myradon(I, M)

angles = 1:180/M:180;

n = length(angles);
sinogram = zeros(size(I,2), n);

for i = 1:n
   %tmp = imrotate(I, 90-angles(i), 'bilinear', 'crop');
   tmp = imrotate(I, angles(i), 'bilinear', 'crop');
   sinogram(:,i) = (sum(tmp))';
end
end

