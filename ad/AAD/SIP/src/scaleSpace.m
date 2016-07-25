function [ imgs ] = scaleSpace( img, sigmas )
  filter = fspecial('log', ceil(sigmas(1)*3)*2+1,sigmas(1));
  imgs = conv2(double(img) .* (sigmas(1)^2), double(filter), 'same');
  for i = 2:length(sigmas)
    filter = fspecial('log', ceil(sigmas(i)*3)*2+1,sigmas(i));
    imgs(:,:,i) = conv2(double(img) .* (sigmas(i)^2), double(filter), 'same');
  end
end

