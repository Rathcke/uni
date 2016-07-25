function [ xidx, yidx, vals ] = nBrightest( img , n )
  [xs, ys] = size(img);

  xidx = zeros(1, n);
  yidx = zeros(1, n);
  vals = zeros(1, n);
  for x = 1:xs
    for y = 1:ys
      for i = 1:n
        if img(x,y) > vals(i)
          c = img(x,y);
          cx = x;
          cy = y;
          for j = i:n
            k = vals(j);
            kx = xidx(j);
            ky = yidx(j);
            vals(j) = c;
            xidx(j) = cx;
            yidx(j) = cy;
            c = k;
            cx = kx;
            cy = ky;
          end
        end
      end
    end
  end
end

