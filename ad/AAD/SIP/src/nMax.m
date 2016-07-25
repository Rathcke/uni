function [ xidx, yidx, zidx, vals ] = nMax( imgs, n, mult)
  xidx = zeros(1,n);
  yidx = zeros(1,n);
  zidx = zeros(1,n);
  vals = zeros(1,n);
  [xs, ys, zs] = size(imgs);
  zImgs = ones(xs+2,ys+2,zs+2) * mult;
  zImgs(2:xs+1,2:ys+1,2:zs+1) = imgs;
  bob = zeros(1,zs);
  for z = 2:(zs+1)
    for x = 2:(xs+1)
      for y = 2:(ys+1)
        c = zImgs(x,y,z);
        if c > zImgs(x-1,y,z) &&...
           c > zImgs(x+1,y,z) &&...
           c > zImgs(x,y-1,z) &&...
           c > zImgs(x,y+1,z) &&...
           c > zImgs(x,y,z-1) &&...
           c > zImgs(x,y,z+1)
          bob(z-1) = bob(z-1) + 1;
          val = c;
          for i = 1:n
            if val > vals(i)
              c = val;
              cx = x - 1;
              cy = y - 1;
              cz = z - 1;
              for j = i:n
                k = vals(j);
                kx = xidx(j);
                ky = yidx(j);
                kz = zidx(j);
                vals(j) = c;
                xidx(j) = cx;
                yidx(j) = cy;
                zidx(j) = cz;
                c = k;
                cx = kx;
                cy = ky;
                cz = kz;
              end
              break
            end
          end
        end
      end
    end
  end
  bob
end

