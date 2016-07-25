function [ filter ] = LoG(t, s, sz)
  if mod(sz,2) < 1
    error('Size has to be an odd value');
  end
  filter = zeros(sz,sz);
  offset = ceil(sz/2);
  t2 = t^2;
  s2 = s^2;
  for x = 1:sz
    x2 = (x-offset)^2;
    for y = 1:sz
      y2 = (y-offset)^2;
      filter(x,y) = (t2*exp(-(x2+y2)/(2*s2+2*t2))*(2*s2+2*t2-x2-y2))/(2*pi*(s2+t2)^3);
    end
  end  
end

