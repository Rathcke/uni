function Is = scale(I,s,dr,dc);
%SCALE  Gaussian Scale-Space using the Fourier Domain
%
%       Is = scale(I,s,dr,dc)
%         I - the Fourier transform of a matrix or an image
%         s - the standard deviation of the Gaussian (must be larger than 0)
%         dr - the derivative order in the direction of the rows
%         dc - the derivative order in the direction of the columns
%
%       This is an implementation of the Gaussian scale space on matrices.
%       The convolution is implemented in the Fourier domain and for that
%       reason the number of rows and columns of the matrix must be powers
%       of 2.
%       Fractional valued dr and dc are possible, but be warned the result
%       will probably be complex.
%       The complexity of this algorithm is O(n) where n is the total number
%       of elements of I.
%
%       To calculate an image of scale (variance) 2^2 use,
%         I2 = real(ifft2(scale(fft2(I),2,0,0)));
%       To derive an image once in the direction of rows at scale 1^2 do,
%         I2 = real(ifft2(scale(fft2(I),1,1,0)));
%
%       Kim S. Pedersen has corrected a minor bug N-1 -> N, May 30, 1999
%       and tested the output against the tgv program, and it delivers the
%       same output.
%
%       Niels H. Olsen has suggested to loosen the constraints of power of 2
%       image sizes.  The code now accepts all images of even sizes.  I'm
%       not sure that the code is optimal for non power of 2 images, but a
%       simple test of cutting before or after scale appears to result only
%       in minor differences.  April 18, 2001.
%
%       I've corrected a minor bug concerning 1D signals.  April 17, 2002.
%
%       Copyright: Jon Sporring, January 1, 1996

if (s < 0)
  error('s must be larger than or equal 0');
else 
  if s == 0
    if (dr == 0) & (dc == 0)
      Is = I;
    else
      Is = zeros(size(I));
    end
  else
    rows = size(I,1);
    cols = size(I,2);

    G = zeros(rows,cols);
    if s == Inf
      G(1,1) = 1;
      Is = I.*G;
    else
      if((rows > 2 & rem(rows,2) ~= 0) | (cols > 2 & rem(cols,2) ~= 0))
	error('The image must have even side lengths');
      else
	% Calculate the Fourier transform of a gaussian fct.
	if (rows > 1) & (cols > 1) 
	  % 2 dimensional image
	  G(1:rows/2+1, 1:cols/2+1) = exp(-(repmat(([0:rows/2]'/rows).^2,[1,cols/2+1])+repmat(([0:cols/2]/cols).^2,[rows/2+1,1]))*(s*2*pi)^2/2);  
	  G(rows/2+1:rows, 1:cols/2+1) = flipud(G(2:rows/2+1, 1:cols/2+1));
	  G(1:rows/2+1, cols/2+1:cols) = fliplr(G(1:rows/2+1, 2:cols/2+1));
	  G(rows/2+1:rows, cols/2+1:cols) = fliplr(flipud(G(2:rows/2+1, 2:cols/2+1)));
	else
	  % 1 dimensional image
	  [val,ind] = max([rows,cols]);
	  G(1:val/2+1) = exp(-([0:val/2]'/val).^2*(s*2*pi)^2/2);
	  G(val/2+1:val) = flipdim(G(2:val/2+1),ind);
	end
	
	% Calculate the Differentiation matrix
	j = sqrt(-1);
	if (rows > 1) & (cols > 1)
	  x = [0:rows/2-1,-rows/2:-1]/rows;
	  y = [0:cols/2-1,-cols/2:-1]/cols;
	  DG = (x.^dr)'*(y.^dc)*(j*2*pi)^(dr+dc);
	else
	  if rows > 1
	    x = [0:rows/2-1,-rows/2:-1]/rows;
	    DG = (j*2*pi*x').^dr;
	  else
	    y = [0:cols/2-1,-cols/2:-1]/cols;
	    DG = (j*2*pi*y).^dc;
	  end
	end

	Is = I.*G.*DG;
      end
    end
  end
end
