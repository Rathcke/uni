function f = tf_meyer(x)
% Calculates the Meyer function of 3 variables at x.  If x is not
% provided the standard initial point is returned.

if ~exist('x'),  f = [0.02;4000;250];  return;  end;
x1 = x(1);  x2 = x(2);  x3 = x(3); 
y = [34780 28610 23650 19630 16370 13720 11540 9744 ...
     8261 7030 6005 5147 4427 3820 3307 2872];
t = 45 + 5*[1:16];
fi = x1*exp(x2*(ones(1,16)./(t+x3))) - y;
f = fi*fi';
