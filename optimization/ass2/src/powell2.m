function [f,G] = tf_pow02(x)
% Calculates Powell's function of 2 variables at x.  If x is not
% provided the standard initial point is returned.

G = -1;
if ~exist('x'),  f = [0;1];  return;  end;
x1 = x(1);  x2 = x(2);  
f1 = 10000*x1*x2 - 1;  f2 = exp(-x1) + exp(-x2) - 1.0001;
f = f1*f1 + f2*f2;
