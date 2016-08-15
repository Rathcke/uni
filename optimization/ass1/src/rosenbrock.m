function f = rosenbrock(x)
% Calculates Rosenbrocks function at x.  If x is not
% provided the standard initial point is returned.
%

if ~exist('x'),  f = [-1.2;1];  return;  end;
f = (x(1)-1)^2 + 100*(x(2)-x(1)*x(1))^2;