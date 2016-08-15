function f = sombrero(x);
    
    n = 2;      % the dimension of the problem.
    if ~exist('x'),
        f = ones(n,1);   % get the initial point
    else
        f = x' * ( [1:n]' .* x);  % get the function value
        f = -x(1) + ( max(f-100,0) )^2 - f;
	f = f/100;
    end;
return;