
function [f,x] = math412demo(fname,method,LStype,info,acc,...
    maxitns,LSlimit)
% WARNING: THIS CODE IS FOR DEMONSTRATING THE BASIC PROPERTIES OF
% VARIOUS ALGORITHM TYPES ONLY.  USE AT OWN RISK.
% [bestf,x] = math412demo(fname, search_direction_type, 
%             line_search_method, info, accuracy, maxitns, LSlimit)
%
% eg [f,x] = math412demo('rosenbrock','N','BA',3,1e-5,30,45);
%
% The argument fname must contain the file name of the function to 
% be minimized, inside single quotation marks.  The function fname 
% must be of the form [f] = fname(x); and return the initial point 
% if x is not specified. The variable accuracy gives the stopping 
% tolerance on the gradient estimate's norm.  If accuracy < 0 it 
% is replaced with 10^accuracy.  maxitns and LSlimit give the number
% of iterations and number of function evaluations in a single line
% search at which the algorithm automatically stops.
%   Choices of search_direction_type are: Steepest descent ('SD'),
% Newton ('N'), modified Newton ('MN'), and quasi-Newton ('QN'),
% Memoryless QN ('MQN'), and QN and MQN with initial diagonal 
% scaling ('QND' and 'MQND').  MQN and MQND are implemented using
% matrices and are O(n) slower than vector only versions.
%   Choices of line_search_method types are: Back-tracking Armijo ('BA'),
% forward- (and back-)tracking ('AF'), and Wolfe-Powell ('WP').
%   The variable info gives the level of printout.  If info < 0, its
% absolute value is used, and the algorithm pauses after each printout. 
%   If info =
% 0  print out nothing.
% 1  print out only at start and end.
% 2  print out after every iteration.
% 3  as for 2, but `H not PD using SD' means the Hessian is not positive 
%    definite, so the steepest descent direction has been used, and 
%    `adding mu.I to H' means that mu times the identity was added to the 
%    Hessian to make it positive definite.  'no QN update' means 
%    the BFGS update was abandoned due to loss of positive definiteness.

% INITIALIZATION

format compact;
if nargin < 2,  
    fprintf('FILE NAME OR METHOD NOT SPECIFIED: RUN ABANDONED \n');  return;
end;
if ~exist('LStype'),  LStype = 'AF';  end;
if ~exist('info'),  info = 1;  end;
if info < 0,  info = abs(info);  printstop = 1;  else  printstop = 0;  end;
if ~exist('acc'),  acc = 0.00001; elseif acc < 0,  acc = 10^acc;  end;
if ~exist('LSlimit'),  LSlimit = 45;  end;
if ~exist('maxitns'),
    switch method,
        case {'N','MN'},  maxitns = 30;
        case {'QN','MQN','QND','MQND','SD'},  maxitns = 150;  
    end;  
end;

% STEP 1: Initialize the variables
stop = 0;  itn = 0;  nx = 1;

% get the initial point
no_x_0 = 0;
if ~exist('x'),  x = feval(fname);  no_x_0 = 1;  end;
sizex = size(x);  n = max(sizex);  if sizex(2) > 1,  x = x';  end;

% initialize the remaining variables.
g = zeros(n,1);  oldg = g;  p = zeros(n,1);  oldp = p;
D = ones(n,1);  h = 1e-6;  mu = 0;  QNabort = 0;

% evaluate the initial function value and gradient
f = feval(fname,x);  
[g,nx,D,oldg,fp] = estimate_g(fname,method,x,h,n,f,g,D,nx);

% Do the initial printout.
initial_print(info,fname,method,LStype,n,maxitns,h,acc,f,no_x_0);

% Get the Cholesky factors of the Hessian or its estimate
switch method, 
    case {'N','MN'},
    [HR,nx,mu] = get_HChol(fname,method,x,h,n,f,fp,D,nx,info,stop);
    case 'SD',    HR = 0;
    case {'QN','MQN'},    HR = eye(n);
    case {'QND','MQND'},  HR = diag( sqrt( max(1e-8,D))); 
end;
     
while stop == 0,                          % MAIN LOOP
    itn = itn+1;

    % Calculate the new search direction
    [p,oldp] = next_search_direction(p,g,oldg,D,HR,method);

    % Do a line search
    [x,f,nx,stop,alpha] = line_search(fname,LStype,x,f,p,g,LSlimit,nx,stop);
       
    % Do the printout for this iteration.
    if info >1,  
        iteration_print(f,g,itn,nx,alpha,p,mu,QNabort,printstop,info);   
    end;
    
    % estimate gradient at the new iterate.
    [g,nx,D,oldg,fp] = estimate_g(fname,method,x,h,n,f,g,D,nx);
 
    % Check the stopping conditions and do end of iteration print
    stop = check_stop(f,g,acc,itn,maxitns,info,stop);
    
    % Calculate the upper triangular Cholesky factor of the Hessian.
    switch method, 
        case {'N','MN'},
        [HR,nx,mu] = get_HChol(fname,method,x,h,n,f,fp,D,nx,info,stop);
        case {'QN','MQN','MQND','QND'},
        [HR,QNabort] = update_HChol(fname,method,n,f,g,oldg,D,HR,p,alpha,info);
    end;
    
end;                                    % MAIN LOOP

% do the final printout.
final_print(fname,method,LStype,info,x,f,g,itn,nx,n);
switch method,
    case {'MQN','MQND'},
        fprintf('WARNING: MQN AND MQND IMPLEMENTED USING MATRICES\n');
end;

% --------------------------------------------------------------
function [stop] = check_stop(f,g,acc,itn,maxitns,info,stop);

% CHECK THE STOPPING CONDITIONS
if stop > 1,  fprintf('\n HALT BECAUSE OF A LINE SEARCH FAILURE \n');  end;
if norm(g) < min(1,acc*(1+abs(f))),  
   if info > 0,
       fprintf('\n HALT BECAUSE GRADIENT RESTRICTION SATISFIED: normg = %12.6g\n',norm(g));
   end;
   stop = 1;
elseif itn >= maxitns,
   if info > 0,
       fprintf('\n HALT BECAUSE MAXIMUM NUMBER OF ITERATIONS REACHED: normg = %12.6g\n',norm(g));   
   end;
   stop = 1;
end;
     
% --------------------------------------------------------------
    function [p,oldp] = next_search_direction(p,g,oldg,D,HR,method)
   
    oldp = p;
    switch method,
        case 'SD',  p = -g; 
        case {'QN','N','MN','MQN','MQND','QND'} 
        p = -HR \ ( HR' \ g );
    end;
    
% --------------------------------------------------------------

function [g,nx,D,oldg,fp] = estimate_g(fname,method,x,h,n,f,g,D,nx)
% Calculating the gradient using central differences on a MAXIMAL frame
% aligned with the axes

xx = x;  oldg = g;  fp = zeros(n,1);
for ii=1:n,   
   incr = ( abs(xx(ii)) + 1)*h;
   xx(ii) = xx(ii)+incr; 
   fplus = feval(fname,xx);  fp(ii) = fplus;
   xx(ii) = xx(ii)-2*incr;
   fminus = feval(fname,xx);
   xx(ii) = x(ii);
   g(ii) = (fplus - fminus)/(2*incr);
   D(ii) = (fplus + fminus - 2*f)/(incr*incr);
end;
nx = nx + 2*n;
return;

% ------------------------------------------------------------------
function [HR,nx,mu] = get_HChol(fname,method,x,h,n,f,fp,D,nx,info,stop)
        
% estimate the Hessian
HH = diag(D);  mu = 0;
for ii = 1:n,
    for jj = ii+1:n,
        incii = ( abs(x(ii)) + 1)*h;
        incjj = ( abs(x(jj)) + 1)*h;
        xx = x;  xx(ii) = x(ii) + incii;  xx(jj) = x(jj) + incjj;
        HH(ii,jj) = (feval(fname,xx) - fp(ii) - fp(jj) + f)/(incii*incjj);
        HH(jj,ii) = HH(ii,jj);
    end;
end;

% do not count function evaluations if stopping as HR not needed then.
if stop,  HR = HH;  return;  else  nx = nx + n*(n-1)/2;   end;  
    
% Cholesky factor the Hessian
[HR,notposdef] = chol(HH);  % chol routine returns upper triangular factor.
if ~notposdef,  return;  end;
if method=='N', 
    mu = -2;  HR = eye(n);   % change to steepest descent direction.
else % method is MN
    mu = 1;
    while notposdef,
        [HR,notposdef] = chol(HH + mu*eye(n));  
        if notposdef,  mu = mu*4;  end;
    end;
end;
return;

% ------------------------------------------------------------------
function [HR,QNabort] = update_HChol(fname,method,n,f,g,oldg,D,HR,p,alpha,info)
        
% update the Cholesky factors of the estimate of the Hessian
switch method,
    case 'MQN',  HR = eye(n);  
    case 'MQND',  HR = diag( sqrt( max(1e-8,D))); 
end;
oldHR = HR;  QNabort = 0;
y = g - oldg;   s = alpha*p;   yts = y'*s;  
if yts <= 0, QNabort = 1;  return;  end;
% Add y y' / y's to B
vec = y/sqrt(yts);    HR = cholupdate(HR,vec,'+');
% Now subtract (B s s' B) / (s' B s)
vec = oldHR*s;   vec = (oldHR'*vec)/norm(vec);
[HR,QNabort] = cholupdate(HR,vec,'-');
if QNabort,  HR = oldHR;  end;
return;

% --------------------------------------------------------------
function [x,f,nx,stop,alpha] = line_search(fname,LStype,x,f,p,g,LSlimit,nx,stop)

rho = 1e-4;  sigma = 0.9;  lsnx = 0;

alphaa = 1;   xa = x + alphaa*p;  
fa = feval(fname,xa);  lsnx = lsnx+1;
ptg = p'*g;
while fa >= f + rho*alphaa*ptg & ~stop,
    fb = fa;  xb = xa;  alphab = alphaa;
    alphaa = alphaa/2;   xa = x + alphaa*p;
    fa = feval(fname,xa);  lsnx = lsnx+1;
    if lsnx > LSlimit,  stop = 2;  end;
end; % of while

if LStype=='BA' | (LStype=='AF' & lsnx > 1),  
    x = xa;  f = fa;  alpha = alphaa;  nx = nx + lsnx;  return;  
end;

if LStype=='AF' | (LStype=='WP' & lsnx < 2),   
    % lsnx<2 means alpha = 1 gave descent, so do forward tracking.
    alphab = 4;   xb = x + alphab*p;
    fb = feval(fname,xb);  lsnx = lsnx+1;
    while fb < f+rho*alphab*ptg & lsnx<=LSlimit & (fb<fa | LStype=='WP'),
        alphaa = alphab;  fa = fb;  xa = xb;
        alphab = 4*alphab;
        xb = x + alphab*p;
        fb = feval(fname,xb);  lsnx = lsnx+1;
    end; % of while
end;

if LStype=='AF',  
    x = xa;  f = fa;  alpha = alphaa;  nx = nx + lsnx;  return;  
end;

% only the WP line search should reach this point.
ptgalpha = get_ptg_at_alpha(fname,x,alphaa,p);  lsnx = lsnx+2;
if ptgalpha >= sigma*ptg,
    x = xa;  f = fa;  alpha = alphaa;  nx = nx + lsnx;  return;  
end;
  
lscontinue = 1;
while lscontinue,
    alphac = (alphaa + alphab)/2;  
    xc = x + alphac*p;  
    fc = feval(fname,xc);  lsnx = lsnx + 1;
    if fc >= f + rho*alphac*ptg,
        alphab = alphac;
    else
        ptgalpha = get_ptg_at_alpha(fname,x,alphac,p);  
        lsnx = lsnx+2;
        if ptgalpha >= sigma*ptg,
            x = xc;  f = fc;  alpha = alphac;  
            nx = nx + lsnx;  lscontinue = 0; 
        else
            alphaa = alphac;
        end;
    end;
    if lsnx > LSlimit,  
        stop = 2;  lscontinue = 0;  alpha = alphac;  nx = nx + lsnx;
    end;
end;

% --------------------------------------------------------------

function ptg_alpha = get_ptg_at_alpha(fname,x,alpha,p)
    
xplus = x + (1 + 1e-6)*alpha*p;
xminus = x + (1 - 1e-6)*alpha*p;
fplus = feval(fname,xplus);  fminus = feval(fname,xminus);  
ptg_alpha = (fplus - fminus)/(2e-6*alpha);
return;

% --------------------------------------------------------------
    function iteration_print(f,g,itn,nx,alpha,p,mu,QNabort,printstop,info)

fprintf(' %9i %12.6g %12i',itn,f,nx);
fprintf(' %12.6g %12.6g %12.6g',norm(g),alpha,alpha*norm(p));
if info > 2 & mu > 0,  fprintf(' Adding %8.2g.I to H',mu);  end;
if info > 2 & mu < 0,  fprintf(' H not PD: Use SD');  end;
if QNabort & info > 2,  fprintf(' no QN update');   end;
fprintf('  \n');

if itn/30 == floor(itn/30),
   fprintf('\n\n       itn      f value   nr f evals        ||g||   ');
   fprintf('     alpha   steplength\n\n');
end;

if printstop,  pause;  end;

% --------------------------------------------------------------

function initial_print(info,fname,method,LStype,n,maxitns,h,acc,f,no_x_0);

if info > 0, 
   fprintf(' ----------------------------------------------\n');
   fprintf(' File name of function is %c%c%c%c%c%c%c%c%c%c%c%c%c%c ',fname);
   fprintf('\n ----------------------------------------------\n\n');
end;

if info > 0,
    if no_x_0,
       fprintf('No initial point: using initial pt from function\n\n');
    end;
   fprintf(' number of variables = %9i \n',n);
   fprintf(' maximum of %9i iterations \n',maxitns);  
   fprintf(' increment for gradient estimation = %12.4g \n',h);
   fprintf(' requested termination accuracy = %8.4g \n',acc);
   fprintf(' function value at initial point = %8.4g \n \n',f);   
   fprintf('       itn      f value   nr f evals        ||g||   ');
   fprintf('     alpha   steplength \n');
end;


% --------------------------------------------------------------

function final_print(fname,method,LStype,info,x,f,g,itn,nx,n);

if info > 0, 
   fprintf('\n');  fprintf(' --------------- FINAL SUMMARY ------------------\n');
   fprintf(' File name of function is: %c%c%c%c%c%c%c%c%c%c%c%c%c%c',fname);
   fprintf(' \n');
   fprintf('\n');  fprintf(' with function value %15.8g \n',f);
   fprintf(' after %6i  iterations and  %6i  function evaluations\n',itn,nx);
   fprintf(' Magnitude of gradient at final iterate is %12.6g \n',norm(g));
   if n < 7,
      fprintf(' Current x = %8.4g ',x(1));
      for ii = 2:n,  fprintf('  %8.4g ',x(ii));  end;
   end;
   fprintf(' \n');   fprintf(' \n');
   fprintf(' Method type: %c%c%c%c%c',method); 
   fprintf('          Line search type: %c%c',LStype); 
   fprintf('\n');   fprintf('\n');
end;

% -------------------------------------------------------------
        
function [f] = nzf1( x )

   n = 13;
   if ~exist('x'),  f = ones( n, 1 );  return;  end;
   f = (3 * x(1) + 0.1 * ( x(2) - x(3) )^2 - 60)^2;
   temp = x(2)^2 + x(3)^2 + x(4)^2 * ( 1 + x(4)^2 ) + x(7);
   temp = temp + x(6) / ( 1 + x(5)^2 ) + sin( 0.001 * x(5) );
   f = f + temp^2;
   f = f + (x(7) + x(8)-x(9)^2 + x(11))^2;
   f = f + (log( 1 + x(11)^2 ) + x(12) - 5 * x(13) + 20)^2;
   f = f + (x(5) + x(6) + x(5) * x(6) + 10*x(10) - 50)^2;

