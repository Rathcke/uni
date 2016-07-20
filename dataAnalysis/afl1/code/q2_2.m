load('logitdata')

N = length(admit);
X = [gpa gre];

mdl = fitglm(X,admit,'Distribution','binomial');
w_est = mdl.Coefficients.Estimate;

theta = @(s) exp(s) / (1 + exp(s));

h = @(s) theta(transpose(w_est)*s);
%h = @(s) theta([-4.8774 0.7547 0.2676]*s);

p = transpose([1 X(10,1) X(10,2)]);

h(p)