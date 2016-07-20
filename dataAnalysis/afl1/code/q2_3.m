load('logitdata')

N = length(admit);
X = [gpa gre];

mdl = fitglm(X,admit,'Distribution','binomial');
w_est = mdl.Coefficients.Estimate;

w_init = [0 0 0]';
w = fminunc(@negloglike, w_init)
w_est
