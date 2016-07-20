function res = errorIn(w)
    load('logitdata')

    N = length(admit);
    X = [gpa gre];

    mdl = fitglm(X,admit,'Distribution','binomial');
    w_est = mdl.Coefficients.Estimate;

    theta = @(s) exp(s) / (1 + exp(s));

    y = (admit-.5)*2;
    X1 = [ones(400,1) X];
    sum = 0;
    
    for i = 1:N
        sum = sum + log(1+exp(-y(i,:)*w'*X1(i,:)'));
    end
    
    res = sum/N;

end