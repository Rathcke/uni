function [w_e_in, res] = gradientDesc(init, step, t)
    load('logitdata')

    N = length(admit);
    X = [gpa gre];

    mdl = fitglm(X,admit,'Distribution','binomial');
    w_est = mdl.Coefficients.Estimate;

    theta = @(s) exp(s) / (1 + exp(s));

    w_e_in = [];
    y = (admit-.5)*2;
    X1 = [ones(400,1) X];
    w = init';
    
    for j = 1:t
        j
        sum = 0;
        for i = 1:N
           sum = sum + (-y(i,:)*X1(i,:)'*(theta(-y(i,:)*w'*X1(i,:)')));
        end
        grad = sum/N;
        v = -grad;
        w = w + step*v;
        
        if mod(j, 500) == 0
            w_e_in = [w_e_in errorIn(w)];
        end
    end
    
    res = w;
    
end