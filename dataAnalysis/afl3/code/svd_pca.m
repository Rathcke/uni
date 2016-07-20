function [Z, X_hat, V] = svd_pca(X, k)

    [U,S,V] = svd(X);
    V_k = V(:, 1:k);
    X_hat = X*(V_k*V_k');
    Z = X*V_k;
    
end