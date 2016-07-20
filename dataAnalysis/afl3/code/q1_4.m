load 'diatoms';

m_vect = mean(diatoms);

X = bsxfun(@minus, diatoms, m_vect);

samp_covar_mat = 1/length(m_vect)*(X'*X);

[Z, X_hat] = svd_pca(X, 3);

s = eig(samp_covar_mat);
s = s(end:-1:1);

spec = cumsum(s)/sum(s);
I = 1:5;
spec(I)
plot(I,spec(I))

spec(3)