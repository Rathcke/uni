load 'diatoms';

m_vect = mean(diatoms);

X = bsxfun(@minus, diatoms, m_vect);

samp_covar_mat = 1/length(m_vect)*(X'*X);

[Z, X_hat, V] = svd_pca(X, 3);

s = eig(samp_covar_mat);
s = s(end:-1:1);
t = sqrt(s);

for i = 1:3
    subplot(3,1,i)
    hold on
    p1 = m_vect-2*t(i)*V(:, i)';
    p2 = m_vect-t(i)*V(:, i)';
    p3 = m_vect;
    p4 = m_vect+t(i)*V(:, i)';
    p5 = m_vect+2*t(i)*V(:, i)';
    plot(p1(1:2:end), p1(2:2:end))
    plot(p2(1:2:end), p2(2:2:end))
    plot(p3(1:2:end), p3(2:2:end))
    plot(p4(1:2:end), p4(2:2:end))
    plot(p5(1:2:end), p5(2:2:end))
    hold off
end