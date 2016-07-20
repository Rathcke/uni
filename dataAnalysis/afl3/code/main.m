load 'diatoms';

m_vect = mean(diatoms);

X = bsxfun(@minus, diatoms, m_vect);

[Z, X_hat] = svd_pca(X, 3);
dia2 = X_hat(1,:) + m_vect;

hold on
subplot(2,1,1)
plot (diatoms(1,1:2:end), diatoms(1, 2:2:end))
axis equal
hold off

hold on
subplot(2,1,2);
plot (dia2(1,1:2:end), dia2(1, 2:2:end))
axis equal
hold off