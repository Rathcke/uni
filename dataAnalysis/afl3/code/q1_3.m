load 'diatoms';

m_vect = mean(diatoms);

X = bsxfun(@minus, diatoms, m_vect);

[Z, X_hat] = svd_pca(X, 3);
dia1 = X_hat(1,:) + m_vect;
dia2 = X_hat(2,:) + m_vect;
dia3 = X_hat(3,:) + m_vect;

subplot(3,1,1)
hold on
plot (diatoms(1,1:2:end), diatoms(1, 2:2:end))
plot (dia1(1,1:2:end), dia1(1, 2:2:end))
axis equal
hold off

subplot(3,1,2)
hold on
plot (diatoms(2,1:2:end), diatoms(2, 2:2:end))
plot (dia2(1,1:2:end), dia2(1, 2:2:end))
axis equal
hold off

subplot(3,1,3)
hold on
plot (diatoms(3,1:2:end), diatoms(3, 2:2:end))
plot (dia3(1,1:2:end), dia3(1, 2:2:end))
axis equal
hold off
