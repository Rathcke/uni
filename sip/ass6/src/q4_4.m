x = [0, 0, 1, 1, 0];
y = [0, 1, 1, 0, 0];
x2 = [0.4, 0.6, 1.3, 1, 0.4];
y2 = [1, 0.6, 0.8, 1.6, 1];
X = [transpose(x) transpose(y)];
Y = [transpose(x2) transpose(y2)];

tform = cp2tform(X, Y, 'projective');
U = tformfwd(tform, X(:,1), X(:,2));

hold all
title('Projective mapping');
plot(Y(:,1), Y(:,2));
plot(U(:,1), U(:,2));


