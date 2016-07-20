function u = soft_svm(X,Y,C)
%function takes in X of size N x d, no leading 1s
%Y of size N x 1
%C, which I read on absalon was supposed to be C/N
%Final large adjustment was setting -A,-c on quadprog call
[N,d] = size(X);

Q = zeros(N+d+1,N+d+1);
Q(2:d+1,2:d+1) = eye(d);

p = [zeros(d+1,1); C/N*ones(N,1)];

A = zeros(2*N, N+d+1);
A(1:N,1) = Y;
A(1:N,2:d+1) = (Y * ones(1, N))' * X;
size(Y * ones(1,N) * X);
A(1:N,d+2:d+1+N) = eye(N); 
A(N+1:2*N,d+2:d+1+N) = eye(N);

c = [ones(N,1); zeros(N,1)];

% YS: You should negate both A and c (assuming quadprog matches its manual)
u = quadprog(Q,p,-A,-c); %tried various negatives, since matlab definition was different