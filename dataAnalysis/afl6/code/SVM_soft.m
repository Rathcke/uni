% N is number of datapoints
% X is data in rows
% Y is labels in a column
% C is a number
function u = SVM_soft(X, Y, C)
   
    % Get the datasample dimension
    [N, d]=size(X);
    
    % p: a column of N 0's followed by N C/N's
    p = [zeros(d+1,1); (C/N)*ones(N,1)];
    
    % c: a column of N 1's followed by N 0's
    c = [ones(N,1); zeros(N,1)];
    
    % Q is a (d+N+1)x(d+N+1) matrix with a dxd identity matrix in entry 2x2
    Q = zeros(N+d+1,N+d+1);
    Q(2:d+1,2:d+1) = eye(d);
    
    % A: first a 2N x (d+N+1) matrix of 0's
    A = zeros(2*N, d+N+1);
    
    % The vector of labels
    A(1:N,1) = Y;

    % YX: labels multiplied by data matrix with a column of 1's to the left
    A(1:N,2:d+1) = (Y * ones(1, d)) .* X;
    
    % NIden: a NxN Identity matrix
    NIden = eye(N);
    
    % NIden is inserted to the right in A
    % both in the right top and bottom right corner
    % of A
    A(1:N,d+2:d+N+1) = NIden;
    A(N+1:2*N,d+2:d+N+1) = NIden;

    u = quadprog(Q,p,-A,-c);    
end
