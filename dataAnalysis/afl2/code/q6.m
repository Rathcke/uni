testImages = loadMNISTImages('t10k-images.idx3-ubyte');
trainImages = loadMNISTImages('train-images.idx3-ubyte');
testLabels = loadMNISTLabels('t10k-labels.idx1-ubyte');
trainLabels = loadMNISTLabels('train-labels.idx1-ubyte');

%imshow(reshape(trainImages(:, 22), 28, 28));

N = [10 : 10 : 90, 100 : 100 : 900, 1000 : 500 : 5500];

% Set what labels are compared - "label1" is the one taken from
% the test set and compared to the two labels taken from the training set.
label1 = 5;
label2 = 6;

% Find images for the chosen labels.
D0 = find(testLabels == label1);
D1 = find(testLabels == label2);

C0 = find(trainLabels == label1);
C1 = find(trainLabels == label2);

% Initialization of X and Y's for plotting
X = []; X2 = []; X3 = []; X4 = [];
Y = []; Y2 = []; Y3 = []; Y4 = [];

% Different K's
K_list = [1 3 5 7];

% Counts for what label occurs the most times.
label1_count = 0; 
label2_count = 0;

for g = 1:4
    
    K = K_list(g);
    
    for h = 1:10
        M = N(h);

        % Counts for how many times the number was guessed correctly.
        label1_count = 0; 

        for i = 1:500
            switched = 0;
            R = zeros(K,2);
            R(:,1) = inf;
            for j = 1:M*2
                if switched == 0
                    cur = sum((testImages(:, D0(i)) - trainImages(:, C0(j))).^2);   
                else
                    cur = sum((testImages(:, D0(i)) - trainImages(:, C1(j-M))).^2);
                end
                ind = 0;
                worstDis = 0;
                for k = 1:K
                    if cur < R(k,1)
                        if R(k,1) >= worstDis
                            worstDis = R(k,1);
                            ind = k;
                        end
                    end
                end
                if ind ~= 0
                    if switched == 1
                        R(ind,2) = trainLabels(C1(j-M));
                    else
                        R(ind,2) = trainLabels(C0(j));
                    end
                    R(ind,1) = cur;
                end
                if j == M
                    switched = 1;
                end
            end
            % mode doesn't work if there's only one row.
            if K == 1
                if R(2) == label1
                    label1_count = label1_count + 1;
                else
                    label2_count = label2_count + 1;
                end
            else
                S = mode(R);
                if S(2) == label1
                    label1_count = label1_count + 1;
                else
                    label2_count = label2_count + 1;
                end
            end
        end
        if K == 1
            X = [X M];
            Y = [Y label1_count/i];
        end
        if K == 3
            X2 = [X2 M];
            Y2 = [Y2 label1_count/i];
        end
        if K == 5
            X3 = [X3 M];
            Y3 = [Y3 label1_count/i];
        end
        if K == 7
            X4 = [X4 M];
            Y4 = [Y4 label1_count/i];
        end  
    end
end

figure
plot(X, Y, 'r')
hold on
plot(X2, Y2, 'b')
plot(X3, Y3, 'g')
plot(X4, Y4, 'y')
xlabel('Iterations')
ylabel('E in')
legend('K1', 'K3', 'K5', 'K7') 
title('Comparison of different K')