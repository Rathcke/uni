trainImages = loadMNISTImages('train-images-idx3-ubyte');
trainLabels = loadMNISTLabels('train-labels-idx1-ubyte');
realImages = loadMNISTImages('t10k-images-idx3-ubyte');
realLabels = loadMNISTLabels('t10k-labels-idx1-ubyte');

% Find indices of labels 0,1,5,6,8 in real and train data.
t0_idx = find(trainLabels == 0);
t1_idx = find(trainLabels == 1);
t5_idx = find(trainLabels == 5);
t6_idx = find(trainLabels == 6);
t8_idx = find(trainLabels == 8);
r0_idx = find(realLabels == 0);
r1_idx = find(realLabels == 1);
r5_idx = find(realLabels == 5);
r6_idx = find(realLabels == 6);
r8_idx = find(realLabels == 8);

% Assign the actual images of the indices.
t01_250 = trainImages(:,[t0_idx(1:250);t1_idx(1:250)]);
t08_250 = trainImages(:,[t0_idx(1:250);t8_idx(1:250)]);
t56_250 = trainImages(:,[t5_idx(1:250);t6_idx(1:250)]);
t01_500 = trainImages(:,[t0_idx(1:500);t1_idx(1:500)]);
t08_500 = trainImages(:,[t0_idx(1:500);t8_idx(1:500)]);
t56_500 = trainImages(:,[t5_idx(1:500);t6_idx(1:500)]);
t01_1000 = trainImages(:,[t0_idx(1:1000);t1_idx(1:1000)]);
t08_1000 = trainImages(:,[t0_idx(1:1000);t8_idx(1:1000)]);
t56_1000 = trainImages(:,[t5_idx(1:1000);t6_idx(1:1000)]);
r01 = realImages(:,[r0_idx(1:500);r1_idx(1:500)]);
r08 = realImages(:,[r0_idx(1:500);r8_idx(1:500)]);
r56 = realImages(:,[r5_idx(1:500);r6_idx(1:500)]);
%% Q4.1 01
C = [10^-6,10^-5,10^-4,10^-3,10^-2,10^-1,1,10,100];
clf;
[Wcard, E_test, E_in] = soft_svm_v2(t01_250,250,r01);
subplot(3,1,1)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');
legend('E_{in}','E_{test}','|w|');

[Wcard, E_test, E_in] = soft_svm_v2(t01_500,500,r01);
subplot(3,1,2)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');

[Wcard, E_test, E_in] = soft_svm_v2(t01_1000,1000,r01);
subplot(3,1,3)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');

%% 08
clf;
[Wcard, E_test, E_in] = soft_svm_v2(t08_250,250,r08);
subplot(3,1,1)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');
legend('E_{in}','E_{test}','|w|');
[Wcard, E_test, E_in] = soft_svm_v2(t08_500,500,r08);
subplot(3,1,2)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');

[Wcard, E_test, E_in] = soft_svm_v2(t08_1000,1000,r08);
subplot(3,1,3)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');

%% 56
clf;
[Wcard, E_test, E_in] = soft_svm_v2(t56_250,250,r56);
subplot(3,1,1)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');
legend('E_{in}','E_{test}','|w|');

[Wcard, E_test, E_in] = soft_svm_v2(t56_500,500,r56);
subplot(3,1,2)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');

[Wcard, E_test, E_in] = soft_svm_v2(t56_1000,1000,r56);
subplot(3,1,3)
plotyy(C, [E_in ; E_test], C, Wcard, 'semilogx');