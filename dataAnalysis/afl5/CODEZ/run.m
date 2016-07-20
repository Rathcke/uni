function [Ein, Etest, wlength] = plot_soft_svm(X,length,real)
C = [10^-6,10^-5,10^-4,10^-3,10^-2,10^-1,1,10,100];
wlength = zeros(1, length(C));
Ein = zeros(1, length(C));
Etest = zeros(1, length(C));
count = 0;
for c = C
  count = count + 1;
  u = soft_svm(X',[ones(length,1) ; ones(length,1) * (-1)],c);
  b = u(1);
  w = u(2:785)';
  Ein_temp = 0;
  Etest_temp = 0;
  for j = 1:length *2
    if j <= length && w * X(:,j) + b < 0
        Ein_temp = Ein_temp + 1;
    end
    if j > length && w * X(:,j) + b > 0
        Ein_temp = Ein_temp + 1;
    end
  end
  for j = 1:1000
    if j <= 500 && sign(w * real(:,j) + b) < 0
        Etest_temp = Etest_temp + 1;
    end
    if j > 500 && w * real(:,j) + b) > 0
        Etest_temp = Etest_temp + 1;
    end
  end
  wlength(count) = norm(w);
  Ein(count) = Ein_temp / (2*length);
  Etest(count) = Etest_temp / 1000;
end
plotyy(C, [Ein ; Etest], C, wlength, 'semilogx');