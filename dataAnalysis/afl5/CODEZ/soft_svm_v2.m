function [Wcard, E_test, E_in] = soft_svm_v2(dataset,length_of_data,real_images)
Wcard = [];
E_in = [];
E_test = [];
for C = [10^-6,10^-5,10^-4,10^-3,10^-2,10^-1,1,10,100]
  U = soft_svm(dataset',[ones(length_of_data,1) ; ones(length_of_data,1) * (-1)],C);
  B = U(1);
  W = U(2:785)';
  Wcard = [Wcard norm(W)];
  E_in_tmp = 0;
  E_test_tmp = 0;
  for j = 1:500
    if W * real_images(:,j) + B < 0
      E_test_tmp = E_test_tmp + 1;
    end
  end
  for j = 501:1000
    if W * real_images(:,j) + B > 0
      E_test_tmp = E_test_tmp + 1;
    end
  end
  E_test = [E_test E_test_tmp / 1000];
  for j = 1:length_of_data
    if W * dataset(:,j) + B < 0
      E_in_tmp = E_in_tmp + 1;
    end
  end
  for j = length_of_data+1:length_of_data*2
    if W * dataset(:,j) + B > 0
      E_in_tmp = E_in_tmp + 1;
    end
  end
  E_in = [E_in E_in_tmp / (2*length_of_data)];
end