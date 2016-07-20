trainImages = loadMNISTImages('train-images-idx3-ubyte');
trainLabels = loadMNISTLabels('train-labels-idx1-ubyte');


indices = [ find(trainLabels == 0,100);
            find(trainLabels == 1,100);
            find(trainLabels == 2,100);
            find(trainLabels == 3,100);
            find(trainLabels == 4,100);
            find(trainLabels == 5,100);
            find(trainLabels == 6,100);
            find(trainLabels == 7,100);
            find(trainLabels == 8,100);
            find(trainLabels == 9,100) ];
digitset = trainImages(:,indices);
%% q5_1
clf
[~, Ein, ~] = q5_kmeans(2,digitset);
plot(Ein);
%% q5_2
clf
[~, Ein, ~] = q5_kmeans(4,digitset);
plot(Ein);
%% q5_3-4
resCentroids = [];
resEins = [];
resCurClusts = [];
for k = 2:1:16
  Ein = inf;
  for i = 1:5
    [centroidsTmp, EinTmp, curClustTmp] = q5_kmeans(k,digitset);
    if EinTmp(end) < Ein
      curClusts = curClustTmp;
      Ein = EinTmp(end);
      centroids = centroidsTmp;
    end
  end
  resCentroids = [resCentroids centroids];
  resEins = [resEins Ein];
  resCurClusts = [resCurClusts curClusts];
end

%% q5_5
clf
plot(2:16,resEins);

%% q5_6
clf
offset = sum(2:9);
for i = 1:10
  subplot(3,4,i), imshow(reshape(resCentroids(:,offset + i), 28, 28));
end

%% q5_7
curClust10 = [resCurClusts(8001:9000) ; trainLabels(indices)'];
weightedError = 0;
for i=1:10
  labels = curClust10(2, find(curClust10(1,:)==i));
  [~,freq] = mode(labels);
  errTmp = (length(labels) - freq)/length(labels);
  weightedError = weightedError + errTmp * (length(labels)/1000);
end
disp(weightedError)

%% q5_8
clf
[V,Z] = pca(digitset');
hold on
scatter(Z(1:100,1),Z(1:100,2),'*');
scatter(Z(101:200,1),Z(101:200,2));
scatter(Z(201:300,1),Z(201:300,2),'.');
scatter(Z(301:400,1),Z(301:400,2),'d');
scatter(Z(401:500,1),Z(401:500,2),'+');
scatter(Z(501:600,1),Z(501:600,2),'x');
scatter(Z(601:700,1),Z(601:700,2),'s');
scatter(Z(701:800,1),Z(701:800,2),'^');
scatter(Z(801:900,1),Z(801:900,2),'o');
scatter(Z(901:1000,1),Z(901:1000,2),'p');
legend('0','1','2,','3','4','5','6','7','8','9');
hold off
%% q5_9
clf
p = 1;
cc_from = 1;
cc_to = 1000;
marks = 'osd^v><phosd^v><phsd^v><ph';
colors = 'ymcrgbkymcrgbymcrgbk';
for i=[1,3,7,9,15]
  offset_start = sum(2:i) + 1;
  offset_end = offset_start + i;
  [V,Z] = pca([digitset resCentroids(:,offset_start:offset_end)]');
  subplot(3,2,p);
  p = p + 1;
  curClusts = resCurClusts(cc_from:cc_to);
  hold on
  for j=1:(i+1)
    clustPoints = find(curClusts==j);
    scatter(Z(clustPoints,1),Z(clustPoints,2),colors(j),marks(j));
  end
  for j=1:(i+1)
    scatter(Z(1000 + j,1),Z(1000 + j,2),100,'filled',colors(j),marks(j),'MarkerEdgeColor',[0 0 0]);
  end
  if i == 3
    cc_from = cc_from + 2000;
    cc_to = cc_to + 2000;
  end
  if i == 9
    cc_from = cc_from + 4000;
    cc_to = cc_to + 4000;
  end
  cc_from = cc_from + 2000;
  cc_to = cc_to + 2000;
  hold off
end