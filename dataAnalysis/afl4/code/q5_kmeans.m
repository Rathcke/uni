function [ centroids , Ein, curClust] = myKmeans( k, images)
  centroids = [];
  for i=1:k
    centroids = [centroids images(:,randi([1 1000]))];
  end
  dist = zeros(k,length(images));
  curClust = zeros(1,length(images));
  Ein = [];
  oldCentroids = zeros(size(centroids));
  while sum(abs(centroids - oldCentroids)) > 0.0001
    for i=1:k
      dist(i,:) = sum(bsxfun(@minus,images,centroids(:,i)).^2,1);
    end
    EinTmp = 0;
    for i=1:length(images)
      [closestClustDist,curClust(i)] = min(dist(:,i));
      EinTmp = EinTmp + closestClustDist;
    end
    Ein = [Ein ; EinTmp / length(images)];
    oldCentroids = centroids;
    for i=1:k
      centroids(:,i) = mean(images(:,find(curClust == i)),2);
    end
  end
end
