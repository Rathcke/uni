data_x = [1 0 0 -1 0 0 -2 100 -100 0 0];
data_y = [0 1 -1 0 2 -2 0 0 0 100 -100];
color = [1 1 1 1 2 2 2 0 0 0 0];

hold on
[v,c] = voronoin([data_x' data_y']);
axis([-2 2, -2 2])
for i = 1:length(c)
    if all(c{i}~=1)
        patch(v(c{i},1),v(c{i},2), color(i));
    end
end
hold off