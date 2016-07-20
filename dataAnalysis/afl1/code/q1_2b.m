w =[2; 2];
x =[1; 2];
y =-1;
w2 = w+y*x;


hold on
grid on
xlim([-3,3])
ylim([-3,3])
line([0 w(1,1)],[0 w(2,1)], 'Color', [0,1,0])
plot(x(1,1),x(2,1),'*')
line([0 w2(1,1)],[0 w2(2,1)])
line([-3,3],[3,-3], 'Color', [1,0,0])
line([0,0],[3,-3], 'Color', [1,0,0])
hold off