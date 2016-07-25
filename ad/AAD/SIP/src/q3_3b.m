G = @(x, y, s) t/(sqrt(2*pi*s^2))*1/(2*pi*(s^2+t^2))*exp(-x^2/(2*(s^2+t^2)));
X = -40:40;
Y = -40:40;
Y10 = arrayfun(@(x) G(x, Y, 10), X);
Y20 = arrayfun(@(x) G(x, Y, 20), X);
Y30 = arrayfun(@(x) G(x, Y, 30), X);

subplot(1, 3, 1); 3dplot(X, Y10); title('\sigma = 10');
    maxIndex = find(max(Y10) == Y10);
    strmax = ['Maximum at t = ', num2str(X(maxIndex))];
    text(X(maxIndex), Y10(maxIndex), strmax, 'HorizontalAlignment','right');
subplot(1, 3, 2); plot(X, Y20); title('\sigma = 20');
    maxIndex = find(max(Y20) == Y20);
    strmax = ['Maximum at t = ', num2str(X(maxIndex))];
    text(X(maxIndex), Y20(maxIndex), strmax, 'HorizontalAlignment','right');
subplot(1, 3, 3); plot(X, Y30); title('\sigma = 30');
    maxIndex = find(max(Y30) == Y30);
    strmax = ['Maximum at t = ', num2str(X(maxIndex))];
    text(X(maxIndex), Y30(maxIndex), strmax, 'HorizontalAlignment','right');