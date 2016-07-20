[w_e_in res] = gradientDesc(w_init', 0.1, 20000);

figure
hold on
plot(0, errorIn(w_est), '*g')
for i = 1:length(w_e_in)
    plot(i*500, w_e_in(1, i), '*b')
end
xlabel('Iterations')
ylabel('E in')
hold off