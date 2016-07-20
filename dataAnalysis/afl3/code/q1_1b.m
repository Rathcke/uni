load 'diatoms';
m_vect = mean(diatoms);

hold on
plot(m_vect(1:2:end), m_vect(2:2:end))
axis equal;
hold off