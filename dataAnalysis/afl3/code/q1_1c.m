load 'diatoms';

m_vect = mean(diatoms);

X = bsxfun(@minus, diatoms, m_vect);

samp_covar_mat = 1/length(m_vect)*X'*X;