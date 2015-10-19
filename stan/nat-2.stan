data {
	int<lower=1> k;
	int<lower=1> t[k];
	int<lower=1> pollsters;
	int<lower=1> pollster[k];
	int<lower=1> n[k];
	int<lower=0> con[k];
	int<lower=0> lab[k];
	int<lower=0> ld[k];
	int<lower=0> ukip[k];
	int<lower=0> green[k];
	int<lower=0> other[k];

	real<lower=0> sigma_gamma;  // median scale expected for `gamma`'s prior
}
transformed data {
	int<lower=0> votes[k,6];

	// Turn the 6 individual vectors of estimated polling preference
	// counts into a `k`-by-6 matrix of counts ("votes").
	for (i in 1:k) {
		votes[i][1] <- con[i];
		votes[i][2] <- lab[i];
		votes[i][3] <- ld[i];
		votes[i][4] <- ukip[i];
		votes[i][5] <- green[i];
		votes[i][6] <- other[i];
	}
}
parameters {
	// underlying base level of party preference
	// (6 party-specific parameters for a Dirichlet distribution)
	vector<lower=0>[6] alpha;

	// poll-specific party preference simplex
	simplex[6] theta[k];

	// pollster effects on party preference (multipliers on `alpha`)
	vector<lower=0>[6] gamma[pollsters];

	// scale parameter for lognormal prior on `gamma` values
	real<lower=0> sigma;
}
model {
	vector<lower=0>[6] temp_gamma_alpha;  // `gamma`-adjusted `alpha`

	sigma ~ lognormal(sigma_gamma, 0.3);
	for (i in 1:pollsters) {
		for (j in 1:6) {
			gamma[i][j] ~ lognormal(0, sigma);
		}
	}
	for (i in 1:k) {
		for (j in 1:6) {
			temp_gamma_alpha[j] <- gamma[pollster[i]][j] * alpha[j];
		}
		theta[i] ~ dirichlet(temp_gamma_alpha);
		votes[i] ~ multinomial(theta[i]);
	}
}
