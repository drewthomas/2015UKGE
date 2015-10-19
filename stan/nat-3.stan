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
}
transformed data {
	int<lower=0> votes[k,6];

	/* Turn the 6 individual vectors of estimated voting intention
	   counts into a `k`-by-6 matrix of counts ("votes"). */
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
	// overall prior confidence in the underlying party pref. dist.
	// (typical magnitude of elements of `alpha`)
	real<lower=0> alpha0;

	// underlying base distribution of party preference
	// (6 party-specific parameters for a Dirichlet distribution)
	vector<lower=0>[6] alpha;

	// poll-specific party preference proportions (hence a simplex)
	simplex[6] theta[k];

	// pollster effects on measured party preference
	// (pollster-specific additive offsets to `alpha`)
	vector<lower=0>[6] beta[pollsters];

	// party-specific scale parameter for prior on `beta` values
	// (expected relative error in `alpha` for each party)
	vector<lower=0>[6] sigma;

	// shape parameter of `beta`'s gamma distribution prior
	real<lower=0> beta_prior_shape;
}
model {
	/* Set a loose prior on `alpha0` based on the total sample size of
	   all of the polls, and the fact that there are 6 poll options. */
	alpha0 ~ lognormal(log(sum(n) / 6.0), 0.9);

	/* Set a loose prior on `alpha` too. `alpha` is a 6-vector that's
	   scaled by `alpha0`, so the prior on `alpha`'s elements is that
	   they're about 1.
	   N.B.: `alpha` is a 6-vector, but Stan's smart enough to vectorize
	   the `alpha` sampling statement automatically, so I don't have to
	   explicitly loop over `alpha`'s 6 elements. */
	alpha ~ lognormal(log(1), 0.9);

	/* I'd guesstimate that the typical magnitude `sigma` of a pollster's
	   relative bias for/against a random party is about 20%, and that the
	   prior on a gamma distribution of `beta` around that average has a
	   shape parameter of maybe 0.3 -- but I don't have very strong prior
	   beliefs about that, so set broad hyper-priors for both the "20%"
	   and "0.3" prior parameters.
	   N.B.: like `alpha`, `sigma`, is a 6-vector, and its sampling is
	   automatically vectorized by Stan. */
	sigma ~ lognormal(log(0.2), 1.1);
	beta_prior_shape ~ lognormal(log(0.3), 0.7);

	/* Set the prior for each pollster-specific party bias based on
	   the prior's scale parameter `sigma` and shape parameter
	   `beta_prior_shape`. */
	for (i in 1:pollsters) {
		for (j in 1:6) {
			beta[i][j] ~ gamma(beta_prior_shape,
			                   1 / (sigma[j] * alpha0 * alpha[j]));
		}
	}

	/* Nearly there. The prior for `theta`, a single poll's underlying
	   party preference proportions, is a Dirichlet distribution based on
	   adding the overall party preference proportions from `alpha` &
	   `alpha0` to pollster's estimated bias `beta`. The key observable,
	   `votes`, is then assumed to come from a multinomial distribution
	   parametrized by `theta`.
	   N.B.: I once again exploit Stan's automatic vectorization here; both
	   `theta` & `votes` are 2-D arrays, but I need only iterate over one
	   dimension explicitly. */
	for (i in 1:k) {
		theta[i] ~ dirichlet((alpha0 * alpha) + beta[pollster[i]]);
		votes[i] ~ multinomial(theta[i]);
	}
}
