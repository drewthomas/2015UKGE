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
	simplex[6] last_ge;
}
transformed data {
	vector<lower=0>[6] last_elec;
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

	/* Rescale the vote proportions from the 2010 general election so
	   that the mean of their natural logarithms is 1. */
	last_elec <- exp(log(last_ge) / mean(log(last_ge)));
}
parameters {
	// overall prior confidence in the underlying party pref. dist.
	// (typical magnitude of elements of `alpha`)
	real<lower=0> alpha0;

	// underlying base distribution of party preference
	// (party-specific 6-vector parameter for a Dirichlet distribution)
	vector<lower=0>[6] alpha;

	// poll-specific party preference proportions (hence a simplex)
	simplex[6] theta[k];

	// pollster effects on measured party preference
	// (pollster-specific additive offsets to `alpha`)
	vector<lower=0>[6] beta[pollsters];

	// party-specific scale parameter for prior on `beta` values
	// (expected relative error in `alpha` for each party)
	vector<lower=0>[6] sigma_beta;

	// shape parameter of `beta`'s gamma distribution prior
	real<lower=0> beta_prior_shape;

	// expected typical `sigma_beta` value
	// (general expected relative error in `alpha`, not party-specific)
	real<lower=0> meta_sigma;

	// date effect on underlying party preference (relative to day after)
	vector<lower=0>[6] delta[max(t)-1];

	// average daily drift in underlying party preference (mean of `delta`)
	vector<lower=0>[6] mu_delta;

	// typical magnitude of daily drift in party pref. (s.d. of `delta`)
	vector<lower=0>[6] sigma_delta;
}
model {
	// date effects on underlying party preference, to be computed below
	vector<lower=0>[6] day[max(t)];

	/* Set a loose prior on `alpha0` based on the total sample size of
	   all of the polls, the fact that there are 6 poll options, and the
	   presumption that 25% of polled respondents are undecided. Then
	   set a loose prior on `alpha` based on the 2010 election results.
	   N.B.: `alpha` & `last_elec` are 6-vectors, but Stan's smart enough
	   to vectorize this sampling statement automatically, so I don't
	   have to explicitly loop over `alpha` & `last_elec`'s elements. */
	alpha0 ~ lognormal(log(0.75 * sum(n) / 6.0), 0.8);
	alpha ~ lognormal(log(last_elec), 0.8);

	/* I'd guesstimate that the typical magnitude `sigma_beta` of a
	   pollster's relative bias for/against a random party is about 20%,
	   and that the prior on a gamma distribution of `beta` around that
	   average has a shape parameter of maybe 0.5 -- but those prior
	   beliefs are weak, so set broad hyper-priors for both the "20%"
	   and "0.5" (hyper-)prior parameters. */
	meta_sigma ~ lognormal(log(0.2), 0.8);
	sigma_beta ~ lognormal(log(meta_sigma), 1.1);  // auto-vectorized!
	beta_prior_shape ~ lognormal(log(0.5), 0.8);

	/* Set the prior for each pollster-specific party bias based on
	   the prior's scale parameter `sigma_beta` and shape parameter
	   `beta_prior_shape`. */
	for (i in 1:pollsters) {
		for (j in 1:6) {
			beta[i][j] ~ gamma(beta_prior_shape,
			                   1 / (sigma_beta[j] * alpha0 * alpha[j]));
		}
	}

	/* Set priors on the parameters representing day-to-day fluctuations
	   in party preference: `mu_delta` is the average day-to-day
	   multiplier, and is presumably close to 1; `sigma_delta` is the
	   typical size of day-to-day multiplier fluctuations, and is
	   likely small, though perhaps not, so its prior is more diffuse. */
	mu_delta ~ lognormal(log(1), 0.005);
	sigma_delta ~ lognormal(log(0.015), 0.6);

	/* Set the lognormal prior on `delta`, then compute the date effects
	   implied by `delta` and cache them in `day`.
	   Stan doesn't like iterating backwards through the days, so I have to
	   iterate forwards and flip `i` in array indices in the loop.
	   N.B.: I again exploit Stan's automatic vectorization here; both
	   `delta` & `day` are 2-D arrays, but I need only iterate over one
	   dimension explicitly. */
	day[max(t)] <- rep_vector(1.0, 6);
	for (i in 1:(max(t)-1)) {
		delta[max(t)-i] ~ lognormal(log(mu_delta), sigma_delta);
		day[max(t)-i] <- delta[max(t)-i] .* day[max(t)-i+1];
	}

	/* Nearly there. The prior for `theta`, a single poll's underlying party
	   preference proportions, is a Dirichlet distribution based on adding
	   the overall party preference proportions from `alpha` & `alpha0` to
	   the pollster's estimated bias `beta` and the day effect `day`. The
	   key observable, `votes`, is then assumed to come from a multinomial
	   distribution parametrized by `theta`.
	   N.B.: once more, I use Stan's automatic vectorization here to avoid
	   explicitly iterating over both dimensions. */
	for (i in 1:k) {
		theta[i] ~ dirichlet(((alpha0 * alpha) + beta[pollster[i]])
		                     .* day[t[i]]);
		votes[i] ~ multinomial(theta[i]);
	}
}
