data {
	int<lower=1> k;
	int<lower=1> t[k];
	int<lower=1> pollster[k];
	int<lower=1> n[k];
	real<lower=0,upper=1> con[k];
	real<lower=0,upper=1> lab[k];
	real<lower=0,upper=1> ld[k];
	real<lower=0,upper=1> ukip[k];
	real<lower=0,upper=1> green[k];
	real<lower=0,upper=1> other[k];
}
transformed data {
	simplex[6] theta[k];
	real total;

	# explicitly convert each poll's observed results into a simplex
	# (this prevents blow-ups when the total percentages add to 99%)
	for (i in 1:k) {
		total <- con[i] + lab[i] + ld[i] + ukip[i] + green[i] + other[i];
		theta[i][1] <- con[i] / total;
		theta[i][2] <- lab[i] / total;
		theta[i][3] <- ld[i] / total;
		theta[i][4] <- ukip[i] / total;
		theta[i][5] <- green[i] / total;
		theta[i][6] <- other[i] / total;
	}
}
parameters {
	vector<lower=0>[6] alpha;  // 6 party-specific concentration parameters
}
model {
	for (i in 1:k) {
		theta[i] ~ dirichlet(alpha);
	}
}
