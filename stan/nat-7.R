pollster_names <- c("YouGov", "Ipsos-MORI", "Panelbase", "BMG",
                    "Survation", "ICM", "TNS", "Opinium", "Populus",
                    "ComRes", "Lord Ashcroft")
party_colours <- c("blue", "red", "gold", "purple", "darkgreen", "darkgrey")

psters <- length(pollster_names)

# Read Stan's big MCMC output file.
# Specify that all of the data are numeric so it doesn't take as long.
stan <- read.table("nat-7-out.csv", header=TRUE, sep=",",
                   colClasses="numeric")

# Compute absolute alpha values as the product of `alpha0` and each of
# the normalized alpha values from Stan.
first_alpha_col <- match("alpha.1", colnames(stan))
stan_alpha <- stan[first_alpha_col:(5 + first_alpha_col)]
alpha <- unname(colMeans(stan_alpha))
sum_al <- sum(alpha)

# Display alpha-derived headline results: the estimated underlying
# party preference proportions and its implied observation error.
acf_sum <- function(x)
{
	return(sum(acf(x, plot=FALSE, lag.max=(length(x)-1))$acf))
}
print("CON LAB LD UKIP GREEN OTHER")
print(round(alpha))
print(round(100.0 * alpha / sum_al, 1))
alpha_se <- apply(stan_alpha, 2, sd) / sqrt(length(stan_alpha[,1]))
alpha_acf_sum <- apply(stan_alpha, 2, acf_sum)
alpha_se <- alpha_se * sqrt(1 + (2 * alpha_acf_sum))
print(round(100.0 * alpha_se / sum_al, 2))

# Read in the original polling data.
orig_data <- read.table("../results-A-1.dat", header=TRUE)
orig_data$END <- as.Date(orig_data$END)

# Set nicer graphical options for the plotting below.
par(las=1, mar=c(5,4,1,1))

# Make a dot plot of how many polls each polling organization did.
pster_table <- table(orig_data$POLL)
dotchart(c(pster_table), xlab="number of polls by pollster")
grid(col="darkgrey")
abline(v=seq(0, max(pster_table), 5), col="grey", lty="dotted")

# Return an empty data frame/matrix with the right dimensions for storing
# a parameter value for a party-by-something-else effect.
empty_party_summary_matrix <- function(row_names)
{
	mat <- data.frame(matrix(nrow=length(row_names), ncol=6))
	colnames(mat) <- c("CON", "LAB", "LD", "UKIP", "GREEN", "OTHER")
	rownames(mat) <- substr(row_names, 1, 10)
	return(mat)
}

# Return an empty data frame/matrix with the right dimensions for storing
# a parameter value for each pollster-party combination.
empty_pollster_party_summary_matrix <- function()
{
	return(empty_party_summary_matrix(pollster_names))
}

# Locate the beta-related columns of Stan output.
first_beta_col <- match("beta.1.1", colnames(stan))

# Compute summary statistics for every pollster-party bias in the matrix
# of beta values.
be_mean <- empty_pollster_party_summary_matrix()
be_sd <- empty_pollster_party_summary_matrix()
be_median <- empty_pollster_party_summary_matrix()
be_025th <- empty_pollster_party_summary_matrix()
be_975th <- empty_pollster_party_summary_matrix()
qu025 <- function(x) { return(quantile(x, 0.025)) }
qu975 <- function(x) { return(quantile(x, 0.975)) }
for (pster in 1:psters) {
	col_idxs <- (first_beta_col - 1 + pster) + (psters * (0:5))
	be_mean[pster,] <- apply(stan[, col_idxs], 2, mean)
	be_sd[pster,] <- apply(stan[, col_idxs], 2, sd)
	be_median[pster,] <- apply(stan[, col_idxs], 2, median)
	be_025th[pster,] <- apply(stan[, col_idxs], 2, qu025)
	be_975th[pster,] <- apply(stan[, col_idxs], 2, qu975)
}
#print(round(cbind(be_mean, be_sd)))  # display beta summary

# Convert the average pollster-party bias effects represented by the beta
# summary statistics into understandable units: percentage points.
be_mean_dotchart <- empty_pollster_party_summary_matrix()
for (i in 1:psters) {
	be_mean_dotchart[i,] <- alpha + be_mean[i,]
	be_mean_dotchart[i,] <-
		100 * ((be_mean_dotchart[i,] / sum(be_mean_dotchart[i,]))
	           - (alpha / sum_al))
}

# Draw a dot plot of the now-intelligible pollster-party bias effects.
# Reverse the order of `be_mean`'s columns to accommodate `dotchart`.
be_mean_dotchart <- be_mean_dotchart[, 6:1]
dotchart(t(as.matrix(be_mean_dotchart)), cex=3/5, pch=16,
#         color=c("darkgrey", "darkgreen", "purple", "gold", "red", "blue"),
         color=rev(party_colours),
         xlab="pollster-party effect (percentage points)")
grid()
abline(v=0, lty="dashed")

# Draw 95% credibility intervals for each pollster-party effect.
for (i in 1:psters) {
	for (j in 1:6) {
		lower <- alpha + be_mean[i,]
		lower[j] <- alpha[j] + be_025th[i,j]
		lower <- 100 * ((lower / sum(lower)) - (alpha / sum_al))
		upper <- alpha + be_mean[i,]
		upper[j] <- alpha[j] + be_975th[i,j]
		upper <- 100 * ((upper / sum(upper)) - (alpha / sum_al))
		y <- (8 * (psters + 1 - i)) - 1 - j
		arrows(unlist(lower[j]), y, unlist(upper[j]), y,
		       0.04, 90, 3, col="#0000005a")
	}
}

# Compute day effects and display a fortnightly summary.
first_delta_col <- match("delta.1.1", colnames(stan))
n_days <- 1 + as.integer(diff(range(orig_data$END)))
day_eff <- empty_party_summary_matrix(1:n_days)
for (i in 1:6) {
	col_idxs <- first_delta_col - 1 + ((i-1) * (n_days-1)) + (1:(n_days-1))
	day_eff[n_days, i] <- 1
	for (j in (n_days-1):1) {
		day_eff[j,i] <- mean(stan[, col_idxs[j]]) * day_eff[j+1, i]
	}
}
print(round(day_eff[c(seq(1, n_days - 1, 14), n_days),], 2))

draw_model_poll_retrodiction <- function()
{
	x <- seq(min(orig_data$END), max(orig_data$END), by=1)
	for (i in 1:6) {
		y <- 100.0 * day_eff[,i] * alpha[i] / sum_al
		lines(x, y, col=party_colours[i], lty="twodash", lwd=2)
	}
}

# Plot poll result time series, and the model's retrodiction of them.
source("../view-func.R")  # for the `plot_poll_summary` function
plot_poll_summary(orig_data)
draw_model_poll_retrodiction()
legend(as.Date("2015-03-01"), 27,
       lty=c("solid", "twodash", "dashed"), lwd=c(1,2,1), bg="white",
       legend=c("loess curve fits to data", "Stan model estimates",
                "actual final vote share"), cex=0.8)

# Display each party's average delta value.
first_mu_delta_col <- match("mu_delta.1", colnames(stan))
print(colMeans(stan[, first_mu_delta_col:(5+first_mu_delta_col)]))

# Adjust poll results for model-estimated party-pollster effects, then
# plot time series of adjusted poll results (and the model's retrodiction
# again).
adj_data <- orig_data
be_mean_rows_for_adj <- match(orig_data$POLLSTER, pollster_names)
for (i in 1:6) {
	party_pster_eff <- rev(be_mean_dotchart)[be_mean_rows_for_adj, i]
	adj_data[4+i] <- adj_data[4+i] - party_pster_eff
}
plot_poll_summary(adj_data, "adjusted voting intention %")
draw_model_poll_retrodiction()
legend(as.Date("2014-12-18"), 27,
       lty=c("solid", "twodash", "dashed"), lwd=c(1,2,1), bg="white",
       legend=c("loess curve fits to pollster-party-effect-adjusted data",
                "Stan model estimates", "actual final vote share"), cex=0.8)
