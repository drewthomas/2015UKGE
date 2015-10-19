psters <- 9  # number of pollsters
pollster_names <- c("YouGov", "Ipsos-MORI", "Survation", "ICM", "TNS",
                    "Opinium", "Populus", "ComRes", "Lord Ashcroft")

stan <- read.table("nat-2-out.csv", header=TRUE, sep=",",
                   colClasses="numeric")

alpha <- c(mean(stan$alpha.1), mean(stan$alpha.2), mean(stan$alpha.3),
           mean(stan$alpha.4), mean(stan$alpha.5), mean(stan$alpha.6))
print("CON LAB LD UKIP GREEN OTHER")
print(signif(alpha, 3))
print(signif(alpha / sum(alpha), 3))

first_gamma_col <- match("gamma.1.1", colnames(stan))
#gamma <- stan[, 229:282]
gamma <- stan[, first_gamma_col:(53+first_gamma_col)]

par(las=1, mar=c(5,4,1,1))

plot(density(gamma[,1]), xlim=range(gamma), ylim=c(0, 4), log="x")
grid()
for (i in 2:(dim(gamma)[2])) {
	lines(density(gamma[,i]))
}

empty_pollster_party_summary_matrix <- function()
{
	mat <- data.frame(matrix(nrow=psters, ncol=6))
	colnames(mat) <- c("CON", "LAB", "LD", "UKIP", "GREEN", "OTHER")
	rownames(mat) <- substr(pollster_names, 1, 10)
	return(mat)
}

gamma_mean <- empty_pollster_party_summary_matrix()
gamma_sd <- empty_pollster_party_summary_matrix()
gamma_median <- empty_pollster_party_summary_matrix()
gamma_5th <- empty_pollster_party_summary_matrix()
gamma_95th <- empty_pollster_party_summary_matrix()
qu5 <- function(x) { return(quantile(x, 0.05)) }
qu95 <- function(x) { return(quantile(x, 0.95)) }
for (pster in 1:psters) {
	col_idxs <- (first_gamma_col - 1 + pster) + (psters * (0:5))
	gamma_mean[pster,] <- apply(stan[, col_idxs], 2, mean)
	gamma_sd[pster,] <- apply(stan[, col_idxs], 2, sd)
	gamma_median[pster,] <- apply(stan[, col_idxs], 2, median)
	gamma_5th[pster,] <- apply(stan[, col_idxs], 2, qu5)
	gamma_95th[pster,] <- apply(stan[, col_idxs], 2, qu95)
}
print(signif(cbind(gamma_mean, gamma_sd), 2))
print(signif(gamma_median, 2))

# draw dot plot of pollster effects by party
# reverse the order of `gamma_mean`'s columns to accommodate `dotchart`
gamma_mean_for_dotchart <- gamma_mean[, 6:1]
dotchart(t(as.matrix(log(gamma_mean_for_dotchart) / log(1.1))), cex=0.8, pch=16,
         color=c("darkgrey", "darkgreen", "purple", "gold", "red", "blue"),
         xlab="pollster-party effect (alpha multiplier in units of 10%)")
grid()
abline(v=0, lty="dashed")

# add error bars to dot plot
for (i in 1:psters) {
	for (j in 1:6) {
		y <- (8 * (psters + 1 - i)) - 1 - j
		arrows(log(gamma_5th[i,j]) / log(1.1), y,
		       log(gamma_95th[i,j]) / log(1.1), y,
		       0.05, 90, 3, col="#0000003f")
	}
}
