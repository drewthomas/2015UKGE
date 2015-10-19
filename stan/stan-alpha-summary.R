pa_cols_1 <- c("blue", "red", "gold", "purple", "darkgreen", "grey")
pa_cols_2 <- c("#0000ff60", "#ff000060", "#dfdf006f",
               "#ff00ff60", "#00ff0060", "#00000060")

sas <- read.table("stan-alpha-summary.dat", header=TRUE)
sas$DATE <- as.Date(sas$DATE)

parties <- unique(sas$PART)

pdf("stan-alpha-summary.pdf", 10, 7)

par(las=1, mar=c(2, 2, 0.5, 0.5), mfrow=c(2,3))

# Iterate over the models, with one plotting panel for each.
for (i in min(sas$ID):max(sas$ID)) {

	# Pick out the results for model `i`, and the reuslts for the first
	# party from model `i`, and put those in `d` & `d1` respectively.
	d <- sas[sas$ID == i,]
	d1 <- d[d$PART == parties[1],]

	# Plot estimated voting intention percentage over time for party 1
	# according to model `i`. Include 95% confidence bands.
	plot(d1$DATE, d1$PER, col=pa_cols_1[1],
	     xlim=range(sas$DATE), ylim=c(1, max(sas$PER)),
	     xlab="date model fitted", ylab="%")
	err <- 1.96 * d1$SE
	polygon(c(d1$DATE, rev(d1$DATE)), c(d1$PER + err, rev(d1$PER - err)),
	        border=NA, col=pa_cols_2[1])

	# Number this plot with the model's ID number.
	text(mean(range(sas$DATE)), 22.5, i, cex=3)

	# Plot percentages and confidence bands for the remaining parties.
	for (j in 2:length(parties)) {
		party <- parties[j]
		dj <- d[d$PART == party,]
		err <- 1.96 * dj$SE
		points(dj$DATE, dj$PER, col=pa_cols_1[j])
		polygon(c(dj$DATE, rev(dj$DATE)),
		        c(dj$PER + err, rev(dj$PER - err)),
		        border=NA, col=pa_cols_2[j])
	}

	# Add a grid and a 0% line.
	grid(col="#00000050")
	abline(h=0, lty="dotted")

	# Finish the plot by adding the final vote shares actually obtained
	# in the 2015 general election.
	vote_shares <- c(36.9, 30.4, 7.9, 12.6, 3.8)
	vote_shares <- c(vote_shares, 100 - sum(vote_shares))  # other parties
	abline(h=vote_shares, lty="dashed",
	       col=c("#0000ffa0", "#ff0000a0", "#bbb00fa0",
	             "#bb00bfa0", "#00c000a0", "#7f7f7fa0"))
	
}

dev.off()
