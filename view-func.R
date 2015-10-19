points_y_against_end_date <- function(d, y, ...)
{
	pollsters <- as.integer(as.factor(d$POLLSTER))
	points(d$END, d[[y]], cex=sqrt(d$N / 3e3),
           pch=(1 + max(pollsters) - pollsters), ...)
}

draw_on_loess <- function(d, model_specification, ...)
{
	lines(d$END, predict(loess(model_specification, d, d$N, span=1/3)), ...)
}

plot_poll_summary <- function(d, yla="observed voting intention %")
{
	par(las=1, mar=c(5,4,1,1))

	plot(d$END, d$CON, col="#0000ffa0", ylim=range(d[, 5:10]),
	     cex=sqrt(d$N / 3e3), pch=as.integer(as.factor(d$POLLSTER)),
	     xlab="poll end date", ylab=yla)
	grid(col="grey")
	points_y_against_end_date(d, "LAB", col="#ff0000a0")
	points_y_against_end_date(d, "LD", col="#bbb00fa0")
	points_y_against_end_date(d, "UKIP", col="#bb00bfa0")
	points_y_against_end_date(d, "GREEN", col="#00c000a0")
	points_y_against_end_date(d, "OTHER", col="#7f7f7fa0")

	draw_on_loess(d, CON ~ as.numeric(END), col="#0000ff")
	draw_on_loess(d, LAB ~ as.numeric(END), col="#ff0000")
	draw_on_loess(d, LD ~ as.numeric(END), col="#bbb00f")
	draw_on_loess(d, UKIP ~ as.numeric(END), col="#bb00bf")
	draw_on_loess(d, GREEN ~ as.numeric(END), col="#00c000")
	draw_on_loess(d, OTHER ~ as.numeric(END), col="#7f7f7f")

	abline(h=0, lty="dotted")

	abline(h=c(36.9, 30.4, 7.9, 12.6, 3.8,
	           100 - sum(c(36.9, 30.4, 7.9, 12.6, 3.8))),
	       col=c("#0000ff", "#ff0000", "#bbb00f",
	             "#bb00bf", "#00c000", "#7f7f7f"),
	       lty="dashed")
}

plot_poll_data_from_current_directory <- function()
{
	d <- read.table("results-A-1.dat", header=TRUE)
	d$END <- as.Date(d$END)
	print(table(rowSums(d[, 5:10])))  # check vote percentages sum near 100%
	print(table(substr(d$POLLSTER, 1, 5)))
	plot_poll_summary(d)
}
