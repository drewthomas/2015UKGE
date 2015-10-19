read_stan <- function(path)
{
	return(read.table(path, header=TRUE, sep=",", colClasses="numeric"))
}

extract_alpha <- function(stan)
{
	acf_sum <- function(x)
	{
		return(sum(acf(x, plot=FALSE, lag.max=(length(x)-1))$acf))
	}

	first_alpha_col <- match("alpha.1", colnames(stan))
	stan_alpha <- stan[first_alpha_col:(5 + first_alpha_col)]
	alpha <- unname(colMeans(stan_alpha))
	sum_al <- sum(alpha)
	per <- 100.0 * alpha / sum_al

	alpha_se <- apply(stan_alpha, 2, sd) / sqrt(length(stan_alpha[,1]))
	alpha_acf_sum <- apply(stan_alpha, 2, acf_sum)
	alpha_se <- alpha_se * sqrt(1 + (2 * alpha_acf_sum))
	se <- 100.0 * alpha_se / sum_al

	return(signif(rbind(per, se), 4))
}

write_alpha <- function(extracted, model_id)
{
	print(extracted)
	extr <- data.frame(t(extracted))
	d <- data.frame(DATE=Sys.Date(), ID=as.numeric(model_id),
	                PART=c("CON", "LAB", "LD", "UKIP", "GREEN", "OTHER"),
	                PER=extr$per, SE=extr$se)
	write.table(d, "stan-alpha-summary.dat", TRUE, c(1,3), "\t",
	            row.names=FALSE, col.names=FALSE)
}

sta7 <- read_stan("nat-7-out.csv")
write_alpha(extract_alpha(sta7), 7)
sta6 <- read_stan("nat-6-out.csv")
write_alpha(extract_alpha(sta6), 6)
sta5 <- read_stan("nat-5-out.csv")
write_alpha(extract_alpha(sta5), 5)
sta4 <- read_stan("nat-4-out.csv")
write_alpha(extract_alpha(sta4), 4)
sta3 <- read_stan("nat-3-out.csv")
write_alpha(extract_alpha(sta3), 3)
#sta2 <- read_stan("nat-2-out.csv")
#write_alpha(extract_alpha(sta2), 2)
