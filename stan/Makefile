R=R --slave --vanilla
STANPRINT=~/cmdstan-2.6.0/bin/print

stan-alpha-summary.dat: summarize-model-output.R stan-alpha-summary.R nat-7-out.csv nat-6-out.csv nat-5-out.csv nat-4-out.csv nat-3-out.csv
	${R} < summarize-model-output.R
	${R} < stan-alpha-summary.R

# before making this target, run
#  ~/cmdstan-2.6.0$ make ../projects/forecasting/2015UKGE/stan/nat-7
nat-7-out.csv: nat-7 nat-A.data.R
	./nat-7 sample num_samples=4000 data file=nat-A.data.R output file=nat-7-out.csv
	${STANPRINT} nat-7-out.csv | grep -v "   4000  "
	${R} < nat-7.R

# before making this target, run
#  ~/cmdstan-2.6.0$ make ../projects/forecasting/2015UKGE/stan/nat-6
nat-6-out.csv: nat-6 nat-A.data.R
	./nat-6 sample num_samples=4000 data file=nat-A.data.R output file=nat-6-out.csv
	${STANPRINT} nat-6-out.csv | grep -v "   4000  "
	${R} < nat-6.R

# before making this target, run
#  ~/cmdstan-2.6.0$ make ../projects/forecasting/2015UKGE/stan/nat-5
nat-5-out.csv: nat-5 nat-A.data.R
	./nat-5 sample num_samples=4000 data file=nat-A.data.R output file=nat-5-out.csv
	${STANPRINT} nat-5-out.csv | grep -v "   4000  "
#	${R} < nat-5.R

# before making this target, run
#  ~/cmdstan-2.6.0$ make ../projects/forecasting/2015UKGE/stan/nat-4
nat-4-out.csv: nat-4 nat-A.data.R
#	cd .. && ./proc-res-A.py  # prepare nat-A.data.R in this subdirectory
	./nat-4 sample num_samples=4000 data file=nat-A.data.R output file=nat-4-out.csv
	${STANPRINT} nat-4-out.csv | grep -v "   4000  "

# before making this target, run
#  ~/cmdstan-2.6.0$ make ../projects/forecasting/2015UKGE/stan/nat-3
nat-3-out.csv: nat-3 nat-A.data.R
	./nat-3 sample num_samples=4000 data file=nat-A.data.R output file=nat-3-out.csv
	${STANPRINT} nat-3-out.csv | grep -v "   4000  "
#	${R} < nat-3.R

# before making this target, run
#  ~/cmdstan-2.6.0$ make ../projects/forecasting/2015UKGE/stan/nat-2
nat-2-out.csv: nat-2 nat-A.data.R
	./nat-2 sample num_samples=4000 data file=nat-A.data.R output file=nat-2-out.csv
	${STANPRINT} nat-2-out.csv | grep -v "   4000  "
#	${R} < nat-2.R

# before making this target, run
#  ~/cmdstan-2.6.0$ make ../projects/forecasting/2015UKGE/stan/nat-1
nat-1-out.csv: nat-1 nat-A.data.R
	./nat-1 sample num_samples=9000 data file=nat-A.data.R output file=nat-1-out.csv
	${STANPRINT} nat-1-out.csv
#	${R} < nat-1.R

# prepare nat-A.data.R in this subdirectory
nat-A.data.R: ../proc-res-A.py ../results-A-1.dat
	cd .. && ./proc-res-A.py
