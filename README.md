2015 UK general election forecasting models
===========================================

These are data files and computer programs I put together in the lead-up to the UK's 2015 election, to experiment with [Stan](http://mc-stan.org/), and to see how well I could predict the election's vote percentages by aggregating polls.

The workflow here is probably unclear (I've kept the code pretty much as it was, without trying to clean it up or streamline it) but once the Stan models are compiled into programs it amounts to adding a polling result to `results-A-1.dat`, and then `cd`ing into `[stan/](stan/)` and running `make`.

If changed since the last run, the data file gets preprocessed into `nat-A.data.R` for Stan, and the Stan model executables run for a few hours, producing big output files (tens of megabytes, hence not included here) to be processed by `stan/summarize-model-output.R` and `stan/stan-alpha-summary.R`; that last R script makes the pretty `stan/stan-alpha-summary.pdf`, which plots how the 6 models' predictions have evolved over time. Model-specific R scripts in `stan/` can produce `stan/Rplots.pdf`, which plots a model's output in human-readable form.
