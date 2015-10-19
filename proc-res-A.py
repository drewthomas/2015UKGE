#!/usr/bin/python

import datetime
import sets

date_format = "\"%Y-%m-%d\""  # ISO 8601 for lyfe!

f_out = open("results-A-2.dat", "wb")
f_out.write("# DAY = day on which poll ended\n")

data = [ ]
f_in = open("results-A-1.dat", "rb")
for line in f_in:
	line = line.strip()
	if line.startswith("#"):
		if line.startswith("# START") or line.startswith("# END"):
			continue
		else:
			f_out.write(line + "\n")
	elif line.startswith("START"):
		continue
	else:
		start, end, pster, n, con, lab, ld, ukip, green, other = line.split("\t")
		end = datetime.datetime.strptime(end, date_format)
		data.append([ end, pster, n, con, lab, ld, ukip, green, other ])
		total = sum(map(int, (con, lab, ld, ukip, green, other)))
		if (total < 98) or (total > 102):
			print "Warning: poll represented by this line of data:"
			print line
			print "has voting intention percentages summing to", total
f_in.close()

earliest = min(map(lambda datum: datum[0], data))
pollsters = list(set(map(lambda datum: datum[1], data)))

f_out.write("# Earliest polling date: " + str(earliest) + "\n")

for i in range(len(pollsters)):
	f_out.write("# Pollster %u: %s\n" % ( 1 + i, pollsters[i] ))

f_out.write("DAY\tPOLLSTER\tN\tCON\tLAB\tLD\tUKIP\tGREEN\tOTHER\n")

def day(datum):
	return 1 + (datum[0] - earliest).days

def pollster(datum):
	return 1 + pollsters.index(datum[1])

for datum in data:
	f_out.write("%u\t%u\t%s\n" % ( day(datum), pollster(datum), "\t".join(datum[2:]) ))

f_out.close()

f_out = open("stan/nat-A.data.R", "wb")
f_out.write("k <- " + str(len(data)) + "\n")
f_out.write("t <- c(" + ", ".join(map(str, map(day, data))) + ")\n")
f_out.write("pollsters <- " + str(len(pollsters)) + "\n")
f_out.write("pollster <- c(" + ", ".join(map(str, map(pollster, data)))
                             + ")\n")
f_out.write("n <- c(" + ", ".join(map(str, map(lambda datum: datum[2], data)))
                             + ")\n")
var_names = [ "con", "lab", "ld", "ukip", "green", "other" ]
for i in range(3, 9):
	f_out.write(var_names[i-3] + " <- c(")
	party_n = map(lambda datum: round(int(datum[2]) * float(datum[i]) / 100.0),
	              data)
	f_out.write(", ".join(map(str, map(int, party_n))) + ")\n")
f_out.write("sigma_gamma <- 0.08\n")
f_out.write("last_ge <- c(0.364, 0.290, 0.230, 0.031, 0.009, 0.076)\n")
f_out.close()
