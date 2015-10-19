stan <- read.table("nat-1-out.csv", header=TRUE, sep=",")
alpha <- c(mean(stan$alpha.1), mean(stan$alpha.2), mean(stan$alpha.3),
           mean(stan$alpha.4), mean(stan$alpha.5), mean(stan$alpha.6))
print("CON LAB LD UKIP GREEN OTHER")
print(signif(alpha, 3))
print(signif(alpha / sum(alpha), 3))

# old output:
#> print(signif(alpha, 3))
#alpha.1 alpha.2 alpha.3 alpha.4 alpha.5 alpha.6 
#  124.0   124.0    28.4    59.6    26.6    19.9 
#> print(signif(alpha / sum(alpha), 3))
#alpha.1 alpha.2 alpha.3 alpha.4 alpha.5 alpha.6 
# 0.3230  0.3250  0.0743  0.1560  0.0697  0.0519 
#>
