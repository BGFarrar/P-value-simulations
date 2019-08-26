## P value distributions with peak P 0.03 - 0.05 without p hacking
## p value distributions of Mann Whitney U test, low power and small sample size
p <- NULL

set.seed(1739)
for(i in 1:100000){
  sample1 <- rnorm(6, 111.1, 15)

  sample2 <- rnorm(6, 100, 15)

  p[i] <- wilcox.test(sample1, sample2, alternative = "greater")$p.value
}

## proportion significant
sum(p<0.05)/100000
## power = ~ 0.3

hist(p, xlim=c(0, 0.1), breaks=99, main="p value distribution Mann-Whitney n1=6, n2=6, pwr = 0.3")
abline(h=100000*0.01, col = "red", lty = 2)

## Similar result is obtained using exact rank test 


### Simulate null distribution

q <- NULL

for(i in 1:100000){
  sample1 <- rnorm(6, 100, 15)
  
  sample2 <- rnorm(6, 100, 15)
  
  q[i] <- wilcox.test(sample1, sample2, alternative = "greater")$p.value
}

## proportion significant
sum(q<0.05)/100000
## power = ~ 0.3

hist(q, xlim=c(0, 0.1), breaks=99, main="p values Mann-Whitney n1=6, n2=6, no effect")
abline(h=100000*0.01, col = "red", lty = 2)



### Correct p-value distribution for effect given null distribution
bin1 <- sum(q <= 0.01)
bin2 <- sum(0.01 < q & q <= 0.02)
bin3 <- sum(0.02 < q & q <= 0.03)
bin4 <- sum(0.03 < q & q <= 0.04)
bin5 <- sum(0.04 < q & q <= 0.05)


mean <- (bin1+bin2+bin3+bin4+bin5)/5

pbin1 <- sum(p <= 0.01)
pbin2 <- sum(0.01 < p & p <= 0.02)
pbin3 <- sum(0.02 < p & p <= 0.03)
pbin4 <- sum(0.03 < p & p <= 0.04)
pbin5 <- sum(0.04 < p & p <= 0.05)


p1 <- pbin1*mean/bin1
p2 <- pbin2*mean/bin2
p3 <- pbin3*mean/bin3
p4 <- pbin4*mean/bin4
p5 <- pbin5*mean/bin5

d <- data.frame(x = seq(0.01, 0.05, 0.01), y = as.numeric(c(p1, p2, p3, p4, p5)))


barplot(d$y, space = 0, names.arg = d$x, col = "white", 
        main="CORRECTED p value dist Mann-Whitney n1=6, n2=6, pwr = 0.3", xlab = "corrected p", ylab = "Frequency")
abline(h=100000*0.01, col = "red", lty = 2)

## copyright Benjamin Farrar

