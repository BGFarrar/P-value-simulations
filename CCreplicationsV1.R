## Simulation code accompanying "What can comparative cognition expect from replication studies?"
## Copyright Ben Farrar

if (!require("effsize")) install.packages("effsize")
library(effsize)

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)   

## Compute required differences between groups for a one sided two sample t test, with n=10 and alpha=0.05 ####

power.t.test(10, delta= NULL, 5, power = 0.8, sig.level=0.05, type="two.sample", alternative = "one.sided")
## 80% requires 5.781487 difference between groups

power.t.test(10, delta= NULL, 5, power = 0.5, sig.level=0.05, type="two.sample", alternative = "one.sided")
## 50% requires 3.822626 difference between groups

power.t.test(10, delta= NULL, 5, power = 0.2, sig.level=0.05, type="two.sample", alternative = "one.sided")
## 20% power requires 1.865756 difference between groups

#### Original Study Simulations #####
## Run 10,000 simulations for 80% power
p <- NULL
diff <- NULL

set.seed(22071996)
for(i in 1:10000){
sample1 <- rnorm(10, 50, 5)
sample1 <- ifelse(sample1 < 0, 0, sample1)

sample2 <- rnorm(10, 50-5.781487, 5)
sample2 <- ifelse(sample2 < 0, 0, sample2)

p[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
diff[i] <- mean(sample1) - mean(sample2)
}

## proportion significant
sum(p<0.05)/10000

## Compute mean difference between groups in all samples
mean(diff)

## Compute mean difference between groups in samples leading to p<0.05
d <- data.frame(p, diff)
d1 <- subset(d, p<0.05)
mean(d1$diff)

## Compute inflation of difference estimate in p<0.05 samples vs all samples
(mean(d1$diff)-mean(diff))/mean(diff)


## Run 10,000 simulations for 50% power
p1 <- NULL
diff1 <- NULL

for(i in 1:10000){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-3.822626, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p1[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
  diff1[i] <- mean(sample1) - mean(sample2)
}

## proportion significant
sum(p1<0.05)/10000

## Compute mean difference between groups in all samples
mean(diff1)

## Compute mean difference between groups in samples leading to p<0.05
e <- data.frame(p1, diff1)
e1 <- subset(e, p1<0.05)
mean(e1$diff1)

## Compute inflation of difference estimate in p<0.05 samples vs all samples
(mean(e1$diff1)-mean(diff1))/mean(diff1)



## Run 10,000 simulations at 20% power
p2 <- NULL
diff2 <- NULL

for(i in 1:10000){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-1.865756, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p2[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
  diff2[i] <- mean(sample1) - mean(sample2)
}

## total significant
sum(p2<0.05)/10000

## Compute mean difference between groups in all samples
mean(diff2)

## Compute mean difference between groups in samples leading to p<0.05
f <- data.frame(p2, diff2)
f1 <- subset(f, p2<0.05)
mean(f1$diff2)

## Compute inflation of difference estimate in p<0.05 samples vs all samples
(mean(f1$diff2)-mean(diff2))/mean(diff2)


## Run 10,000 simulations at 5% power - only false positives
p3 <- NULL
diff3 <- NULL

for(i in 1:10000){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p3[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
  diff3[i] <- mean(sample1) - mean(sample2)
}

## total significant
sum(p3<0.05)/10000

## Compute mean difference between groups in all samples
mean(diff3)

## Compute mean difference between groups in samples leading to p<0.05
g <- data.frame(p3, diff3)
g1 <- subset(g, p3<0.05)
mean(g1$diff3)
## Compute inflation of difference estimate in p<0.05 samples vs all samples
(mean(g1$diff3)-mean(diff3))/mean(diff3)


#### Caculate expected number of successfull replications ####
## Mathematically derived using power * proportion / total replication attempts
(0.05*0.05+0.2*0.2 + 0.5*0.5 + 0.8*0.8)/(0.8+0.5+0.2+0.05)

## Number of successful replications by p-value
fivep1 <- sum(p3 <= 0.01)
fivep2 <- sum(0.01 < p3 & p3 <= 0.02)
fivep3 <- sum(0.02 < p3 & p3 <= 0.03)
fivep4 <- sum(0.03 < p3 & p3 <= 0.04)
fivep5 <- sum(0.04 < p3 & p3 <= 0.05)

twentyp1 <- sum(p2 <= 0.01)
twentyp2 <- sum(0.01 < p2 & p2 <= 0.02)
twentyp3 <- sum(0.02 < p2 & p2 <= 0.03)
twentyp4 <- sum(0.03 < p2 & p2 <= 0.04)
twentyp5 <- sum(0.04 < p2 & p2 <= 0.05)

fiftyp1 <- sum(p1 <= 0.01)
fiftyp2 <- sum(0.01 < p1 & p1 <= 0.02)
fiftyp3 <- sum(0.02 < p1 & p1 <= 0.03)
fiftyp4 <- sum(0.03 < p1 & p1 <= 0.04)
fiftyp5 <- sum(0.04 < p1 & p1 <= 0.05)

eightyp1 <- sum(p <= 0.01)
eightyp2 <- sum(0.01 < p & p <= 0.02)
eightyp3 <- sum(0.02 < p & p <= 0.03)
eightyp4 <- sum(0.03 < p & p <= 0.04)
eightyp5 <- sum(0.04 < p & p <= 0.05)


## mathematicaly expected replication success
(pless01 <- (fivep1*0.05 + twentyp1*0.2 + fiftyp1*0.5 + eightyp1*0.8)/(fivep1 + twentyp1 + fiftyp1 + eightyp1))
(p0102 <- (fivep2*0.05 + twentyp2*0.2 + fiftyp2*0.5 + eightyp2*0.8)/(fivep2 + twentyp2 + fiftyp2 + eightyp2))
(p0203 <- (fivep3*0.05 + twentyp3*0.2 + fiftyp3*0.5 + eightyp3*0.8)/(fivep3 + twentyp3 + fiftyp3 + eightyp3))
(p0304 <- (fivep4*0.05 + twentyp4*0.2 + fiftyp4*0.5 + eightyp4*0.8)/(fivep4 + twentyp4 + fiftyp4 + eightyp4))
(p0405 <- (fivep5*0.05 + twentyp5*0.2 + fiftyp5*0.5 + eightyp5*0.8)/(fivep5 + twentyp5 + fiftyp5 + eightyp5))


(p0405noeighty <- (fivep5*0.05 + twentyp5*0.2 + fiftyp5*0.5)/(fivep5 + twentyp5 + fiftyp5))







##### plot p value distributions and simulate replication studies #####
## plot p values from original studies 

d1.1 <- d1$p
e1.1 <- e1$p1
f1.1 <- f1$p2
g1.1 <- g1$p3

sigresults <- c(d1.1, e1.1, f1.1, g1.1)
power <- as.factor(c(rep(80, nrow(d1)), rep(50, nrow(e1)), rep(20, nrow(f1)), rep(5, nrow(g1))))
sigres <- data.frame("p" = sigresults, "power" = power)
                                                               
ggplot(sigres, aes(x = power, y = p, color = power)) + geom_jitter() + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme(legend.position = "none")

ggplot(sigres, aes(x = 1, y = p)) + geom_jitter(colour = power) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme(legend.position = "none")



## Run replication studies
sum(p<0.05)
## Run replication simulations for 80% power
ap <- NULL
adiff <- NULL
for(i in 1:nrow(d1)){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-5.781487, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  ap[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
  adiff[i] <- mean(sample1) - mean(sample2)
}

## proportion significant
sum(ap<0.05)/nrow(d1)

## Compute mean difference between groups in all samples
mean(adiff)




## Run replication simulations for 50% power
ap1 <- NULL
adiff1 <- NULL

for(i in 1:nrow(e1)){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-3.822626, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  ap1[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
  adiff1[i] <- mean(sample1) - mean(sample2)
}

## proportion significant
sum(ap1<0.05)/nrow(e1)

## Compute mean difference between groups in all samples
mean(adiff1)




## Run replication simulations for 20% power
ap2 <- NULL
adiff2 <- NULL

for(i in 1:nrow(f1)){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-1.865756, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  ap2[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
  adiff2[i] <- mean(sample1) - mean(sample2)
}

## total significant
sum(ap2<0.05)/nrow(f1)

## Compute mean difference between groups in all samples
mean(adiff2)


## Run replication simulations for 5% power 
ap3 <- NULL
adiff3 <- NULL

for(i in 1:nrow(g1)){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  ap3[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
  adiff3[i] <- mean(sample1) - mean(sample2)
}

## total significant
sum(ap3<0.05)/nrow(g1)

## Compute mean difference between groups in all samples
mean(adiff3)

#### Plot overall p value distributions #####
replicationresults <- c(ap, ap1, ap2, ap3)


allresults <- data.frame("p" = sigresults, "power" = power, "replicationp" = replicationresults)

setwd("~/PhD/Thesis/What can CC expect from replication studies")
(p <- ggplot(allresults, aes(x = power, y = p, colour = power)) + geom_jitter() + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + ylim(0,1) + 
  theme(legend.position = "none") + ylab("Original p value") + theme(plot.margin = unit(c(1,1,1,1),"cm")))
ggsave("repp1.png", plot = p)
(repp <- ggplot(allresults, aes(x = power, y = replicationp, colour = power)) + geom_jitter()  +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + theme(legend.position = "none")  + 
  ylab("Replication p value") + theme(plot.margin = unit(c(1,1,1,1),"cm")))
ggsave("repp2.png", plot = repp)

