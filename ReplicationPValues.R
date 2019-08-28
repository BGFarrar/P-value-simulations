## Code to plot p-values for replication of published signficant studies of given power
## Copyright Ben Farrar

if (!require("effsize")) install.packages("effsize")
library(effsize)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)                                                                  


## Compute required differences between groups for a one sided two sample t test, with n=10 and alpha=0.05
pwr80 <- power.t.test(10, delta= NULL, 5, power = 0.80, sig.level=0.05, type="two.sample", alternative = "one.sided")$delta
pwr50 <- power.t.test(10, delta= NULL, 5, power = 0.50, sig.level=0.05, type="two.sample", alternative = "one.sided")$delta
pwr20 <- power.t.test(10, delta= NULL, 5, power = 0.20, sig.level=0.05, type="two.sample", alternative = "one.sided")$delta
pwr5 <- 0


set.seed(22071996)

## Run 10,000 simulations for 80% power
p80 <- NULL
for(i in 1:10000){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-pwr80, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p80[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
}

## Run 10,000 simulations for 50% power
p50 <- NULL
for(i in 1:10000){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-pwr50, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p50[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
}

## Run 10,000 simulations for 20% power
p20 <- NULL
for(i in 1:10000){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-pwr20, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p20[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
}

## Run 10,000 simulations for 5% power
p05 <- NULL
for(i in 1:10000){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-pwr5, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p05[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
}

## proportion significant results at each power
sum(p80<0.05)/10000
sum(p50<0.05)/10000
sum(p20<0.05)/10000
sum(p05<0.05)/10000

## collect only signficant p-values at each power
pub80 <- p80[p80<0.05]
pub50 <- p50[p50<0.05]
pub20 <- p20[p20<0.05]
pub05 <- p05[p05<0.05]

## create data frame with p-values and power
sigresults <- c(pub80, pub50, pub20, pub05)
power <- as.factor(c(rep(80, length(pub80)), rep(50, length(pub50)), rep(20, length(pub20)), rep(5, length(pub05))))
sigres <- data.frame("p" = sigresults, "power" = power)

## Plot significant p-values, y axis only p < 0.05
ggplot(sigres, aes(x = power, y = p, color = power)) + geom_jitter() +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + ylim(0,0.05) + 
  theme(legend.position = "none") + ylab("Original p value") + theme(plot.margin = unit(c(1,1,1,1),"cm"))

## Plot significant p-values, y axis only 0 < p < 1
ggplot(sigres, aes(x = power, y = p, colour = power)) + geom_jitter() + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + ylim(0,1) + 
  theme(legend.position = "none") + ylab("Original p value") + theme(plot.margin = unit(c(1,1,1,1),"cm"))


##### Replication studies ######

## Run replication simulations for 80% power, length(pub80) gives original number of significant studies
p80rep <- NULL
for(i in 1:length(pub80)){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-pwr80, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p80rep[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
}

## Run replication simulations for 50% power
p50rep <- NULL
for(i in 1:length(pub50)){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-pwr50, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p50rep[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
}

## Run replication simulations for 20% power
p20rep <- NULL
for(i in 1:length(pub20)){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-pwr20, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p20rep[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
}

## Run replication simulations for 5% power
p05rep <- NULL
for(i in 1:length(pub05)){
  sample1 <- rnorm(10, 50, 5)
  sample1 <- ifelse(sample1 < 0, 0, sample1)
  
  sample2 <- rnorm(10, 50-pwr5, 5)
  sample2 <- ifelse(sample2 < 0, 0, sample2)
  
  p05rep[i] <- t.test(sample1, sample2, alternative = "greater")$p.value
}

## proportion significant results at each power
sum(p80rep<0.05)/length(pub80)
sum(p50rep<0.05)/length(pub50)
sum(p20rep<0.05)/length(pub20)
sum(p05rep<0.05)/length(pub05)

## create data frame with p-values and power
replicationresults <- c(p80rep, p50rep, p20rep, p05rep)
represults <- data.frame("p" = replicationresults, "power" = power)

#### REMINDER OF ORIGINAL P VALUES ##### y axis only p < 0.05
ggplot(sigres, aes(x = power, y = p, color = power)) + geom_jitter() +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + ylim(0,0.05) + 
  theme(legend.position = "none") + ylab("Original p value") + theme(plot.margin = unit(c(1,1,1,1),"cm"))

## Plot replication p-values, y axis only p < 0.05
ggplot(represults, aes(x = power, y = p, color = power)) + geom_jitter() +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + ylim(0,0.05) + 
  theme(legend.position = "none") + ylab("Original p value") + theme(plot.margin = unit(c(1,1,1,1),"cm"))

#### REMINDER OF ORIGINAL P VALUES ##### y axis only p < 0.05
ggplot(sigres, aes(x = power, y = p, colour = power)) + geom_jitter() + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + ylim(0,1) + 
  theme(legend.position = "none") + ylab("Original p value") + theme(plot.margin = unit(c(1,1,1,1),"cm"))

## Plot replication p-values, y axis only 0 < p < 1
ggplot(represults, aes(x = power, y = p, colour = power)) + geom_jitter() + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + ylim(0,1) + 
  theme(legend.position = "none") + ylab("Original p value") + theme(plot.margin = unit(c(1,1,1,1),"cm"))



