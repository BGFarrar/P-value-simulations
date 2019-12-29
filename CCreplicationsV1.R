## Simulation code accompanying "What can comparative cognition expect from replication studies?"
## Ben Farrar

if (!require("effsize")) install.packages("effsize")
library(effsize)

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)  

#### Secion 1 Stimulation Study ####

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

t.test(sample1, sample2, alternative = "greater", var.equal = TRUE)

## proportion significant
sum(p<0.05)/10000

## Compute mean difference between groups in all samples
mean(diff)
sd(diff)

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


## mathematicaly expected replication success for this simulation
(pless01 <- (fivep1*0.05 + twentyp1*0.2 + fiftyp1*0.5 + eightyp1*0.8)/(fivep1 + twentyp1 + fiftyp1 + eightyp1))
(p0102 <- (fivep2*0.05 + twentyp2*0.2 + fiftyp2*0.5 + eightyp2*0.8)/(fivep2 + twentyp2 + fiftyp2 + eightyp2))
(p0203 <- (fivep3*0.05 + twentyp3*0.2 + fiftyp3*0.5 + eightyp3*0.8)/(fivep3 + twentyp3 + fiftyp3 + eightyp3))
(p0304 <- (fivep4*0.05 + twentyp4*0.2 + fiftyp4*0.5 + eightyp4*0.8)/(fivep4 + twentyp4 + fiftyp4 + eightyp4))
(p0405 <- (fivep5*0.05 + twentyp5*0.2 + fiftyp5*0.5 + eightyp5*0.8)/(fivep5 + twentyp5 + fiftyp5 + eightyp5))


(p0405noeighty <- (fivep5*0.05 + twentyp5*0.2 + fiftyp5*0.5)/(fivep5 + twentyp5 + fiftyp5))


## Proportion of successfull replications overall

(sum(p <= 0.05)*0.8 + sum(p1 <= 0.05)*0.5 + sum(p2 <= 0.05)*0.2 + sum(p3 <= 0.05)*0.05) / (sum(p <= 0.05) + sum(p1 <= 0.05) + sum(p2 <= 0.05) + sum(p3 <= 0.05))

## mathematically expected replication success for studies like these in general

0.8*(0.8/(0.8+0.5+0.2+0.05)) + 0.5*(0.5/(0.8+0.5+0.2+0.05)) + 0.2*(0.2/(0.8+0.5+0.2+0.05)) + 0.05*(0.05/(0.8+0.5+0.2+0.05))



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

## Code provided by Manuel Bohn

library(tidyverse)

library(ggthemes)



allresults2 <- allresults %>%
  
  gather(study, p, - power)%>%
  
  mutate(study = ifelse(study == "p", "Original", "Replication"))



ggplot(data = allresults2, aes(x = p, col = study, fill = study))+
  
  geom_density(alpha = .75)+
  
  geom_vline(xintercept = 0.05, lty = 2)+
  
  theme_bw() + xlim (0,1)+ facet_grid(power~.)+
  
  + ylim()
  
  scale_color_ptol()+
  
  scale_fill_ptol()

## End Manuel Bohn code

setwd("~/PhD/Thesis/What can CC expect from replication studies")
(p <- ggplot(allresults, aes(x = power, y = p, colour = power)) + geom_jitter() + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + ylim(0,1) + 
  theme(legend.position = "none") + ylab("Original p value") + theme(plot.margin = unit(c(1,1,1,1),"cm")))
ggsave("repp1.png", plot = p)
(repp <- ggplot(allresults, aes(x = power, y = replicationp, colour = power)) + geom_jitter()  +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + theme_bw() + theme(legend.position = "none")  + 
  ylab("Replication p value") + theme(plot.margin = unit(c(1,1,1,1),"cm")))
ggsave("repp2.png", plot = repp)

#### Secion 2 Stimulation Study ####
### Data simulation function - DeBruine and Barr 2019 - doi: 10.31234/osf.io/xp5cy

#### Load packages and set up functions

library("lme4")        # model specification / estimation
library("afex")        # anova and deriving p-values from lmer
library("broom.mixed") # extracting data from model fits
library("Rmisc")

# set up the custom data simulation function
my_sim_data <- function(nsubj  = 7, # number of subjects
                        nitem  = c(possible = 5, impossible = 5),  # number of items
                        b0     = 1000, # grand mean
                        b1     =  200, # effect of category
                        I0i_sd =  5, # by-item random intercept sd
                        S0s_sd = 100, # by-subject random intercept sd
                        S1s_sd =  40, # by-subject random slope sd
                        scor   = 0.2, # correlation between intercept and slope
                        err_sd = 200  # residual (standard deviation)
) {
  # simulate items
  items <- data.frame(
    item_id = 1:sum(nitem),
    category = rep(c("possible", "impossible"), nitem),
    cat = rep(c(-0.5, +0.5), nitem), # effect-code category
    I0i = rnorm(sum(nitem), 0, I0i_sd)
  )
  
  # simulate subjects
  Sigma <- matrix(c(S0s_sd^2, S0s_sd * S1s_sd * scor,
                    S0s_sd * S1s_sd * scor, S1s_sd^2), 
                  nrow = 2, byrow = TRUE) 
  S <-MASS::mvrnorm(nsubj, c(0, 0), Sigma)
  
  subjects <- data.frame(
    subj_id = 1:nsubj,
    S0s = S[, 1],
    S1s = S[, 2]
  )
  
  # simulate trials
  trials <- expand.grid(subj_id = subjects$subj_id,
                        item_id = items$item_id)
  trials$err = rnorm(nrow(trials), mean = 0, sd = err_sd)
  
  # join subject and item tables
  joined <- merge(trials, subjects, by = "subj_id")
  dat_sim <- merge(joined, items, by = "item_id")
  dat_sim$LT <- b0 + dat_sim$I0i + dat_sim$S0s + 
    (b1 + dat_sim$S1s) * dat_sim$cat + dat_sim$err
  
  dat_sim
}

# set up the power function
my_lmer_power <- function(...) {
  # ... is a shoLTcut that forwards any arguments to my_sim_data()
  dat_sim <- my_sim_data(...)
  ## Running model with just random intercepts
  mod_sim <- lmer(LT ~ cat + (1 | subj_id),
                  dat_sim, REML = FALSE)
  
  broom.mixed::tidy(mod_sim)
}
#### Run simulations ####
## Running simulations for 1, 5 and 100 trials in each condition, with a large (200 msec), medium (100 msec) 
## or small (50 msec), effect size (my definitions, not Cohen's).
set.seed(20)

## Number of simulations
nSims <- 10000
alpha <- 0.05

#### One trial per condition ####

## 200 msec effect size, 1 trial each condition
## here, using a paired t test as most lmers were singular/failed to converge
# set up power function for single trial
p1.200 <- NULL
est1.200 <- NULL

## p1.200lmer <- NULL code to run and get p vals from lmer not run for time purposes
## est1.200lmer <- NULL

for(i in 1:nSims){
  dat_sim <- my_sim_data(b1=200, nitem  = c(possible = 1, impossible = 1))
  ## mod_sim <- lmer(LT ~ cat + (1 | subj_id),
  ##   dat_sim, REML = FALSE)
  ## p1.200lmer[i] <- broom.mixed::tidy(mod_sim)$p.value[2]
  ## est1.200lmer[i] <- broom.mixed::tidy(mod_sim)$estimate[2]
  b <- spread(dat_sim[ , c(2, 6, 9)], category, LT)
  p1.200[i] <- t.test(b$impossible, b$possible, paired = TRUE)$p.value
  est1.200[i] <- t.test(b$impossible, b$possible, paired = TRUE)$estimate
}

## data.frame(est1.200lmer, est1.200)

## pwr.1.200lmer <- sum(p1.200lmer <0.05)/nSims

(pwr1.200 <- sum(p1.200<alpha)/nSims)

## number significant 
sum(p1.200<alpha)

## nummber significant in wrong direction
sum(p1.200<alpha & est1.200 < 0 )

## plot an example
dat_sim <- my_sim_data(b1=200, nitem  = c(possible = 1, impossible = 1))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot1.200 <- ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +   xlab(paste("power =", round(pwr1.200, digits = 2))) +   ylab("Looking Time") + coord_cartesian(ylim=c(600,1500))

## 100 msec effect size, 1 trial each condition
## here, using a paired t test as most lmers were singular/failed to converge
# set up power function for single trial
p1.100 <- NULL
est1.100 <- NULL

for(i in 1:nSims){
  dat_sim <- my_sim_data(b1=100, nitem  = c(possible = 1, impossible = 1))
  b <- spread(dat_sim[ , c(2, 6, 9)], category, LT)
  p1.100[i] <- t.test(b$impossible, b$possible, paired = TRUE)$p.value
  est1.100[i] <- t.test(b$impossible, b$possible, paired = TRUE)$estimate
}

(pwr1.100 <- sum(p1.100<alpha)/nSims)
mean(est1.100)

## plot an example
dat_sim <- my_sim_data(b1=100, nitem  = c(possible = 1, impossible = 1))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot1.100 <- ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +   xlab(paste("power =", round(pwr1.100, digits = 2))) +   ylab("Looking Time") + coord_cartesian(ylim=c(600,1500))

## 50 msec effect size, 1 trial each condition
## here, using a paired t test as most lmers were singular/failed to converge
# set up power function for single trial
p1.50 <- NULL
est1.50 <- NULL

for(i in 1:nSims){
  dat_sim <- my_sim_data(b1=50, nitem  = c(possible = 1, impossible = 1))
  b <- spread(dat_sim[ , c(2, 6, 9)], category, LT)
  p1.50[i] <- t.test(b$impossible, b$possible, paired = TRUE)$p.value
  est1.50[i] <- t.test(b$impossible, b$possible, paired = TRUE)$estimate
}

(pwr1.50 <- sum(p1.50<alpha)/nSims)
mean(est1.50)

## plot an example
dat_sim <- my_sim_data(b1=50, nitem  = c(possible = 1, impossible = 1))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot1.50 <-ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +   xlab(paste("power =", round(pwr1.50, digits = 2 ))) +   ylab("Looking Time") + coord_cartesian(ylim=c(600,1500))

#### Five trials per condition ####

## 200 msec effect size
sims <- replicate(nSims, my_lmer_power(b1=200, nitem  = c(possible = 5, impossible = 5)), simplify = FALSE)
sims <- lapply(sims, as.data.frame)
sims <- do.call("rbind", sims)

fcat <- sims[sims$effect == "fixed" & sims$term == "cat", ]
(pwr5.200 <- mean(fcat$p.value < alpha))
mean_estimate5.200 <- mean(fcat$estimate)
mean_se5.200 <- mean(fcat$std.error)

## plot an example
dat_sim <- my_sim_data(b1=200, nitem  = c(possible = 5, impossible = 5))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot5.200 <-ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +   xlab(paste("power =", round(pwr5.200, digits = 2))) +   ylab("Looking Time") + coord_cartesian(ylim=c(600,1500))

## 100 msec effect size
sims <- replicate(nSims, my_lmer_power(b1=100, nitem  = c(possible = 5, impossible = 5)), simplify = FALSE)
sims <- lapply(sims, as.data.frame)
sims <- do.call("rbind", sims)

fcat <- sims[sims$effect == "fixed" & sims$term == "cat", ]
(pwr5.100 <- mean(fcat$p.value < alpha))
mean_estimate5.100 <- mean(fcat$estimate)
mean_se5.100 <- mean(fcat$std.error)

## plot an example
dat_sim <- my_sim_data(b1=100, nitem  = c(possible = 5, impossible = 5))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot5.100 <-ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +   xlab(paste("power =", round(pwr5.100, digits = 2))) +   ylab("Looking Time") + coord_cartesian(ylim=c(600,1500))

## 50 msec effect size
sims <- replicate(nSims, my_lmer_power(b1=50, nitem  = c(possible = 5, impossible = 5)), simplify = FALSE)
sims <- lapply(sims, as.data.frame)
sims <- do.call("rbind", sims)

fcat <- sims[sims$effect == "fixed" & sims$term == "cat", ]
(pwr5.50 <- mean(fcat$p.value < alpha))
mean_estimate5.50 <- mean(fcat$estimate)
mean_se5.50 <- mean(fcat$std.error)

## plot an example
dat_sim <- my_sim_data(b1=50, nitem  = c(possible = 5, impossible = 5))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot5.50 <-ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +   xlab(paste("power =", round(pwr5.50, 2))) +   ylab("Looking Time") + coord_cartesian(ylim=c(600,1500))

#### One-hundred trials per condition ####

## 200 msec effect size
sims <- replicate(nSims, my_lmer_power(b1=200, nitem  = c(possible = 100, impossible = 100)), simplify = FALSE)
sims <- lapply(sims, as.data.frame)
sims <- do.call("rbind", sims)

fcat <- sims[sims$effect == "fixed" & sims$term == "cat", ]
(pwr100.200 <- mean(fcat$p.value < alpha))
mean_estimate100.200 <- mean(fcat$estimate)
mean_se100.200 <- mean(fcat$std.error)

## plot an example
dat_sim <- my_sim_data(b1=200, nitem  = c(possible = 100, impossible = 100))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot100.200 <- ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  xlab(paste("power =", round(pwr100.200, digits = 2))) +
  ylab("Looking Time") + coord_cartesian(ylim=c(600,1500))

## 100 msec effect size
sims <- replicate(nSims, my_lmer_power(b1=100, nitem  = c(possible = 100, impossible = 100)), simplify = FALSE)
sims <- lapply(sims, as.data.frame)
sims <- do.call("rbind", sims)

fcat <- sims[sims$effect == "fixed" & sims$term == "cat", ]
(pwr100.100 <- mean(fcat$p.value < alpha))
mean_estimate100.100 <- mean(fcat$estimate)
mean_se100.100 <- mean(fcat$std.error)

## plot an example
dat_sim <- my_sim_data(b1=100, nitem  = c(possible = 100, impossible = 100))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot100.100 <- ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +   xlab(paste("power =", round(pwr100.100, digits =2 ))) +   ylab("Looking Time") + coord_cartesian(ylim=c(600,1500))

## 50 msec effect size
sims <- replicate(nSims, my_lmer_power(b1=50, nitem  = c(possible = 100, impossible = 100)), simplify = FALSE)
sims <- lapply(sims, as.data.frame)
sims <- do.call("rbind", sims)

fcat <- sims[sims$effect == "fixed" & sims$term == "cat", ]
(pwr100.50 <- mean(fcat$p.value < alpha))
mean_estimate100.50 <- mean(fcat$estimate)
mean_se100.50 <- mean(fcat$std.error)

## plot an example
dat_sim <- my_sim_data(b1=50, nitem  = c(possible = 100, impossible = 100))
dat_sim$subj_id <- factor(dat_sim$subj_id)

df <- summarySE(dat_sim, measurevar="LT", groupvars=c("category","subj_id"))

plot100.50 <- ggplot(df, aes(x=category, y=LT, group=subj_id, color=subj_id)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LT-ci, ymax=LT+ci), width=.2,
                position=position_dodge(0.05)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +   xlab(paste("power =", round(pwr100.50, digits = 2))) +  
  ylab("Looking Time") + coord_cartesian(ylim=c(600,1500)) + coord_cartesian(ylim=c(600,1500))

#### Plot all ####
par(mfrow = c(3, 3)) # Create a 3 x 3 plotting matrix
library(gridExtra)
grid.arrange(plot1.50, plot5.50, plot100.50, plot1.100, plot5.100, plot100.100, plot1.200, plot5.200, plot100.200, nrow=3)




