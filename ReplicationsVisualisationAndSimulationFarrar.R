## Code accompanying Farrar, Voudouris and Clayton: 
## Replications, Comparisons, Sampling and the Problem
## of Representativeness in Animal Behavior and Cognition Research

## Code sufficient to reproduce the simualation results and Figures 4-8

## Packages, install if necessary
library(ggplot2)
library(tidyverse)
library(extrafont)
## NB, for extrafont to work, if it is your first time using, fonts must be registered with the computer. See help files. 
library(scales)


# set parameters for behavior 1
b01 <- 800 # intercept; i.e., the grand mean
S0s_sd1 <- 100 # by-subject random intercept sd
L0l_sd1 <- 100 # by-location random intercept sd
err_sd1 <- 50 # residual error

# set parameters for behaviour2
b02 <- 80 # intercept; i.e., the grand mean
S0s_sd2 <- 10 # by-subject random intercept sd
L0l_sd2 <- 10 # by-location random intercept sd
err_sd2 <- 5 # residual error


## generate sites and subjects
nsites <- 100
nsubj <- 100

subjectID <- as.factor(1:(nsubj*nsites))
siteID <- as.factor(rep(seq(1:nsites), each=nsubj)) 
subjeff1 <- rnorm(nsites*nsubj, 0, S0s_sd1)
locationeff1 <- rep(rnorm(nsites, 0, L0l_sd1), each=nsubj)
error1 <- rnorm(nsites*nsubj, 0, err_sd1)
subjeff2 <- rnorm(nsites*nsubj, 0, S0s_sd2)
locationeff2 <- rep(rnorm(nsites, 0, L0l_sd2), each=nsubj)
error2 <- rnorm(nsites*nsubj, 0, err_sd2)

data <- data.frame(subjectID, siteID, subjeff1, locationeff1, b01, error1, subjeff2, locationeff2, b02, error2)
data$behavior1 <- data$subjeff1 + data$locationeff1 + data$b01 + data$error1
data$behavior2 <- data$subjeff2 + data$locationeff2 + data$b02 + data$error2

## Figure 4A: All animals 

ggplot(data, aes(x=behavior1, y=behavior2)) + geom_point(alpha=0.5, colour="grey") + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=16)
  )

## Figure 4B: All animals by site

ggplot(data, aes(x=behavior1, y=behavior2, colour=siteID)) + geom_point(alpha=0.1) + 
  xlab("Neophobia") + ylab("Self-control") + scale_fill_ordinal() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=16)
  )

## Figure 5A: Sample from and plot single site

s <- round(runif(1, 0, nsites))
singlesite <- subset(data, siteID == s)

ggplot(data, aes(x=behavior1, y=behavior2)) + geom_point(alpha=0.1, colour="grey") +
  geom_point(data = singlesite, aes(x=behavior1, y=behavior2), color="#481568FF") + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=16)
  )

## Sample 10 individuals from the site

singlesitesample <- sample_n(singlesite, 10, FALSE)

## Figure 5B: highlight the 10 individuals from this site

ggplot(data, aes(x=behavior1, y=behavior2, colour=4)) + geom_point(alpha=0.1, colour="grey") + theme_minimal() +
  geom_point(data = singlesitesample, aes(x=behavior1, y=behavior2), color="#481568FF", size=3) + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=16)
  )

## Perform "replication study". Sample another 10 individuals from another site.

s2 <- round(runif(1, 0, nsites))
secondsite <- subset(data, siteID == s2)
secondsitesample <- sample_n(secondsite, 10, FALSE)

## Figure 6: Replication study

ggplot(data, aes(x=behavior1, y=behavior2, colour=4)) + geom_point(alpha=0.1, colour="grey") + theme_minimal() +
  geom_point(data = singlesitesample, aes(x=behavior1, y=behavior2), color="#481568FF", size= 3) +  
  geom_point(data = secondsitesample, aes(x=behavior1, y=behavior2), color="chocolate1", size = 3) + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=16)
  )

## Compare behavior at both sites

t.test(singlesitesample$behavior1, secondsitesample$behavior1, alternative = "two.sided")
t.test(singlesitesample$behavior2, secondsitesample$behavior2, alternative = "two.sided")


## Compare observed effects relative to original paramters

(secondsite$locationeff1 - singlesitesample$locationeff1) / b01

(mean(secondsitesample$behavior1) - mean(singlesitesample$locationeff1)) / b01


(secondsite$locationeff2 - singlesitesample$locationeff2) / b02

(mean(secondsitesample$behavior2) - mean(singlesitesample$locationeff2)) / b02


mean(secondsitesample$behavior1) / b01
mean(singlesitesample$locationeff1) / b01

mean(singlesitesample$locationeff2) / b02
mean(secondsitesample$behavior2) / b02


### Comparison, simulate a second species

# behavior 1
b012 <- b01 + 1.5*L0l_sd1 # intercept; i.e., the grand mean
b022 <- 1*b02

## simulate new data
subjeff21 <- rnorm(nsites*nsubj, 0, S0s_sd1)
locationeff21 <- rep(rnorm(nsites, 0, L0l_sd1), each=nsubj)
error21 <- rnorm(nsites*nsubj, 0, err_sd1)
subjeff22 <- rnorm(nsites*nsubj, 0, S0s_sd2)
locationeff22 <- rep(rnorm(nsites, 0, L0l_sd2), each=nsubj)
error22 <- rnorm(nsites*nsubj, 0, err_sd2)

data$Species <- "Chimpanzees"

data2 <- data.frame(subjectID, siteID, subjeff21, locationeff21, b012, error21, subjeff22, locationeff22, b022, error22)
data2$behavior1 <- data2$subjeff21 + data2$locationeff21 + data2$b012 + data2$error21
data2$behavior2 <- data2$subjeff22 + data2$locationeff22 + data2$b022 + data2$error22
data2$Species <- "Bonobos"


compdata <- rbind(data[,c(1, 2, 11, 12, 13)], data2[,c(1, 2, 11, 12, 13)])

(mean(data2$behavior1) - mean(data$behavior1)) / sd(data$behavior1)

## Figure 7A: Display global behavior for both "species"

ggplot(compdata, aes(x=behavior1, y=behavior2, colour=Species)) + geom_point(alpha=0.2) +
  scale_color_manual(values = c("grey", "lightblue")) +
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=16)
  )


## Generate data for 10 individuals from a single site Species 1

sp1 <- round(runif(1, 0, nsites))
sp1singlesite <- subset(compdata, Species == "Chimpanzees" & siteID == sp1)
sp1singlesitesample <- sample_n(sp1singlesite, 10, FALSE)


## Generate data for 10 individuals from a single site Species 2

sp2 <- round(runif(1, 0, nsites))
sp2secondsite <- subset(compdata, Species == "Bonobos" & siteID == sp2)
sp2secondsitesample <- sample_n(sp2secondsite, 10, FALSE)

## Figure 7B: Plot species comparison

ggplot(compdata, aes(x=behavior1, y=behavior2, colour=Species)) + geom_point(alpha=0.05) +
  geom_point(data = sp1singlesitesample, aes(x=behavior1, y=behavior2), color="blue", size = 3) + 
  geom_point(data = sp2secondsitesample, aes(x=behavior1, y=behavior2), color="black", size = 3) + 
  scale_color_manual(values = c("grey", "lightblue")) +
  theme_minimal()  + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=16)
  )

t.test(sp2secondsitesample$behavior1, sp1singlesitesample$behavior1, alternative = "two.sided")
t.test(sp2secondsitesample$behavior2, sp1singlesitesample$behavior2, alternative = "two.sided")


### Simulation Study

### set up new data with new params if required, larger sample for power calculations


set.seed(64)
# behavior 1
b01 <- 800 # intercept; i.e., the grand mean
S0s_sd1 <- 100 # by-subject random intercept sd
L0l_sd1 <- 100 # by-location random intercept sd
err_sd1 <- 50 # residual error

# behaviour2
b02 <- 80 # intercept; i.e., the grand mean
S0s_sd2 <- 10 # by-subject random intercept sd
L0l_sd2 <- 10 # by-location random intercept sd
err_sd2 <- 5 # residual error

## bonobo behavior 1
bb01 <- b01 + 1.5*L0l_sd1

## bonobo behavior 2

bb02 <- b02 + 0* L0l_sd2

## Generate sites and subjects, perform t-test comparing randomly sampled site of chimpanzees with randomly sampleed 
## site of bonobos

subj <- 10

pb1 <- NULL
pb2 <- NULL
stat1 <- NULL
stat2 <- NULL
nSims <- 100000
for(i in 1:nSims){
  chimpbehavior1 <- rnorm(subj, 0, S0s_sd1) + rnorm(1, 0, L0l_sd1) + b01 + rnorm(10, 0, err_sd1) 
  chimpbehavior2 <- rnorm(subj, 0, S0s_sd2) + rnorm(1, 0, L0l_sd2) + b02 + rnorm(10, 0, err_sd2) 
  bonbehavior1 <- rnorm(subj, 0, S0s_sd1) + rnorm(1, 0, L0l_sd1) + bb01 + rnorm(10, 0, err_sd1) 
  bonbehavior2 <- rnorm(subj, 0, S0s_sd2) + rnorm(1, 0, L0l_sd2) + bb02 + rnorm(10, 0, err_sd2)
  pb1[i] <- t.test(chimpbehavior1, bonbehavior1, alternative = "two.sided")$p.value
  stat1[i] <- t.test(chimpbehavior1, bonbehavior1, alternative = "two.sided")$statistic
  pb2[i] <- t.test(chimpbehavior2, bonbehavior2, alternative = "two.sided")$p.value
  stat2[i] <- t.test(chimpbehavior2, bonbehavior2, alternative = "two.sided")$statistic
}

hist(pb1, breaks = 20)
sum(pb1<0.05)/nSims

hist(pb2, breaks = 20)
sum(pb2<0.05)/nSims

hist(stat1)

hist(stat2)

stat11 <- data.frame("p" = pb1, "stat" = -stat1, "comp" = "Neophobia")
stat12 <- data.frame("p" = pb2, "stat" = stat2, "comp" = "Self-Control")

stat <- rbind(stat11, stat12)

my_x_title <- expression(paste(italic("p"), "-value"))

## Figure 9: p-value distributions

ggplot(stat, aes(x=p)) + geom_density(alpha=0.2) + facet_grid(.~comp) + xlim(0, 1) + 
  geom_vline(xintercept = 0.05, linetype="dotted", 
  color = "blue", size=1) +
  theme_minimal()  + 
  xlab(my_x_title) + 
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    text=element_text(family="Times New Roman", size=16)
  )






                            
                            