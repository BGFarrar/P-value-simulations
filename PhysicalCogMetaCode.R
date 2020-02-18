## Code accompanying "Claims and statistical inference in studies of animal physical cognition." by Farrar et al. 

## setwd() ## set the working directory to the file location

## load packages
library(tidyverse)
library(ggplot2)
library(extrafont)

## RUN THE TWO LINES BELOW IF FIRST TIME USING EXTRA FONT
## font_import()
## loadfonts(device="win")       #Register fonts for Windows bitmap output

windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
##Load data
pcog <- read.csv("PhysicalCogData.csv", header=T)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### Sample size ####
pcog$SampleSize <- as.numeric(pcog$SampleSize)
summary(pcog$SampleSize)
length(pcog$SampleSize)
ggplot(pcog, aes(x=SampleSize, fill=Group)) + geom_dotplot(binwidth = 1, stackgroups = TRUE, binpositions = "all") +
  theme_classic() + theme(legend.position="bottom") + 
  coord_cartesian(ylim=c(0, 0.05)) +
  theme(text=element_text(family="Times New Roman", size=12)) + xlab("Sample Size") + scale_x_continuous(breaks=seq(0,60,5)) +
  scale_fill_manual(values=cbPalette) +
  theme(axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.title = element_blank())

#### Claims ####

summary(pcog$ClaimLevel)

ggplot(data=subset(pcog, !is.na(ClaimLevel)), aes(x=ClaimLevel, fill=ClaimLevel)) + geom_bar() +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + theme_classic() +
  theme(text=element_text(family="Times New Roman", size=12)) + xlab("Type of Claim") + 
  theme(axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = "none")


ggplot(data=subset(pcog, !is.na(ClaimLevel)), aes(x=ClaimLevel, fill=ClaimLevel)) + geom_bar() +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + facet_grid(Group ~ .) + theme_minimal() +
  theme(text=element_text(family="Times New Roman", size=12)) + xlab("Type of Claim") + 
  theme(axis.title.y =element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = "none") + scale_y_continuous(breaks=seq(0,25,5)) 

summarise(ClaimLevel ~ Group, data=pcog)

with( pcog , aggregate( ClaimLevel , by=list(Group) , FUN=summary)  )


#### Inferences

## number of group inferences
length(na.omit(pcog$GroupInference))

## number of group p vals
group <- pcog$pgroup[!is.na(pcog$pgroup)]
length(group)
sum(group<0.05)

## number of individual inferences
length(na.omit(pcog$IndividualInference))

## number of individual p vals
indiv <- pcog$pindividual[!is.na(pcog$pindividual)]
length(indiv)
sum(indiv<0.05)

## number of papers with both group and individual inferences
nrow(na.omit(pcog[, 9:10]))

singlepaper<-distinct(pcog, Authors, .keep_all=TRUE )
nrow(na.omit(singlepaper[, 9:10]))



#### P value distributions ####



###Generate data for 80%, 20% and 5% power
## code edited from Lakens' Coursera 
power.t.test(n=50, delta = NULL, sd=5, power = 0.8, sig.level = 0.05, type="one.sample", alternative = "two.sided")
power.t.test(n=50, delta = NULL, sd=5, power = 0.2, sig.level = 0.05, type="one.sample", alternative = "two.sided")
nSims <- 100000

set.seed(631990)

##80% power
p <-numeric(nSims) #set up empty variable to store all simulated p-values

#Run simulation
for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 2.020923, sd = 5) #Simulate data with specified mean, standard deviation, and sample size
  z<-t.test(x, mu=0) #perform the t-test against mu (set to value you want to test against)
  p[i]<-z$p.value #get the p-value and store it
}
sum(p<0.05)
sum(p<0.05)/nSims

psig <- p[ which(p <= 0.05,) ]
p80 <- density(psig) # returns the density data

##20% power
p2 <-numeric(nSims) #set up empty variable to store all simulated p-values

#Run simulation
for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 0.8065326, sd = 5) #Simulate data with specified mean, standard deviation, and sample size
  z<-t.test(x, mu=0) #perform the t-test against mu (set to value you want to test against)
  p2[i]<-z$p.value #get the p-value and store it
}
sum(p2<0.05)
sum(p2<0.05)/nSims

p2sig <- p2[ which(p2 <= 0.05,) ]
p20 <- density(p2sig) # returns the density data


## 5% power

fp <-numeric(nSims) #set up empty variable to store all simulated p-values

#Run simulation
for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 0, sd = 5) #Simulate data with specified mean, standard deviation, and sample size
  z<-t.test(x, mu=0) #perform the t-test against mu (set to value you want to test against)
  fp[i]<-z$p.value #get the p-value and store it
}
sum(fp<0.05)
sum(fp<0.05)/nSims

fpsig <- fp[ which(fp <= 0.05,) ]
p0 <- density(fpsig) # returns the density data


## Get sig group effects from coded data

groupsig <- pcog[pcog$pgroup <= 0.05, ] 
groupvals <- groupsig$pgroup[!is.na(groupsig$pgroup)]


## Get sig individual effects from coded data 

indivsig <- pcog[pcog$pindividual <= 0.05, ] 
indivsig <- indivsig$pindividual[!is.na(indivsig$pindividual)]
id <- density(indivsig) # returns the density data


data <- c(rep("Individual", length(indivsig)), rep("Group", length(groupvals)), rep("5%", length(fpsig)), rep("20%", length(p2sig)), rep("80%", length(psig)))
values <- c(indivsig, groupvals, fpsig, p2sig, psig)

df <- data.frame(data, values)

df$data <- factor(df$data, levels=c("80%","20%","5%","Group", "Individual"))

ggplot(df, aes(x=values, fill=data)) + geom_density() + facet_grid(data ~ .) + theme_classic() + xlab("p-value") +
  theme(strip.background = element_blank(),
    strip.text.x = element_blank(), 
    text=element_text(family="Times New Roman", size=12), 
    axis.title.y =element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(), 
    legend.position = "none") + scale_fill_manual(values=cbPalette[1:5])

eighty <- subset(df, data == "80%")
twenty <- subset(df, data == "20%")
five <- subset(df, data == "5%")
ind <- subset(df, data == "Individual")
g <- subset(df, data == "Group")


hist(eighty$values)
hist(twenty$values)
hist(five$values)
hist(ind$values)
hist(g$values)

ggplot(g, aes(x=values)) + geom_density(colour=cbPalette[4], fill=cbPalette[4]) + facet_grid(data ~ .) + theme_classic() + xlab("p-value") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        text=element_text(family="Times New Roman", size=12), 
        axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = "none") + scale_fill_manual(values=cbPalette[1:5])

ggplot(ind, aes(x=values)) + geom_density(colour=cbPalette[5], fill=cbPalette[5]) + facet_grid(data ~ .) + theme_classic() + xlab("p-value") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        text=element_text(family="Times New Roman", size=12), 
        axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = "none") + scale_fill_manual(values=cbPalette[1:5])

ggplot(five, aes(x=values)) + geom_density(colour=cbPalette[3], fill=cbPalette[3]) + facet_grid(data ~ .) + theme_classic() + xlab("p-value") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        text=element_text(family="Times New Roman", size=12), 
        axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = "none") + scale_fill_manual(values=cbPalette[1:5])

ggplot(twenty, aes(x=values, fill=cbPalette[2])) + geom_density(colour=cbPalette[2], fill=cbPalette[2]) + facet_grid(data ~ .) + theme_classic() + xlab("p-value") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        text=element_text(family="Times New Roman", size=12), 
        axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = "none")

ggplot(eighty, aes(x=values)) + geom_density(colour=cbPalette[1], fill=cbPalette[1]) + facet_grid(data ~ .) + theme_classic() + xlab("p-value") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        text=element_text(family="Times New Roman", size=12), 
        axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = "none")





#### N passing #####
Npass <- pcog[, c(2,6, 11:12)]
Npass <- na.omit(Npass)
names(Npass) <- c("Title", "Experiment", "Original Sample Size", "Number Passing")
Npass$ProportionPassing <-  Npass$`Number Passing` / Npass$`Original Sample Size`
summary(Npass$ProportionPassing)
sum(Npass$ProportionPassing == 0)
sum(Npass$ProportionPassing == 1)
sum(Npass$`Number Passing` == 0)
summary(Npass$`Number Passing`)

ggplot(Npass, aes(x=ProportionPassing)) + geom_dotplot(binwidth=0.02, fill = "#999999") + theme_classic() + xlab("Proportion Passing") +
  theme(text=element_text(family="Times New Roman", size=15), 
        axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

Npass1 <- gather(Npass, "Condition", "Number", 3:4)
Npass1 <- Npass1 %>% unite("Experiment", 1:2, remove = FALSE)
Npass1 <- Npass1[, c(1, 4:5)]

Condition <- c("Original Sample Size", "Number Passing")
ggplot(Npass1, aes(x=Condition, y=Number, group=Experiment)) + geom_point() +
  geom_line(colour = "#999999") + theme_classic() + ylab("Number of Animals") + scale_x_discrete(limits = Condition) + xlab("") +
  theme(text=element_text(family="Times New Roman", size=17), 
        axis.line.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) + scale_y_continuous(breaks=seq(0,60,10))



##### Appendix code #####

## Simulate mixture of research power
nSims <- 10000

ap <- numeric(nSims)
for(i in 1:nSims/4){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 0, sd = 5) #Simulate data with specified mean, standard deviation, and sample size
  z<-t.test(x, mu=0) #perform the t-test against mu (set to value you want to test against)
  ap[i]<-z$p.value #get the p-value and store it
}
for(i in 1:nSims/1.5){ #for each simulated experiment
  x<-rnorm(n = 50, mean =  0.8065326, sd = 5) #Simulate data with specified mean, standard deviation, and sample size
  z<-t.test(x, mu=0) #perform the t-test against mu (set to value you want to test against)
  ap[i+nSims/4]<-z$p.value #get the p-value and store it
}
for(i in 1:nSims/12){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 2.020923, sd = 5) #Simulate data with specified mean, standard deviation, and sample size
  z<-t.test(x, mu=0) #perform the t-test against mu (set to value you want to test against)
  ap[i+(nSims/12)*11]<-z$p.value #get the p-value and store it
}

sum(ap<0.05)
sum(ap<0.05)/nSims
length(apsig)
ap

apsig <- ap[ which(ap <= 0.05,) ]
apd <- density(apsig) # returns the density data
plot(apd)


data <- c(rep("Individual", length(indivsig)), rep("Group", length(groupvals)), rep("mixture", length(apsig)))
values <- c(indivsig, groupvals, apsig)

df <- data.frame(data, values)

df$data <- factor(df$data, levels=c("mixture","Group", "Individual"))

ggplot(df, aes(x=values, fill=data)) + geom_density() + facet_grid(data ~ .) + theme_classic() + xlab("p-value") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        text=element_text(family="Times New Roman", size=12), 
        axis.title.y =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), 
        legend.position = "none") + scale_fill_manual(values=cbPalette[1:5])


