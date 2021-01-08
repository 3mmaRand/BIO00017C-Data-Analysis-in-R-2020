## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)


## ----echo=FALSE, results='hide'------------------------------------------
library(tidyverse)


## ----waspimport, echo = FALSE--------------------------------------------
#---CODING ANSWER---
# I have all my data in a folder called data; your directory structure may differ
wasp  <-  read.table("../data/wasp.txt", header = T)


## ------------------------------------------------------------------------
ggplot(data = wasp, aes(x = status, y = time)) +
  geom_violin()


## ----waspmean, echo = FALSE, results='hide'------------------------------
#---CODING ANSWER---
wasp %>% 
  group_by(status) %>% 
  summarise(mean(time))



## ----waspsummary, echo = FALSE, results='hide'---------------------------
#---CODING ANSWER---
waspsummary <- wasp %>%
  group_by(status) %>%
  summarise(mean = mean(time),
            std = sd(time),
            n = length(time),
            se = std/sqrt(n))



## ----waspt---------------------------------------------------------------
t.test(data = wasp,
       time ~ status,
       var.equal = T)


## ------------------------------------------------------------------------
# add the group means to the data
wasp <- merge(wasp, waspsummary, by = "status")


## ------------------------------------------------------------------------
# add the group means to the data
# add the residuals
wasp <- wasp %>%
  mutate(residual = time - mean)


## ------------------------------------------------------------------------
shapiro.test(wasp$residual)


## ------------------------------------------------------------------------
ggplot(data = wasp,
       aes(x = mean, y = residual)) +
  geom_point()


## ----waspfigdemo, echo = FALSE, fig.width = 4, fig.height = 4------------
ggplot() +
  geom_point(data = wasp, aes(x = status, y = time),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.2) +
  ylab("Time (hr)") +
  xlab(NULL) +
    ylim(0, 70) +
  scale_x_discrete(labels = c("Mated", "Unmated")) +
  theme_classic()



## ---- fig.width = 4, fig.height = 4--------------------------------------
ggplot()


## ---- fig.width = 4, fig.height = 4--------------------------------------
ggplot() +
  geom_point(data = wasp, aes(x = status, y = time))


## ---- fig.width = 4, fig.height = 4--------------------------------------
ggplot() +
  geom_point(data = wasp, aes(x = status, y = time),
             position = position_jitter(width = 0.1, height = 0))


## ---- fig.width = 4, fig.height = 4--------------------------------------
ggplot() +
  geom_point(data = wasp, aes(x = status, y = time),
             position = position_jitter(width = 0.1, height = 0),
             colour = "grey50")


## ---- fig.width = 4, fig.height = 4--------------------------------------
ggplot() +
  geom_point(data = wasp, aes(x = status, y = time),
             position = position_jitter(width = 0.1, height = 0),
             colour = "grey50") +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) 
  


## ---- fig.width = 4, fig.height = 4--------------------------------------
ggplot() +
  geom_point(data = wasp, aes(x = status, y = time),
             position = position_jitter(width = 0.1, height = 0),
             colour = "grey50") +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.3)
  


## ---- fig.width = 4, fig.height = 4--------------------------------------
ggplot() +
  geom_point(data = wasp, aes(x = status, y = time),
             position = position_jitter(width = 0.1, height = 0),
             colour = "grey50") +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.3) +
  ylab("Time (hr)") +
  xlab(NULL) +
  ylim(0, 70)
  


## ---- fig.width = 4, fig.height = 4--------------------------------------
ggplot() +
  geom_point(data = wasp, aes(x = status, y = time),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.2) +
  ylab("Time (hr)") +
  xlab(NULL) +
  ylim(0, 70) +
  scale_x_discrete(labels = c("Mated", "Unmated")) +
  theme_classic()


## ----grousedata, echo = FALSE--------------------------------------------
# I did this by putting the data in two columns then using gather() f
# dataframe of males and females
grouse <- data.frame(males = c(5, 16, 8, 64, 51, 11, 9, 7), 
                     females = c(0, 2, 1, 3, 6, 10, 4, 12)) %>% 
  gather(key = sex, value = nematodes)



## ----echo=FALSE----------------------------------------------------------
grouse %>% 
  group_by(sex) %>% 
  summarise(median(nematodes))


## ----grousewilcox--------------------------------------------------------
wilcox.test(data = grouse, nematodes ~ sex)


## ----grousefig, fig.width = 4, fig.height = 4----------------------------
ggplot(grouse, aes(x = sex, y = nematodes) ) +
  geom_boxplot() + 
  xlab("Sex") +
  ylab("Number of nematodes") +
  theme_classic()


## ----geneimport, echo = FALSE--------------------------------------------
#---CODING ANSWER---
# I have my data files in a folder called data
coliexp  <-  read.table("../data/coliexp.txt", header = T)


## ----genepairedt---------------------------------------------------------
t.test(data = coliexp, expression ~ temperature, paired = T)



## ----csativa, include=FALSE----------------------------------------------
#---THINKING AND CODING ANSWER---
csativa  <-  read.table("../data/csativa.txt", header = T)
str(csativa)

# First realise that this is a two sample test. You have two independent samples
#  - there are a total of 100 different plants and the values in one 
#  group have no relationship to the values in the other.

# create a rough plot of the data  
ggplot(data = csativa, aes(x = plant, y = omega)) +
  geom_violin()
# note the modified plants seem to have lowere omega!

# create a summary of the data
csativasum <- csativa %>%
  group_by(plant) %>%
  summarise(mean = mean(omega),
            std = sd(omega),
            n = length(omega),
            se = std/sqrt(n))

# The data seem to be continuous so it is likely that a t-test will be fine
t.test(data = csativa, omega ~ plant, var.equal = TRUE)
# So there is a significant difference but you need to make sure you know the direction!

# let's check the assumptions
# add the group means to the data
csativa <- merge(csativa, csativasum[,1:2], by = "plant")
# add the residuals
csativa <- csativa %>%
  mutate(residual = omega - mean)

# normality test and plot
shapiro.test(csativa$residual)
ggplot(data = csativa,
       aes(x = mean, y = residual)) +
  geom_point()

# One could argue that the variance is greater in one group. In which case you could run a Welch's t-test
t.test(data = csativa, omega ~ plant)


# You would write it up like this:
# Wild plants have a significantly higher omega 3 content (($\bar{x} \pm s.e.$ =  56.41 $\pm$ 1.11) than modified plants (49.46 $\pm$ 0.82)(Welch's t-test: t = 5.03; _d.f._ = 90.36; _n1_ =  50, _n2_ = 50; _p_ < 0.001).

# A figure 
ggplot() +
  geom_point(data = csativa, aes(x = plant, y = omega),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = csativasum, 
                aes(x = plant, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = csativasum, 
                aes(x = plant, ymin = mean, ymax = mean),
                width = 0.2) +
  xlab("Plant type") +
  ylab("Amount of Omega 3 (units)") +
  scale_x_discrete(labels = c("GMO", "WT")) +
  ylim(0, 80) +
  theme_classic()
   


## ----sheep, include=FALSE------------------------------------------------
#---THINKING AND CODING ANSWER---
#the data are paired. although the two treatments are not applied to the same individual, they are applied to each of a set of twins. the first sheep fed unfertilised grass is the twin of the first sheep fed fertilsed grass. this means the columns are not independent and we need to do a paired test (either a paired wilcoxon or a paired t). These data do not appear to be normally distributed (not many values, integers) a non-parametric test is probably preferable. 

# read in the data
sheep  <-  read.table("../data/sheep.txt", header = T)
str(sheep)

# summarise the data
sheep %>% 
  group_by(grass) %>% 
  summarise(median(weight))

# run the wilcoxon
wilcox.test(data = sheep, weight ~ grass, paired = T)


# You would write it up like this:
# Within a set of twins, the individual fed unfertilissed grass achieved a signifcantly greater adult weight than that fed fertilsed grass (Wilcoxon signed rank test: V = 8, p-value = 0.009).

sheep <- sheep %>% mutate(twin = factor(c(1:14, 1:14)))
ggplot(data = sheep, aes(x = grass, 
                         y = weight, 
                         group = twin, 
                         colour = twin)) +
  scale_x_discrete(labels = c("Fertilised", "Unfertilised"), 
                   name = "Grass") +
  geom_point() +
  geom_line() +
  ylab("Weight (Kg)") +
  ylim(0, 55) +
  theme_classic() +
  theme(legend.position = "none")


