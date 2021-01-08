## ------------------------------------------------------------------------
x <- rnorm(10, 15, 2)


## ------------------------------------------------------------------------
x <- round(x, 2)


## ------------------------------------------------------------------------
y <- rnorm(10, 18, 2)


## ------------------------------------------------------------------------
data <- stack(data.frame(x, y))


## ------------------------------------------------------------------------
names(data) <- c("measure", "group")


## ------------------------------------------------------------------------
mean1 <- 15
mean2 <- 18
# make all the values in a group the group mean
measure <- rep (c(mean1, mean2), each = 10)
# then add the random variation
measure <- measure + rnorm(20, 0, 2)
# create the grouping variable
group <- rep(c("x", "y"), each = 10)
# put them in a dataframe
data <- data.frame(measure, group)


## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE--------------
## write.table(data, file = "../data/mydat.txt", row.names = F)


## ----echo = FALSE, results = "hide", warning = FALSE, message = FALSE----
#--- ANSWER---
# blood glucose is the response variable, then there are two groups in the treatment. since the 15 people in each group are different (one measure for each person) then we have a two-sample t-test.
# we need to generate data which meet the assumptions of the two-sample t which means both samples are normally distributed and and the variances are the same. the effect given is that the glucose should be lower in the drug group than the placebo group
# first the placebo group. i'm going to make the blood glucose level have a mean of 10 and an sd of 1.5
placebo <- rnorm(15, 10, 1.5)
# now the drug group. the blood glucose level needs to be lower - i'll use 8. The sd should the same 1.5
drug <- rnorm(15, 8, 1.5)    
# create dataframe with the glucose in one column and a grouping variable in another:
data <- stack(data.frame(placebo, drug))
# To rename dataframe column names: 
names(data) <- c("glucose", "treatment") 
# doing a two sample t
t.test(data$glucose ~ data$treatment, var.equal = T, paired = F)
# mine is sig. the way to alter the signifcance level is to adjust the differnce between the means and/or the sd. if the differnce bweteen the means is big reltive to the sd then the test will be more signifcant. if the difference beween the means is small realtive to the sd then the test will be less siginifcant - and you can go as far as making it NS
# figure - either of these would be fine
source("../functions/summarySE.R")
glusummary <- summarySE(data, measurevar = "glucose", groupvars = c("treatment"))
library(ggplot2)
fig <- ggplot(glusummary, aes(x = treatment, y = glucose) ) + 
          geom_bar(stat = "identity", fill = "white", colour = "black") +
          geom_errorbar(aes(ymin = glucose - se, ymax = glucose + se), width = .1) + 
          ylab("Blood glucose (units)") +
          xlab("Treatment") +
          theme_bw()
fig <- ggplot(data, aes(x = treatment, y = glucose)) + 
  geom_jitter() +
  stat_summary(fun.data = mean_sdl, mult = 1, 
               geom = "point", color = "red", size = 4) +
  stat_summary(fun.data = mean_sdl, mult = 1, 
               geom = "errorbar", color = "red", width = 0.1) +
  ylim(0, 14) +
  theme_bw()



## ----echo = FALSE, results = "hide", warning = FALSE, message = FALSE, eval = FALSE----
## #--- ANSWER---
## # a parametric test would no longer be possible if the data were not normal.
## # there are several ways this could be tackled.
## # You can open the data file and replace some of the values - make several the same/nearly the same
## # or add extreme values or make them bimodal
## 
## # another way is to generate random numbers from a different distribution
## # for example, the binomial distribution
## data$glucose2 <- c(rbinom(15, 8, 0.2), rbinom(15, 8, 0.1))
## tapply(data$glucose2, data$treatment, hist)
## tapply(data$glucose2, data$treatment, shapiro.test)
## # these are small sample so you might find they are still not statistically
## # different from normal.
## # Note: not rejecting a null hypothesis doesn't mean it is true....it just means you cant
## # rule it out
## 
## #mann-whitney aka wilcoxon rank sum test would be the non-parametric alternative
## wilcox.test(data$glucose2 ~ data$treatment, paired = F)
## 


## ----echo = FALSE, results = "hide", warning = FALSE, message = FALSE, eval = FALSE----
## #--- ANSWER---
## #size is the response variable and location is the explanatyory variable. since there are four locations we need a one-way anova with a post-hoc test
## # i am going to set the means for each group then add random variation to all of them but i could use the same method as i used for the two-sample t-test and randomly draw the four sample separately
## #i'm setting a and b to be the same as requested
## meana <- 5
## meanb <- 5
## meanc <- 7
## meand <- 9
## #the size of each individual if every individual is its location mean size
## size <- rep (c(meana, meanb, meanc, meand), each = 8)
## #adding random variation
## size <- size + rnorm(32, 0, 1)
## #making the group variable
## location <- rep(c("a", "b", "c", "d"), each = 8)
## #putting the two variables togther
## data <- data.frame(size, location)
## mod <- aov(data$size ~ data$location)
## summary(mod)
## TukeyHSD(mod)
## fig <- plot(TukeyHSD(mod), cex.axis = 0.7)
## 
## beetsummary <- summarySE(data, measurevar = "size",
##                  groupvars = c("location"))
## fig <- ggplot(beetsummary, aes(x = location, y = size) ) +
##           geom_bar(stat = "identity",
##                     fill = "white", colour = "black") +
##           geom_errorbar(aes(ymin = size - se,
##                             ymax = size + se), width = .1) +
##           ylab("Size (units)") +
##           ylim(0, 10) +
##           xlab("Location") +
##           scale_x_discrete(labels = c("alpha", "beta", "gamma", "delta")) +
##           annotate("text", x = 1, y = 3, label = "a") +
##           annotate("text", x = 2, y = 3, label = "a") +
##           theme_bw()
## 


## ----echo = FALSE, results = "hide", warning = FALSE, message = FALSE----
#--- ANSWER---
#neuron length is the response variable and T1 and T2 are the explanatory variables. since there are two explanatory variables we need a two-way anova. since there are only two levels of each factor, we are not going to need a post-hoc test
# i am going to set the means for each group then add random variation to all of them but i could use the same method as i used for the two-sample t-test and randomly draw the four sample separately
#one way to tackle this problem is to think about what the graph would look like. It will be a clustered barchart with one treatment on the x-axis and the other shown by different coloured bars. If T2 is on the x-axis and T1 is the different bars then we want
#
#   ___      ___      T1
#   |X|      |X|   [ ] 0
#___|X|   ___|X|   [x] 1
#| ||X|   | ||X|
#| ||X|   | ||X|
#| ||X|   | ||x|
#  0        1
#      T2
#
#the T2 0 and T2 1 are the same on average - i.e., no effect of T2. There is a difference bwetween T1 0 and 1 and that difference is the same of both levels of T2 so there is no interaction
#
#
# I've gone with 8 reps. it wasnt specified

mean00 <- 10
mean01 <- 10
mean10 <- 12
mean11 <- 12
#the neuron length of each rep if every rep is its treatment combination mean length
length <- rep (c(mean00, mean01, mean10, mean11), each = 8)
#adding random variation
length <- length + rnorm(32, 0, 2)
#making the group variable
T1 <- rep(c("0", "1"), each = 16)
T2 <- rep(c("0", "1"), each = 8, times = 2)
#putting the three variables togther
data <- data.frame(length, T1, T2)
mod <- aov(data$length ~ data$T1*data$T2)
summary(mod)    
#i have the intended effects
neursummary <- summarySE(data, measurevar = "length", 
                         groupvars = c("T1", "T2"))
fig <- ggplot(neursummary, aes(x = T2, y = length, fill = T1) ) +
          geom_bar(stat = "identity", colour = "black", position = position_dodge()) +
          geom_errorbar(aes(ymin = length - se, 
                            ymax = length + se), width = .1, position = position_dodge(0.9)) +
          ylab("length (units)") +
          ylim(0, 14) +
          scale_fill_manual(values = c("#FFFFFF", "#CCCCCC")) +
          theme_bw()
    
     


## ----echo = FALSE, results = "hide", warning = FALSE, message = FALSE----
#--- ANSWER---
# if there is an interaction, but the treatments seem ns then that suggests one treatment has opposite effects on each level of the other:
#
#   ___   ___         T1
#   |X|   | |      [ ] 0
#___|X|   | |___   [x] 1
#| ||X|   | ||X|
#| ||X|   | ||X|
#| ||X|   | ||x|
#  0        1
#      T2
#

mean00 <- 10
mean01 <- 12
mean10 <- 12
mean11 <- 10
# the neuron length of each rep if every rep is its treatment combination mean length
length <- rep (c(mean00, mean01, mean10, mean11), each = 8)
# adding random variation
length <- length + rnorm(32, 0, 2)
# making the group variable
T1 <- rep(c("0", "1"), each = 16)
T2 <- rep(c("0", "1"), each = 8, times = 2)
# putting the three variables togther
data <- data.frame(length, T1, T2)
mod <- aov(data$length ~ data$T1*data$T2)
summary(mod)    
# i have the intended effects
neursummary <- summarySE(data, measurevar = "length", 
                         groupvars = c("T1", "T2"))
fig <- ggplot(neursummary, aes(x = T2, y = length, fill = T1) ) +
          geom_bar(stat = "identity", colour = "black", position = position_dodge()) +
          geom_errorbar(aes(ymin = length - se, 
                            ymax = length + se), width = .1, position = position_dodge(0.9)) +
          ylab("length (units)") +
          ylim(0, 14) +
          scale_fill_manual(values = c("#FFFFFF", "#CCCCCC")) +
          theme_bw()
    
     


## ----echo = FALSE, results = "hide", warning = FALSE, message = FALSE----
#--- ANSWER---
# our reponse variable is blood glucose and our explanatory variable is something we have set. this is a regression
# first make the x variable
dose <- seq(5, 50, 5)
# the y variable is going to be produced by a straight line relationship with some random variation
# we can chose any numbers for the intercept and slope but the slope must be positive
b0 <- 10
b1 <- 2
glucose <- b0 + b1*dose
# add the random variation
glucose <- glucose + rnorm(10, 0, 10)
data <- data.frame(glucose, dose)
mod <- lm(data = data, glucose ~ dose)
summary(mod)
fig <- ggplot(data, aes(x = dose, y = glucose)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, colour = "black")+
  ylab("Glucose (units)") +
  xlab("Dose (mg)") +
  theme_bw()




## ----echo = FALSE, results = "hide", warning = FALSE, message = FALSE----
#--- ANSWER---
# for a correlation, both variables need to be randomly sampled and all we need to do to give a negative correlation is to sort them so the largets scores of one variable go with the smallest scores on the other
# it doesn't matter what the values themselves are, so i have gone with the default mean = 0 and sd = 1
x1 <- rnorm(10)
x2 <- rnorm(10)    
# now sort in opposite directions
x1 <- sort(x1, decreasing = FALSE)
x2 <- sort(x2, decreasing = TRUE) 
cor.test(x1, x2)

# our r value will be close to minus 1 because we don't have any point out of order. if ytoiu do a rank correlation on that data the r will be exactly -1
cor.test(x1, x2, method = "spearman")


