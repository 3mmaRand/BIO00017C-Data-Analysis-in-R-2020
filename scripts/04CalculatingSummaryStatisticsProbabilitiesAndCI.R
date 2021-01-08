## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)


## ----echo=FALSE----------------------------------------------------------
library(knitr)
library(kableExtra)
stattable <- read.table("stattable.txt", 
                        sep = "/", 
                        header = TRUE, 
                        row.names = 1)
kable_styling(kable(stattable, "html") )



## ----echo=FALSE, fig.width=4,fig.height=4--------------------------------
# figure to show IQ
# only for illustration    
m <- 100
sd <- 15
IQ <- seq(40, 160, 1)
curve(dnorm(x, 100, 15),
      xlim = c(40, 160), bty = "n",
      axes = F, xlab = "IQ", ylab = "") 
axis(1, pos = 0)
    


## ------------------------------------------------------------------------
# create variables for the parameter values
m <- 100
sd <- 15


## ------------------------------------------------------------------------
# Now use pnorm()
pnorm(115, m, sd)


## ----echo=FALSE, fig.width=4,fig.height=4--------------------------------
# fig to illustrate IQ < 115
# only for illustration
IQ <- seq(40, 160, 1)
cord.x <- c(40, seq(40, 115, 1), 115) 
cord.y <- c(0, dnorm(seq(40, 115, 1), 100, 15), 0) 
curve(dnorm(x, 100, 15),
      xlim = c(40, 160),
      bty = "n",axes = F,
      xlab = "IQ", ylab = "") 
polygon(cord.x, cord.y, col = "gray")
axis(1, pos = 0)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
pnorm(115, m, sd, lower.tail = FALSE)


## ----echo=FALSE, fig.width=4,fig.height=4--------------------------------
# fig to illustrate IQ between 85 and 115
# You sketch should look like this:
IQ <- seq(40, 160, 1)
cord.x <- c(85, seq(85, 115, 1), 115) 
cord.y <- c(0, dnorm(seq(85, 115, 1), 100, 15), 0) 
curve(dnorm(x, 100, 15),
      xlim = c(40, 160),
      bty = "n",axes = F,
      xlab = "IQ", ylab = "") 
polygon(cord.x, cord.y, col = "gray")
axis(1, pos = 0)

## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
pnorm(115, m, sd) - pnorm(85, m, sd)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
v <- 1.96 * 15


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
pnorm(m + v, m, sd) - pnorm(m - v, m, sd)
# 95% because 95% of values lies between +1.96 s.d. and -1.96 s.d.



## ------------------------------------------------------------------------
 qnorm(0.2, m, sd)


## ----echo=FALSE, fig.width=4,fig.height=4--------------------------------
# fig to illustrate IQ value that 20% people are below

IQ <- seq(40, 160, 1)
cord.x <- c(0, seq(0, 87, 1), 87) 
cord.y <- c(0, dnorm(seq(0, 87, 1), 100, 15), 0) 
curve(dnorm(x, 100, 15),
      xlim = c(40, 160),
      ylim = c(-0.006, 0.03),
      bty = "n",axes = F,
      xlab = "IQ", ylab = "") 
polygon(cord.x, cord.y, col = "gray")
text(65, 0.01, "p = 0.2")
arrows(65, 0.009, 75, 0.004, length = .15)
text(87, -0.006, "?", col = "red")
arrows(87, -0.004,87, 0, length = .15, col = "red")
axis(1, pos = 0, labels = FALSE)




## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
qnorm(0.025, m, sd)


## ----echo=FALSE, fig.width=6,fig.height=4.5------------------------------
# the figure for 99% 
# only for illustration
z <- qnorm(0.995)
cord.x <- c(-z, seq(-z, z, 0.01), z) 
cord.y <- c(0, dnorm(seq(-z, z, 0.01)), 0) 
curve(dnorm(x, 0, 1),
      xlim = c(-3, 3),
      ylim = c(-0.15, 0.45),
      bty = "n", 
      axes = F, xlab = "IQ",
      ylab = "") 
polygon(cord.x, cord.y, col = "gray")
arrows(-2.7, 0.1, -2.7, 0.01, length = .15)
text(-2.6, 0.11, "p = 0.005")
arrows(2.7, 0.1, 2.7, 0.01, length = .15)
text(2.6, 0.11, "p = 0.005")
text(0, 0.13, "p = 0.99")
text(z, -0.06, "?", col = "red")
arrows(z, -0.04,z, 0, length = .15, col = "red")
text(-z, -0.06, "?", col = "red")
arrows(-z, -0.04,-z, 0, length = .15, col = "red")
axis(1, pos = 0, labels = FALSE)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
qnorm(0.005, m, sd)
qnorm(0.995, m, sd)
# between 61.4 and 138.6


## ------------------------------------------------------------------------
n <- 5
se <- 15 / sqrt(n)


## ------------------------------------------------------------------------
pnorm(115, m, se)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
n <- 10
se <- 15 / sqrt(n)
pnorm(105, m, se, lower.tail = FALSE)
# it is about 0.146


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
bee <- read.table("../data/beewing.txt", header = FALSE)
str(bee)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
names(bee) <- "wing"
str(bee)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
m <- mean(bee$wing)
sd <- sd(bee$wing)
n <- length(bee$wing)
se <- sd / sqrt(n)


## ----warning=FALSE,message=FALSE-----------------------------------------
q <- qnorm(0.975)


## ----warning=FALSE,message=FALSE-----------------------------------------
(lcl <- m - q * se)
(ucl <- m + q * se)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
q <- qnorm(0.995)
lcl <- m - q * se
ucl <- m + q * se
#---THINKING ANSWER---
# the mean is 4.55 +/- 0.077. We are 95% certain the populations mean lies between 4.627 and 4.473


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
neur <- read.table("../data/neuron.txt", header = TRUE)
str(neur)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
m <- mean(neur$csa)



## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
sd <- sd(neur$csa)
n <- length(neur$csa)
se <- sd / sqrt(n)


## ------------------------------------------------------------------------
df <- length(neur$csa) - 1; df


## ------------------------------------------------------------------------
t <- qt(0.975, df = df); t


## ------------------------------------------------------------------------
round(m + t * se, 2)
round(m - t * se, 2)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING AND THINKING ANSWER---
adip <- read.table("../data/adipocytes.txt", header = T)
str(adip)
library(tidyverse)
adip_summary <- adip %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(adiponectin),
            sd = sd(adiponectin),
            n = length(adiponectin),
            se = sd/sqrt(n))
#since there are only 15 values in each group we need to use the t value to consruct our ci
t <- qt(0.975, df = 14)
#And the confidence interval are:
round(adip_summary$mean + t * adip_summary$se, 2)
round(adip_summary$mean - t * adip_summary$se, 2)
#we conclude we're 95% certain the mean for the conrol group is between 4.73 and 6.36 and the mean for the nicotinic group is between 6.52 and 8.50
#More usually we might put is like this:
round(t * adip_summary$se, 2)
# the mean for the control group is 5.55 +/- 0.82 and that for the nicotinic group is 7.51 +/- 0.99



## ----echo=FALSE,results="hide"-------------------------------------------
   #---CODING AND THINKING ANSWER---
#This question is asking what is the probability of having a value of 6 or more if you come from the healthy people distribution
m <- 3
sd <- 2.8
# Now use pnorm()
pnorm(6, m, sd, lower.tail = F)
#it is 0.142

