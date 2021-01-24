## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,	
                      warning = FALSE,
                      fig.width = 4, 
                      fig.height = 4, 
                      fig.retina = 3)


## ----include=FALSE------------------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(RefManageR)


## ---- load-refs, include=FALSE, cache=FALSE-----------------------------------------------------------
BibOptions(check.entries = FALSE,
           bib.style = "authoryear",
           cite.style = "authoryear",
           style = "markdown",
           hyperlink = TRUE,
           dashed = FALSE,
           longnamesfirst = FALSE,
           max.names = 2)
myBib <- ReadBib("../refs/refs.bib", check = FALSE)


## -----------------------------------------------------------------------------------------------------
library(tidyverse)


## ----echo=FALSE---------------------------------------------------------------------------------------

stattable <- read.table("stattable.txt", 
                        sep = "/", 
                        header = TRUE, 
                        row.names = 1)
kable_styling(knitr::kable(stattable, "html") )



## ----echo=FALSE, fig.width=4,fig.height=4-------------------------------------------------------------
# figure to show IQ
# only for illustration    
m <- 100
sd <- 15
IQ <- seq(40, 160, 1)
curve(dnorm(x, 100, 15),
      xlim = c(40, 160), bty = "n",
      axes = F, xlab = "IQ", ylab = "") 
axis(1, pos = 0)
    


## -----------------------------------------------------------------------------------------------------
# create variables for the parameter values
m <- 100
sd <- 15


## -----------------------------------------------------------------------------------------------------
# Now use pnorm()
pnorm(115, m, sd)


## ----echo=FALSE, fig.width=4,fig.height=4-------------------------------------------------------------
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


## ----echo=FALSE,results="hide"------------------------------------------------------------------------
#---CODING ANSWER---
pnorm(115, m, sd, lower.tail = FALSE)


## ----echo=FALSE, fig.width=4,fig.height=4-------------------------------------------------------------
# fig to illustrate IQ between 85 and 115
# Your sketch should look like this:
IQ <- seq(40, 160, 1)
cord.x <- c(85, seq(85, 115, 1), 115) 
cord.y <- c(0, dnorm(seq(85, 115, 1), 100, 15), 0) 
curve(dnorm(x, 100, 15),
      xlim = c(40, 160),
      bty = "n",axes = F,
      xlab = "IQ", ylab = "") 
polygon(cord.x, cord.y, col = "gray")
axis(1, pos = 0)


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
pnorm(115, m, sd) - pnorm(85, m, sd)


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
v <- 1.96 * 15


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
pnorm(m + v, m, sd) - pnorm(m - v, m, sd)
# this should be what you expect because 95% of values lies between +1.96 s.d. and -1.96 s.d.



## -----------------------------------------------------------------------------------------------------
qnorm(0.2, m, sd)


## ----echo=FALSE, fig.width=4,fig.height=4-------------------------------------------------------------
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




## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
qnorm(0.025, m, sd)


## ----echo=FALSE, fig.width=6,fig.height=4.5-----------------------------------------------------------
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


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
qnorm(0.005, m, sd)
qnorm(0.995, m, sd)
# between 61.4 and 138.6


## -----------------------------------------------------------------------------------------------------
n <- 5
se <- 15 / sqrt(n)


## -----------------------------------------------------------------------------------------------------
pnorm(115, m, se)


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
n <- 10
se <- 15 / sqrt(n)
pnorm(105, m, se, lower.tail = FALSE)
# it is about 0.146


## ----eval=FALSE, echo=FALSE---------------------------------------------------------------------------
## #---CODING ANSWER---
## # assuming you have set up your folders as instructed earlier
## bee <- read_table2("data/beewing.txt")
## str(bee)


## ----include=FALSE------------------------------------------------------------------------------------
# note my directory structure differs so I need a different path
bee <- read_table2("../data/beewing.txt")
str(bee)


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
m <- mean(bee$wing)
sd <- sd(bee$wing)
n <- length(bee$wing)
se <- sd / sqrt(n)


## -----------------------------------------------------------------------------------------------------
q <- qnorm(0.975)


## -----------------------------------------------------------------------------------------------------
lcl <- m - q * se
ucl <- m + q * se


## -----------------------------------------------------------------------------------------------------
lcl
ucl


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
# qnorm(0.975) gives the quantile for 95%. For 99% we need qnorm(0.995)
q <- qnorm(0.995)
lcl <- m - q * se
ucl <- m + q * se
#---THINKING ANSWER---
# the mean is 4.55 +/- 0.077. We are 95% certain the populations mean lies between 4.627 and 4.473


## ----eval=FALSE, echo=FALSE---------------------------------------------------------------------------
## #---CODING ANSWER---
## ## assuming you have set up your folders as instructed earlier
## neur <- read_table2("data/neuron.txt")
## str(neur)


## ----include=FALSE------------------------------------------------------------------------------------
# note my directory structure differs so I need a different path
neur <- read_table2("../data/neuron.txt")


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
m <- mean(neur$csa)



## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
# I created intermediate variables for sd and n
sd <- sd(neur$csa)
n <- length(neur$csa)
se <- sd / sqrt(n)


## -----------------------------------------------------------------------------------------------------
df <- length(neur$csa) - 1


## -----------------------------------------------------------------------------------------------------
t <- qt(0.975, df = df)


## -----------------------------------------------------------------------------------------------------
(m + t * se) %>% round(2)


## ----include=FALSE------------------------------------------------------------------------------------
(m - t * se) %>% round(2)


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING AND THINKING ANSWER---
# data import
# if you continue with the directory structure you have for this workshop and have saved adipocytes.txt to the data folder, you will need:
# adip <- read_table2("data/adipocytes.txt")
# I have a different structure and need:
adip <- read_table2("../data/adipocytes.txt")
# examine the structure
str(adip)
# summarise
adip_summary <- adip %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(adiponectin),
            sd = sd(adiponectin),
            n = length(adiponectin),
            se = sd/sqrt(n))
# since there are only 15 values in each group we need to use the 
# t value to construct our ci
t <- qt(0.975, df = 14)
#And the confidence interval are:
(adip_summary$mean + t * adip_summary$se) %>% round(2)
(adip_summary$mean - t * adip_summary$se) %>% round(2)
# we conclude we're 95% certain the mean for the control group is 
# between 4.73 and 6.36 and the mean for the nicotinic group is 
# between 6.52 and 8.50. More usually we might put is like this:
(t * adip_summary$se) %>% round(2)
# the mean for the control group is 5.55 +/- 0.82 and that for the nicotinic group is 7.51 +/- 0.99



## ----refs, echo=FALSE, results="asis"-----------------------------------------------------------------
PrintBibliography(myBib)  

