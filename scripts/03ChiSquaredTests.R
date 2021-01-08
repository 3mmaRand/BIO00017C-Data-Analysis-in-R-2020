## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)


## ----echo=FALSE----------------------------------------------------------
#---CODING ANSWER---
obs <- c(43, 36, 35, 38, 48, 26, 24) 


## ----echo=FALSE, results="hide"------------------------------------------
#---CODING ANSWER---
str(obs)
# it's a vector containing integers


## ------------------------------------------------------------------------
# the total number of inductions
totalinductions <- sum(obs)



## ------------------------------------------------------------------------
# the expected values
# note that R mulitples every element in the c() by totalinductions
exp <- c(1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7) * totalinductions



## ------------------------------------------------------------------------
# calculate the chisquared
chisqd <- sum(((obs - exp)^2) / exp)



## ----echo=FALSE, results="hide"------------------------------------------
#---CODING ANSWER---
df <- length(obs) - 1
# note: you might have used df = 6 but using the length of the 
# observation data is more reproducible. You can use the same code
#  for any goodness of fit test and only have to update the obs vector (the data).


## ------------------------------------------------------------------------
pchisq(chisqd, df = df, lower.tail = FALSE)



## ------------------------------------------------------------------------
# Combining the weekdays and the weekend days
# first combine the obs
obs2 <- c(sum(obs[1:5]), sum(obs[6:7])) 

# then calculate the expected values
exp2 <- c(5/7, 2/7) * totalinductions



## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
# calculate the chisquared
chisqd <- sum(((obs2 - exp2)^2) / exp2)
df <- length(obs2) - 1
pchisq(chisqd, df = df, lower.tail = FALSE)
# Signifcant


## ------------------------------------------------------------------------
chisq.test(obs)


## ------------------------------------------------------------------------
exp2 <- c(5/7, 2/7)
chisq.test(obs2, p = exp2)


## ----echo=FALSE----------------------------------------------------------
#---CODING ANSWER---
#I have my files organised differently so I use a different 'path' to my file
inductions <- read.table("../data/inductions.txt", header = T)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
str(inductions) 


## ------------------------------------------------------------------------
indtab <- table(inductions$day)


## ------------------------------------------------------------------------
chisq.test(indtab)


## ------------------------------------------------------------------------
# load tidyverse for ggplot2 - you only have to load once in a session
# tidyverse gives us ggplot and several other packages. tidyverse is known as a metapackage (it's several packages)
library(tidyverse)

ggplot(data = inductions, aes(x = day)) +
  geom_bar() +
  theme_classic()

# themes change several things at once about the appearance. 
# Classic is useful one but there are others you can play
# with. Type theme_ and use the tab key to see the options


## ------------------------------------------------------------------------
 levels(inductions$day)


## ------------------------------------------------------------------------
inductions$day <- fct_relevel(inductions$day,
                              "mon", 
                              "tues",
                              "wed",
                              "thurs",
                              "fri",
                              "sat",
                              "sun")

levels(inductions$day)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
obs <- c(65, 412, 31, 356)


## ------------------------------------------------------------------------
# list of two elments
# the two variables are whether someone has an ulcer or not and whether they are blood group O or A
vars <- list(ulcer = c("yes","no"), blood = c("O", "A"))
vars



## ------------------------------------------------------------------------
ulcers <- matrix(obs, nrow = 2, dimnames = vars)


## ------------------------------------------------------------------------
chisq.test(ulcers, correct = FALSE)
# you should look up the command in the manual to see what correct = FALSE does


## ------------------------------------------------------------------------
chisq.test(ulcers, correct = FALSE)$expected


## ----echo=FALSE----------------------------------------------------------
#---CODING ANSWER---
#I have my files organised differently so I use a different 'path' to my file
blood_ulcers <- read.table("../data/blood_ulcers.txt", header = T)


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
str(blood_ulcers) 


## ------------------------------------------------------------------------
ulctab <- table(blood_ulcers$blood, blood_ulcers$ulcer)
# examine the result
ulctab


## ------------------------------------------------------------------------
chisq.test(ulctab, correct = FALSE)


## ----echo=FALSE----------------------------------------------------------
#---CODING ANSWER---
obs <- c(79, 17, 19, 20) 


## ----echo=FALSE, results="hide"------------------------------------------
#---CODING ANSWER---
str(obs)
# it's a vector containing integers


## ----echo=FALSE, results="hide"------------------------------------------
# using the inbuilt test
# give the expected ratio
exp <- c(9/16, 3/16, 3/16, 1/16)
chisq.test(obs, p = exp)

# we can examine the expected values
chisq.test(obs, p = exp)$expected


## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
##################################################################################
#                            Slugs - tabulated                                   #
##################################################################################
#  name the rows and columns
vars <- list(pop = c("x","y"), colour = c("black", "brown", "red"))
slugs <- matrix(c(27, 39, 17, 10, 9, 21), nrow = 2, dimnames = vars)
# gives me
#    colour
# pop black brown red
#   x    27    17   9
#   y    39    10  21

# for a contingency test like this one where the expected values are
# derived from the data, the inbuilt chi squared test works well
#
chisq.test(slugs)
# X-squared = 6.5726, df = 2, p-value = 0.03739
# p < 0.05 so we reject the null hypothesis i.e., the proportions of the colour 
# forms are significantly different in the two populations 
# (mostly as a result of differences in the brown and red classes - 
# look at the differences between observed and expected values for 
# the three colour forms in the table above).
chisq.test(slugs)$expected
#       colour
# pop    black    brown      red
#   x 28.43902 11.63415 12.92683
#   y 37.56098 15.36585 17.07317



## ----echo=FALSE,results="hide"-------------------------------------------
#---CODING ANSWER---
##################################################################################
#                            Slugs - raw                                         #
##################################################################################
# import the data
slugs <- read.table("../data/slugs.txt", header = T)

# put it into a table
slugtab <- table(slugs$colour, slugs$pop)

# carry out the test
chisq.test(slugtab)

