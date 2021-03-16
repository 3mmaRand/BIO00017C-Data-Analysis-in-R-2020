## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,	
                      warning = FALSE,
                      fig.width = 4, 
                      fig.height = 4, 
                      fig.retina = 3)


## ----include=FALSE----------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(RefManageR)


## ---- load-refs, include=FALSE, cache=FALSE---------------------------------------------------------------------------------------
BibOptions(check.entries = FALSE,
           bib.style = "authoryear",
           cite.style = "authoryear",
           style = "markdown",
           hyperlink = TRUE,
           dashed = FALSE,
           longnamesfirst = FALSE,
           max.names = 2)
myBib <- ReadBib("../refs/refs.bib", check = FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------
#---CODING ANSWER---
obs <- c(43, 36, 35, 38, 48, 26, 24) 


## ---------------------------------------------------------------------------------------------------------------------------------
# the total number of inductions
totalinductions <- sum(obs)



## ---------------------------------------------------------------------------------------------------------------------------------
# the expected values
# note that R multiplies every element in the c() by totalinductions
exp <- c(1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7) * totalinductions



## ---------------------------------------------------------------------------------------------------------------------------------
# calculate the chisquared
chisqd <- sum(((obs - exp)^2) / exp)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------
#---CODING ANSWER---
df <- length(obs) - 1
# note: you might have used df = 6 but using the length of the 
# observation data is more reproducible. You can use the same code
#  for any goodness of fit test and only have to update the obs vector (the data).


## ---------------------------------------------------------------------------------------------------------------------------------
pchisq(chisqd, df = df, lower.tail = FALSE)



## ----fig.width = 6, echo=FALSE----------------------------------------------------------------------------------------------------
p <- pchisq(chisqd, df = df, lower.tail = FALSE) %>% round(3)
ggplot(data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = dchisq,
                geom = "area",
                fill = "gray",
                args = list(
                  df = df),
                xlim = c(chisqd, 20)) +
  stat_function(fun = dchisq,
                geom = "line",
                args = list(
                  df = df)) +
  scale_y_continuous(name = "", expand = c(0, 0)) +
  scale_x_continuous(name = "Chi-squared value", 
                     expand = c(0, 0),
                     breaks = seq(0, 20, 2)) +
  annotate("text", x = 15, y = 0.03,
           label = paste("p = ",p)) +
  annotate("segment", 
           x = 15, y = 0.025,
           xend = 13.5, yend = 0.01,
           label = paste("p = ",p)) +
  theme_classic() 


## ---------------------------------------------------------------------------------------------------------------------------------
# Combining the weekdays and the weekend days
obs2 <- c(sum(obs[1:5]), sum(obs[6:7])) 



## ---------------------------------------------------------------------------------------------------------------------------------
exp2 <- c(5/7, 2/7) * totalinductions


## ----include=FALSE----------------------------------------------------------------------------------------------------------------
#---CODING ANSWER---
# calculate the chisquared
chisqd <- sum(((obs2 - exp2)^2) / exp2)
df <- length(obs2) - 1
pchisq(chisqd, df = df, lower.tail = FALSE)
# Significant


## ---------------------------------------------------------------------------------------------------------------------------------
chisq.test(obs)


## ---------------------------------------------------------------------------------------------------------------------------------
exp2 <- c(5/7, 2/7)
chisq.test(obs2, p = exp2)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------
births <- read_table2("../data/births.txt")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------
DT::datatable(births)


## ----echo=FALSE, eval=FALSE-------------------------------------------------------------------------------------------------------
## #---CODING ANSWER---
## inductions <- read_table2("data-raw/inductions.txt")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------
#---I have a different directory structure so need
inductions <- read_table2("../data/inductions.txt")


## ---------------------------------------------------------------------------------------------------------------------------------
indtab <- table(inductions$day)
indtab


## ---------------------------------------------------------------------------------------------------------------------------------
chisq.test(indtab)


## ---------------------------------------------------------------------------------------------------------------------------------
ggplot(data = inductions, aes(x = day)) +
  geom_bar() +
  theme_classic()


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------
## ?geom_bar


## ---------------------------------------------------------------------------------------------------------------------------------
inductions$day <- fct_relevel(inductions$day,
                              "mon", 
                              "tues",
                              "wed",
                              "thurs",
                              "fri",
                              "sat",
                              "sun")

levels(inductions$day)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------
ggplot(data = inductions, aes(x = day)) +
  geom_bar() + 
  scale_y_continuous(name = "Frequency", 
                     expand = c(0, 0)) +
  scale_x_discrete(name = "", 
                   expand = c(0, 0),
                   labels = c("Monday",
                              "Tuesday",
                              "Wednesday",
                              "Thursday",
                              "Friday",
                              "Saturday",
                              "Sunday")) +
  coord_flip() +
  theme_classic()


## ----echo=FALSE,results="hide"----------------------------------------------------------------------------------------------------
#---CODING ANSWER---
obs <- c(65, 412, 31, 356)


## ---------------------------------------------------------------------------------------------------------------------------------
# list of two elments
# the two variables are whether someone has an ulcer or not and whether they are blood group O or A
vars <- list(ulcer = c("yes","no"), blood = c("O", "A"))
vars



## ---------------------------------------------------------------------------------------------------------------------------------
ulcers <- matrix(obs, nrow = 2, dimnames = vars)
ulcers


## ---------------------------------------------------------------------------------------------------------------------------------
chisq.test(ulcers, correct = FALSE)
# you should look up the command in the manual to see what correct = FALSE does


## ---------------------------------------------------------------------------------------------------------------------------------
chisq.test(ulcers, correct = FALSE)$expected


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------
## #---CODING ANSWER---
## blood_ulcers <- read_table2("data-raw/blood_ulcers.txt")
## str(blood_ulcers)


## ----include=FALSE----------------------------------------------------------------------------------------------------------------
#I have a different directory structure
blood_ulcers <- read_table2("../data/blood_ulcers.txt")


## ---------------------------------------------------------------------------------------------------------------------------------
ulctab <- table(blood_ulcers$blood, blood_ulcers$ulcer)
# examine the result
ulctab


## ---------------------------------------------------------------------------------------------------------------------------------
chisq.test(ulctab, correct = FALSE)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------
#---CODING ANSWER---
obs <- c(79, 17, 19, 20) 


## ----include=FALSE----------------------------------------------------------------------------------------------------------------
# using the inbuilt test
# give the expected ratio
exp <- c(9/16, 3/16, 3/16, 1/16)
chisq.test(obs, p = exp)

# we can examine the expected values
chisq.test(obs, p = exp)$expected


## ----include=FALSE----------------------------------------------------------------------------------------------------------------
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



## ----include=FALSE----------------------------------------------------------------------------------------------------------------
#---CODING ANSWER---
##################################################################################
#                            Slugs - raw                                         #
##################################################################################
# import the data
# my path differs, yours should be "data-raw/slugs.txt"
slugs <- read_table2("../data/slugs.txt")

# put it into a table
slugtab <- table(slugs$colour, slugs$pop)

# carry out the test
chisq.test(slugtab)


## ----refs, echo=FALSE, results="asis"---------------------------------------------------------------------------------------------
PrintBibliography(myBib)  

