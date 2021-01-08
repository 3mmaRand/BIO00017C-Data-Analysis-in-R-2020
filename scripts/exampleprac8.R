######################################################################
#                                                                    #
#   The effect of Juvenile hormone on mandible size in stag beetles. #
#                                                                    #     
######################################################################

######################################################################
#                             Introduction                           #
######################################################################

# The concentration of juvenile hormone in stag beetles is known to 
# influence mandible growth. Groups of stag beetles were injected 
# with different concentrations of juvenile hormone (arbitrary units) 
# and their average mandible size (mm) determined. The experimenters
# planned to analyse their data with regression. The data are 
# in ../data/stag.txt

# The data are organised into 2 columns
# 'data.frame':	16 obs. of  2 variables:
# $ jh  : int  0 2 4 6 8 10 12 14 16 18 ...
# $ mand: num  0.56 0.35 0.28 1.22 0.48 0.86 0.68 0.77 0.55 1.18 ...


######################################################################
#                            Set up                                  #
######################################################################


# working directory (in my case, yours will differ)
setwd("M:/web/17C - 2018/scripts")
# note: if you are using RStudio projects, this is unnecessary
# packages
library(tidyverse)
# Hadley Wickham (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version
# 1.2.1. https://CRAN.R-project.org/package=tidyverse


######################################################################
#                       Import and tidy data                         #
######################################################################

# data are in ../data
# read the data in and check the structure
stag <- read.table("../data/stag.txt", header = T)
str(stag)


######################################################################
#                     Exploratory Analysis                           #
######################################################################

# looking at the dataset there doesn't seem to be anything very 
# obviously non-normal (such as lots of values the same, too many 
# zeros, or extreme values
# The goal of the analysis is to determine the effect of a 
# continuous explanatory variable (JH) on a continuous response 
# (mand). This is a regression. Linear regression requires any
# effect of JH of mand to be linear therefore we will do a quick plot
# before proceedding
# quick plot
ggplot(stag, aes(x = jh, y = mand) ) +
  geom_point()
# looks linear-ish on the scatter
# the sceanrio seems to suit regresion - JH has been choosen or set, mandible has been measured
# we will check the other assumptions after we have run the lm

# get summaries of the variable
summary(stag)
# jh            mand       
# Min.   : 0.0   Min.   :0.2800  
# 1st Qu.: 7.5   1st Qu.:0.5575  
# Median :15.0   Median :0.8150  
# Mean   :15.0   Mean   :0.9038  
# 3rd Qu.:22.5   3rd Qu.:1.2225  
# Max.   :30.0   Max.   :1.6600  

######################################################################
#                     Statistical Analysis                           #
######################################################################

# Linear regression using lm()

mod <- lm(data = stag, mand ~ jh)
summary(mod)
# mand = 0.032 * jh + 0.419
# the slope of the line is significantly different from zero 
# / the jh explains a significant amount of the variation in 
# mand (ANOVA: F = 16.63; d.f. = 1, 14; p = 0.00113).
# the intercept is 0.419 and differs significantly from zero. 54%
# variation in mandible size is explained by juvenile hormone
# concentration

# Assumptions
# Examine the residuals to check the assumptions

plot(mod, which = 1) 
# the variance looks approximately equal along x

hist(mod$residuals)
# the residuals look approxiamtely normal

# normaility test on the residuals
shapiro.test(mod$residuals) 
# N.S

# These results are consistent with the assumptions being met
# but note that the sample size is quite small so large devations
# from normality would be required for signifcance.
# On balance the use of regression is probably justifiable
# but ideally the experiment would be better if multiple individauls were measure at each of the chosen juvenile hormone levels.

# The coefficients for the best fitting straight line can be accessed 
# from the mod object
# The intercept is the first coefficient and the slope is the second 
# and these can be used to make predictions

intercept <- mod$coefficients[1]
intercept
##  (Intercept) 
## 0.4193382
slope <- mod$coefficients[2]
slope 
##       jh 
## 0.03229412
# Suppose want to know the mandible size for a JH level of 15
jh <- 15
slope * jh + intercept
# mandible size is predicted to be 0.9mm

# we can get a several predictions using the predict function
# first generate the jh values for which we want predictions

newdata <- data.frame(jh = seq(10, 30, 5))
newdata$predictedman <- predict(mod, newdata = newdata)
# > newdata
# jh predictedman
# 1 10    0.7422794
# 2 15    0.9037500
# 3 20    1.0652206
# 4 25    1.2266912
# 5 30    1.3881618


######################################################################
#                                 Figure                             #
######################################################################
# The following would be a suitable plot
ggplot(stag, aes(x = jh, y = mand) ) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, colour = "black") +
  ylab("Mandible size (mm)") +
  ylim(0, 2) +
  xlim(0, 32) +
  xlab("Juvenile hormone (arbitrary units)") +
  theme_classic()
#Figure x. The effect of juvenile hormone injections on the mandible size of stag beetles.    


