## ----setup, include=FALSE----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,	
                      warning = FALSE,
                      fig.width = 4, 
                      fig.height = 4, 
                      fig.retina = 3)


## ----include=FALSE-----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(RefManageR)


## ---- load-refs, include=FALSE, cache=FALSE----------------------------------------------------------------------------------
BibOptions(check.entries = FALSE,
           bib.style = "authoryear",
           cite.style = "authoryear",
           style = "markdown",
           hyperlink = TRUE,
           dashed = FALSE,
           longnamesfirst = FALSE,
           max.names = 2)
myBib <- ReadBib("../refs/refs.bib", check = FALSE)


## ----------------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## ----echo=FALSE, eval=FALSE--------------------------------------------------------------------------------------------------
## #---CODING ANSWER---
## height <- read_table2("data-raw/height.txt")


## ----echo=FALSE--------------------------------------------------------------------------------------------------------------
height <- read_table2("../data/height.txt")


## ----fig.width=4,fig.height=4------------------------------------------------------------------------------------------------
ggplot(height, aes(x = sister, y = brother) ) +
  geom_point()
  


## ----------------------------------------------------------------------------------------------------------------------------
cor.test(data = height, ~ brother + sister, method = "pearson")


## ----fig.width=4,fig.height=4------------------------------------------------------------------------------------------------
fig1 <- ggplot(height, aes(x = sister, y = brother)) +
  geom_point() +
  scale_x_continuous(name = "Heights of sister (cm)",
                     limits = c(120, 190),
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Heights of brother (cm)",
                     limits = c(120, 190),
                     expand = c(0, 0)) +
   theme_classic()

fig1


## ----echo = FALSE------------------------------------------------------------------------------------------------------------
# I choose to do a png but you may have a different preference
ggsave("figures/sis-bro.png",
       plot = fig1,
       width = 3.5,
       height = 3.5,
       units = "in",
       dpi = 300)



## ----------------------------------------------------------------------------------------------------------------------------
height2 <- rbind(height, height)
View(height2)


## ----include=FALSE-----------------------------------------------------------------------------------------------------------
#---CODING ANSWER---
cor.test(data = height2, ~ brother + sister, method = "pearson")


## ----------------------------------------------------------------------------------------------------------------------------
cor.test(data = height, ~ brother + sister, method = "spearman")


## ----------------------------------------------------------------------------------------------------------------------------
library(readxl)


## ----eval=FALSE, echo=FALSE--------------------------------------------------------------------------------------------------
## #---CODING ANSWER---
## # we need the readxl package that was introduced in the last workshop
## library(readxl)
## # assign file name to variable because I'll use it more than once
## file <- "data-raw/plant.xlsx"
## # list the sheets in the file
## excel_sheets(file)
## # You might want to open th file in excel to make sure you know what is in it.
## # I'm a bit lazy - and worried by being wrong - so will just guess the are data in the one named sheet. If I guess wrong, then I might open the file!
## plant <- read_excel(file, sheet = "plant")
## 


## ----include=FALSE-----------------------------------------------------------------------------------------------------------
#---CODING ANSWER---
# my directory structure differs so I need
library(readxl)
file <- "../data/plant.xlsx"
excel_sheets(file)
plant <- read_excel(file, sheet = "plant")



## ----include=FALSE-----------------------------------------------------------------------------------------------------------
#---CODING ANSWER---
ggplot(plant, aes(x = day, y = mass)) +
  geom_point()


## ----------------------------------------------------------------------------------------------------------------------------
mod <- lm(data = plant, mass ~ day)
summary(mod)


## ----include=FALSE-----------------------------------------------------------------------------------------------------------
#---CODING AND THINKING ANSWER---
plot(mod, which = 1)
hist(mod$residuals)
shapiro.test(mod$residuals)
#These look ok


## ----fig.width=4,fig.height=4------------------------------------------------------------------------------------------------
ggplot(plant, aes(x = day, y = mass)) +
  geom_point() +
  geom_smooth(method = lm, 
              se = FALSE, 
              colour = "black") +
  scale_x_continuous(name = "Day",
                     limits = c(0, 65),
                     expand = c(0,0)) +
  scale_y_continuous(name = "Mass (g)",
                     limits = c(0, 120),
                     expand = c(0,0)) +
  theme_classic()
  


## ---- echo = FALSE, fig.width=4,fig.height=4---------------------------------------------------------------------------------
fig2 <- ggplot(plant, aes(x = day, y = mass)) +
   geom_point() +
  geom_smooth(method = lm, 
              se = FALSE, 
              colour = "black") +
  scale_x_continuous(name = "Day",
                     limits = c(0, 65),
                     expand = c(0,0)) +
  scale_y_continuous(name = "Mass (g)",
                     limits = c(0, 120),
                     expand = c(0,0)) +
  annotate("text", x = 15, y = 100, label = expression(italic("y = 1.6 x - 8.69"))) +
  theme_classic()
fig2  


## ----echo = FALSE------------------------------------------------------------------------------------------------------------
ggsave("figures/plant-growth.png",
       plot = fig2,
       width = 3.5,
       height = 3.5,
       units = "in",
       dpi = 300)



## ----include=FALSE-----------------------------------------------------------------------------------------------------------
#---CODING AND THINKING ANSWER---
# this example is designed to emphasise the importance of plotting your data first
sprint <- read_table2("../data/sprint.txt")
# Anxiety is discrete but ranges from 16 to 402 meaning the gap between possible measures is small and 
# the variable could be treated as continuous if needed. Time is a continuous measure that has decimal places and which we would expect to follow a normal distribution 

# explore with a plot
ggplot(sprint, aes(x = anxiety, y = time) ) +
  geom_point()

# A scatterplot of the data clearly reveals that these data are not linear. There is a good relationship between the two variables but since it is not linear, a correlation (either parametric or non-parametric) is inappropriate. A Pearson's correlation comes out at close to zero and NS (since the scatter around a linear correlation would be great).



## ----include=FALSE-----------------------------------------------------------------------------------------------------------
#---CODING AND THINKING ANSWER---
#read the data in and check the structure
stag <- read_table2("../data/stag.txt")
str(stag)

# jh is discrete but order and has been chose by the experimenter - it is the explanatory variable.  
# the response is mandible size which has decimal places and is something we would expect to be 
# normally distributed. So far, common sense suggests the assumptions of regression are met.

# exploratory plot
ggplot(stag, aes(x = jh, y = mand)) +
  geom_point()
# looks linear-ish on the scatter
# regression still seems appropriate
# we will check the other assumptions after we have run the lm

# build the statistical model
mod <- lm(data = stag, mand ~ jh)

# examine it
summary(mod)
# mand = 0.032*jh + 0.419
# the slope of the line is significantly different from zero / the jh explains a significant amount of the variation in mand (ANOVA: F = 16.63; d.f. = 1,14; p = 0.00113).
# the intercept is 0.419 and differs significantly from zero 

# checking the assumption
plot(mod, which = 1) 
# we're looking for the variance in the residuals to be equal along the x axis.
# with a small data set there is some apparent heterogeneity but it doesn't look too.
# 
hist(mod$residuals)
# We have some skew which gain might be partly a result of a small sample size.
shapiro.test(mod$residuals) # the also test not sig diff from normal

# On balance the use of regression is probably justifiable but it is borderline
# but ideally the experiment would be better if multiple individuals were measure at
# each of the chosen juvenile hormone levels.

# a better plot
ggplot(stag, aes(x = jh, y = mand) ) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, colour = "black") +
  scale_x_continuous(name = "Juvenile hormone (arbitrary units)",
                     expand = c(0, 0),
                     limits = c(0, 32)) +
  scale_y_continuous(name = "Mandible size (mm)",
                     expand = c(0, 0),
                     limits = c(0, 2)) +
  theme_classic()
#Figure x. The effect of juvenile hormone injections on the mandible size of stag beetles.   


## ----refs, echo=FALSE, results="asis"----------------------------------------------------------------------------------------
PrintBibliography(myBib)  

