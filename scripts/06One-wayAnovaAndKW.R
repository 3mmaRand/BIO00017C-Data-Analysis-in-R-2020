## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)


## ----echo=FALSE, results='hide'------------------------------------------
library(tidyverse)


## ----import, echo=FALSE, results="hide"----------------------------------
#---CODING ANSWER---
seal <- read.table("../data/seal.txt", header = T)
str(seal)


## ----echo=FALSE, results="hide"------------------------------------------
ggplot(data = seal, aes(x = species, y = myoglobin)) +
  geom_violin()


## ----waspsummary, echo = FALSE, results='hide'---------------------------
#---CODING ANSWER---
sealsummary <- seal %>%
  group_by(species) %>%
  summarise(mean = mean(myoglobin),
            std = sd(myoglobin),
            n = length(myoglobin),
            se = std/sqrt(n))



## ----anovatest-----------------------------------------------------------
mod <- aov(data = seal, myoglobin ~ species)
summary(mod)


## ----posthoc-------------------------------------------------------------
TukeyHSD(mod)


## ----poshocplot, fig.height=5--------------------------------------------
plot(TukeyHSD(mod), cex.axis = 0.7)   #cex.axis just changes the size of the axis labels
# I could also use TukeyHSD(mod) %>% plot(cex.axis = 0.7)


## ----normalityafter------------------------------------------------------
shapiro.test(mod$residuals)


## ----normalityafter2-----------------------------------------------------
plot(mod, which = 1)


## ----sealfig, fig.width = 5, fig.height = 5------------------------------
ggplot() +
  geom_point(data = seal, aes(x = species, y = myoglobin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = sealsummary, 
                aes(x = species, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = sealsummary, 
                aes(x = species, ymin = mean, ymax = mean),
                width = 0.2) +
  ylab(expression("Myoglobin concentration g "*Kg^{-1})) +
  ylim(0, 80) +
  scale_x_discrete(labels = c("Bladdernose", "Harbour", "Weddell"), 
                   name = "Seal Species") +
  theme_classic()


## ----sealfigannotated, fig.width = 5, fig.height = 5---------------------
ggplot() +
  geom_point(data = seal, aes(x = species, y = myoglobin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = sealsummary, 
                aes(x = species, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = sealsummary, 
                aes(x = species, ymin = mean, ymax = mean),
                width = 0.2) +
  ylab(expression("Myoglobin concentration g "*Kg^{-1})) +
  ylim(0, 80) +
  scale_x_discrete(labels = c("Bladdernose", "Harbour", "Weddell"), 
                   name = "Seal Species") +
  annotate("segment", x = 1, xend = 2, 
           y = 72, yend = 72,
           colour = "black") +
  annotate("segment", x = 2, xend = 2, 
           y = 72, yend = 70,
           colour = "black") +
  annotate("segment", x = 1, xend = 1, 
           y = 72, yend = 70,
           colour = "black") +
  annotate("text", x = 1.5,  y = 74, 
           label = "*", size = 8) +
  theme_classic()
  


## ----echo = FALSE, results = "hide"--------------------------------------
#---CODING ANSWER---
leaf <- read.table("../data/leaf.txt", header = T)
str(leaf)


## ----echo = FALSE,results='hide'-----------------------------------------
#---CODING ANSWER---
leaf %>% 
  group_by(birch) %>% 
  summarise(mean = mean(eggs),
            median = median(eggs),
            n = length(eggs))


## ------------------------------------------------------------------------
kruskal.test(data = leaf, eggs ~ birch)


## ----echo = FALSE, results = "hide", warning = FALSE, message = FALSE----
library(pgirmess)


## ------------------------------------------------------------------------
kruskalmc(data = leaf, eggs ~ birch)


## ----echo = FALSE, fig.width = 5, fig.height = 5-------------------------
#---CODING ANSWER---
ggplot(leaf, aes(x = birch, y = eggs) ) +
  geom_boxplot() +
  xlab("Birch") +
  ylab("Number of eggs") +
  ylim(0, 105) +
  annotate("segment", x = 2, xend = 3, 
           y = 100, yend = 100,
           colour = "black") +
  annotate("segment", x = 2, xend = 2, 
           y = 100, yend = 97,
           colour = "black") +
  annotate("segment", x = 3, xend = 3, 
           y = 100, yend = 97,
           colour = "black") +
  annotate("text", x = 2.5,  y = 102, 
           label = "*", size = 8) +
  theme_classic()


## ------------------------------------------------------------------------
# figure saving settings
units = "in"
fig_w <- 3.2
fig_h <- fig_w
dpi <- 300
device <- "tiff" # this is format often required by journals; you may want png or jpg


## ------------------------------------------------------------------------
fig1 <- ggplot(leaf, aes(x = birch, y = eggs) ) +
  geom_boxplot() +
  xlab("Birch") +
  ylab("Number of eggs") +
  ylim(0, 105) +
  annotate("segment", x = 2, xend = 3, 
           y = 100, yend = 100,
           colour = "black") +
  annotate("segment", x = 2, xend = 2, 
           y = 100, yend = 97,
           colour = "black") +
  annotate("segment", x = 3, xend = 3, 
           y = 100, yend = 97,
           colour = "black") +
  annotate("text", x = 2.5,  y = 102, 
           label = "*", size = 8) +
  theme_classic()


ggsave("fig1-birch.tif", 
       plot = fig1, 
       device = device,
       width = fig_w, 
       height = fig_w,
       units = units,
       dpi = dpi)


## ----include=FALSE-------------------------------------------------------
#---CODING AND THINKING ANSWER---
#read in the data and look at structure
sweat <- read.table("../data/sweat.txt", header = T)
str(sweat)

# quick plot of the data
ggplot(data = sweat, aes(x = gp, y = na)) +
  geom_boxplot()
ggplot(data = sweat, aes(x = gp, y = na)) +
  geom_point()

# Since the sample sizes are small and not the same in each group and the 
# variance in the FA gp looks a bit lower, I'm leaning to a non-parametric test K-W.
# However, don't panic if you decided to do an anova

# calculate some summary stats 
sweatsum <- sweat %>% 
  group_by(gp) %>% 
  summarise(mean = mean(na),
            std = sd(na),
            n = length(na),
            median = median(na))


# Kruskal-Wallis
kruskal.test(data = sweat, na ~ gp)
# We can say there is a difference between the groups in the sodium 
# content of their sweat (chi-squared = 11.9802, df = 2, p-value = 0.002503).
# Unfit and unacclimatised people have most salty sweat, 
# Fit and acclimatised people the least salty.

# a post-hoc test to see where the sig differences lie:

kruskalmc(data = sweat, na ~ gp)
# Fit and acclimatised people (FA) have significantly less sodium in their
#  sweat than the unfit and unacclimatised people (UU). 
# Fit and unacclimatised (FU) people have sodium concentrations 
# more similar to the FA group but don't reach significance 
# for being different to UU. See figure 1.

ggplot(sweat, aes(x = gp, y = na) ) +
  geom_boxplot() +
  xlab("Group") +
  ylab(expression("Sodium"*mu*"mol"*l^{-1})) +
  scale_x_discrete(labels = c("Fit Acclimatised", 
                              "Fit Unacclimatised", 
                              "Unfit Unacclimatised"), 
                   name = "") +
  ylim(0, 100) +
  annotate("segment", x = 1, xend = 3, 
           y = 90, yend = 90,
           colour = "black") +
  annotate("segment", x = 1, xend = 1, 
           y = 90, yend = 87,
           colour = "black") +
  annotate("segment", x = 3, xend = 3, 
           y = 90, yend = 87,
           colour = "black") +
  annotate("text", x = 2,  y = 93, 
           label = "**", size = 8) +
  theme_classic()

#Figure 1. Sodium content of sweat for three groups: Fit and acclimatised
#(FA), Fit and unacclimatised (FU) and Unfit and unacclimatised (UU). Heavy lines
#indicate the median, boxes the interquartile range and whiskers the range. 



## ----include=FALSE-------------------------------------------------------
#---CODING AND THINKING ANSWER---
######################################################################
#                                                                    #
#   A comparison of the effects of five insect pesticides on the     #
#   insect biomass in treated plots.                                 #
#                                                                    #     
######################################################################

######################################################################
#                             Introduction                           #
######################################################################

# The data are given in biomass.txt are taken from an experiment 
# in which the insect pest biomass (g) was measured on plots sprayed 
# with water (control) or one of five different insecticides. 
# The goal of the analysis was to determine if the insecticides 
# vary in their effectiveness and specifically advise on:
#   - use of insecticide E
#   - the choice between A and D
#   - the choice between C and B

# The data are organised with an insecticide treatment group in
# each column:
# 'data.frame':	10 obs. of  6 variables:
# $ WaterControl: num  350 324 359 255 208 ...
# $ A           : num  159 146 116 135 137 ...
# $ B           : num  150.1 154.4 69.5 150.7 212.6 ...
# $ C           : num  80 266.4 161.2 161.4 51.2 ...
# $ D           : num  267 110 221 160 198 ...
# $ E           : num  350 320 359 255 208 ...

######################################################################
#                       Import and tidy data                         #
######################################################################

# data are in ../data
biom <- read.table("../data/biomass.txt", header = T)

# check structure
str(biom)

# 'data.frame':	10 obs. of  6 variables:
# $ WaterControl: num  350 324 359 255 208 ...
# $ A           : num  159 146 116 135 137 ...
# $ B           : num  150.1 154.4 69.5 150.7 212.6 ...
# $ C           : num  80 266.4 161.2 161.4 51.2 ...
# $ D           : num  267 110 221 160 198 ...
# $ E           : num  350 320 359 255 208 ...

# The data are organised with an insecticide treatment group in
# each column. Put the data into tidy format.

biom <- gather(biom, key = spray, value = biomass)

######################################################################
#                     Exploratory Analysis                           #
######################################################################

# quick plot of the data
ggplot(data = biom, aes(x = spray, y = biomass)) +
  geom_boxplot()


# summary statistics
biomsum <- biom %>% 
  group_by(spray) %>% 
  summarise(mean = mean(biomass),
            median = median(biomass),
            sd = sd(biomass),
            n = length(biomass),
            se = sd / sqrt(n))

# conclusion: the sample sizes are equal, 10 is a smallish but
# reasonable sample size
# the means and medians are similar to each other (expected for
# normally distributed data), A has a smaller variance 

# We have one explanatory variable, "spray" comprising 6 levels
# Biomass has decimal places and we would expect such data to be 
# normally distributed therefore one-way ANOVA is the desired test
# - we will check the assumptions after building the model

######################################################################
#                     Statistical Analysis                           #
######################################################################

# Carrying out an ANOVA
model <- aov(data = biom, biomass ~ spray)
summary(model)
# There is a very highly signifcant effect of spray identity on pest 
# biomass (F = 26.5; d.f., 5, 54; p < 0.001).

# Carrying out a Tukey Honest Signifcant differences test
# to see where differences bewteen spray treatments lie
# ordering can make it easier to understand. Plot included 
TukeyHSD(model, ordered = T)
plot(TukeyHSD(model, ordered = T), cex.axis = 0.5)

# signifcant comparisions
#                   diff        lwr       upr     p adj
# D-A             76.505  11.729841 141.28016 0.0118570
# E-A            175.515 110.739841 240.29016 0.0000000
# WaterControl-A 175.915 111.139841 240.69016 0.0000000
# E-C            155.710  90.934841 220.48516 0.0000000
# WaterControl-C 156.110  91.334841 220.88516 0.0000000
# E-B            154.323  89.547841 219.09816 0.0000001
# WaterControl-B 154.723  89.947841 219.49816 0.0000000
# E-D             99.010  34.234841 163.78516 0.0004759
# WaterControl-D  99.410  34.634841 164.18516 0.0004477

# Note smaller values (insect biomass) indicates more 
# effective control

# All sprays are better than the water control except E. 
# This is probably the most important result.
# What advice would you give to a person currently using insecticide E?
# Don't bother!! It's no better than water. Switch to any of 
# the other sprays
# 

# Sorting the summary table by the mean can make it easier to 
# interpret the results
arrange(biomsum, mean)
# What advice would you give to a person currently
#   + trying to choose between A and D? Choose A because A has sig lower
#   insect biomass than D 
#   + trying to choose between C and B? It doesn't matter because there is 
#   no differnce in insect bbiomass. Use other criteria to chose (e.g., price)

# We might report this like:
# There is a very highly signifcant effect of spray identity on pest 
# biomass (F = 26.5; d.f., 5, 54; p < 0.001). Post-hoc testing 
# showed E was no more effective than the control; A, C and B were 
# all better than the control but could be equally as good as each
# other; D would be a better choice than the control or E but 
# worse than A. See figure 1


######################################################################
#                                 Figure                             #
######################################################################

# uses the summary data
# I reordered the bars to make is easier for me to annotate with

ggplot() +
  geom_point(data = biom, aes(x = reorder(spray, biomass), y = biomass),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = biomsum, 
                aes(x = spray, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = biomsum, 
                aes(x = spray, ymin = mean, ymax = mean),
                width = 0.2) +
  ylim(0, 520) +
  ylab("Pest Biomass (units)") +
  xlab("Spray treatment") +
  # E and control are one group
  annotate("segment", x = 4.5, xend = 6.5, 
           y = 397, yend = 397,
           colour = "black", size = 1) +
  annotate("text", x = 5.5,  y = 385, 
           label = "N.S", size = 4) +
  # WaterControl-D and E-D    ***
  annotate("segment", x = 4, xend = 5.5, 
           y = 410, yend = 410,
           colour = "black") +
  annotate("segment", x = 4, xend = 4, 
           y = 410, yend = 400,
           colour = "black") +
  annotate("segment", x = 5.5, xend = 5.5, 
           y = 410, yend = 400,
           colour = "black") +
  annotate("text", x = 4.5,  y = 420, 
           label = "***", size = 5) +
  # WaterControl-B ***
  annotate("segment", x = 3, xend = 5.5, 
         y = 440, yend = 440,
         colour = "black") +
  annotate("segment", x = 3, xend = 3, 
           y = 440, yend = 430,
           colour = "black") +
  annotate("segment", x = 5.5, xend = 5.5, 
           y = 440, yend = 430,
           colour = "black") +
  annotate("text", x = 4,  y = 450,
           label = "***", size = 5) +
  # WaterControl-C ***
  annotate("segment", x = 2, xend = 5.5, 
           y = 475, yend = 475,
           colour = "black") +
  annotate("segment", x = 2, xend = 2, 
           y = 475, yend = 465,
           colour = "black") +
  annotate("segment", x = 5.5, xend = 5.5, 
           y = 475, yend = 465,
           colour = "black") +
  annotate("text", x = 3.5,  y = 485, 
           label = "***", size = 5) +
  # WaterControl-A ***
  annotate("segment", x = 1, xend = 5.5, 
         y = 510, yend = 510,
         colour = "black") +
  annotate("segment", x = 1, xend = 1, 
           y = 510, yend = 500,
           colour = "black") +
  annotate("segment", x = 5.5, xend = 5.5, 
           y = 510, yend = 500,
           colour = "black") +
  annotate("text", x = 3.5,  y = 520, 
           label = "***", size = 5) +  
# A-D ***
  annotate("segment", x = 1, xend = 4, 
         y = 330, yend = 330,
         colour = "black") +
  annotate("segment", x = 1, xend = 1, 
           y = 330, yend = 320,
           colour = "black") +
  annotate("segment", x = 4, xend = 4, 
           y = 330, yend = 320,
           colour = "black") +
  annotate("text", x = 2.5,  y = 335, 
           label = "*", size = 5) +
  theme_classic()

# Figure 1. The mean pest biomass following various insecticide treatments.
# Error bars are +/- 1 S.E. Significant comparisons are indicated.

