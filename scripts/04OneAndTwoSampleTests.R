## ----setup, include=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,	
                      warning = FALSE,
                      fig.width = 4, 
                      fig.height = 4, 
                      fig.retina = 3)


## ----include=FALSE----------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(RefManageR)


## ---- load-refs, include=FALSE, cache=FALSE---------------------------------------------------
BibOptions(check.entries = FALSE,
           bib.style = "authoryear",
           cite.style = "authoryear",
           style = "markdown",
           hyperlink = TRUE,
           dashed = FALSE,
           longnamesfirst = FALSE,
           max.names = 2)
myBib <- ReadBib("../refs/refs.bib", check = FALSE)


## ---------------------------------------------------------------------------------------------
library(tidyverse)


## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------
## #---CODING ANSWER---
## # import
## adip  <-  read_table("data/adipocytes.txt")
## str(adip)


## ---- include=FALSE---------------------------------------------------------------------------
# importing for emma
# my directory structure differs
adip  <-  read_table("../data/adipocytes.txt")
str(adip)


## ---------------------------------------------------------------------------------------------
ggplot(data = adip, aes(x = treatment, y = adiponectin)) +
  geom_violin()


## ----adipsummary, echo = FALSE----------------------------------------------------------------
#---CODING ANSWER---
adipsummary <- adip %>%
  group_by(treatment) %>%
  summarise(mean = mean(adiponectin),
            std = sd(adiponectin),
            n = length(adiponectin),
            se = std/sqrt(n))



## ----echo=FALSE-------------------------------------------------------------------------------
knitr::kable(adipsummary) %>% kableExtra::kable_styling()


## ----adipt------------------------------------------------------------------------------------
t.test(data = adip,
       adiponectin ~ treatment,
       var.equal = T)


## ---------------------------------------------------------------------------------------------
# add the group means to the data
adip <- merge(adip, adipsummary[1:2], by = "treatment")


## ---------------------------------------------------------------------------------------------
# add the residuals
adip <- adip %>%
  mutate(residual = adiponectin - mean)


## ---------------------------------------------------------------------------------------------
ggplot(data = adip,
       aes(x = mean, y = residual)) +
  geom_point()


## ---------------------------------------------------------------------------------------------
ggplot(data = adip,
       aes(x = residual)) +
  geom_histogram(bins = 10)


## ---------------------------------------------------------------------------------------------
shapiro.test(adip$residual)


## ----adipfigdemo, echo = FALSE, fig.width = 4, fig.height = 4---------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "Adiponectin (pg/mL)", limits = c(0, 12), expand = c(0, 0)) +
  scale_x_discrete(name = "Treatment", labels = c("Control", "Nicotinic acid")) +
  theme_classic()



## ---- fig.width = 4, fig.height = 4-----------------------------------------------------------
ggplot()


## ---- fig.width = 4, fig.height = 4-----------------------------------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin))


## ---- fig.width = 4, fig.height = 4-----------------------------------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin),
             position = position_jitter(width = 0.1, height = 0))


## ---- fig.width = 4, fig.height = 4-----------------------------------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "grey50")


## ---- fig.width = 4, fig.height = 4-----------------------------------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "grey50") +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean - se, ymax = mean + se),
                width = 0.3) 
  


## ---- fig.width = 4, fig.height = 4-----------------------------------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "grey50") +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean, ymax = mean),
                width = 0.2)
  


## ---- fig.width = 4, fig.height = 4-----------------------------------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "grey50") +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "Adiponectin (pg/mL)", 
                     limits = c(0, 12), 
                     expand = c(0, 0)) +
  scale_x_discrete(name = "Treatment", 
                   labels = c("Control", "Nicotinic acid"))
  


## ---- fig.width = 4, fig.height = 4-----------------------------------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "Adiponectin (pg/mL)", 
                     limits = c(0, 12), 
                     expand = c(0, 0)) +
  scale_x_discrete(name = "Treatment", 
                   labels = c("Control", "Nicotinic acid")) +
  theme_classic()


## ----echo = FALSE-----------------------------------------------------------------------------
ggsave("adipocytes.png",
       width = 5,
       height = 4,
       units = "in")


## ---- echo = FALSE, fig.width = 4, fig.height = 4---------------------------------------------
ggplot() +
  geom_point(data = adip, aes(x = treatment, y = adiponectin),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = adipsummary, 
                aes(x = treatment, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = "Adiponectin (pg/mL)", 
                     limits = c(0, 12), 
                     expand = c(0, 0)) +
  scale_x_discrete(name = "Treatment", 
                   labels = c("Control", "Nicotinic acid")) +
  annotate("segment", x = 1, xend = 2, 
           y = 11.3, yend = 11.3,
           colour = "black") +
  annotate("segment", x = 2, xend = 2, 
           y = 11.3, yend = 11,
           colour = "black") +
  annotate("segment", x = 1, xend = 1, 
           y = 11.3, yend = 11,
           colour = "black") +
  annotate("text", x = 1.5,  y = 11.7, 
           label = expression(italic(p)~"= 0.003")) +
  theme_classic()


## ----grousedata, include = FALSE--------------------------------------------------------------
#---CODING ANSWER---
# I did this by putting the data in two columns then using pivot_longer
grouse <- data.frame(gordon = c(5, 16, 8, 64, 51, 11, 9, 7, 43, 49), 
                     moss = c(0, 2, 1, 3, 6, 10, 4, 12, 19, 20)) %>% 
  pivot_longer(cols = everything(),
               names_to = "estate",
               values_to = "nematodes")



## ----echo = FALSE-----------------------------------------------------------------------------
DT::datatable(grouse)


## ----include=FALSE----------------------------------------------------------------------------
#---CODING ANSWER---
grouse %>% 
  group_by(estate) %>% 
  summarise(median(nematodes))


## ----grousewilcox-----------------------------------------------------------------------------
wilcox.test(data = grouse, nematodes ~ estate)


## ----grousefig, fig.width = 4, fig.height = 4-------------------------------------------------
ggplot(data = grouse, aes(x = estate, y = nematodes) ) +
  geom_boxplot() 


## ----include = FALSE, fig.width = 4, fig.height = 4-------------------------------------------
#---CODING ANSWER---
ggplot(data = grouse, aes(x = estate, y = nematodes) ) +
  geom_boxplot() + 
  scale_x_discrete(name = "Estate", labels = c("Gordon", "Moss")) +
  scale_y_continuous(name = "Number of nematodes") +
  theme_classic()


## ----geneimport, echo = FALSE-----------------------------------------------------------------
#---CODING ANSWER---
# I have my data files in a folder called data
coliexp  <-  read_table("../data/coliexp.txt")


## ----genepairedt------------------------------------------------------------------------------
t.test(data = coliexp, expression ~ temperature, paired = T)



## ----csativa, include=FALSE-------------------------------------------------------------------
#---THINKING AND CODING ANSWER---
csativa  <-  read_table("../data/csativa.txt")
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

# plot
ggplot(data = csativa,
       aes(x = mean, y = residual)) +
  geom_point()
# normality test
shapiro.test(csativa$residual)

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
  scale_x_discrete(name = "Plant type", labels = c("GMO", "WT")) +
  scale_y_continuous(name = "Amount of Omega 3 (units)",
                     expand = c(0, 0),
                     limits = c(0, 80)) +
  theme_classic()
   


## ----sheep, include=FALSE---------------------------------------------------------------------
#---THINKING AND CODING ANSWER---
#the data are paired. although the two treatments are not applied to the same individual, they are applied to each of a set of twins. the first sheep fed unfertilised grass is the twin of the first sheep fed fertilsed grass. this means the columns are not independent and we need to do a paired test (either a paired wilcoxon or a paired t). These data do not appear to be normally distributed (not many values, integers) a non-parametric test is probably preferable. 

# read in the data
sheep  <-  read_table("../data/sheep.txt")
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
  geom_point() +
  geom_line() +
  scale_x_discrete(name = "Grass",
                   labels = c("Fertilised", "Unfertilised")) +
  scale_y_continuous(name = "Weight (Kg)", 
                     expand = c(0, 0),
                     limits = c(0, 55)) +
  theme_classic() +
  theme(legend.position = "none")



## ----refs, echo=FALSE, results="asis"---------------------------------------------------------
PrintBibliography(myBib)  

