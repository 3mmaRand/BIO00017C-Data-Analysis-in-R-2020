## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)


## ----echo=FALSE, results='hide'------------------------------------------
library(tidyverse)


## ----load-haven, include=FALSE-------------------------------------------
library(haven)


## ----importperi, include=FALSE-------------------------------------------
periwinkle <- read_sav("../data/periwinkle.sav")


## ----eval=FALSE----------------------------------------------------------
## periwinkle <- read_sav("raw_data/periwinkle.sav")


## ------------------------------------------------------------------------
glimpse(periwinkle)


## ------------------------------------------------------------------------
attr(periwinkle$season, "labels")
attr(periwinkle$species, "labels")


## ------------------------------------------------------------------------
periwinkle <- periwinkle %>% 
  mutate(season = as_factor(season),
         species = as_factor(species))


## ----eval=FALSE----------------------------------------------------------
## write.table(periwinkle, "processed_data/periwinkle.txt")


## ------------------------------------------------------------------------
ggplot(data = periwinkle, aes(x = season, y = para, fill = species)) +
  geom_boxplot()


## ----perisum-------------------------------------------------------------
perisum <- periwinkle %>% 
  group_by(season, species) %>% 
  summarise(mean = mean(para),
            median = median(para),
            sd = sd(para),
            n = length(para),
            se = sd / sqrt(n))



## ----anovatestperi-------------------------------------------------------
mod <- aov(data = periwinkle, para ~ season * species)
summary(mod)


## ----posthoc-------------------------------------------------------------
TukeyHSD(mod)


## ----assumptionperi, include=FALSE---------------------------------------
#---CODING AND THINKING ANSWER---
plot(mod, which = 1)
hist(mod$residuals)
shapiro.test(mod$residuals)
#These look ok


## ----echo=FALSE, fig.height=5, fig.width=6-------------------------------

ggplot() +
  geom_point(data = periwinkle, aes(x = season, y = para, shape = species),
             position = position_jitterdodge(dodge.width = 1, jitter.width = 0.4, jitter.height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean - se, ymax = mean + se, group = species),
                width = 0.4, position = position_dodge(width = 1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean, ymax = mean, group = species),
                width = 0.3, position = position_dodge(width = 1) ) +
  ylab("Number of parasites") +
  ylim(0, 125) +
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank())

  


## ----fig.height=5, fig.width=5-------------------------------------------

ggplot() +
  geom_point(data = periwinkle, aes(x = season, y = para),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean - se, ymax = mean + se),
                width = 0.4, size = 1) +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean, ymax = mean),
                width = 0.3, size = 1 )

  


## ----fig.height=5, fig.width=6-------------------------------------------

ggplot() +
  geom_point(data = periwinkle, aes(x = season, y = para, shape = species),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean - se, ymax = mean + se),
                width = 0.4, size = 1) +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean, ymax = mean),
                width = 0.3, size = 1)
  


## ----fig.height=5, fig.width=6-------------------------------------------
ggplot() +
  geom_point(data = periwinkle, aes(x = season, y = para, shape = species),
             position = position_jitterdodge(dodge.width = 1, 
                                             jitter.width = 0.4, 
                                             jitter.height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean - se, ymax = mean + se, group = species),
                width = 0.4, size = 1,
                position = position_dodge(1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean, ymax = mean, group = species),
                width = 0.3, size = 1, 
                position = position_dodge(1))

  


## ----fig.height=5, fig.width=6-------------------------------------------

ggplot() +
  geom_point(data = periwinkle, aes(x = season, y = para, shape = species),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.4,
                                             jitter.height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean - se, ymax = mean + se, group = species),
                width = 0.4, size = 1,
                position = position_dodge(width = 1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean, ymax = mean, group = species),
                width = 0.3, size = 1,
                position = position_dodge(width = 1) ) +
  ylab("Number of parasites") +
  ylim(0, 125) +
  xlab("Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.2))

  


## ----include = FALSE, results = 'hide', fig.height=5, fig.width=6--------

# the signifcant comparisons are                           p adj
# Summer:Littorina saxatilis-Spring:Littorina saxatilis    0.0000041
# Summer:Littorina nigrolineata-Spring:Littorina saxatilis 0.0003558
# Spring:Littorina nigrolineata-Summer:Littorina saxatilis 0.0198124
perfig <- ggplot() +
  geom_point(data = periwinkle, aes(x = season, y = para, shape = species),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.4,
                                             jitter.height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean - se, ymax = mean + se, group = species),
                width = 0.4, size = 1,
                position = position_dodge(width = 1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, ymin = mean, ymax = mean, group = species),
                width = 0.3, size = 1,
                position = position_dodge(width = 1) ) +
  ylab("Number of parasites") +
  ylim(0, 125) +
  xlab("Season") +
  # Spring:Littorina nigrolineata-Summer:Littorina saxatilis *
  annotate("segment", 
           x = 1.25, xend = 1.75, 
           y = 110, yend = 110,
           colour = "black") +
  annotate("segment", 
           x = 1.25, xend = 1.25,
           y = 110, yend = 105,
           colour = "black") +
  annotate("segment", 
           x = 1.75, xend = 1.75,
           y = 110, yend = 105,
           colour = "black") +
  annotate("text", 
           x = 1.5,  y = 112,
           label = "***", size = 6) +
  # Summer:Littorina nigrolineata-Spring:Littorina saxatilis: ***
  annotate("segment", 
           x = 1.25, xend = 0.75,
           y = 90, yend = 90,
           colour = "black") +
  annotate("segment", 
           x = 1.25, xend = 1.25,
           y = 90, yend = 85,
           colour = "black") +
  annotate("segment", 
           x = 0.75, xend = 0.75,
           y = 90, yend = 85,
           colour = "black") +
  annotate("text", x = 1,  y = 92,
           label = "**", size = 6) +
# Summer:Littorina saxatilis-Spring:Littorina saxatilis: ***
  annotate("segment",
           x = 0.75, xend = 1.75,
           y = 120, yend = 120,
           colour = "black") +
  annotate("segment",
           x = 0.75, xend = 0.75,
           y = 120, yend = 115,
           colour = "black") +
  annotate("segment",
           x = 1.75, xend = 1.75,
           y = 120, yend = 115,
           colour = "black") +
  annotate("text", x = 1.25,  y = 123,
           label = "***", size = 6) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.2))


## ----echo = FALSE--------------------------------------------------------
# you will need figures/periwinkle.tif to save in your figures directory
ggsave("periwinkle.tif",
       plot = perfig,
       device = "tiff",
       width = 5,
       height = 4,
       units = "in",
       dpi = 300)
# or
ggsave("periwinkle.png",
       plot = perfig,
       width = 5,
       height = 4,
       units = "in",
       dpi = 300)



## ---- include=FALSE, results='hide'--------------------------------------
#---CODING AND THINKING ANSWER---

# import the data using the haven package
yield <- read_dta("../data/yield.dta")
str(yield)

# make explanatory variables factors 
# I've renamed them as nitrogen and potassium too
yield <- yield %>% 
  mutate(n = as_factor(n),
         k = as_factor(k)) %>% 
  rename(Nitrogen = n,
         Potassium = k)


#looking at the dataset there doesn't seem to be anything very obviously non-normal (lots of values the same, too many zeros, or extreme values)
#we will check the assumptions after we have run the anova

# Do a rough plot of the data
ggplot(data = yield, aes(x = Nitrogen, y = kg, fill = Potassium)) +
  geom_boxplot()



#a summary of the data (means and se) is useful both to help us understand the data and for plotting later
yieldsum <- yield %>% 
  group_by(Nitrogen, Potassium) %>% 
  summarise(mean = mean(kg),
            median = median(kg),
            sd = sd(kg),
            n = length(kg),
            se = sd/sqrt(n))
# # A tibble: 4 x 7
# # Groups:   Nitrogen [2]
#   Nitrogen Potassium  mean median    sd     n    se
#   <fct>    <fct>     <dbl>  <dbl> <dbl> <int> <dbl>
# 1 low      low        9.72   9.93  4.29    10  1.36
# 2 low      high      16.5   16.0   5.26    10  1.66
# 3 high     low       15.6   16.4   3.74    10  1.18
# 4 high     high      21.0   21.8   3.75    10  1.19


# build the anova model
mod <- aov(data = yield, kg ~ Nitrogen * Potassium)
summary(mod)

#                    Df Sum Sq Mean Sq F value   Pr(>F)    
# Nitrogen            1  273.2   273.2  14.741  0.00048 ***
# Potassium           1  369.7   369.7  19.945 7.58e-05 ***
# Nitrogen:Potassium  1    4.3     4.3   0.232  0.63283    
# Residuals          36  667.2    18.5   
 # There was a significantly greater yield at high nitrogen than at low nitrogen (ANOVA: F = 14.7; d.f. = 1,36; p < 0.001) and at high potassium than at low potassium (F = 19.9; d.f. = 1,36; p < 0.001). These effects were independent.

# check the assumptions
plot(mod, which = 1)
hist(mod$residuals)
shapiro.test(mod$residuals)
# the variance is a bit lower in the group with the highest mean
# and the histogram has a bit of bumpat the low end but it looks ok

 
# it is questionable whether we really need a post-hoc test as
# the effect of nitorgen must be between high and low and the 
# effect of potassium must be between high and low (there are only two
# categories in each explanatory) and there is no interaction.
# However, lets do one any way
TukeyHSD(mod) 

# $`Nitrogen:Potassium`
#                      diff        lwr       upr     p adj
# high:low-low:low    5.883  0.6976609 11.068339 0.0209708
# low:high-low:low    6.736  1.5506609 11.921339 0.0066292
# high:high-low:low  11.307  6.1216609 16.492339 0.0000060
# low:high-high:low   0.853 -4.3323391  6.038339 0.9705373
# high:high-high:low  5.424  0.2386609 10.609339 0.0374574
# high:high-low:high  4.571 -0.6143391  9.756339 0.1004742
# All means differ except
#   low N:high K - high N:low K  p = 0.9705373 (red and blue boxes in the middle)
#   high N:high K - low N:high K p = 0.1004742 (the two blue boxes).
# In this situation, I might label those means that do NOT differ with the same letter code
# rather than adding a lot of lines indicating significance.

# figure
ggplot() +
  geom_point(data = yield, aes(x = Nitrogen, y = kg, shape = Potassium),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.4,
                                             jitter.height = 0),
             colour = "gray50", size = 2) +
  geom_errorbar(data = yieldsum, 
                aes(x = Nitrogen, ymin = mean - se, ymax = mean + se, group = Potassium),
                width = 0.4, size = 1,
                position = position_dodge(width = 1)) +
  geom_errorbar(data = yieldsum, 
                aes(x = Nitrogen, ymin = mean, ymax = mean, group = Potassium),
                width = 0.3, size = 1,
                position = position_dodge(width = 1) ) +
  ylab("Yield (kg)") +
  ylim(0, 30) +
  annotate("text", x = 1.25,  y = 5,
           label = "a", size = 5) +
  annotate("text", x = 1.75,  y = 5,
           label = "a", size = 5) +
  annotate("text", x = 1.25,  y = 3,
           label = "b", size = 5) +
  annotate("text", x = 2.25,  y = 3,
           label = "b", size = 5) +
  theme_classic() +
  theme(legend.position = c(0.1, 0.9))


