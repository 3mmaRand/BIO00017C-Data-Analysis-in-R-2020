## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,	
                      warning = FALSE,
                      fig.width = 4, 
                      fig.height = 4, 
                      fig.retina = 3)


## ----include=FALSE----------------------------------------------------
library(tidyverse)
library(kableExtra)
library(RefManageR)


## ---- load-refs, include=FALSE, cache=FALSE---------------------------
BibOptions(check.entries = FALSE,
           bib.style = "authoryear",
           cite.style = "authoryear",
           style = "markdown",
           hyperlink = TRUE,
           dashed = FALSE,
           longnamesfirst = FALSE,
           max.names = 2)
myBib <- ReadBib("../refs/refs.bib", check = FALSE)


## ---------------------------------------------------------------------
library(tidyverse)


## ----include=FALSE----------------------------------------------------
library(readxl)


## ----eval=FALSE-------------------------------------------------------
## file <- "data-raw/periwinkle.xlsx"


## ----echo=FALSE-------------------------------------------------------
# I have a different
file <- "../data/periwinkle.xlsx"


## ---------------------------------------------------------------------
excel_sheets(file)


## ---------------------------------------------------------------------
spr <- read_excel(file, sheet = "spring")
str(spr)


## ----echo=FALSE-------------------------------------------------------
#---CODING ANSWER---
sum <- read_excel(file, sheet = "summer")


## ---------------------------------------------------------------------
periwinkle <- bind_rows(spr, sum)


## ----echo=FALSE-------------------------------------------------------
glimpse(periwinkle)


## ---------------------------------------------------------------------
write_delim(periwinkle, "data-processed/periwinkle.txt")


## ---------------------------------------------------------------------
ggplot(data = periwinkle, aes(x = season, y = para, fill = species)) +
  geom_boxplot()


## ---------------------------------------------------------------------
perisum <- periwinkle %>% 
  group_by(season, species) %>% 
  summarise(mean = mean(para),
            median = median(para),
            sd = sd(para),
            n = length(para),
            se = sd / sqrt(n))



## ---------------------------------------------------------------------
mod <- aov(data = periwinkle, para ~ season * species)
summary(mod)


## ----echo=FALSE-------------------------------------------------------
#---CODING ANSWER---
TukeyHSD(mod)


## ----include=FALSE----------------------------------------------------
#---CODING AND THINKING ANSWER---
plot(mod, which = 1)
# the variance is similar across the values of para
hist(mod$residuals)
# maybe a little skew (tail to the right) but IMO fairly symmetrical
shapiro.test(mod$residuals)
# and the distribution is not significantly difference from the normal distribution. As there are 100 values, we can be reasonably confident that an NS means there is not difference rather than we just don't have enough data to detect one.


## ----echo=FALSE, fig.height=5, fig.width=6----------------------------

ggplot() +
  geom_point(data = periwinkle, 
             aes(x = season, y = para, shape = species),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.3,
                                             jitter.height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean - se, 
                    ymax = mean + se,
                    group = species),
                width = 0.4, 
                position = position_dodge(width = 1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean,
                    ymax = mean, 
                    group = species),
                width = 0.3, 
                size = 1,
                position = position_dodge(width = 1) ) +
  scale_y_continuous(name = "Number of parasites",
                     expand = c(0, 0),
                     limits = c(0, 130)) +
  scale_x_discrete(name = "Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.95))



## ----fig.height=5, fig.width=5----------------------------------------
ggplot() +
  geom_point(data = periwinkle, 
             aes(x = season, y = para),
             position = position_jitter(width = 0.3,
                                        height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean - se, 
                    ymax = mean + se),
                width = 0.4, 
                position = position_dodge(width = 1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean,
                    ymax = mean),
                width = 0.3, 
                size = 1)
  


## ----fig.height=5, fig.width=6----------------------------------------

ggplot() +
  geom_point(data = periwinkle,
             aes(x = season, y = para, shape = species),
             position = position_jitter(width = 0.3, 
                                        height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean - se,
                    ymax = mean + se),
                width = 0.4, size = 1) +
  geom_errorbar(data = perisum, 
                aes(x = season,
                    ymin = mean,
                    ymax = mean),
                width = 0.3, 
                size = 1)
  


## ----fig.height=5, fig.width=6----------------------------------------
ggplot() +
  geom_point(data = periwinkle, 
             aes(x = season, y = para, shape = species),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.3,
                                             jitter.height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean - se, 
                    ymax = mean + se,
                    group = species),
                width = 0.4, 
                position = position_dodge(width = 1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean,
                    ymax = mean, 
                    group = species),
                width = 0.3, 
                size = 1,
                position = position_dodge(width = 1) ) 

  


## ----fig.height=5, fig.width=6----------------------------------------
ggplot() +
  geom_point(data = periwinkle, 
             aes(x = season, y = para, shape = species),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.3,
                                             jitter.height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean - se, 
                    ymax = mean + se,
                    group = species),
                width = 0.4, 
                position = position_dodge(width = 1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean,
                    ymax = mean, 
                    group = species),
                width = 0.3, 
                size = 1,
                position = position_dodge(width = 1) ) +
  scale_y_continuous(name = "Number of parasites",
                     expand = c(0, 0),
                     limits = c(0, 130)) +
  scale_x_discrete(name = "Season") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.95))


## ----include = FALSE, results = 'hide', fig.height=5, fig.width=6-----

# the signifcant comparisons are                           p adj
# Summer: saxatilis-Spring: saxatilis    0.0000041
# Summer: nigrolineata-Spring: saxatilis 0.0003558
# Spring: nigrolineata-Summer: saxatilis 0.0198124
perfig <- ggplot() +
  geom_point(data = periwinkle, 
             aes(x = season, y = para, shape = species),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.3,
                                             jitter.height = 0),
             colour = "gray50") +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean - se, 
                    ymax = mean + se,
                    group = species),
                width = 0.4, 
                position = position_dodge(width = 1)) +
  geom_errorbar(data = perisum, 
                aes(x = season, 
                    ymin = mean,
                    ymax = mean, 
                    group = species),
                width = 0.3, 
                size = 1,
                position = position_dodge(width = 1) ) +
  scale_y_continuous(name = "Number of parasites",
                     expand = c(0, 0),
                     limits = c(0, 140)) +
  scale_x_discrete(name = "Season") +
  # Summer: nigrolineata-Spring: saxatilis 0.0003558
  annotate("segment", 
           x = 1.25, xend = 1.75, 
           y = 112, yend = 112,
           colour = "black") +
  annotate("segment", 
           x = 1.25, xend = 1.25,
           y = 112, yend = 110,
           colour = "black") +
  annotate("segment", 
           x = 1.75, xend = 1.75,
           y = 112, yend = 110,
           colour = "black") +
  annotate("text", 
           x = 1.5,  y = 114,
           label = "***", size = 6) +
  # Spring: nigrolineata-Summer: saxatilis 0.0198124
  annotate("segment", 
           x = 1.75, xend = 0.75,
           y = 104, yend = 104,
           colour = "black") +
  annotate("segment", 
           x = 1.75, xend = 1.75,
           y = 104, yend = 102,
           colour = "black") +
  annotate("segment", 
           x = 0.75, xend = 0.75,
           y = 104, yend = 102,
           colour = "black") +
  annotate("text", x = 1.25,  y = 106,
           label = "*", size = 6) +
# Summer: saxatilis-Spring: saxatilis    0.0000041
  annotate("segment",
           x = 1.25, xend = 2.25,
           y = 120, yend = 120,
           colour = "black") +
  annotate("segment",
           x = 1.25, xend = 1.25,
           y = 120, yend = 118,
           colour = "black") +
  annotate("segment",
           x = 2.25, xend = 2.25,
           y = 120, yend = 118,
           colour = "black") +
  annotate("text", x = 1.75,  y = 122,
           label = "***", size = 6) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.95))


## ----echo = FALSE-----------------------------------------------------
ggsave("figures/periwinkle.tif",
       plot = perfig,
       device = "tiff",
       width = 5,
       height = 4,
       units = "in",
       dpi = 300)
# or
ggsave("figures/periwinkle.png",
       plot = perfig,
       device = "png",
       width = 5,
       height = 4,
       units = "in",
       dpi = 300)



## ---- include=FALSE, results='hide'-----------------------------------
#---CODING AND THINKING ANSWER---
# import the data 
# note - if you structured as directed, your path will be data-raw/yield.xlsx
file <- "../data/yield.xlsx"
excel_sheets(file)
low_low <- read_excel(file, sheet = "low-low")
high_low <- read_excel(file, sheet = "high-low")
low_high <- read_excel(file, sheet = "low-high")
high_high <- read_excel(file, sheet = "high-high")
yield <- bind_rows(low_low, high_low, low_high, high_high)

glimpse(yield)

# looking at the dataset there doesn't seem to be anything very obviously non-normal (such as lots of values the same, too many zeros, or extreme values)
# we will check the assumptions after we have run the anova

# Do a rough plot of the data
ggplot(data = yield, aes(x = nitrogen, y = kg, fill = potassium)) +
  geom_boxplot()



#a summary of the data (means and se) is useful both to help us understand the data and for plotting later
yieldsum <- yield %>% 
  group_by(nitrogen, potassium) %>% 
  summarise(mean = mean(kg),
            median = median(kg),
            sd = sd(kg),
            n = length(kg),
            se = sd/sqrt(n))
# # A tibble: 4 x 7
# # Groups:   nitrogen [2]
#   nitrogen potassium  mean median    sd     n    se
#   <chr>    <chr>     <dbl>  <dbl> <dbl> <int> <dbl>
# 1 high     high      21.0   21.8   3.75    10  1.19
# 2 high     low       15.6   16.4   3.74    10  1.18
# 3 low      high      16.5   16.0   5.26    10  1.66
# 4 low      low        9.72   9.93  4.29    10  1.36


# build the anova model
mod <- aov(data = yield, kg ~ nitrogen * potassium)
summary(mod)

#                    Df Sum Sq Mean Sq F value   Pr(>F)    
# nitrogen            1  273.2   273.2  14.741  0.00048 ***
# potassium           1  369.7   369.7  19.945 7.58e-05 ***
# nitrogen:potassium  1    4.3     4.3   0.232  0.63283    
# Residuals          36  667.2    18.5    
 # There was a significantly greater yield at high nitrogen than at low nitrogen (ANOVA: F = 14.7; d.f. = 1,36; p < 0.001) and at high potassium than at low potassium (F = 19.9; d.f. = 1,36; p < 0.001). These effects were independent.

# check the assumptions
plot(mod, which = 1)
hist(mod$residuals)
shapiro.test(mod$residuals)
# the variance is a bit lower in the group with the highest mean
# and the histogram has a bit of bump at the low end but it looks ok

 
# it is questionable whether we really need a post-hoc test as
# the effect of nitrogen must be between high and low and the 
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
  geom_point(data = yield, 
             aes(x = nitrogen, y = kg, shape = potassium),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.25,
                                             jitter.height = 0),
             colour = "black", 
             size = 2) +
  geom_errorbar(data = yieldsum, 
                aes(x = nitrogen, 
                    ymin = mean - se,
                    ymax = mean + se,
                    group = potassium),
                width = 0.4, size = 1,
                position = position_dodge(width = 1)) +
  geom_errorbar(data = yieldsum, 
                aes(x = nitrogen, 
                    ymin = mean, 
                    ymax = mean, 
                    group = potassium),
                width = 0.3, size = 1,
                position = position_dodge(width = 1) ) +
  scale_y_continuous(name = "Yield (kg)", 
                     limits = c(0, 40),
                     expand = c(0, 0)) +
  scale_x_discrete(name = "Nitrogen", 
                   labels = c("High", "Low")) +
  scale_shape_discrete(name = "Potassium", 
                   labels = c("High", "Low"),
                   solid = FALSE) +
  annotate("text", x = 1.25,  y = 5,
           label = "a", size = 5) +
  annotate("text", x = 1.75,  y = 5,
           label = "a", size = 5) +
  annotate("text", x = 1.25,  y = 3,
           label = "b", size = 5) +
  annotate("text", x = 2.25,  y = 3,
           label = "b", size = 5) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.9))



## ----refs, echo=FALSE, results="asis"---------------------------------
PrintBibliography(myBib)  

