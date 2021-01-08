######################################################################
#                                                                    #
#   The effect of different growth media on the growth of            #
#   three E.coli strains.                                            #
#                                                                    #     
######################################################################

######################################################################
#                             Introduction                           #
######################################################################

# The data in ecoli.txt are from an investigation of the growth of 
# three E.coli strains on four different media. The data are measures 
# of optical density (in arbitrary units) which gives an indication 
# of the number of cells in the medium. These data were analysed with
# the aim of making recommendations to researchers about the best 
# strain or medium to use.

# The data are organised into 3 columns
# 'data.frame':	96 obs. of  3 variables:
#   $ dens  : num  2.9 9.8 12.8 7 11.6 10.9 14.4 5.1 17.7 12.1 ...
# $ Strain: int  3 Strainds, 1, 2 and 3
# $ medium: Factor w/ 4 levels "Circle", "Colibroth","Eplus", "GoCo"    


######################################################################
#                            Set up                                  #
######################################################################


# working directory (in my case, yours will differ)
# note: if you are using RStudio projects, this is unnecessary
setwd("M:/web/17C - 2018/scripts")

# packages

library(tidyverse)
# Hadley Wickham (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version
# 1.2.1. https://CRAN.R-project.org/package=tidyverse

######################################################################
#                       Import and tidy data                         #
######################################################################

# data are in ../data
ecoli <- read.table("../data/ecoli.txt", header = T)

# check structure
str(ecoli)

# 'data.frame':	96 obs. of  3 variables:
# $ dens  : num  2.9 9.8 12.8 7 11.6 10.9 14.4 
# $ Strain: int  1 1 1 1 1 1 1 1 1 1 ...
# $ medium: Factor w/ 4 levels "Circle","Colibroth"

# names of the different media
levels(ecoli$medium)
# "Circle"    "Colibroth" "Eplus"     "GoCo"   


# The data are in tidy format but Strain is treated as a measure.
# Make $Strain a factor
ecoli$Strain <- factor(ecoli$Strain)

# check structure
str(ecoli)
# 'data.frame':	96 obs. of  3 variables:
# $ dens  : num  2.9 9.8 12.8 7 11.6 10.9 14.4 5.1 17.7 12.
# $ Strain: Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1
# $ medium: Factor w/ 4 levels "Circle","Colibroth",..


######################################################################
#                     Exploratory Analysis                           #
######################################################################

# looking at the dataset there doesn't seem to be anything very 
# obviously non-normal (such as lots of values the same, too many 
# zeros, or extreme values

# calculate some summary statistics
# summary for strains
summarySE(ecoli, measurevar = "dens",
          groupvars = c("Strain"))
# Strain  N     dens       sd        se       ci
# 1      1 32 12.60625 5.216657 0.9221833 1.880805
# 2      2 32 14.94063 6.296177 1.1130174 2.270014
# 3      3 32 15.29063 6.289231 1.1117895 2.267510

# Strain 3 has grown the best on average

# summary for media
summarySE(ecoli, measurevar = "dens",
          groupvars = c("medium"))
# medium  N     dens       sd       se       ci
# 1    Circle 24 12.17917 6.440293 1.314619 2.719497
# 2 Colibroth 24 16.79583 5.072473 1.035414 2.141917
# 3     Eplus 24 15.92500 5.314480 1.084814 2.244108
# 4      GoCo 24 12.21667 5.951519 1.214849 2.513106

# Colibroth promotes best growth on average but Eplus is close


# Treatment combination summary. Summary will be useful for figure
ecolisummary <- summarySE(ecoli, measurevar = "dens",
                          groupvars = c("Strain", "medium"))

# Strain    medium N    dens       sd       se       ci
# 1       1    Circle 8  9.3125 3.968604 1.403114 3.317836
# 2       1 Colibroth 8 14.6875 4.837779 1.710413 4.044485
# 3       1     Eplus 8 17.3125 3.267781 1.155335 2.731933
# 4       1      GoCo 8  9.1125 3.751357 1.326305 3.136213
# 5       2    Circle 8 10.6000 7.297945 2.580213 6.101235
# 6       2 Colibroth 8 15.3125 4.888014 1.728174 4.086482
# 7       2     Eplus 8 15.7250 6.790697 2.400874 5.677165
# 8       2      GoCo 8 18.1250 4.278768 1.512773 3.577140
# 9       3    Circle 8 16.6250 5.695800 2.013770 4.761808
# 10      3 Colibroth 8 20.3875 3.860954 1.365053 3.227838
# 11      3     Eplus 8 14.7375 5.670207 2.004721 4.740412
# 12      3      GoCo 8  9.4125 4.909884 1.735906 4.104766

# it looks as though different media favour different strains.  

# conclusion: n = 8 for each treatment combination is quite
# small but the design is balanced, the data have decimal places,
# lack repeated repeated values and extreme values

# We would expect such OD data to be normally distributed therefore
# two-way ANOVA is the desired test if the assumptions are met

######################################################################
#                     Statistical Analysis                           #
######################################################################

# The assumptions of the two-way ANOVA test are 1. the residuals are
# normally distributed and 2. the variances is homogenous.
# These will be investigated in the ANOVA model constructed.

# Carrying out an ANOVA to examine the effect of strain, the effect 
# of medium and whether their effects are independent
mod <- aov(data = ecoli, dens ~ Strain * medium)
summary(mod)
# Medium appears significant (F = 5.5, d.f. = 3. 84, p = 0.0017); 
# strain appears NS but there is a signifcant interaction present 
# indicating that the effect of medium depends on the strain 
# that is being considered. 

# investigate the dependence of effects with an interactions plot
# consider both ways round
interaction.plot(ecoli$Strain, ecoli$medium,ecoli$dens)
interaction.plot(ecoli$medium, ecoli$Strain, ecoli$dens)
# No single medium is best for all strains, GoCo is best 
# for strain 2 but worse for strains 1 and 3. If you wanted 
# to chose one medium which was 'ok' for any strain, you 
# might chose Colibroth or Eplus as the other two media are e
# specially bad for at least one of the strains. Eplus is the 
# most independent of strain. Post-hoc tests are required to
# establish is these patterns are signifcant

# Carrying out a Tukey Honest Signifcant differences test
# to see where differences bewteen treatment groups. There are many
# comparisons and understanding the results means going through
# these carefully. There are no shortcuts.
# There are many comparisons so a plot is not that useful here
TukeyHSD(mod)
# Significant comparisons listed only
# $`Strain:medium`
#                             diff        lwr       upr     p adj
# 3:Colibroth-1:Circle     11.0750   2.543727 19.606273 0.0020260
# 2:GoCo-1:Circle           8.8125   0.281227 17.343773 0.0365730
# 3:Colibroth-2:Circle      9.7875   1.256227 18.318773 0.0113418
# 1:GoCo-3:Colibroth      -11.2750 -19.806273 -2.743727 0.0015266
# 2:GoCo-1:GoCo             9.0125   0.481227 17.543773 0.0290721
# 3:GoCo-2:GoCo            -8.7125 -17.243773 -0.181227 0.0409305

# Notable from this and summary data: 
# If you are using GoCo, use strain 2 (it's better than 1 and 
# 3 on GoCo). If you're only using Circle use Strain 3. But 
# best combination is Colibroth and Strain 3. No strain
# goes especially badly on Colibroth or Eplus

######################################################################
#                                 Figure                             #
######################################################################

# uses the summary data


ggplot(ecolisummary, aes(x = medium, y = dens, fill=Strain) ) +
  geom_bar(stat="identity", colour="black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = dens - se, ymax = dens + se),
                width = 0.4, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#FFFFFF", "#CCCCCC", "#999999")) +
  ylab("Optical density (units)") +
  ylim(0, 30) +
  xlab("Medium") +
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) +
  # 2:GoCo-1:GoCo *
  annotate("segment", x = 3.7, xend = 3.95, 
           y = 20.5, yend = 20.5,
           colour = "black") +
  annotate("segment", x = 3.7, xend = 3.7, 
           y = 20.5, yend = 20,
           colour = "black") +
  annotate("segment", x = 3.95, xend = 3.95, 
           y = 20.5, yend = 20,
           colour = "black") +
  annotate("text", x = 3.85,  y = 20.8, 
           label = "*", size = 5) +
  # 3:GoCo-2:GoCo *
  annotate("segment", x = 4.3, xend = 4.05, 
         y = 20.5, yend = 20.5,
         colour = "black") +
  annotate("segment", x = 4.3, xend = 4.3, 
           y = 20.5, yend = 20,
           colour = "black") +
  annotate("segment", x = 4.05, xend = 4.05, 
           y = 20.5, yend = 20,
           colour = "black") +
  annotate("text", x = 4.15,  y = 20.8, 
           label = "*", size = 5)+ 
  # 1:GoCo-3:Colibroth   **   
  annotate("segment", x = 3.7, xend = 2.35, 
           y = 22.5, yend = 22.5,
           colour = "black") +
  annotate("segment", x = 2.35, xend = 2.35, 
           y = 22.5, yend = 22,
           colour = "black") +
  annotate("segment", x = 3.7, xend = 3.7, 
           y = 22.5, yend = 22,
           colour = "black") +
  annotate("text", x = 3,  y = 22.8, 
           label = "**", size = 5) +
  # 2:GoCo-3:Colibroth
  annotate("segment", x = 4, xend = 2.35, 
           y = 23.5, yend = 23.5,
           colour = "black") +
  annotate("segment", x = 2.35, xend = 2.35, 
           y = 23.5, yend = 23,
           colour = "black") +
  annotate("segment", x = 4, xend = 4, 
           y = 23.5, yend = 23,
           colour = "black") +
  annotate("text", x = 3.1,  y = 23.8, 
           label = "**", size = 5) +
  # 3:Colibroth-2:Circle
  annotate("segment", x = 1, xend = 2.25, 
           y = 22.5, yend = 22.5,
           colour = "black") +
  annotate("segment", x = 2.25, xend = 2.25, 
           y = 22.5, yend = 22,
           colour = "black") +
  annotate("segment", x = 1, xend = 1, 
           y = 22.5, yend = 22,
           colour = "black") +
  annotate("text", x = 1.6,  y = 22.8, 
           label = "*", size = 5) +
  # 3:Colibroth-1:Circle
  annotate("segment", x = 0.7, xend = 2.25, 
           y = 23.5, yend = 23.5,
           colour = "black") +
  annotate("segment", x = 2.25, xend = 2.25, 
           y = 23.5, yend = 23,
           colour = "black") +
  annotate("segment", x = 0.7, xend = 0.7, 
           y = 23.5, yend = 23,
           colour = "black") +
  annotate("text", x = 1.5,  y = 23.8, 
           label = "**", size = 5)+
  # 2:GoCo-1:Circle 
  annotate("segment", x = 0.7, xend = 4, 
           y = 24.5, yend = 24.5,
           colour = "black") +
  annotate("segment", x = 4, xend = 4, 
           y = 24.5, yend = 24,
           colour = "black") +
  annotate("segment", x = 0.7, xend = 0.7, 
           y = 24.5, yend = 24,
           colour = "black") +
  annotate("text", x = 2.4,  y = 24.8, 
           label = "*", size = 5)
# Figure 2. The effect of media on the growth of different 
# strains of E.coli. Error bars are $\pm$ 1 S.E. Comparisons 
# significant under post-hoc testing with Tukey's Honest 
# Significant Difference test

