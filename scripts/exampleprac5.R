######################################################################
#                                                                    #
#   Effect of fertilisation on the length of time female             #
#   parasitic wasps take to lay eggs.                                #
#                                                                    #     
######################################################################

######################################################################
#                             Introduction                           #
######################################################################


# Wasps and other Hymenopterans (Ants and Bees) are haplo-diplod:  
# unfertilised eggs are haploid and develop into males, whereas 
# fertilised eggs are diploid and develop into females. 
# Researchers wanted to know if mating status affected the time taken
# for a female wasp to to lay its eggs on a beetle larva.. 

# The data are in ../data/wasp.txt Each row represents an individual
# wasp. The first column gives the time taken (in hours) and the 
# second column indicates whether they are mated (1) or unmated (2)

######################################################################
#                            Set up                                  #
######################################################################

# working directory (in my case, yours will differ)
setwd("M:/web/17C - 2018/scripts")

# packages
library(tidyverse)
# Hadley Wickham (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version 1.2.1.
# https://CRAN.R-project.org/package=tidyverse

######################################################################
#                       Import data                                  #
######################################################################

# data are in ../data
wasp  <-  read.table("../data/wasp.txt", header = T)

# check structure
str(wasp)
# 'data.frame':	60 obs. of  2 variables:
  # $ time  : int  22 33 14 24 26 18 17 12 35 27 ...
# $ status: Factor w/ 2 levels "mated","unmated": 1 1 1 1 1 1 1 1 1 1 ...

#

######################################################################
#                                Explore                             #
######################################################################

# quick plot
ggplot(data = wasp, aes(x = status, y = time)) +
  geom_violin()
# the figure suggested unmated females may take longer to lay their 
# eggs

# create summary statistics
waspsummary <- wasp %>%
group_by(status) %>%
  summarise(mean = mean(time),
            std = sd(time),
            n = length(time),
            se = std/sqrt(n))

# the scenario suggusts a two sample t test as the females are 
# either mated or unmated, not both. There is no link between the 
# first mated value and the first unmated value

######################################################################
#                                 Test                               #
######################################################################

# carry out a two-sample t-test
t.test(data = wasp,
       time ~ status,
       var.equal = T)
# Conclusion
# Unmated females take significantly longer (31.1 +/- 1.9 hr) than 
# mated females (23.4  +/- 1.6 hr) to complete egg laying (t-test: 
# t = 3.14; d.f. = 58; p = 0.003)

######################################################################
#                        Check assumptions                           #
######################################################################


# the assumptions are that the residuals are normally and homogenously 
# distributed across x.
# calculate the residuals - the difference between predicted
#  (i.e., the group mean) and observed values. 

# add column that holds the mean for the group each value 
# belongs to by 'merging' the summary data into the raw data:
wasp <- merge(wasp, waspsummary[,1:2], by = "status")

# Add a column for each 'residual'
wasp <- wasp %>%
  mutate(residual = time - mean)

# examine the distribution of the residuals. 
shapiro.test(wasp$residual)
# Not significant - our data are consistent with meeting the assumption.

ggplot(data = wasp,
       aes(x = mean, y = residual)) +
  geom_point()
# There's a bit of an outlier in one group but this looks 'ok' - 
# homogenously distributed across x.



######################################################################
#                                 Figure                             #
######################################################################

ggplot() +
  geom_point(data = wasp, aes(x = status, y = time),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = waspsummary, 
                aes(x = status, ymin = mean, ymax = mean),
                width = 0.2) +
  ylab("Time (hr)") +
  xlab(NULL) +
  ylim(0, 70) +
  scale_x_discrete(labels = c("Mated", "Unmated")) +
  theme_classic()