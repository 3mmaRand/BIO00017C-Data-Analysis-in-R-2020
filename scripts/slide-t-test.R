# A number of subspecies of the common chaffinch have been described, based principally on the differences in the pattern and colour of the adult male plumage. The subspecies can be divided into three groups: the "coelebs group" that occurs in Europe and Asia, the "spondiogenys group" in North Africa, and the "canariensis group" on the Canary Islands.[12] 


library(tidyverse)
chaff  <-  read_table2("data/chaff.txt")
str(chaff)

# First realise that this is a two sample test. You have two independent samples
#  - there are a total of 40 different individuals and the values in one 
#  group have no relationship to the values in the other.

# create a rough plot of the data  
ggplot(data = chaff, 
       aes(x = subspecies, y = mass)) +
  geom_violin()

# or you could use 
ggplot(data = chaff, 
       aes(x = subspecies, y = mass)) +
  geom_boxplot()

# note the coelebs seem to have lower mass

# create a summary of the data
chaffsum <- chaff %>%
  group_by(subspecies) %>%
  summarise(mean = mean(mass),
            std = sd(mass),
            n = length(mass),
            se = std/sqrt(n))

# The data seem to be continuous so it is likely that a t-test will be fine
t.test(data = chaff, 
       mass ~ subspecies, 
       var.equal = TRUE)
# So there is a significant difference but you need to make sure you know the direction!

# let's check the assumptions
# add the group means to the data
chaff <- merge(chaff, chaffsum[,1:2], by = "subspecies")
# add the residuals
chaff <- chaff %>%
  mutate(residual = mass - mean)

# plot
ggplot(data = chaff,
       aes(x = mean, y = residual)) +
  geom_point()

ggplot(data = chaff,
       aes(x = residual)) +
  geom_histogram(bins = 10)
# normality test
shapiro.test(chaff$residual)


# A figure 
ggplot() +
  geom_point(data = chaff, aes(x = subspecies, y = mass),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = chaffsum, 
                aes(x = subspecies, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = chaffsum, 
                aes(x = subspecies, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_x_discrete(name = "Subspecies", labels = c("Canariensis", "Coelebs")) +
  scale_y_continuous(name = "Mass (g)",
                     expand = c(0, 0),
                     limits = c(0, 30)) +
  theme_classic()



arabidopsis <- data.frame(type = rep(c("mutant", "wild"), each = 8),
                     leaves = c(3,5,6,7,3,4,5,8,8,9,6,10,7,8,9,10))

# create a rough plot of the data  
ggplot(data = arabidopsis, 
       aes(x = type, y = leaves)) +
  geom_violin()

# or you could use 
ggplot(data = arabidopsis, 
       aes(x = type, y = leaves)) +
  geom_boxplot()

# note the mutatnts seem to have fewer leaves

# create a summary of the data
arabidopsissum <- arabidopsis %>%
  group_by(type) %>%
  summarise(median = median(leaves),
            n = length(leaves))



wilcox.test(data = arabidopsis, leaves ~ type)
