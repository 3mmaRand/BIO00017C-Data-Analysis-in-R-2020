# The null hypotheses here are:
#   mean of F.flappa (averaged over the regions) = mean of F.concocti (averaged over the regions),
#   mean of north (averaged over the spp) = mean of south (averaged over the spp) and 
#   the effects of the two factors are independent.
#   
  

library(tidyverse)
butter  <-  read_table2("data/butterf.txt")
glimpse(butter)


# create a rough plot of the data  
ggplot(data = butter, 
       aes(x = spp, y = winglen, fill = region)) +
  geom_violin()


# note the F.concocti have bigger winglen in the north; other group means approx the same. suggests interaction.

# create a summary of the data
buttersum <- butter %>%
  group_by(region, spp) %>%
  summarise(mean = mean(winglen),
            std = sd(winglen),
            n = length(winglen),
            se = std/sqrt(n))

# The data seem to be continuous so it is likely that a t-test will be fine
t.test(data = butter, 
       winglen ~ spp, 
       var.equal = TRUE)
# So there is a significant difference but you need to make sure you know the direction!

# let's check the assumptions
# add the group means to the data
butter <- merge(butter, buttersum[,1:2], by = "spp")
# add the residuals
butter <- butter %>%
  mutate(residual = winglen - mean)

# plot
ggplot(data = butter,
       aes(x = mean, y = residual)) +
  geom_point()

ggplot(data = butter,
       aes(x = residual)) +
  geom_histogram(bins = 10)
# normality test
shapiro.test(butter$residual)


# A figure 
ggplot() +
  geom_point(data = butter, aes(x = spp, y = winglen),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = buttersum, 
                aes(x = spp, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = buttersum, 
                aes(x = spp, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_x_discrete(name = "spp", labels = c("Canariensis", "Coelebs")) +
  scale_y_continuous(name = "winglen (g)",
                     expand = c(0, 0),
                     limits = c(0, 30)) +
  theme_classic()



culture <- read_csv("data/culture.csv")

Sys.which("python")
