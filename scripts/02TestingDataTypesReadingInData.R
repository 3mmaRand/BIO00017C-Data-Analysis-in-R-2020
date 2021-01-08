## ---- message=FALSE, warning=FALSE, include=FALSE------------------------
# I have my files organised differently so I use a different 'path' to my file
pigeon <- read.table("../data/pigeon.txt")


## ---- message=FALSE, warning=FALSE, include=FALSE------------------------
#---CODING ANSWER---
str(pigeon)


## ---- message=FALSE, warning=FALSE---------------------------------------
names(pigeon)[1] <- "interorbital"

#and look again with str
str(pigeon)


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
pigeon$interorbital[14]
#or
pigeon[14, 1] 


## ------------------------------------------------------------------------
hist(pigeon$interorbital)


## ----echo=FALSE----------------------------------------------------------
#---CODING ANSWER---
names(pigeon)[1] <- "interorbital"
hist(pigeon$interorbital,
     xlim = c(8, 14),
     main = NULL,
     xlab = "Width (mm)",
     ylab = "Number of pigeons",
     col = "grey")


## ---- message=FALSE, warning=FALSE---------------------------------------
# library statement is only needed once per session
library(tidyverse)

ggplot(data = pigeon, aes(x = interorbital)) +
  geom_histogram()



## ---- message=FALSE, warning=FALSE---------------------------------------
ggplot(data = pigeon, aes(x = interorbital)) +
  geom_histogram(bins = 10, colour = "black")



## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
mean(pigeon$interorbital)
sd(pigeon$interorbital)
length(pigeon$interorbital) 


## ---- message=FALSE, warning=FALSE, include=FALSE------------------------
#I have my files organised differently so I use a different 'path' to my file
data2 <- read.table("../data/pigeon2.txt", header = T)


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
str(data2) 


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
mean(data2$A)
mean(data2$B) 


## ------------------------------------------------------------------------
# library statement is only needed once per session
data3 <- gather(data = data2, key = population, value = distance)
str(data3)


## ------------------------------------------------------------------------
mean(data3$distance[data3$population == "A"])


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
mean(data3$distance[data3$population == "B"])


## ------------------------------------------------------------------------
data3 %>% 
  group_by(population) %>% 
  summarise(mean(distance))


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
# variance
data3 %>% 
  group_by(population) %>% 
  summarise(var(distance))

# the number of cases (rows) is given by the length of the vector
data3 %>% 
  group_by(population) %>% 
  summarise(length(distance))


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE,fig.width=4,fig.height=4----
#---CODING ANSWER---
# histograms for the word document
# Create a histogram for each population, trying to match those illustrated.
hist(data3$distance[data3$population == "A"],
     xlim = c(8, 14),
     ylim = c(0, 15),
     main = NULL,
     xlab = "Width (mm)",
     ylab = "Number of pigeons",
     col = "pink")
hist(data3$distance[data3$population == "B"],
     xlim = c(8, 14),
     ylim = c(0, 15),
     main = NULL,
     xlab = "Width (mm)",
     ylab = "Number of pigeons",
     col = "lightblue")


## ------------------------------------------------------------------------
# generate some numbers
nums <- sample(1:100, size = 10, replace = FALSE)

# transformation either:
# a) nested functions
tnums <- log(sqrt(nums))
# b) intermediates
sqrtnums <- sqrt(nums)
tnums <- log(sqrtnums)


## ------------------------------------------------------------------------
tnums <- nums %>% 
  sqrt() %>% 
  log()


## ------------------------------------------------------------------------
tnums <- nums %>% 
  sqrt(.) %>% 
  log(.)

