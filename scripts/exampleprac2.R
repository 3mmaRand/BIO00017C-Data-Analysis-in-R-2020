######################################################################
#                                                                    #
#   This script covers two exercises from the 17C practical          #
#   2. Testing, Data types and Reading in data. These are:           #
#     A. Importing data in the working directory and exploring       #
#        data with one group                                         #
#     B. Importing data not in the working directory and exploring   # 
#        data with more than one group                               #
#                                                                    #
#   Note: keep your lines relatively short                           # 
#         have spaces around all your operators                      #
#                      ( <- , +, =, %>%, etc )                       #
######################################################################

# packages needed
# it is good practice to load all the packages you are going to need
# at the start.
library(tidyverse)

######################################################################
#   A. Importing data from the working directory                     #
#      and exploring data with one group                             #
######################################################################

# The data in pigeon.txt are 40 measurements of interorbital width
# (in mm) in a sample of domestic pigeons measured to the nearest 
# 0.1mm

# Read in the data 
pigeon <- read.table("pigeon.txt")

# examine the structure
str(pigeon)
# it's a data.frame of 40 obs. of  1 variable: $interorbital: num

# Change the variable name
names(pigeon)[1] <- "interorbital"


# Exploring single group data
# 14th value in the column is extracted with 
pigeon$interorbital[14]
# or
pigeon[14,1] 

# Plot a histogram of the data. 
hist(pigeon$interorbital,
     xlim = c(8, 14),
     main = NULL,
     xlab = "Width (mm)",
     ylab = "Number of pigeons",
     col = "grey")

# mean, standard deviation and the number of cases
mean(pigeon$interorbital)
sd(pigeon$interorbital)
length(pigeon$interorbital) 


######################################################################
#   A. Importing data not in the wd & exploring data in >1 group     #
######################################################################

# Practice with 'paths' and 'relative paths'.
# The data in data/pigeon2.txt are similar data of interorbital  
# distances in two columns rather than just one.  They are from two  
# different populations, A and B.

 
# Read the data 
data2 <- read.table("data/pigeon2.txt", header = TRUE)

# look at the structure 
str(data2) 
# first value in each column as the variable name.

# Mean for each population
mean(data2$A)
mean(data2$B) 


# Put data in tidy format: measurements are all in one 
# column and a second column gives the group. 
# See Wickham, 2014 
data3 <- gather(data = data2, 
                key = population, 
                value = distance)
str(data3)

# Note, an alternative way to do this is with the pipe
data3 <- data2 %>% 
        gather(key = population, 
               value = distance)

# Examine the structure 
str(data3)

# reanme columns
names(data3) <- c("distance", "population")

# Exploring this multi-group data
# finding the mean by selection
mean(data3$distance[data3$population == "A"])
mean(data3$distance[data3$population == "B"])

# Using group_by and summarise to find the means - easier for many groups
data3 %>% 
        group_by(population) %>% 
        summarise(mean(distance))
 
# Using group_by and summarise to find the number of cases and variance of each group 
data3 %>% 
        group_by(population) %>% 
        summarise(var(distance))

data3 %>% 
        group_by(population) %>% 
        summarise(length(distance))

# Note we could do all three in one command
data3 %>% 
        group_by(population) %>% 
        summarise(mean(distance),
                  var(distance),
                  length(distance))
