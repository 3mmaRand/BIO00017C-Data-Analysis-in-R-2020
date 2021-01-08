######################################################################
#                                                                    #
#   This script covers four exercises from the 17C practical         #
#   4. Calculating summary statistics, probabilities and             #
#   confidence intervals. These are:                                 #
#     A. Calculating Probabilities and Quantiles for a single value  #
#     B. Calculating Probabilities and Quantiles for samples         # 
#     C. Confidence intevals for large samples                       #
#     D. Confidence intevals for small samples                       # 
#                                                                    #
######################################################################


######################################################################
#   Set up                                                           #
######################################################################

# set working directory
setwd("M:/web/17C - 2017.yrk/pracs")

# load require packages
# no additional packages needed

######################################################################
#   A. Calculating Probabilities and Quantiles for single values     #
######################################################################

## Using pnorm()

# I.Q. in the U.K. population is normally distributed with a mean of 
# 100 and a standard deviation of 15. 
# Work out the probability of having an I.Q. of 115 or less. 

# variables for the parameter values
m <- 100                # mean
sd <- 15                # standard deviation

# probability of having an I.Q. of 115 or less
# Note the default is lower.tail = TRUE which gives us less than
pnorm(115, m, sd)


# figure to illustrate the probability IQ < 115
# only for illustration
# values for the horizontal axis
IQ <- seq(40, 160, 1)
# to create the shaded part we need to define the cords that bound 
# the shape
# sequence numbers from 40 to 115 in steps of 1
cord.x <- c(40, seq(40, 115, 1), 115) 
# dnorm gives the height of the curve do define the upper part of the 
# shape
cord.y <- c(0, dnorm(seq(40, 115, 1), 100, 15), 0) 
# the plot without the shading, no axes
curve(dnorm(x, 100, 15),
      xlim = c(40, 160),
      bty = "n",axes = F,
      xlab = "IQ", ylab = "") 
#add the gray shaded part
polygon(cord.x, cord.y, col = "gray")
# add the horizontal axis
axis(1, pos = 0)



# probability of having an IQ of 115 OR MORE?
pnorm(115, m, sd, lower.tail = FALSE)
    
# probability of having an IQ between 85 and 115? 
pnorm(115, m, sd) - pnorm(85, m, sd)

# What is 1.96 * the standard deviation
v <- 1.96 * 15
   
# probability of having an IQ between -1.96 standard deviations 
# and +1.96 standard deviations?
pnorm(m + v, m, sd) - pnorm(m - v, m, sd)
# 95% because 95% of values lies between +1.96 s.d. and -1.96 s.d.
# we would get the same for pnorm(+1.96) - pnorm(-1.96)
   
## Using qnorm()
# finds the IQ associated with a particular probability.
# continue with mean = 100 and standard deviation = 15 

# I.Q. value below which 0.2 (20%) of people fall 
qnorm(0.2, m, sd)

# I.Q. below which value are 0.025 (2.5%) of people fall?
qnorm(0.025, m, sd)
 
# 99% of the population fall between 
qnorm(0.005, m, sd)
qnorm(0.995, m, sd)
#between 61.4 and 138.6

# a figure for 99% 
# only for illustration
z <- qnorm(0.995)
cord.x <- c(-z, seq(-z, z, 0.01), z) 
cord.y <- c(0, dnorm(seq(-z, z, 0.01)), 0) 
curve(dnorm(x, 0, 1), xlim = c(-3, 3),
      bty = "n", axes = F, xlab = "IQ", ylab = "") 
polygon(cord.x, cord.y, col = "gray")
arrows(-2.7, 0.1, -2.7, 0.01, length = .15)
text(-2.6, 0.11, "p = 0.005")
arrows(2.7, 0.1, 2.7, 0.01, length = .15)
text(2.6, 0.11, "p = 0.005")
text(0, 0.13, "p = 0.99")
axis(1, pos = 0, labels = FALSE)


######################################################################
#   B. Calculating Probabilities and Quantiles for samples           #
######################################################################
# The only difference in using `pnorm()` and `qnorm()` for samples 
# is in what we give as the sd argument. 
# Since we are now thinking about the distribution of the sample 
# means, we need to use the standard error.

# probability of getting a sample of 5 having a mean I.Q. of 115 
# or less
# First, calculate the standard error
n <- 5                  # sample size
se <- 15 / sqrt(n)      # standard error

# probability of a sample of 5 having a mean I.Q. of 115 or less
pnorm(115, m, se)

# probability of sample of size 10 having a mean of 105 or more?
# reculculate the standard error
n <- 10                 # sample size
se <- 15 / sqrt(n)      # standard error
# probability 
pnorm(105, m, se, lower.tail = FALSE)
# it is about 0.146
 

######################################################################
#   C. Calculating Confidence intervals large samples                #
######################################################################

# The data in ../data/beewing.txt  are left wing widths of 100  honey
# bees (mm). The confidence interval for large samples is given by: 
# the mean plus/minus 1.96 times the standard error
# 1.96 is the quantile for 95% confidence.

# Read in the data and check the structure of the resulting dataframe
bee <- read.table("../data/beewing.txt", header = FALSE)
str(bee)
 
# Rename the column to 'wing'
names(bee) <- "wing"
str(bee)
 
# Calculate and assign to variables: the mean, standard deviation and standard error

m <- mean(bee$wing)     # mean
sd <- sd(bee$wing)      # standard deviation
n <- length(bee$wing)   # sample size
se <-sd / sqrt(n)       # standard error
 
# quantile for the 95% confidence interval 
q <- qnorm(0.975)

# confidence interval calculation
# brackets assign a variable AND output result to screen
(lcl <- m - q * se)
(ucl <- m + q * se)
 
# 99% CI 
q <- qnorm(0.995)
lcl <- m - q * se
ucl <- m + q * se

######################################################################
#   C. Calculating Confidence intervals  small samples)              #
######################################################################

# The confidence interval for small samples is given by: 
# the mean plus/minus t times the standard error
# where t changes depending on the sample size

# The fatty acid Docosahexaenoic acid (DHA) is a major component of
# membrane phospholipids in nerve cells and deficiency leads to many
# behavioural and functional deficits. The cross sectional area of
# neurons in the CA 1 region of the hippocampus of normal rats is 
# 155 mu m^2. A DHA deficient diet was fed to 8 animals and the 
# cross sectional area (csa) of neurons is given in ../data/neuron.txt

#Read in the data and check the structure of the resulting dataframe
neur <- read.table("../data/neuron.txt", header = TRUE)
str(neur)
 
# calculate mean, m
m <- mean(neur$csa)

# calculaate standard error to se
sd <-sd(neur$csa)       # sample standard deviation
n <- length(neur$csa)   # sample size
se <-sd / sqrt(n)       # standard error
 
# degrees of freedom (the number in the sample minus one). 
df <- length(neur$csa) - 1

# The t value for that many df at the 95% level is
t <- qt(0.975, df = df); t
# note we use 0.975 just like we had to for qnorm

# the confidence interval by
round(m + t * se, 2)    # upper limit
round(m - t * se, 2)    # lower limit
