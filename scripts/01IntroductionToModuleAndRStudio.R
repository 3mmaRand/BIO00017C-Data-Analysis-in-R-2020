## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
n <- 0:5


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
str(n)
# it's a vector containing integers


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
freq <- c(4, 13, 14, 15, 13, 5)
freq


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
sum(freq)
# we expect that answer to be 64


## ----fig.width=4,fig.height=4--------------------------------------------
barplot(freq)


## ----warning=FALSE,message=FALSE-----------------------------------------
# plot the names below each bar
barplot(freq, names.arg = n)


## ----echo=FALSE,fig.width=4,fig.height=4,results="hide",warning=FALSE,message=FALSE----
#---CODING ANSWER---
barplot(freq, names.arg = n, 
        xlab = "Number of males", 
        ylab = "Number of nests")


## ----echo=FALSE,results="hide",fig.width=4,fig.height=4,warning=FALSE,message=FALSE----
#---CODING ANSWER---
barplot(freq, names.arg = n, 
        xlab = "Number of males",
        ylab = "Number of nests",
        col = "red")


## ----echo=FALSE,results="hide",fig.width=4,fig.height=4,warning=FALSE,message=FALSE----
#---CODING ANSWER---
barplot(freq, names.arg = n, 
        xlab ="Number of males",
        ylab ="Number of nests",
        col = c("red", "blue", "yellow"))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
n_of_nests <- 64
clutch_size <- 5
p_of_male <- 0.5


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
nest_data <- data.frame(n_males = rbinom(n_of_nests, clutch_size, p_of_male))


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
str(nest_data)
# it's a dataframe containing one column, n_males, of integers


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
library(ggplot2)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
ggplot(data = nest_data, aes(x = n_males)) +
  geom_bar()


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
ggplot(data = nest_data, aes(x = n_males)) +
  geom_bar() +
  xlab("Number of males")

