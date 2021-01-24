## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,	
                      warning = FALSE,
                      fig.width = 4, 
                      fig.height = 4, 
                      fig.retina = 3)


## ----include=FALSE------------------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(RefManageR)


## ---- load-refs, include=FALSE, cache=FALSE-----------------------------------------------------------
BibOptions(check.entries = FALSE,
           bib.style = "authoryear",
           cite.style = "authoryear",
           style = "markdown",
           hyperlink = TRUE,
           dashed = FALSE,
           longnamesfirst = FALSE,
           max.names = 2)
myBib <- ReadBib("../refs/refs.bib", check = FALSE)


## -----------------------------------------------------------------------------------------------------
library(tidyverse)


## ----eval=FALSE---------------------------------------------------------------------------------------
## pigeon <- read_table2("pigeon.txt")


## ----include=FALSE------------------------------------------------------------------------------------
# I have my files organised differently so I use a different 'path' to my file
pigeon <- read_table2("../data/pigeon.txt")


## ----echo=FALSE---------------------------------------------------------------------------------------
#---CODING ANSWER---
str(pigeon)


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
mean(pigeon$distance)


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
# standard deviation
sd(pigeon$distance)
# number of values
length(pigeon$distance) 


## -----------------------------------------------------------------------------------------------------
ggplot(data = pigeon, aes(x = distance)) +
  geom_histogram()



## -----------------------------------------------------------------------------------------------------
ggplot(data = pigeon, aes(x = distance)) +
  geom_histogram(bins = 10)



## ----echo=FALSE---------------------------------------------------------------------------------------
ggplot(data = pigeon, aes(x = distance)) +
  geom_histogram(bins = 10) +
  scale_x_continuous(name = "Interorbital distance (mm)",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Frequency",
                     expand = c(0, 0))



## ----echo=FALSE---------------------------------------------------------------------------------------
# I chose a green
ggplot(data = pigeon, aes(x = distance)) +
  geom_histogram(bins = 10,
                 colour = "black",
                 fill = "#d8e39f") +
  scale_x_continuous(name = "Interorbital distance (mm)",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Frequency",
                     expand = c(0, 0)) 



## -----------------------------------------------------------------------------------------------------
ggplot(data = pigeon, aes(x = distance)) +
  geom_histogram(bins = 10,
                 colour = "black",
                 fill = "#d8e39f") +
  scale_x_continuous(name = "Interorbital distance (mm)",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Frequency",
                     expand = c(0, 0)) +
  theme_classic()



## ----eval=FALSE---------------------------------------------------------------------------------------
## pigeon2 <- read_table2("data/pigeon2.txt")


## ----echo=FALSE---------------------------------------------------------------------------------------
#I have my files organised differently so I use a different 'path' to my file
pigeon2 <- read_table2("../data/pigeon2.txt")


## ----include=FALSE------------------------------------------------------------------------------------
#---CODING ANSWER---
mean(pigeon2$A)
mean(pigeon2$B) 


## ----echo=FALSE, out.width="200px"--------------------------------------------------------------------
knitr::kable(pigeon2[1:3,], 
             format = "html") %>%
  kable_styling(font_size = 16,
                full_width = F, position = "left")


## ----echo=FALSE---------------------------------------------------------------------------------------
pigeon3 <- pivot_longer(data = pigeon2, 
                        cols = everything(), 
                        names_to = "population", 
                        values_to = "distance")


## ----echo=FALSE---------------------------------------------------------------------------------------
knitr::kable(head(pigeon3[c(1:3,41:43),]), 
             format = "html",
             row.names = FALSE) %>%
  kable_styling(font_size = 16,
                full_width = F, position = "left")


## -----------------------------------------------------------------------------------------------------
pigeon3 <- pivot_longer(data = pigeon2, 
                        cols = everything(), 
                        names_to = "population", 
                        values_to = "distance")



## -----------------------------------------------------------------------------------------------------
pigeon3 %>% 
  group_by(population) %>% 
  summarise(mean = mean(distance))


## -----------------------------------------------------------------------------------------------------
pigeon3 %>% 
  group_by(population) %>% 
  summarise(mean = mean(distance),
            n = length(distance))


## ----include=FALSE------------------------------------------------------------------------------------
pigeon3 %>% 
  group_by(population) %>% 
  summarise(mean = mean(distance),
            n = length(distance),
            sd = sd(distance), 
            se = sd/sqrt(n))   


## -----------------------------------------------------------------------------------------------------
ggplot(data = pigeon3, aes(x = distance)) +
  geom_histogram()


## -----------------------------------------------------------------------------------------------------
ggplot(data = pigeon3, aes(x = distance)) +
  geom_histogram()


## -----------------------------------------------------------------------------------------------------
ggplot(data = pigeon3, aes(x = distance)) +
  geom_histogram() +
  facet_grid(.~population)


## ----echo=FALSE---------------------------------------------------------------------------------------
ggplot(data = pigeon3, aes(x = distance)) +
   geom_histogram(bins = 10,
                 colour = "black",
                 fill = "#d8e39f") +
  scale_x_continuous(name = "Interorbital distance (mm)",
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Frequency",
                     expand = c(0, 0),
                     limits = c(0, 15)) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  facet_grid(.~population)


## -----------------------------------------------------------------------------------------------------
ggsave("fig1.png",
       width = 5,
       height = 4,
       units = "in")


## -----------------------------------------------------------------------------------------------------
# generate some numbers
nums <- sample(1:100, size = 10, replace = FALSE)

# transformation either:
# a) nested functions
tnums <- log(sqrt(nums))
# b) intermediates
sqrtnums <- sqrt(nums)
tnums <- log(sqrtnums)


## -----------------------------------------------------------------------------------------------------
tnums <- nums %>% 
  sqrt() %>% 
  log()


## -----------------------------------------------------------------------------------------------------
tnums <- nums %>% 
  sqrt(.) %>% 
  log(.)


## ----refs, echo=FALSE, results="asis"-----------------------------------------------------------------
PrintBibliography(myBib)  

