## ----setup, include=FALSE-------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,	
                      warning = FALSE,
                      fig.width=4, 
                      fig.height=4, 
                      fig.retina=3)


## ----include=FALSE--------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(RefManageR)


## ---- load-refs, include=FALSE, cache=FALSE-------------------------------
BibOptions(check.entries = FALSE,
           bib.style = "authoryear",
           cite.style = "authoryear",
           style = "markdown",
           hyperlink = TRUE,
           dashed = FALSE,
           longnamesfirst = FALSE,
           max.names = 2)
myBib <- ReadBib("../refs/refs.bib", check = FALSE)


## ---- echo=FALSE----------------------------------------------------------
n <- c(0, 1, 2, 3, 4, 5)
freq <- c(4, 13, 14, 15, 13, 5)
df <- data.frame(n, freq)
knitr::kable(df, 
             format = "html",
             col.names = c("No. males", "No. nests"),
             row.names = FALSE) %>% 
  kable_styling()


## ---- echo=FALSE----------------------------------------------------------
nest_data <- data.frame(n = factor(n), freq)
ggplot(data = nest_data, aes(x = n, y = freq)) +
  geom_col(colour = "black", fill = "white", width = 1) +
  scale_x_discrete(name = "Number of males",
                   expand = c(0, 0)) +
  scale_y_continuous(name = "Number of nests",
                     expand = c(0, 0)) +
  theme_classic()


## -------------------------------------------------------------------------
# the number of males in a clutch of five
n <- 0:5


## -------------------------------------------------------------------------
str(n)


## ----include=FALSE--------------------------------------------------------
#---CODING ANSWER---
## the number of nests with 0 to 5 males
freq <- c(4, 13, 14, 15, 13, 5)
str(freq)


## -------------------------------------------------------------------------
# the total number of nests
sum(freq)


## ---- echo=FALSE----------------------------------------------------------
df$product <- df$n * df$freq
totalm <- sum(df$product)
totaln <- sum(df$freq)
dfchar <- df %>%
  mutate_all(as.character)
dfchar <- rbind(dfchar, c("Total", totaln, totalm))
dfchar %>% kable(format = "html",
                 col.names = c("No. males", "No. nests", "No. males *No. nests"),
                 row.names = FALSE) %>% 
  kable_styling(font_size = 16) %>%
  row_spec(6, hline_after = TRUE) %>%
  row_spec(7, bold = TRUE, background = "#C0C2C9")


## -------------------------------------------------------------------------
total_nests <- sum(freq)


## -------------------------------------------------------------------------
total_males <- sum(n * freq)


## -------------------------------------------------------------------------
total_males/total_nests


## -------------------------------------------------------------------------
library(tidyverse)


## -------------------------------------------------------------------------
nest_data <- data.frame(n = factor(n), freq)


## ----include=FALSE--------------------------------------------------------
str(nest_data)


## ---- fig.height=3.5------------------------------------------------------
ggplot(data = nest_data, aes(x = n, y = freq)) +
  geom_col()


## ----eval=FALSE-----------------------------------------------------------
## ?geom_col


## ----plot-last1, fig.show = 'hide'----------------------------------------
ggplot(data = nest_data, aes(x = n, y = freq)) +
  geom_col(fill = "lightblue")


## -------------------------------------------------------------------------
ggplot(data = nest_data, aes(x = n, y = freq, fill = n)) +
  geom_col()


## ----echo=FALSE-----------------------------------------------------------
ggplot(data = nest_data, aes(x = n, y = freq)) +
  geom_col(fill = "lightblue", width = 1)


## ----echo=FALSE-----------------------------------------------------------
ggplot(data = nest_data,
       aes(x = n, y = freq)) +
  geom_col(fill = "lightblue", 
           width = 1, 
           colour = "black")


## -------------------------------------------------------------------------
ggplot(data = nest_data, aes(x = n, y = freq)) +
  geom_col(fill = "lightblue", 
           width = 1, 
           colour = "black") +
  scale_x_discrete(expand = c(0, 0)) + #<<
  scale_y_continuous(expand = c(0, 0)) #<<


## ----echo=FALSE-----------------------------------------------------------
ggplot(data = nest_data, aes(x = n, y = freq)) +
  geom_col(fill = "lightblue", 
           width = 1, 
           colour = "black") +
  scale_x_discrete(expand = c(0, 0),
                   name = "Number of Males") + #<<
  scale_y_continuous(expand = c(0, 0),
                     name = "Number of Nests") 




## ----refs, echo=FALSE, results="asis"-------------------------------------
PrintBibliography(myBib)  

