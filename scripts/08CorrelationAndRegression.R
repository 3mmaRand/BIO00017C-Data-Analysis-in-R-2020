## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
height <- read.table("../data/height.txt",header=T)
str(height)


## ---- message=FALSE, warning=FALSE, include=FALSE------------------------
library(ggplot2)


## ----fig.width=4,fig.height=4--------------------------------------------
ggplot(height, aes(x = sister, y = brother) ) +
  geom_point()
  


## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
height2 <- stack(height[1:2])
str(height2)


## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
ggplot(data = height2, aes(values))+
  geom_histogram(bins = 5 ,colour = "black", fill = "white") +
  facet_grid(. ~ ind)


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE---------------
#---CODING ANSWER---
# i used the stacked data
tapply(height2$values, height2$ind, shapiro.test)



## ------------------------------------------------------------------------
cor.test(height$sister, height$brother, method="pearson")


## ----fig.width=4,fig.height=4--------------------------------------------
ggplot(height, aes(x = sister, y = brother)) +
  geom_point() +
  xlim(120, 180) + ylim(120, 190) +
  xlab("Heights of sister (cm)") +
  ylab("Heights of brother (cm)") +
  theme_bw()


## ------------------------------------------------------------------------
height3 <- rbind(height, height)
str(height3)


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE,eval=FALSE----
## #---CODING ANSWER---
##   cor.test(height3$sister,height3$brother,method="pearson")


## ---- message=FALSE, warning=FALSE, include=FALSE------------------------
#---CODING ANSWER---
height$sister_inch <- height$sister / 2.54



## ---- message=FALSE, warning=FALSE, include=FALSE------------------------
#---CODING ANSWER---
cor.test(height$sister_inch,height$brother,method="pearson")


## ------------------------------------------------------------------------
cor.test(height$sister_inch,height$brother,method="spearman")


## ----echo=FALSE, results="hide", warning=FALSE, message=FALSE------------
#---CODING ANSWER---
plant <- read.table("../data/plant.txt",header =T )
str(plant)


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE,eval=FALSE----
## #---CODING ANSWER---
## ggplot(plant, aes(x=day, y=mass) ) +
##   geom_point()


## ------------------------------------------------------------------------
mod <- lm(data = plant, mass ~ day)
summary(mod)


## ------------------------------------------------------------------------
plot(mod, which=1)
plot(mod, which=2)
shapiro.test(mod$residuals)


## ----fig.width=4,fig.height=4--------------------------------------------
ggplot(plant, aes(x=day, y=mass)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE,colour="black")+
  ylab("Mass (g)") +
  ylim(0,120) +
  xlim(0,65) +
  xlab("Day") +
  theme_bw()
  


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE,eval=FALSE----
## #---CODING AND THINKING ANSWER---
## #this example is designed to emphasise the importance of plotting your data first
## sprint <- read.table("../data/sprint.txt", header=T)
## ggplot(sprint, aes(x = anxiety, y = time) ) +
##   geom_point()
## 
## #A scatterplot of the data clearly reveals that these data are not linear. There is a good relationship between the two variables but since it is not linear, a correlation (either parametric or non-parametric) is inappropriate. A Pearson's correlation comes out at close to zero and NS (since the scatter around a linear correlation would be great).
## 


## ----echo=FALSE,results="hide",warning=FALSE,message=FALSE,eval=FALSE----
## #---CODING AND THINKING ANSWER---
## #read the data in and check the structure
## stag <- read.table("../data/stag.txt",header=T)
## str(stag)
## #quick plot
## ggplot(stag, aes(x=jh, y=mand) ) +
##   geom_point()
## #looks linear-ish on the scatter
## #the sceanrio seems to suit regresion - JH has been choosen or set, mandible has been measured
## #we will check the other assumptions after we have run the lm
## mod <- lm(data = stag, mand~jh)
## summary(mod)
## # mand = 0.032*jh + 0.419
## # the slope of the line is significantly different from zero / the jh explains a significant amount of the variation in mand (ANOVA: F = 16.63; d.f. = 1,14; p = 0.00113).
## # the intercept is 0.419 and differs significantly from zero
## 
## #it is good idea to examine the residuals to check the assumptions of the anova
## plot(mod,which=1) # the variance looks equal
## plot(mod,which=2) # the residuals look normal
## shapiro.test(mod$residuals) # the also test not sig diff from normal
## 
## # The following would be a suitable plot
##    ggplot(stag, aes(x=jh, y=mand) ) +
##   geom_point() +
##   geom_smooth(method=lm,se=FALSE,colour="black")+
##   ylab("Mandible size (mm)") +
##   ylim(0,2) +
##   xlim(0,32) +
##   xlab("Juvenile hormone (arbitrary units)") +
##   theme_bw()
## #Figure 2. The effect of juvenile hormone injections on the mandible size of stag beetles.

