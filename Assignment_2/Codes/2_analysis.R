############################
##   Assignment for DA2   ##
##     and for Coding     ##
##                        ##
##    Titanic data set    ##
##                        ##
##      Analysis of       ##
##       the data         ##
############################

rm(list = ls())

library(tidyverse)
library(ggcorrplot)
library(ggthemes)
library(stargazer)

require(scales)
library(lspline)
library(estimatr)
library(texreg)

library(xtable)
library(moments)
library(car)


df <- read.csv("https://raw.githubusercontent.com/Viki-Meszaros/CEU-Data_analysis_2/main/Assignment_2/Data/Clean/Titanic_clean.csv")
df$X <- NULL

corr <- round(cor( 
  keep(df, is.numeric)), 3)

  ggcorrplot(corr)

  
# SIMPLE LPM MODELS -------------------------------------------------------
## AGE
lpm1 <- lm(Survived ~ Female, data=df)
summary(lpm1, vcov=sandwich)

# visualize this regression
df$pred1 <- predict(lpm1)

table(df$pred1, df$Survived)


#create weights
tdf <-df %>%
  group_by(Female, Survived) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))


g1<-ggplot(data = df, label=survived) +
  geom_point(aes(x = Female, y = pred1), size = 1,  shape = 16) +
  geom_line(aes(x = Female, y = pred1),  size=0.7) +
  geom_point(aes(x = Female, y = stayshealthy, size=weight_2), shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Current smoker",y = "Staying healthy / Predicted probability of ")+
  coord_cartesian(xlim = c(0, 1), ylim=c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,1))+
  theme_calc() 
g1    


## FARE 

## SEX

## TICKET CLASS
