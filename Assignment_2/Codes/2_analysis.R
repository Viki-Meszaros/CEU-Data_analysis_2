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
## SEX
lpm1 <- lm(Survived ~ Female, data=df)
summary(lpm1, vcov=sandwich)

# visualize this regression
df$pred1 <- predict(lpm1)

table(df$pred1, df$Survived)


#create weights
df <-df %>%
  group_by(Female, Survived) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))


ggplot(data = df) +
  geom_line(aes(x = Female, y = pred1),  size = 0.8, color = "cyan3", alpha = 0.6 ) +
  geom_point(aes(x = Female, y = pred1), size = 3, color = "cyan4",  shape = 16) +
  geom_point(aes(x = Female, y = Survived, size=weight_2), color = "purple3", shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Female",y = "Survived / Predicted probability of ")+
  coord_cartesian(xlim = c(0, 1), ylim=c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(limits = c(0,1), breaks = c(0,1), labels = c("0" = "Male", "1" = "Female"))+
  theme_calc() 


## PCLASS
lpm2 <- lm(Survived ~ Pclass_1 + Pclass_2, data = df)
summary(lpm2, vcoc=sandwich)

df$pred2 <- predict(lpm2)

df <-df %>%
  group_by(Pclass, Survived) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))


ggplot(data = df) +
  geom_line(aes(x = Pclass, y = pred2),  size = 0.8, color = "cyan3", alpha = 0.6 ) +
  geom_point(aes(x = Pclass, y = pred2), size = 3, color = "cyan4",  shape = 16) +
  geom_point(aes(x = Pclass, y = Survived, size=weight_2), color = "purple3", shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Socio_economic class",y = "Survived / Predicted probability of ")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  theme_calc() 

## EMBARKED
lpm3 <- lm(Survived ~ C + Q, data = df)
summary(lpm3, vcoc=sandwich)

df$pred3 <- predict(lpm3)



## AGE
lpm2 <- lm(Survived ~ Age, data = df)
summary(lpm2, vcoc=sandwich)

ggplot(data = df) +
  geom_point(aes(x = ))

## FARE

## TICKET CLASS
























