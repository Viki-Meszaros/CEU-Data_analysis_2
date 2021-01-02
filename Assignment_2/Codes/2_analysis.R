############################
##   Assignment for DA2   ##
##     and for Coding     ##
##                        ##
##    Titanic data set    ##
##                        ##
##      Analysis of       ##
##       the data         ##
############################

library(stargazer)

require(scales)
library(lspline)
library(estimatr)
library(texreg)

library(xtable)
library(moments)
library(car)

# SIMPLE LPM MODELS -------------------------------------------------------
## AGE
lpm1 <- lm(stayshealthy ~ smoking, data=share)
summary(lpm1, vcov=sandwich)

# visualize this regression
share$pred1 <- predict(lpm1)

table(share$pred1, share$smoking)
table(share$stayshealthy, share$smoking)

#create weights
share<-share %>%
  group_by(smoking, stayshealthy) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))


g1<-ggplot(data = share, label=smoking) +
  geom_point(aes(x = smoking, y = pred1), size = 1,  shape = 16) +
  geom_line(aes(x = smoking, y = pred1),  size=0.7) +
  geom_point(aes(x = smoking, y = stayshealthy, size=weight_2), shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Current smoker",y = "Staying healthy / Predicted probability of ")+
  coord_cartesian(xlim = c(0, 1), ylim=c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,1))+
  theme_calc() 
g1    


## FARE 

## SEX

## TICKET CLASS
