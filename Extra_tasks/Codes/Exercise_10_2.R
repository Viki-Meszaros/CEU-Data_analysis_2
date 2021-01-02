#########################
##   Data analysis 2   ##
##                     ##
##    Exercise 10.2    ##
#########################

# CLEAR MEMORY
rm(list=ls())

# Import libraries 
library(tidyverse)
library(stargazer)
library(haven)
library(scales)
library(lspline)


#### GET DATA AND SAMPLE SELECTION
hotels <- read_csv("https://raw.githubusercontent.com/Viki-Meszaros/CEU-Data_analysis_2/main/Extra_tasks/Data/Hotels_clean.csv")
hotels$X1 <- NULL

# Apply filters:  Milan, hotels, 3-4 stars, in 2017 November weekday
hotels <- hotels %>% 
  filter(city_actual=="Milan") %>%
  filter(accommodation_type=="Hotel") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
  filter(year == 2017 & month == 11 & weekend == 0) %>% 
  filter(price <= 400)


#####################################################################
# TAKE LOG 
hotels$lnprice <- log(hotels$price)

hotels$distance2<-hotels$distance
hotels$distance2[hotels$distance2<0.05] <- 0.05

hotels$lndistance<-log(hotels$distance2)


# Stars: binary indicators
hotels$star35 = ifelse(hotels$stars==3.5, 1, 0)
hotels$star4 = ifelse(hotels$stars==4, 1, 0)


summary(hotels$price)
summary(hotels$distance)
summary(hotels$lnprice)


#####################################################################
# Regressions
#####################################################################
# Basic
reg0 <- lm(lnprice ~ ratingta, data=hotels)

reg1 <- lm(lnprice ~ distance, data=hotels)

reg2 <- lm(lnprice ~ distance + ratingta, data=hotels)



# Predicted values 
reg3 <- lm(lnprice ~ lspline(distance, c(1,4)) + lspline(ratingta, 3.5) + star35 + star4, data=hotels)
summary(reg3, vcov=sandwich)

hotels$lnprice_hat <- predict(reg3)
hotels$lnprice_resid <- hotels$lnprice - hotels$lnprice_hat
hotels$bestdeals <- ifelse(hotels$lnprice_resid %in% tail(sort(hotels$lnprice_resid, decreasing=TRUE),5),TRUE,FALSE)

# Compare R-sqared with distance only
reg4 <- lm(lnprice ~ lspline(distance, c(1,4)), data=hotels)
summary(reg4)

stargazer_r(list(reg1, reg2, reg3, reg4), se = 'robust', digits=3, out=paste(output,"T10_hotels2_R.tex",sep=""))


# List of 5 best deals
hotels %>%
  select(hotel_id, price, lnprice_resid, distance, stars, rating) %>%
  arrange(lnprice_resid) %>%
  .[1:5,] %>%
  as.data.frame() %>% 
  stargazer(summary= FALSE, digits = 1, out = paste(output,"T10_hotels_best_deals.tex",sep=""))

# y - yhat graph
y_yhat_hotels<- ggplot(data = hotels, aes(x = lnprice_hat, y = lnprice)) +
  geom_point(aes(color=bestdeals,shape=bestdeals), size = 1.2, fill="deepskyblue4", alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  #geom_smooth_da(method="lm") +
  geom_segment(aes(x = 3.8, y = 3.8, xend = 6, yend =6), size=0.8, color="deeppink4", linetype=2) +
  labs(x = "ln(predicted price, US dollars) ",y = "ln(price, US dollars)")+
  coord_cartesian(xlim = c(3.8, 6), ylim = c(3.8, 6)) +
  scale_shape_manual(name='',values=c(16,21)) +
  geom_segment(aes(x = 4.8, y = 3.9, xend = 4.68, yend = 4.05), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 4.93, y = 3.9, label = "Best deal", size=2.5)+
  theme_classic() +
  theme(axis.text.x=element_text(size=9)) +
  theme(axis.text.y=element_text(size=9)) +
  theme(axis.title.x=element_text(size=9)) +
  theme(axis.title.y=element_text(size=9)) 
y_yhat_hotels



# residual - yhat graph (not in book)
ggplot(data = hotels, aes(x = lnprice_hat, y = lnprice_resid)) +
  geom_point(color = color[1], size = 1,  shape = 16, alpha = 0.6, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="lm", colour=color[4], se=F, size=1) +
  labs(x = "ln(Predicted hotel price, US dollars)",y = "Residuals")+
  coord_cartesian(xlim = c(4, 5.5)) +
  theme_bg() +
  background_grid(major = "xy", minor="xy", size.major = 0.2)    




