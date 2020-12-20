#########################
##   Data analysis 2   ##
##                     ##
##    Exercise 7.3     ##
#########################


# CLEAR MEMORY
rm(list=ls())

# packages
library(tidyverse)
library(grid)
library(viridis)


#### GET DATA AND SAMPLE SELECTION
hotels <- read_csv("https://raw.githubusercontent.com/Viki-Meszaros/CEU-Data_analysis_2/main/Extra_tasks/Data/Hotels_clean.csv")
hotels$X1 <- NULL

# Apply filters:  Milan, hotels, 3-4 stars, in 2017 November weekday
hotels <- hotels %>% 
  filter(city_actual=="Milan") %>%
  filter(accommodation_type=="Hotel") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
  filter(year == 2017 & month == 11 & weekend == 0)  
 

# save a copy of the work file
write_csv(hotels, "C:/Users/admin/Documents/CEU/Fall_semester/Data_analysis_2/Github/CEU-Data_analysis_2/Extra_tasks/Data/Milan_hotels.csv")


# SUMMARY STATISTICS ON PRICE AND DISTANCE
descr_price <- hotels %>% dplyr::select(price) %>% 
  dplyr::summarize(mean=mean(price),
                   sd=sd(price),
                   min=min(price),
                   max=max(price),
                   p50=quantile(price,.50),
                   p95=quantile(price,.95),
                   n=length(price)
  )

descr_dist <- hotels %>%
  dplyr::select(distance) %>% 
  dplyr::summarize(mean=mean(distance),
                   sd=sd(distance),
                   min=min(distance),
                   max=max(distance),
                   p50=quantile(distance,.50),
                   p95=quantile(distance,.95),
                   n=length(distance)
  )
print(descr_price)
print(descr_dist)

# Remove objects
rm(descr_dist, descr_price)


# Distribution of distance
hotels %>%
  select(distance) %>% 
  ggplot(aes(distance)) +
  geom_histogram(fill="deepskyblue4") +
  theme_classic()+
  labs(x = "Distance", y = "Number of hotels")

 
hotels %>%
  select(price) %>% 
  ggplot(aes(price)) +
  geom_histogram(fill="deeppink4") +
  theme_classic()+
  labs(x = "Price", y = "Number of hotels")

# For distance the distribution looks quite ok, but for price there is a strong outlier above 800 so i decided to exclude that as it seems an influential observation
# my aim is to find a good deal, it is clearly not one, and is would put an unnecessary influence on my results.

hotels <- hotels %>% 
  filter(price <= 400)

# Initial scatterplot for distance and price
hotels %>%
  ggplot(aes(x=distance, y=price)) +
  geom_point(color="deepskyblue4") +
  theme_classic()+
  labs(x = "Distance", y = "Price")



### 4 DISTANCE CATEGORIES
hotels <-hotels %>% mutate(dist4=0.5+ 1*as.numeric(hotels$distance>=1) + 1*as.numeric(hotels$distance>=2) + 2.5*as.numeric(hotels$distance>=3))
dist4 <- hotels %>% group_by(dist4) %>% dplyr::summarize(Eprice_cat4=mean(price))
hotels<-left_join(hotels,dist4)
hotels %>% group_by(dist4) %>% dplyr::summarize(mean_dist=mean(distance), 
                                                sd_dist=sd(distance),
                                                min_dist=min(distance),
                                                max_dist=max(distance),
                                                mean_dist=mean(price), 
                                                sd_dist=sd(price),
                                                min_dist=min(price),
                                                max_dist=max(price),
                                                N=n())

### Bin scatter with 4 bins
ggplot(data = hotels) +
  geom_point(aes(x = dist4, y = Eprice_cat4), 
             size = 4 , color = "deepskyblue4", na.rm=T) +
  geom_text(aes(x = dist4, y = Eprice_cat4, label = round(Eprice_cat4)), hjust = -0.6, vjust = 0, color = "black", size = 3) +
  expand_limits(x = 0.01, y = 0.01) +
  coord_cartesian(xlim = c(0,7), ylim = c(0, 400)) +
  scale_y_continuous(expand=c(0.01,0.01),limits = c(0, 400), breaks = seq(0, 400, by=50)) +
  scale_x_continuous(expand=c(0.01,0.01), limits= c(0,7), breaks = c(0, 1, 2, 3,4,5, 6,7)) +
  labs(x = "Distance to city center (miles)", y = "Price (US dollars)") +
  theme_classic()


### Non-parametric regression with 4 bins

p1 <- ggplot(data = hotels) +
  geom_point(aes(x = distance, y = price), color = "deepskyblue4", size = 2,  shape = 16, alpha = 0.5, show.legend=F, na.rm = TRUE) + 
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 7), breaks=seq(0, 7, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  labs(x = "Distance to city center (miles)",y = "Price (US dollars)")+
  theme_classic() 

# Scatterplot with step function (we use 1km bits for simplicity using 4 bits for 3-7km)
hotels <-hotels %>% mutate(dist4_s = 1*as.numeric(hotels$distance>=1) + 1*as.numeric(hotels$distance>=2) +   1*as.numeric(hotels$distance>=3) +1*as.numeric(hotels$distance>=4) +1*as.numeric(hotels$distance>=5) + 1*as.numeric(hotels$distance>=6))  
hotels$xend <- c(hotels$dist4_s+1)
hotels$yend <- c(hotels$Eprice_cat4)

# Non-parametric regression graph with 4 bins
p1+
  geom_segment(data=hotels, aes(x = dist4_s, y=yend, xend=xend, yend=yend), color="deeppink", size=0.9, na.rm=TRUE) 



### LOWESS NONPARAMETRIC REGRESSION
p1  +
  geom_smooth(aes(x = distance, y = price), method='loess', color = "deeppink4")



### LINEAR REGRESSIONS
regression <- lm(price ~ distance, data=hotels)
summary(regression)


### SCATTERPLOT + REGRESSION LINE
p1  +
  geom_smooth(aes(x = distance, y = price), method = "lm", color = "deeppink4")


# PREDICTED VALUES & RESIDUALS OF LINEAR REGRESSION 
regression <- lm(price ~ distance, data=hotels)
hotels$predprice <- predict(regression)
hotels$e <- resid(regression)



# histogram of residuals
ggplot(data = hotels, aes (x = e)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 20, fill = "deepskyblue4", 
                 size = 0.2, alpha = 0.8,  show.legend=F, na.rm=TRUE, boundary=1)+
  labs(x = "Residuals", y = "Percent") +
  scale_x_continuous(limits = c(-100, 300), breaks = seq(-100, 300, by = 100)) +
  scale_y_continuous(expand = c(0.0,0.0), limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.05), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() 


# hotels with most negative residuals

reg1 <- lm(price ~ distance, data=hotels)
summary(reg1)

hotels$reg1_resid <- reg1$residuals
hotels$reg1_res <- ifelse(reg1$residuals >=0, "overpriced", "underpriced")
hotels$reg1_res <- ifelse(hotels$reg1_resid %in% tail(sort(reg1$residuals, decreasing=TRUE),5), "bottom5",
                          ifelse(hotels$reg1_resid %in% head(sort(reg1$residuals, decreasing=TRUE), 5), "top5", hotels$reg1_res))


# (stored in a new data frame; check data frame)
bestdeals <- hotels%>%
  arrange(e)%>%
  head(5)
bestdeals




