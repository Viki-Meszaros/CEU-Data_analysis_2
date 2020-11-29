############################
##   Assignment for DA2   ##
##     and for Coding     ##
##                        ##
##         NO. 3          ##
##                        ##
##      Analysis of       ##
##       the data         ##
############################


# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)
library(xtable)
library(moments)
library(kableExtra)
library(car)


# Call the data from github
my_path <- "~/Documents/CEU/Fall_semester/Data_analysis_2/Assignment/Data/"
df <- read_csv(paste0(my_path, 'Clean/covid_pop_10_26_2020_clean.csv'))


####
# 
# Quick check on all HISTOGRAMS
df %>%
  select(-c(country, recovered, active)) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(fill = "deepskyblue4")+
  theme_clean() + 
  scale_fill_gdocs()

# CASES
df %>% 
  select(c( country, cases)) %>% 
  arrange(-cases) %>% 
  head(5)
# The extreme values for cases are not errors, but the number of cases in the United States (8.7M),
# India (7.9M) and Brazil(5.4M). I won't exclude these, as they are important observations in my analysis.  

df %>% 
  filter(cases < 1000)
# There are 31 observations with less than 1000 Covid cases registered.
# There are no observations with NA or zero values, as we excluded them during the data cleaning phase

# DEATH
df %>% 
  select(c( country, death, cases)) %>% 
  arrange(-death) %>% 
  head(10)
# The four extreme values visible on the graph are United States(2.3M), Brazil(1.6M), India(1.2M) and Mexico(0.9M).
# The top three are the same countries for cases and deaths as well, which totally makes sense, as where the number of 
# infections are higher, the number of deaths are higher as well. We don't exclude values as the are not extreme.

df %>% filter(death == 0)
# There are 12 countries in where no one died due to Covid. In all of these countries the number of cases
# were below 500. All in all 1890 cases happened in these countries, compared to total of 43M cases (0.004%).
# As the aim of my analysis is to find if a pattern exist between the number of cases and deaths, 
# I will exclude these countries as they has no meaning for my analysis, and they represent a really small portion of 
# observed cases.

df <- df %>% filter(death > 0)

# POPULATION 
df %>% 
  select(c(country, population)) %>% 
  arrange(-population ) %>% 
  head(10)
# If we check population data, we see that the large values are India and China so we don't exclude any values here either.

summary( df )

# Scaling
# As for all variables I have number of people measured, I wanted to do the same scaling on each. For population, it would make sense
# to do a 1000 (thousand) or 1000000 (million) scaling, but in this case for death 115 observations will have values below one. Due to this, 
# I decided not to do scaling on any of the variables.

# df2 <-df %>% transmute( population = population/1000,
#                        cases = cases/1000,
#                        death = death/1000)



############################
#   Basic scatter-plots   ##
############################
#
#   death = alpha + beta * cases
#
# The most simple scatter-plot showing the number of cases and the number of deaths - level-level model without scaling

ggplot( df , aes(x = cases, y = death)) +
  geom_point() +
  geom_smooth(method="loess", color = "deepskyblue4")+
  theme_clean() +
  labs(x = "Number of registered cases",y = "Number of deaths") +
  annotate("text", x = 8700000, y = 219000, label = "United States", size=4)+
  annotate("text", x = 5400000, y = 152000, label = "Brazil", size=4)+
  annotate("text", x = 7920000,  y = 114000, label = "India", size=4)+
  annotate("text", x = 890000,  y = 84000, label = "Mexico", size=4)

# Level-level model, but scale for the number of cases for checking log-transformation (potential level-log model)
ggplot( df , aes(x = cases, y = death)) +
  geom_point() +
  geom_smooth(method="loess", color = "deepskyblue4")+
  theme_clean()+
  labs(x = "Number of registered cases (ln scale)",y = "Number of deaths") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,10,100,1000,10000, 100000, 500000) ) +
  annotate("text", x = 8700000, y = 219000, label = "United States", size=4)+
  annotate("text", x = 5400000, y = 152000, label = "Brazil", size=4)+
  annotate("text", x = 7920000,  y = 114000, label = "India", size=4)+
  annotate("text", x = 890000,  y = 84000, label = "Mexico", size=4)

# Level-level model, but scale for the number of deaths for checking log-transformation (potential log-level model)
ggplot( df , aes(x = cases, y = death)) +
  geom_point() +
  geom_smooth(method="loess", color = "deepskyblue4")+
  theme_clean()+
  labs(x = "Number of registered cases",y = "Number of deaths (ln scale)") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,10,100, 500, 5000,50000) ) +
  annotate("text", x = 8700000, y = 450000, label = "United States", size=4)+
  annotate("text", x = 5400000, y = 90000, label = "Brazil", size=4)+
  annotate("text", x = 7920000,  y = 70000, label = "India", size=4)


# You can change the scale both the number of cases AND deaths for checking log-transformation (potentiallog-log model)
ggplot( df , aes(x = cases, y = death))  +
  geom_point() +
  geom_smooth(method="loess", color = "deepskyblue4")+
  theme_clean()+
  labs(x = "Number of registered cases (ln scale)",y = "Number of deaths (ln scale)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,10,100, 500, 5000, 50000) )+
  scale_y_continuous( trans = log_trans(), breaks = c(1,10,100, 500, 5000, 50000) )


############################
#       Conclusions       ##
############################

#   1) taking log of the number of cases is needed, but still non-linear pattern in data (it is important that we excluded observations with 0 cases) 
#      as know we only have positive values, so we can take the logs)
#       - Substantive: Level changes is harder to interpret and it gives better understanding to get 
#                      if the number of cases increase by 1% what happens with the number of deaths, 
#                      rather than to see in case 1 more people gets the virus.
#       - Statistical: Due to the different population sizes of the countries, the number of cases has a long right tail (is skewed to the right), 
#                      with the ln() transformation its distribution will be closer to normal 
#   2) using only the number of deaths is possible, but need to model the non-linearity in the data
#       
#   3) taking log of the number of deaths is making the association close to linear!
#       - Substantive: it is likely affected in multiplicative ways, percentage changes are easy to interpret as the number of deaths can have highly #                        different values across countries, so relative percentage differences make comparison easier
#       - Statistical: the number of deaths also has a distribution skewed to the right, so it is feasible to make a ln() transformation
#
  
# Take Log of number of confirmed cases and log number of deaths
df <- df %>% mutate( ln_cases = log( cases ),
                     ln_death = log( death ))


############################
##    Make some models    ##
############################

#
# Calculated level-log models as well just for own interest
#   w death (as level) 
#     reg1: deaths = alpha + beta * ln_cases
#     reg2: deaths = alpha + beta_1 * ln_cases + beta_2 * ln_cases^2
#     reg3: deaths = alpha + beta_1 * ln_cases + beta_2 * ln_cases^2 + beta_3 * ln_cases^3
#     reg4: ln_death = alpha + beta_1 * ln_cases * 1(cases < 100000) + beta_2 * ln_cases * 1( 100000 <= cases <= 200000)
#           + beta_3 * ln_cases * 1(cases >= 200000)
#   
#
#   w ln_death:
#     reg5: ln_death = alpha + beta * ln_cases
#     reg6: ln_death = alpha + beta_1 * ln_cases + beta_2 * ln_cases^2
#     reg7: ln_death = alpha + beta_1 * ln_cases + beta_2 * ln_cases^2 + beta_3 * ln_cases^3
#     reg8: lifeexp = alpha + beta * ln_gdppc, weights: population



# Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( ln_cases_2 = ln_cases^2,
                     ln_cases_3 = ln_cases^3)

# Do the regressions
#
#
# Models with actual number of deaths
#
# First model
reg1 <- lm_robust( death ~ ln_cases , data = df , se_type = "HC2" )
summary( reg1 )

ggplot( data = df, aes( x = ln_cases, y = death ) ) + 
  geom_point( color="deepskyblue4", alpha = 0.8) +
  geom_smooth( method = lm , color = 'deeppink4' )+
  theme_clean()+
  labs(title = 'Regression 1', subtitle = 'Level-log  model using number of deaths',
       x = "ln(Number of registered cases)", y = "Number of deaths" )

# Second model
reg2 <- lm_robust( death ~ ln_cases + ln_cases_2 , data = df )
summary( reg2 )

ggplot( data = df, aes( x = ln_cases, y = death ) ) + 
  geom_point( color= 'deepskyblue4', alpha = 0.8) +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'deeppink4' )+
  theme_clean()+
  labs(title = 'Regression 2', subtitle = 'Level-log quadratic model using number of deaths',
       x = "ln(Number of registered cases)", y = "Number of deaths" )

# Third model
reg3 <- lm_robust( death ~ ln_cases + ln_cases_2 + ln_cases_3, data = df )
summary( reg3 )

ggplot( data = df, aes( x = ln_cases, y = death ) ) + 
  geom_point( color='deepskyblue4', alpha = 0.8) +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'deeppink4' )+
  theme_clean()+
  labs(title = 'Regression 3', subtitle = 'Level-log cubic model using number of deaths',
       x = "ln(Number of registered cases)", y = "Number of deaths" )

# Fourth model 
# Regression with piecewise linear spline:

cutoff_1 <- 480000

ln_cutoff_1 <- log(cutoff_1)

# Use simple regression with the lspline function

reg4 <- lm_robust(death ~ lspline( ln_cases , ln_cutoff_1 ), data = df )
summary( reg4 )

ggplot( data = df, aes( x = ln_cases, y = death ) ) + 
  geom_point( color='deepskyblue4', alpha = 0.8) +
  geom_smooth( formula = y ~ lspline(x, ln_cutoff_1) , method = lm , color = 'deeppink4' )+
  theme_clean()+
  labs(title = 'Regression 4', subtitle = 'Piecewise linear spline',
       x = "ln(Number of registered cases)", y = "Number of deaths" )


# Models with ln(death)
#
# Fifth model
reg5 <- lm_robust( ln_death ~ ln_cases , data = df )
summary( reg5 )

ggplot( data = df, aes( x = ln_cases, y = ln_death ) ) + 
  geom_point( color='deepskyblue4', alpha = 0.8) +
  geom_smooth( method = lm , color = 'deeppink4' ) +
  theme_clean()+
  labs(title = 'Regression 5', subtitle = 'Log-log model using ln(number of deaths)',
       x = "ln(Number of registered cases)", y = "ln(Number of deaths)" )

# Sixth model
reg6 <- lm_robust( ln_death ~ ln_cases + ln_cases_2 , data = df )
summary( reg6 )

ggplot( data = df, aes( x = ln_cases, y = ln_death ) ) + 
  geom_point( color='deepskyblue4', alpha = 0.8) +
  geom_smooth( formula = y ~ poly(x,2), method = lm , color = 'deeppink4' ) +
  theme_clean()+
  labs(title = 'Regression 6', subtitle = 'Log-log quadratic model using ln(number of deaths)',
       x = "ln(Number of registered cases)", y = "ln(Number of deaths)" )

# Seventh model
reg7 <- lm_robust( ln_death ~ ln_cases + ln_cases_2 + ln_cases_3, data = df )
summary( reg7 )

ggplot( data = df, aes( x = ln_cases, y = ln_death ) ) + 
  geom_point( color='deepskyblue4', alpha = 0.8) +
  geom_smooth( formula = y ~ poly(x,3), method = lm , color = 'deeppink4' ) +
  theme_clean()+
  labs(title = 'Regression 7', subtitle = 'Log-log cubic model using ln(number of deaths)',
       x = "ln(Number of registered cases)", y = "ln(Number of deaths)" )


# Eight model
# Weighted-OLS: use reg6 setup and weight with population

reg8 <- lm_robust(ln_death ~ ln_cases, data = df , weights = population)
summary( reg8 )

ggplot(data = df, aes(x = ln_cases, y = ln_death)) +
  geom_point(data = df, aes(size=population),  color = 'deepskyblue4', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='deeppink4')+
  scale_size(range = c(1, 15)) +
  theme_clean()+
  labs(title = 'Regression 8', subtitle = 'Weighted-OLS model using ln(number of deaths)',
       x = "ln(Number of registered cases)", y = "ln(Number of deaths)" )+
  annotate("text", x = 11.4, y = 8.5, label = "China", size=4)+
  annotate("text", x = 15.5, y = 12.7, label = "United States", size=4)+
  annotate("text", x = 15.9,  y = 11.7, label = "India", size=4)
  


#######################################
##    Creating model summary table   ##
#######################################
# 

data_out <- "~/Documents/CEU/Fall_semester/Data_analysis_2/Assignment/Out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 , reg5 , reg6 , reg7, reg8),
         type = 'html',
         custom.model.names = c("Number of deaths - linear","Number of deaths - quadratic","Number of deaths - cubic", "Number of deaths - PLS",
                                "ln(death) - linear","ln(death) - quadratic","ln(death) - cubic", "ln(death) - WOLS"),
         caption = "Modelling the number of deaths and the confirmed cases for Corona virus by countries",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)



#####
# Based on model comparison our chosen model is reg8 -  ln_death ~ ln_cases (weighted by population)
#   Substantive: - log-log interpretation works properly for countries
#                - good to look at relative differences as magnitudes can differ significantly
#   Statistical: - Comparatively high R2 and captures variation well




############################
##   Hypothesis testing   ##
############################        

##
# 1) Coefficient is equal to 0:
# Implemented by default...
summary( reg8 )
library(car)

# H0: ln_cases = 0, HA: ln_cases neq 0

linearHypothesis( reg8 , "ln_cases = 0")


############################
##    Residual analysis   ##
############################

# Get the predicted y values from the model
df$reg8_y_pred <- reg8$fitted.values
df$reg8_y_pred_num <- round(exp(df$reg8_y_pred),2)

# Calculate the errors of the model
df$reg8_res <- df$ln_death - df$reg8_y_pred 
df$reg8_res_num <- df$death-df$reg8_y_pred_num

# Find countries with largest negative errors
df %>% top_n( -5 , reg8_res ) %>% 
      select( country , ln_death, reg8_y_pred , reg8_res ) %>% 
  arrange(reg8_res)

        # Find countries with largest negative errors in number of deaths
        df %>% top_n( -5 , reg8_res_num ) %>% 
          select( country , death, reg8_y_pred_num , reg8_res_num ) %>% 
          arrange(reg8_res_num)

# Find countries with largest positive errors
df %>% top_n( 5 , reg8_res ) %>% 
  select( country , ln_death, reg8_y_pred , reg8_res ) %>% 
  arrange(-reg8_res)

        # Find countries with largest negative errors in number of deaths
        df %>% top_n( 5 , reg8_res_num ) %>% 
          select( country , death, reg8_y_pred_num , reg8_res_num ) %>% 
          arrange(-reg8_res_num)
    



#################################
##   Prediction uncertainty    ##
#################################

# CI of predicted value/regression line is implemented in ggplot
ggplot( data = df, aes( x = ln_cases, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' , se = T )

        ggplot(data = df, aes(x = ln_cases, y = ln_death)) +
          geom_point(data = df, aes(size=population),  color = 'deepskyblue4', shape = 16, alpha = 0.6,  show.legend=F) +
          geom_smooth(aes(weight = population), method = "lm", color='deeppink4', se = T )+
          scale_size(range = c(1, 15)) +
          theme_clean()+
          labs(title = 'Regression 8', subtitle = 'Weighted-OLS model using ln(number of deaths)',
               x = "ln(Number of registered cases)", y = "ln(Number of deaths)" )+
          annotate("text", x = 11.4, y = 8.5, label = "China", size=4)+
          annotate("text", x = 15.5, y = 12.7, label = "United States", size=4)+
          annotate("text", x = 15.9,  y = 11.7, label = "India", size=4)
        
##
# You can get them by predict function
#   interval can be any of c("none", "confidence", "prediction")
#   alpha = 0.05 (default) is the significance level
###
# CI of regression line
pred8_CI <- predict( reg8, newdata = df , interval ="confidence" , alpha = 0.05 )
pred8_CI

# If you want you can ask to calculate the SEs for each point:
# pred4_CI <- predict( reg4, newdata = df , se.fit=T,
#                  interval ="confidence" , alpha = 0.05 )

# Hand made CI for regression line
# 1) Add to datatset:
df <- df %>% mutate( CI_reg8_lower = pred8_CI$fit[,2],
                     CI_reg8_upper = pred8_CI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point(data = df, aes(x = ln_cases, y = ln_death, size=population),  color = 'deepskyblue4', shape = 16, alpha = 0.6,  show.legend=F) +
  scale_size(range = c(1, 15)) +
  geom_line( data = df, aes( x = ln_cases, y = reg8_y_pred ) , color = 'deeppink4' , size = 1 ) +
  geom_line( data = df, aes( x = ln_cases, y = CI_reg8_lower ) , color = 'black' ,
             size = 1 , linetype = "dashed" ) +
  geom_line( data = df, aes( x = ln_cases, y = CI_reg8_upper ) , color = 'black' ,
             size = 1 , linetype = "dashed" ) +
  labs(x = "ln( Number of registered cases )",y = "ln( Number of deaths)") 






  