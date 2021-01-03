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
library(data.table)

require(scales)
library(lspline)
library(estimatr)
library(segmented) 

library(mfx)
library(margins)
library(pscl)
library(modelsummary)

library(texreg)

library(xtable)
library(moments)
library(car)


df <- read.csv("https://raw.githubusercontent.com/Viki-Meszaros/CEU-Data_analysis_2/main/Assignment_2/Data/Clean/Titanic_clean.csv")
df$X <- NULL


# CORRELATION BETWEEN VARIABLES -------------------------------------------

numeric_df <- keep( df , is.numeric )
corr <- round(cor( 
  keep(df, is.numeric)), 3)

  ggcorrplot(corr)

  sum( abs(corr) >= 0.8 & abs(corr) != 1 , na.rm = T) / 2
  sum( abs(corr) >= 0.6 & abs(corr) != 1 , na.rm = T) / 2
# There was only one pair of variables with high correlation --> Pclass and Pclass_1 and it is normal as it covers the same thing
  
  id_cr <- which( abs(corr) >= 0.5 & abs(corr) != 1 )
  pair_names <- expand.grid( variable.names(numeric_df) , variable.names(numeric_df) )
  # Get the pairs:
  high_corr <- pair_names[ id_cr , ]
  high_corr <- mutate( high_corr , corr_val = corr[ id_cr ] )
  high_corr %>% arrange(desc(abs(corr_val)))
  
# Based on this we can conclude that there are no strongly correlted variable that would ave an effect on our model
rm(numeric_df, pair_names, high_corr, corr, id_cr)  
  
# ONE TO ONE SIMPLE LPM MODELS -------------------------------------------------------
## SEX
lpm1_sex <- lm(Survived ~ Female, data=df)
summary(lpm1_sex, vcov=sandwich)

# visualize this regression
df$pred1 <- predict(lpm1_sex)

table(df$pred1, df$Survived)

#create weights
df <- df %>%
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

preds <- data.frame()
preds <- df %>% select(Survived, pred1)
colnames(preds)[3] <- "pred1_Sex"

df$pred1 <- NULL
preds$Female <- NULL

## Female had significantly higher probability to survive compared to men


## PCLASS
lpm2_pclass <- lm(Survived ~ Pclass_1 + Pclass_2, data = df)
summary(lpm2_pclass, vcoc=sandwich)

df$pred2 <- predict(lpm2_pclass)

df <- df %>%
  group_by(Pclass, Survived) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))

ggplot(data = df) +
  geom_line(aes(x = Pclass, y = pred2),  size = 0.8, color = "cyan3", alpha = 0.6 ) +
  geom_point(aes(x = Pclass, y = pred2), size = 3, color = "cyan4",  shape = 16) +
  geom_point(aes(x = Pclass, y = Survived, size=weight_2), color = "purple3", shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Socio-economic class",y = "Survived / Predicted probability of ")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(breaks = c(1, 2, 3) , labels = c("1" = "1st Class", "2" = "2nd Class", "3" = "3rd Class" )) +
  theme_calc() 

preds$pred2_Pclass <- df$pred2

df$pred2 <-NULL
## Those who belong to the 1st socio-economic class had higher probability to survive and this probability decreased along social classes


## EMBARKED
lpm3_embarked <- lm(Survived ~ C + Q, data = df)
summary(lpm3_embarked, vcoc=sandwich)

df$pred3 <- predict(lpm3_embarked)

df <- df %>%
  group_by(Embarked, Survived) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))

ggplot(data = df) +
  geom_point(aes(x = Embarked, y = pred3), size = 3, color = "cyan4",  shape = 16) +
  geom_point(aes(x = Embarked, y = Survived, size=weight_2), color = "purple3", shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Embarked in",y = "Survived / Predicted probability of ")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_x_discrete(labels = c( "Q" = "Queenstown", "S" = "Southhampton", "C" = "Cherbourg")) +
  theme_calc() 

preds$pred3_Embarked <- df$pred3

df$pred3 <-NULL
## Who embarked in Cherbourg had significantly higher probability to survive (maybe the Cabins were filled by arrival and as Cherbourg
## passengers got on board last they got rooms on upper floors)


## TRAVEL ALONE
lpm4_travalone <- lm(Survived ~ Travel_alone, data = df)
summary(lpm4_travalone, vcoc=sandwich)

df$pred4 <- predict(lpm4_travalone)

df <- df %>%
  group_by(Travel_alone, Survived) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))

ggplot(data = df) +
  geom_line(aes(x = Travel_alone, y = pred4),  size = 0.8, color = "cyan3", alpha = 0.6 ) +
  geom_point(aes(x = Travel_alone, y = pred4), size = 3, color = "cyan4",  shape = 16) +
  geom_point(aes(x = Travel_alone, y = Survived, size=weight_2), color = "purple3", shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Travel Alone",y = "Survived / Predicted probability of ")+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(limits = c(0,1), breaks = c(0,1), labels = c("0" = "No", "1" = "Yes"))+
  theme_calc() 

preds$pred4_Travel_alone <- df$pred4

df$pred4 <-NULL
## Who traveled alone had significantly lower probability to survive, probably because during the sinking crew saved women and families first


## AGE
lpm5_age <- lm(Survived ~ Age, data = df)
summary(lpm5_age, vcoc=sandwich)

fit_seg <- segmented( lpm5_age , seg.Z = ~Age, psi = list( Age=c(25, 35 )) )
summary(fit_seg)
## The estimated knot values for age are 27 and 32

df$pred5 <- predict(lpm5_age)

# pattern of association
df %>% 
  ggplot(aes(x = Age, y = Survived)) +
  geom_point() +
  geom_smooth(method = "loess")

# With binned values
ggplot( df , aes( x = Age , y = Survived ) )+
  stat_summary_bin( fun = 'mean' , binwidth = 7, 
                    geom = 'point',  size = 2 ) +
  labs( x = 'Age' , y = 'Survived' )+
  theme(legend.position = "none")+
  geom_smooth(method="loess",formula = y~x) +
  theme_calc()

preds$pred5_Age <- df$pred5

df$pred5 <-NULL
df$weight <- NULL
df$weight_2 <- NULL

rm(fit_seg)
## The probability of survival decreased as someone is older under 25, it increased between 25 and 32 and decreased again above the age of 32


## FARE
lpm6_fare  <- lm(Survived ~ Fare, data = df)
summary(lpm6_fare, vcoc=sandwich)


df %>% 
  ggplot(aes(x = Fare, y = Survived)) +
  stat_summary_bin( fun = 'mean' , binwidth = 0.5, 
                    geom = 'point',  size = 2 ) +
  geom_smooth(method = "loess") +
  theme_calc()

# take the log of fare to get a pattern of asociation closer to linear
df %>% 
  ggplot(aes(x = Fare, y = Survived)) +
  geom_point() +
  scale_x_continuous( trans = "log") +
  geom_smooth(method = "loess") +
  theme_calc()

# with binned values
df %>% 
  ggplot(aes(x = Fare, y = Survived)) +
  stat_summary_bin( fun = 'mean' , binwidth = 0.5, 
                    geom = 'point',  size = 2 ) +
  scale_x_continuous( trans = "log") +
  geom_smooth(method = "loess") +
  theme_calc()

preds$pred6_Fare <- predict(lpm6_fare)

# I decided to take the log values for Fare as the pattern of association is more linear in this case
# Then I created a tdf where I excluded values where the fare price was 0 from df as I could not take the log of them 
# (Most probably with this I excluded people who paid nothing for the trip --> crew members, or people who got the ticket as present)
df <- df %>% mutate( ln_fare= log( Fare ))
tdf <- df %>% filter(Fare != 0)


lpm7_lnfare <- lm(Survived ~ ln_fare, data = tdf)
summary(lpm7_lnfare, vcoc=sandwich)

fit_seg <- segmented( lpm7_lnfare , seg.Z = ~ln_fare, psi = list( ln_fare=c(2)) )
summary(fit_seg)
## the estimated knot value for ln_fare is 2.4

tdf$pred7 <- predict(lpm7_lnfare)

preds_ln <- data.frame()
preds_ln <- tdf %>% select(Survived, pred7)
colnames(preds)[3] <- "pred7_lnfare"

tdf$pred7 <- NULL
preds_ln$Travel_alone <- NULL
rm(fit_seg)

preds <- round(preds)
preds_ln <- round(preds_ln)



# MODELLING ------------------------------------------
set.seed(123)
dt <- sort(sample(seq_len(nrow(df)), nrow(df)*.8))

test <- df[-dt,]
train <- df[dt, ]

train <- train %>% mutate( ln_fare= log( Fare ))
ttrain <- train %>% filter(Fare != 0)

#### 1st model - Sex
lpmm1 <- lm(Survived ~ Female, data = train)
summary(lpmm1, vcoc=sandwich)

train$predm1 <- predict(lpmm1)

table(round(train$predm1), train$Survived)


#### 2nd model - add socio-economic class
lpmm2 <- lm(Survived ~ Female + Pclass_1 + Pclass_2, data = train)
lpmm21 <- lm(Survived ~ Female + Pclass_1 + Pclass_2 + ln_fare, data = ttrain)

summary(lpmm2, vcoc=sandwich)
summary(lpmm21, vcoc=sandwich)

train$predm2 <- predict(lpmm2)

table(round(train$predm2), train$Survived)


#### 3rd model - add Age
lpmm3 <- lm(Survived ~ Female + Pclass_1 + Pclass_2 + Age, data = train)

summary(lpmm3, vcoc=sandwich)

train$predm3 <- predict(lpmm3)

table(round(train$predm3), train$Survived)


#### 4th model - change Age to piecewise linear spline
lpmm4 <- lm(Survived ~ Female + Pclass_1 + Pclass_2 + lspline(Age, c(27,32)), data = train)

summary(lpmm4, vcoc=sandwich)

train$predm4 <- predict(lpmm4)

table(round(train$predm4), train$Survived)
# Using piecewise linear spline for Age gives us only a slightly better R^2, but a more difficult model
# Even though the model is more difficult I decided in favor of the piecewise model as there is significant difference
# in the pattern below 27 (decreasing probability of survival), between 27 and 32 (increasing pattern) and after 32 (decreasing again)


####5th model - add travel alone
lpmm5 <- lm(Survived ~ Female + Pclass_1 + Pclass_2 + lspline(Age, c(27,32)) + Travel_alone, data = train)

summary(lpmm5, vcoc=sandwich)

train$predm5 <- predict(lpmm5)

table(round(train$predm5), train$Survived)


#### 6th model - add embarked
lpmm6 <- lm(Survived ~ Female + Pclass_1 + Pclass_2 + lspline(Age, c(27,32)) + Travel_alone + Q + C, data = train)

summary(lpmm6, vcoc=sandwich)

train$predm6 <- predict(lpmm6)

table(round(train$predm6), train$Survived)


# FINAL PREDICTING MODELS -------------------------------------------------

#### Logit and Probit models

model_formula <- formula( Survived ~ Female + Pclass_1 + Pclass_2 + lspline(Age, c(27, 32)) 
                           + Travel_alone + Q + C )
#### LPM
lpm <-lm( model_formula , data=ttrain)
summary(lpm, vcov=sandwich )

ttrain$pred_lpm <- predict(lpm)

summary(ttrain$pred_lpm)

ggplot( ttrain , aes( x = pred_flpm) ) +
  geom_histogram( fill = 'navyblue' , color = 'grey90', binwidth = 0.08)

ttrain <- ttrain %>% 
  mutate( pred_lpm_100 = ntile(pred_lpm, 100) )

source("C://Users/admin/Documents/CEU/Fall_semester/Data_analysis_2/Github/CEU-Data_analysis_2/Assignment_2/Codes/sum_stat.R")

sum_stat( subset( ttrain , pred_lpm_100==100 ) , 
          c('Female', 'Travel_alone', "Pclass_1", "Pclass_2", "C", "Q", "Age", "Fare"),
          c('mean','median','sd'),
          num_obs = F )

sum_stat( subset( ttrain , pred_lpm_100==1 ) , 
          c('Female', 'Travel_alone', "Pclass_1", "Pclass_2", "C", "Q", "Age", "Fare"),
          c('mean','median','sd'),
          num_obs = F )

#### LOGIT
logit <- glm( model_formula , data=ttrain, family=binomial(link="logit") )
summary(logit)
glance(logit)

# predicted probabilities 
ttrain$pred_logit <- predict.glm(logit, type="response")
summary(tdf$pred_logit)

# Calculate logit marginal differences
logit_marg <- logitmfx( model_formula, data=train, atmean=FALSE, robust = T)
print(logit_marg)


#### PROBIT
probit <- glm( model_formula , data = tdf , family=binomial(link="probit") )
summary(probit)

# predicted probabilities 
ttrain$pred_probit<- predict.glm( probit , type = "response" )
summary( ttrain$pred_probit )

# probit marginal differences
probit_marg <- probitmfx(  model_formula, data=ttrain, atmean=FALSE, robust = T)
print( probit_marg )


#### MODEL SUMMARY OUTPUT
cm <- c('(Intercept)' = 'Constant')
pmodels <- list(lpm, logit, logit_marg, probit, probit_marg)
msummary( pmodels ,
          fmt="%.3f",
          gof_omit = 'train|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|R2|PseudoR2',
          stars=c('*' = .05, '**' = .01),
          coef_rename = cm,
          coef_omit = 'as.factor(country)*',
          output = "C://Users/admin/Documents/CEU/Fall_semester/Data_analysis_2/Github/CEU-Data_analysis_2/Assignment_2/Out/Model_summary.html"
)

# adding pseudo R2 (not work for mfx)
glance_custom.glm <- function(x) data.frame(`PseudoR2` = pR2(x)["McFadden"])
cm <- c('(Intercept)' = 'Constant')
msummary(list(lpm, logit, logit_marg, probit, probit_marg),
         fmt="%.3f",
         gof_omit = 'train|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         coef_omit = 'as.factor(country)*',
         output = "C://Users/admin/Documents/CEU/Fall_semester/Data_analysis_2/Github/CEU-Data_analysis_2/Assignment_2/Out/Model_summary.html"
)


#### Comparing predicted probabilities of logit and probit to LPM

ggplot(data = ttrain) +
  geom_point(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=1,  shape=16) +
  geom_point(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=1,  shape=16) +
  geom_line(aes(x=pred_lpm, y=pred_lpm,    color="45 degree line"), size=1) +
  labs(x = "Predicted probability of staying healthy (LPM)", y="Predicted probability")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_x_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_color_manual(name = "", values=c("forestgreen", "orchid","cyan4"))+
  theme(legend.position=c(0.55,0.08),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4)) +
  theme_calc()


stargazer(list(lpm, logit, probit), digits=3)




sum_stat( subset( ttrain , Survived == 1 ) , 
          c( "pred_lpm","pred_logit","pred_probit" ),
          c("mean","median","min","max","sd"),
          num_obs = F )

sum_stat( subset( ttrain , Survived == 0 ) , 
                 c( "pred_lpm","pred_logit","pred_probit" ),
                 c("mean","median","min","max","sd"),
                 num_obs = F )


#### Bias and Calibration curve

bias <- mean( ttrain$pred_logit ) - mean(ttrain$Survived)

#### LPM
actual_vs_predicted <- ttrain %>%
  ungroup() %>% 
  dplyr::select(actual = Survived, 
                predicted = pred_lpm) 
num_groups <- 10

calibration_d <- actual_vs_predicted %>%
  mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
  group_by(predicted_score_group) %>%
  dplyr::summarise(mean_actual = mean(actual), 
                   mean_predicted = mean(predicted), 
                   num_obs = n())

ggplot( calibration_d,aes(x = mean_actual, y = mean_predicted)) +
  geom_point( color='orchid', size=1.5) +
  geom_line(  color='orchid', size=1) +
  geom_abline( intercept = 0, slope = 1, color='cyan4', size = 1) +
  labs( x = "Actual event probability", y = "Predicted event probability") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  theme_calc()

#### LOGIT
actual_vs_predicted <- ttrain %>%
  ungroup() %>% 
  dplyr::select(actual = Survived, 
                predicted = pred_logit) 
num_groups <- 10

calibration_d <- actual_vs_predicted %>%
  mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
  group_by(predicted_score_group) %>%
  dplyr::summarise(mean_actual = mean(actual), 
                   mean_predicted = mean(predicted), 
                   num_obs = n())

ggplot( calibration_d,aes(x = mean_actual, y = mean_predicted)) +
  geom_point( color='orchid', size=1.5) +
  geom_line(  color='orchid', size=1) +
  geom_abline( intercept = 0, slope = 1, color='cyan4', size = 1) +
  labs( x = "Actual event probability", y = "Predicted event probability") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  theme_calc()

#### PROBIT
actual_vs_predicted <- ttrain %>%
  ungroup() %>% 
  dplyr::select(actual = Survived, 
                predicted = pred_probit) 
num_groups <- 10

calibration_d <- actual_vs_predicted %>%
  mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
  group_by(predicted_score_group) %>%
  dplyr::summarise(mean_actual = mean(actual), 
                   mean_predicted = mean(predicted), 
                   num_obs = n())

ggplot( calibration_d,aes(x = mean_actual, y = mean_predicted)) +
  geom_point( color='orchid', size=1.5) +
  geom_line(  color='orchid', size=1) +
  geom_abline( intercept = 0, slope = 1, color='cyan4', size = 1) +
  labs( x = "Actual event probability", y = "Predicted event probability") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  theme_calc()




#### CONFUSION TABLES
conf_table <- data.frame(ttrain$pred_lpm, ttrain$pred_logit, ttrain$pred_probit)

# Set the threshold value
threshold <- 0.5

# Decide for each observations and each prediction, if larger than the threshold value!
for (i in 1:nrow(conf_table)) {
  for (j in 1:ncol(conf_table)) {
    if (conf_table[i,j]>threshold) {conf_table[i,j]=1}
    else {conf_table[i,j]=0}
  }
}



# confusion matrix - does it seems similar?
for (j in 1:ncol(conf_table)){
  print(prop.table(table(conf_table[, j], ttrain$Survived)))
}















