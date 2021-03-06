############################
##   Assignment for DA2   ##
##     and for Coding     ##
##                        ##
##    Titanic data set    ##
##                        ##
##      Cleaning of       ##
##       the data         ##
############################


# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
library(ggthemes)

# Import the needed data
df2  <- read.csv("https://raw.githubusercontent.com/Viki-Meszaros/CEU-Data_analysis_2/main/Assignment_2/Data/Raw/Titanic_raw.csv", na.strings = c("", "NA"))

###########################
##   I.  Data cleaning   ##
###########################

# DISTRIBUTION GRAPHS -----------------------------------------------------

#### Continuous numeric variables
# AGE
    ggplot(df2, aes(x = Age)) +
    geom_histogram(aes(y = ..density..), fill = "cyan4", color = "white", alpha = 0.6, binwidth = 5) +
    geom_density(aes( y = ..density.. ), color = "cyan4", bw = 5, size = 1) +
    theme_calc() +
    labs(title = "Density plot for Age", x = "Age", y = "")

# FARE  
    ggplot(df2, aes(x = Fare)) +
    geom_histogram(aes(y = ..density..), fill = "cyan4", color = "white", alpha = 0.6, binwidth = 15) +
    geom_density(aes( y = ..density.. ), color = "cyan4", bw = 15, size = 1) +
    theme_calc() +
    labs(title = "Density plot for Fare", x = "Ticket price", y = "")


#### Categorical variables
# SEX
    ggplot(df2, aes(x = Sex)) +
    geom_bar(fill = "cyan4", alpha = 0.8) +
    theme_calc() +
    labs(title = "Sex of the passengers")

# SURVIVED
    ggplot(df2, aes(x = Survived)) +
    geom_bar(fill = "cyan4", alpha = 0.8) +
    theme_calc() +
    labs(title = "Number of passengers who survived and who died", x="", y="") +
    scale_x_continuous(breaks = c(0,1) ,labels = c("0" = "Died", "1" = "Survived"))
    
# TICKET CLASS
    ggplot(df2, aes(x = Pclass)) +
    geom_bar(fill = "cyan4", alpha = 0.8) +
    theme_calc() +
    labs(title = "Different ticket classes", x="", y="") +
    scale_x_continuous(breaks = c(1, 2, 3) ,labels = c("1" = "1st Class", "2" = "2nd Class", "3" = "3rd Class"))
    
# EMBARKED  
    ggplot(df2, aes(x = Embarked)) +
    geom_bar(fill = "cyan4", alpha = 0.8) +
    theme_calc() +
    labs(title = "Different ticket classes", x="", y="")
    
# NUMBER OF PARENTS OR CHILDREN
    ggplot(df2, aes(x = Parch)) +
    geom_bar(fill = "cyan4", alpha = 0.8) +
    theme_calc() +
    labs(title = "Number of parents and/or children a passenger has", x="", y="")
    
# NUMBER OF SIGLINGS
    ggplot(df2, aes(x = SibSp)) +
    geom_bar(fill = "cyan4", alpha = 0.8) +
    theme_calc() +
    labs(title = "Number of siblings a passenger has", x="", y="")
    


# SUMMARY STATISTICS ------------------------------------------------------
#### I do summary statistics for the two continuous variables as for categorical ones it won't make any sense
summary(df2 %>% select(c("Age", "Fare")))  

# Nicer form
    source("https://raw.githubusercontent.com/Viki-Meszaros/CEU-Data_analysis_2/main/Assignment_2/Codes/sum_stat.R")
 
    desc_stat <- sum_stat( df2 , var_names = c('Age', 'Fare'),
                           stats = c('min','1st_qu.', 'median','mean', '3rd_qu','max','sd') )
desc_stat      


# MISSING VALUES ----------------------------------------------------------

# Overall there are 866 NA values (missing values) in the data representing 8.1% of it
sum(is.na(df2))
round(sum(is.na(df2))/(ncol(df2)*891)*100, 2)

# Checking where these values are  
nas <- NULL
for (i in 1:ncol(df2)) {
  nas  <- c(nas, sum(is.na(df2[ , i])))
}

num_NAs <- as_tibble(cbind(names(df2), nas))
colnames(num_NAs) <- c("Variable", "Number of NAs")

num_NAs

rm(i, nas)

# We have missing values for Age, Cabin and Embarked most coming form missing Cabin number.
#
#### For AGE I want to fill in these as it is an important/interesting variable for my analysis.
#### The mean of Age is 29.7 while the median is 28. From this and the histogram as well we can see,
#### that the distribution of Age is skewed to the right. Based on this we won't use the mean as the
#### replacement for NAs rather the median. We will also look at how the median age changes among social
#### groups and genders. 


ggplot(df2) +
 aes(x = "", y = Age, fill = Pclass) +
 geom_boxplot() +
 scale_fill_viridis_c(option = "viridis") +
 theme_calc() +
 theme(legend.position = "none") +
 facet_grid(vars(), vars(Pclass))

df2 %>% 
  group_by(Pclass) %>% 
  summarise("average age" = mean(Age, na.rm = T))

# Average age increases as social class increases 

ggplot(df2) +
 aes(x = "", y = Age, fill = Sex) +
 geom_boxplot() +
 scale_fill_viridis_d(option = "viridis") +
 theme_calc()

df2 %>% 
  group_by(Sex) %>% 
  summarise("average age" = mean(Age, na.rm = T))

# Average age is lower for women compared to men.

ggplot(df2) +
  aes(x = "", y = Age, fill = Sex) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "viridis") +
  theme_calc() +
  facet_wrap(vars(Pclass))

age <- df2 %>% 
  group_by(Sex, Pclass) %>%
  summarise("average age" = round(median(Age, na.rm = T)))

# To make a good replacement of missing values I took into consideration the above mentioned difference 
# by Sex and Pclass. I imputed the six different median values for the six groups by age and social group.

colnames(df2)[6] <- "Age_w_missing"

for (i in 1:nrow(df2)) {
  if (is.na(df2$Age_w_missing[i])) {
    if (df2$Sex[i] == "female") {
      if (df2$Pclass[i] == 1) {
        df2$Age[i] <- 35  
      } else if(df2$Pclass[i] == 2) {
        df2$Age[i] <- 28
      } else { df2$Age[i] <- 22}
    } 
    else {
      if (df2$Pclass[i] == 1) {
        df2$Age[i] <- 41  
      } else if(df2$Pclass[i] == 2) {
        df2$Age[i] <- 31
      } else { df2$Age[i] <- 27}
    }
  }
  else{
    df2$Age[i] <- df2$Age_w_missing[i]
  }
  
}

# Take a look at the how the distribution changed due to the imputing
ggplot(df2, aes(x = Age_w_missing)) +
  geom_histogram(aes(x=Age, y = ..density..), fill = "orchid", color = "white", alpha = 0.7, binwidth = 5 ) +
  geom_histogram(aes(y = ..density..), fill = "cyan4", color = "white", alpha = 0.5, binwidth = 5) +
  geom_density(aes(x = Age, y = ..density.. ), color = "orchid", bw = 5, size = 1) +
  geom_density(aes( y = ..density.. ), color = "cyan4", bw = 5, size = 1) +
  theme_calc() +
  labs(title = "Density plot for Age", x = "", y = "")

# The distribution looks quite ok, so I delete the Age_w_missing values column as we won't need it from now on
df2$Age_w_missing <- NULL


#### For EMBARKED only two values are missing, so I tried to find additional information about the
#### given passengers as I had their names. I managed to find out that both of them embarked in Southhampton.
for (i in 1:nrow(df2)) {
  if(is.na(df2$Embarked[i])) {df2$Embarked[i] <- "S"}
}

#### For CABIN 687 values are missing (77%). I was thinking of using this variable as the different
#### cabin numbers show on which level of the ship the person had the room, but with this amount of
#### missing values I would not be able to impute meaningful values, so it most probably would not
#### give additional value to my predictive model.I decided to exclude this variable in later analysis.
df2$Cabin <- NULL


# DATA TYPES --------------------------------------------------------------

## For categorical variables I created either binary or dummy variables

#### SEX 
# Change male and female to binary ( 0 = male, 1 = female)
df2$Sex <- ifelse(df2$Sex == "female", 1, 0)
colnames(df2)[5] <- "Female"

#### EMBARKED
# I could have changed the letters to numbers like 0,1,2 but then I would assume that there is a relationship between these.
# As there is no logical relationship I will create 2 dummy variables assuming 1 "left_out category". S (Southhampton) will be the
# reference group as most of the passengers embarked there.
df2 %>% 
  ggplot(aes(x = Embarked)) +
  geom_bar(fill = "cyan4", alpha = 0.8) +
  theme_calc()

for (i in 1:nrow(df2)) {
  if (df2$Embarked[i] == "Q") {
    df2$Q[i] <- 1
  } else {df2$Q[i] <- 0}
  if (df2$Embarked[i] == "C"){
    df2$C[i] <- 1
  } else {df2$C[i] <- 0}
}


# FAMILY RELATIONSHIP ---------------------------------------------

## We have two variables that summarizes family relationships in some way SibSp (showing how many
## siblings or spouse someone had abord) and Parch (showing the number of parents/children aboard the Titanic)

df2 %>% 
  group_by(SibSp) %>% 
  summarise( sur = mean(Survived), n_obs = n()) %>% 
  ggplot(aes(x = SibSp, y = sur, color = -n_obs)) + 
  geom_point(size = 3) +
  theme_calc()

df2 %>% 
  group_by(Parch) %>% 
  summarise(sur = mean(Survived), n_obs = n()) %>% 
  ggplot(aes(x = Parch, y = sur, color  = -n_obs)) + 
  geom_point(size = 3) +
  theme_calc()

# The number of observations vary widely among these variables. There a lot more people with 0 or 1 sibling
# then those who had 5 or 8 on board. To get a simpler variable that is also easier to interpret, I created a new
# variable "Travel_alone" (1 = yes, 0 = no) showing if someone was alone on the ship or with family. (This variable
# summarizes SibSp and Parch, when both were 0 the person travelled alone)

for (i in 1:nrow(df2)) {
  if (df2$SibSp[i] == 0 && df2$Parch[i] == 0) {
    df2$Travel_alone[i] <- 1
  } else { df2$Travel_alone[i] <- 0}
}


# SOCIAL CLASS ------------------------------------------------------------

## As social class is a categorical variable, so I created dummy variables for this as well (like for Embarked).
## The reference category (left-out) will be Pclass = 3 as that is the lowest socio-economic class.

for (i in 1:nrow(df2)) {
  if (df2$Pclass[i] == 1) {
    df2$Pclass_1[i] <- 1
  } else {df2$Pclass_1[i] <- 0}
  if (df2$Pclass[i] == 2){
    df2$Pclass_2[i] <- 1
  } else {df2$Pclass_2[i] <- 0}
}



# FINAL DATA FRAME --------------------------------------------------------

final <- df2[, c("Name", "Survived", "Age", "Fare", "Female", "Travel_alone", "Pclass_1",
                "Pclass_2", "C", "Q", "Pclass", "Embarked")]

write.csv(final, "c://Users/admin/Documents/CEU/Fall_semester/Data_analysis_2/Github/CEU-Data_analysis_2/Assignment_2/Data/Clean/Titanic_clean.csv")









