#########################
##   Data analysis 2   ##
##                     ##
##    Exercise 7.3     ##
#########################

rm(list=ls())
library(tidyverse)

# import raw data
b_data <- read_csv("https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/Class_03/data/raw/hotelbookingdata.csv")

# Create a new variable
b_data <- mutate(b_data, nnights = 1)

#Clean accommodationtype column
b_data <- separate( b_data, accommodationtype , "@" , into = c("garbage", "accommodation_type") )

# Remove the variable garbage
b_data <- select(b_data, -garbage)

# Correct the guestreviewratings into a simple numeric variable
b_data <- separate(b_data, guestreviewsrating, "/", 
                   into = c("ratings"))

# Convert character data to numeric 
b_data$ratings <- as.numeric(b_data$ratings)


# How to deal with distance measure
b_data <- mutate(b_data, 
                 distance = as.numeric(gsub("[^0-9\\.]", "", center1distance)),
                 distance_alter = as.numeric(gsub("[^0-9\\.]", "", center2distance)))

## Rename variables
b_data <- rename( b_data ,
                  rating_count = rating_reviewcount,
                  ratingta = rating2_ta,
                  ratingta_count = rating2_ta_reviewcount,
                  country = addresscountryname)

## Replacing missing values
# look at key variable: stars
b_data <- rename( b_data, stars = starrating)

# Replace with Na
b_data <- mutate(b_data, stars = na_if(stars, 0))

# Filter out observation that do not have identifiers
b_data <- filter(b_data, !is.na(hotel_id))

# Filter out duplicates
b_data <- filter( b_data, !duplicated(b_data))

# Remove duplicates to specific variables
sub_data <- subset( b_data, select = c(country, hotel_id))
b_data <- filter ( b_data , !duplicated( 
  subset(b_data, select = c( country, hotel_id, distance, stars, ratings, 
                             price, year, month, weekend, holiday))))

saveRDS("C:/Users/admin/Documents/CEU/Fall_semester/Data_analysis_2/DA_2" ,"Clean_hotels_data.Rds")
write.csv(b_data, "C:/Users/admin/Documents/CEU/Fall_semester/Data_analysis_2/DA_2" )

# check the number of observations in each city to chose one for analysis
tdf <- b_data %>% 
  group_by(city) %>% 
  summarise("num_hotels" = n())

# Finally get hotels Budapest
b_data <- rename( b_data, city = s_city)
hotel_bud <- filter(b_data, city == "Budapest")


# Writing out csv
saveRDS( "C:/Users/admin/Documents/CEU/Fall_semester/Data_analysis_2/DA_2" , "Hotels_Bud.Rds")

 
  
  
  
  