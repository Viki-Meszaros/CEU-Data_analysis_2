########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##      NO. 1         ##
##   Get the data     ##
########################


# Clear memory 
rm(list=ls())

# Call packages
library(WDI)
library(tidyverse)

# Download COVID cross-sectional data
date <- '10-26-2020'
covid_url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/',
            date,'.csv')
covid_raw <- read.csv(covid_url)

# Download population data for 2019
  # Find the appropriate data I want to analyze
population <- WDIsearch('population, total') # indicator I need to use id SP.POP.TOTL

pop_raw <- WDI(indicator=c('SP.POP.TOTL'), 
                country="all", start=2019, end=2019)

# Save the raw files
my_path <- "~/Documents/CEU/Fall_semester/Data_analysis_2/Assignment/Data/"

# Save Covid data file
write_csv(covid_raw, paste0(my_path,'Raw/covid_10_26_2020_raw.csv'))
# Save population data file
write_csv(pop_raw, paste0(my_path,'raw/pop_WDI_2019.csv'))


