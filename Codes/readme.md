These codes are for analysis pattern of association between number of covid cases and the number of deaths due to Covid.

1_get_covid_data.R - downloads raw data from World Development Indicators maintained by World Bank as well as a csv with the covid data and saves it to data/raw folder.

2_clean_covid_data.R - loads the raw data and clean them: create a tidy table where each observation is a country.

3_covid data_analysis.R - loads the clean data and executes simple linear regressions with visual inspections and quantitative analysis. It chooses model and then analyse the residuals.

Covid analysis.Rmd - contains the project as a whole with all the explanations.
