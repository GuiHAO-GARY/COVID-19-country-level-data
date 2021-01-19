library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
# Extract the Covid19 data
covid19_time_series <- list()
covid19_time_series[['confirmed']] <- read_csv(url('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'))
covid19_time_series[['deaths']] <- read_csv(url('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'))
covid19_time_series[['recovered']] <- read_csv(url('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'))
########################################################


##### processing data ######
aggregate_country <- function(dataframe){
  dataframe[, -c(1, 3, 4)] %>%
    group_by(`Country/Region`) %>%
    summarise_all(list(sum)) -> result
  return(result)
}

regulate_country_name <- function(dataframe){
  dataframe$`Country/Region` <- str_replace_all(dataframe$`Country/Region`, 'Korea, South', 'South Korea')
  dataframe$`Country/Region` <- str_replace_all(dataframe$`Country/Region`, 'Taiwan\\*', 'Taiwan')
  dataframe$`Country/Region` <- str_replace_all(dataframe$`Country/Region`, 'Burma', 'Myanmar')
  dataframe$`Country/Region` <- str_replace_all(dataframe$`Country/Region`, 'Czechia', 'Czech Republic')
  dataframe$`Country/Region` <- str_replace_all(dataframe$`Country/Region`, 'US', 'United States')
  return(dataframe)  
}
##############################################

aggregated_confirmed <- covid19_time_series$confirmed %>% 
  regulate_country_name() %>%
  aggregate_country()
aggregated_deaths <- covid19_time_series$deaths %>% 
  regulate_country_name() %>%
  aggregate_country()
aggregated_recover <- covid19_time_series$recovered %>% 
  regulate_country_name() %>%
  aggregate_country() 

Time.default <- colnames(aggregated_confirmed)[length(colnames(aggregated_confirmed))] %>%
  mdy()

########### extract the info on certain date ############
extract_data_on <- function(date = as.character(Time.default)){
  m <- date %>% month()
  d <- date %>% day()
  y <- date %>% 
    year() %>%
    str_sub(start = 3)
  date_format <- paste(m, '/', d, '/', y, sep = '')
  aggregated_confirmed[, c('Country/Region',date_format)] %>%
    bind_cols(aggregated_deaths[, date_format]) %>%
    bind_cols(aggregated_recover[, date_format]) -> alldata
  colnames(alldata) <- c('Country', 'Confirmed', 'Deaths', 'Recovered')
  return(alldata)
}
covid19_cases <- extract_data_on()

