rm(list=ls())
library(tidyverse)
library(rvest)
library(stringr)
library(tidycensus)
library(blsAPI)
library(jsonlite)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
#wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"

setwd(wd)

###################################
### Downloading Datasets ##########
###################################

# 1 -- 1976-2020 Presidential Elections Results by State: Popular Vote
#   -- Dataset from MIT Election Data + Science Lab
#   -- More details: https://doi.org/10.7910/DVN/42MVDX

if (!file.exists(file.path("Input", "1976-2020-president.csv")))
  cat("File 1976-2020-president.csv needed but absent")

# 2 -- 1976-2020 Presidential Elections Results by State: Electoral College Vote
#   -- Dataset scraped from the Electoral College Archive
#   -- More details: https://www.archives.gov/electoral-college

if (!file.exists(file.path("Input", "1976-2020-electoral-college.csv"))){
  
  baseurl <- "https://www.archives.gov/electoral-college/"
  year    <- seq(1976, 2020, by = 4)

  url_all <- vector("double", length(year))
  names(url_all) <- year

  for(i in 1:length(year)){
    url_all[i] <- paste0(baseurl, year[i]) 
    }
  
  get <- function(url){
    request <- read_html(url)
    table <- html_table(request, fill = TRUE)
    ev <- table[[2]][2:53,1:2]
    ev[, 1] <- str_replace_all(ev[, 1], c("\\*+" = "", " $" = ""))
    colnames(ev) <- c("state_name", "electoral_votes")  
    return(ev)
    } 
  
  electoral_college <-
    bind_rows(map(url_all, get), .id = "column_label") %>%
    mutate(column_label = as.double(column_label)) %>%
    filter(state_name != "Total")
  
  write_csv(electoral_college, file.path("Input", "1976-2020-electoral-college.csv"))
}

# 3 -- 2020 Presidential Elections Results by county
#   -- Data developed using The Guardian, townhall.com, Fox News, Politico, and the New York Times
#   -- More details: https://github.com/tonmcg/US_County_Level_Election_Results_08-20

if (!file.exists(file.path("Input", "results_2020.csv")))
  download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv",
                destfile = file.path("Input","results_2020.csv"), mode = "wb")

# 4 -- Socio/Demographic/Economic by county
#   -- Data retrieved from the Census Bureau using tidycensus
#   -- More details: https://cran.r-project.org/web/packages/tidycensus/index.html

if (!file.exists(file.path("Input", "census_data.csv"))) {
  census_api_key("7e8698c00cfcf3a78b3b04d081a8f31445ba4eae")

  list_vars <- load_variables(2019, "acs5", cache = TRUE)

  vars <-                                   # List of Variables
    c(
      "B19013_001",                          # MHI
      "B19083_001",                          # Gini Index
      "B05002_001", "B05002_013",            # Foreign Born
      "B03002_001", "B03002_003", "B03002_004",             # Total Population, Non-Hispanic White, Non-Hispanic Black
      paste0("B01001_0", str_pad(1:49, 2, "left", "0")),    # Population by gender by age group
      paste0("B15001_0", str_pad(1:83, 2, "left", "0")),    # Population 18+ by gender by educational attainment
      paste0("C27001A_0", str_pad(1:10, 2, "left", "0"))    # Population by health insurance by sex by age group
      )
  
  dput(vars)
  
  census_data <-
    get_acs(geography = "county",
            variables = vars,
            year = 2019,
            survey = "acs5") %>%
    left_join(list_vars, by = c("variable" = "name")) %>%
    select(-moe) %>%
    filter(!GEOID == "15005")        # Removing Kalawao County, Hawaii due to Lack of Information
  
  write_csv(census_data, file.path("Input", "census_data.csv"))
} 

# 5 -- Employment/Unemployment data by county
#   -- Data retrieved from the BLS using BLSapi
#   -- More details: https://cran.r-project.org/web/packages/blsAPI/blsAPI.pdf

if (!file.exists(file.path("Input", "laus_data.csv"))) {
  bls_key  <- "e54e539aed6045f29dbc51133d1ad7fe"
  
  counties <- sort(unique(census_data$GEOID))
  seriesid <- c(paste("LAUCN", counties, "000000000", 4, sep = ""),   # Employed
                paste("LAUCN", counties, "000000000", 5, sep = ""))   # Unemployed
  
  list_laus <- list()
  
  for (i in 1:( length(seriesid) / 50 + 1)) {
    
    start <- (i - 1) * 50 + 1
    end   <- start + 49
    
     if (end >= length(seriesid))
       end <- length(seriesid)
     
     payload  <- list("seriesid"  = seriesid[start:end],
                      "startyear" = 2019,
                      "endyear"   = 2020,
                      "registrationKey" = bls_key)
     
     list_laus[[i]] <- blsAPI(payload, api_version = 2, return_data_frame = TRUE)
     
     if (i %% 50 == 0)       # After 50 requests proceeds
       Sys.sleep(15)         # A waiting time due to API rule
     
     cat("Retrieving:", paste0(end / length(seriesid) * 100, "%"), "\n")
     
  }
  
  laus_data <- do.call("rbind", list_laus)
  
  write_csv(laus_data, file.path("Input", "laus_data.csv"))
}

# 6 -- COVID-19 Cases by county and day 
#   -- Data set from CSSE at Johns Hopkins University
#   -- More details: https://github.com/CSSEGISandData

if (!file.exists(file.path("Input", "time_series_covid19_confirmed_US.csv")))
  download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                destfile = file.path("Input","time_series_covid19_confirmed_US.csv"), mode = "wb")

# 7 -- COVID-19 Deaths by county and day 
#   -- Data set from CSSE at Johns Hopkins University
#   -- More details: https://github.com/CSSEGISandData

if (!file.exists(file.path("Input", "time_series_covid19_deaths_US.csv")))
  download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv",
                destfile = file.path("Input","time_series_covid19_deaths_US.csv"), mode = "wb")

# 8 -- Social Mobility by county and day
#   -- Data set from Google Community Mobility Reports
#   -- More details: https://www.google.com/covid19/mobility/

if (!file.exists(file.path("Input", "Region_Mobility_Report_CSVs.zip")))
  download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",
                destfile = file.path("Input","Region_Mobility_Report_CSVs.zip"))

# 9 -- 1980-2020 News Sentiment Analysis
#   -- Data set from Federal Reserve Bank of San Francisco
#   -- More details: https://www.frbsf.org/economic-research/indicators-data/daily-news-sentiment-index/

if (!file.exists(file.path("Input", "news_sentiment_data.xlsx")))
  download.file("https://www.frbsf.org/economic-research/indicators-data/daily-news-sentiment-index/files/news_sentiment_data.xlsx",
                destfile = file.path("Input", "news_sentiment_data.xlsx"), mode = "wb")

# 10 - Trump's Tweets from 2009 to 2021
#   -- Downloaded file from https://www.thetrumparchive.com/
#   -- The FAQ section provides the following Google Drive link
#   -- https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view

if (!file.exists(file.path("Input", "tweets_01-08-2021.json")))
  cat("File tweets_01-08-2021.json needed but absent")

# 11 - AFINN lexicon: New English AFINN wordlist
#   -- More details: https://github.com/fnielsen/afinn/tree/master/afinn/data

if (!file.exists(file.path("Input", "AFINN-en-165.txt")))
  download.file("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt",
                destfile = file.path("Input", "AFINN-en-165.txt"))