rm(list=ls())
options(scipen = 10^5)

library(tidyverse)
library(stringr)
library(lubridate)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
#wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)

##########################################################
# US Presidential Election Results by state, 1976 - 2020 #
##########################################################

# Loading Electoral College by State
college_bystate <-
  read_csv(file.path("Input","1976-2020-electoral-college.csv")) %>%
  rename(year = column_label) %>%
  mutate(state_name = str_replace(state_name, " of ", " Of "))

# Loading/Wrangling Data of Popular Vote, 1976-2020
panel_elections <-
  read_csv(file.path("Input","1976-2020-president.csv")) %>%
  rename(state_lab = state_po, party = party_simplified, absolute = candidatevotes) %>%
  mutate(share = absolute / totalvotes,
         party = str_to_title(party),
         state = str_to_title(state),
         candidate = ifelse(candidate == "MITT, ROMNEY", "ROMNEY, MITT", candidate)) %>%
  filter(party %in% c("Democrat", "Republican")) %>%
  select(year, state, state_lab, candidate, party, share, absolute) %>%
  pivot_longer(cols = c(share, absolute), names_to = "type", values_to = "votes") %>%
  group_by(year, state, state_lab, type) %>%
  mutate(winner = ifelse(votes == max(votes), 1L, 0L)) %>%
  ungroup() %>%
  left_join(college_bystate, by = c("year" = "year", "state" = "state_name"))              # Adding Electoral Votes data

# Saving intermediate data
write_csv(panel_elections, file.path("Intermediate", "1976-2020_panel_elections.csv"))

##########################################################
### Analysis of 2020 US Presidential Election Results ####
##########################################################

# Loading election results by county
# -> Y-var: Republican margin vote [gop_margin]
results_2020 <-
  read_csv(file.path("Input","results_2020.csv")) %>%
  select(GEOID = county_fips,
         county = county_name,
         state = state_name,
         gop_margin = per_point_diff,      
         total_votes                       
         )      

# Loading Socio/Demographic/Economic data by county
# -> X-var: population (count), urban/rural (boolean), women (%), foreign born (%),
# --------- college degree (%), race/ethnicity (%), household income (log), gini (index 0-1),

census_data <-
  read_csv(file.path("Input","census_data.csv")) %>%
  mutate(label = str_replace_all(label, c("(:|!)+" = "_", "(:|!|\\_)?$" = ""))) %>%           # Cleaning separator
  separate(label, c("Estimate", "layer1", "layer2", "layer3", "layer4"),  sep = "\\_") %>%    # Splitting label variable
  separate(variable, c("section", "variable"),  sep = "\\_") %>%                              
  mutate(variable = as.numeric(variable))

# Calculating Total Population and a proxy of Urban/Rural
# based on the size of population (threshold = 65k)
population <-
  census_data %>%
  filter(section == "B01001", variable == 1) %>%
  mutate(urban = estimate > 65000) %>%
  select(GEOID, pop = estimate, urban)    

# Calculating percentage of women
sex <-
  census_data %>%
  filter(section == "B01001", !is.na(layer2), is.na(layer3), is.na(layer4)) %>%           # Keeping only aggregate number
  group_by(GEOID, sex = layer2) %>%
  summarise(pop = sum(estimate, na.rm = TRUE)) %>%
  mutate(share = pop / sum(pop)) %>%                        # Calculating percentage                      
  ungroup() %>%
  filter(sex == "Female") %>%
  select(GEOID, share_women = share)     

# Calculating percentage of age groups
age <-
  census_data %>%
  filter(section == "B01001", !is.na(layer3)) %>%
  mutate(age_group = case_when(variable %in% c(3:6,   27:30) ~ "age_00_17", # 00 to 17 years    # Defining Age Groups
                               variable %in% c(7:10,  31:35) ~ "age_18_24", # 18 to 24 years
                               variable %in% c(11:14, 36:39) ~ "age_25_44", # 25 to 44 years
                               variable %in% c(15:19, 40:43) ~ "age_45_64", # 45 to 64 years
                               variable %in% c(20:25, 44:49) ~ "age_65_99"  # 65+      years
                               )) %>%
  group_by(GEOID, age_group) %>%
  summarise(pop = sum(estimate, na.rm = TRUE)) %>%
  mutate(share = pop / sum(pop)) %>%
  ungroup() %>%
  select(-pop) %>%
  pivot_wider(id_cols = GEOID, names_from = age_group, values_from = share)

# Calculating percentage of foreign born 
foreign <-
  census_data %>%
  filter(section == "B05002") %>%
  select(GEOID, layer2, estimate) %>%
  pivot_wider(id_cols = GEOID, names_from = layer2, values_from = estimate) %>%
  mutate(foreign_share = `Foreign born` / `NA`) %>%       # Calculating percentage
  select(GEOID, foreign_share)

# Calculating percentage of college degree 
college_degree <-
  census_data %>%
  filter(section == "B15001", !is.na(layer4)) %>%
  mutate(educ_level = case_when(layer4 %in% c("Associate's degree",                     # Defining: College Degree or More
                                              "Bachelor's degree",
                                              "Graduate or professional degree") ~ 1,
                              TRUE ~ 0)) %>%
  group_by(GEOID, educ_level) %>%
  summarise(pop = sum(estimate, na.rm = TRUE)) %>%
  mutate(share = pop / sum(pop)) %>%                     # Calculating percentage
  ungroup() %>%
  filter(educ_level == 1) %>%
  select(GEOID, share_college = share)

# Calculating percentage of non-Hispanic White and non-Hispanic Black
race_ethnicity <-
  census_data %>%
  filter(section == "B03002") %>%
  pivot_wider(id_cols = GEOID, names_from = layer3, values_from = estimate) %>%
  mutate(share_white = `White alone` / `NA`,
         share_black = `Black or African American alone` / `NA`) %>%
  select(GEOID, share_white, share_black)

# Calculating the log of household income
hh_income <-
  census_data %>%
  filter(section == "B19013") %>%
  mutate(log_hh_income = log(estimate + 1)) %>%
  select(GEOID, log_hh_income)

# Calculating Gini index
gini <-
  census_data %>%
  filter(section == "B19083") %>%
  select(GEOID, gini =  estimate)

# Loading Employment data by county, Jan19-Dec-20
# -> X-var: Change in unemployment rate Jan-Oct 2020 vs. Jan-Oct 2019

laus_data <-
  read_csv(file.path("Input","laus_data.csv")) %>%
  mutate(period = parse_number(period),
         seriesID = parse_number(seriesID),                         # seriesID contains: fips code & status of employment
         GEOID = seriesID %/% 10000000000,                              
         GEOID = str_pad(GEOID, width = 5, side = "left", pad = 0),
         type  = seriesID %% 10000000000,
         type = case_when(type == 4 ~ "unemployed",
                          type == 5 ~ "employed")) %>%
  filter(!period == c(11, 12)) %>%                          # Removing November and December months from both years
  group_by(GEOID, year, type) %>%
  summarise(jobs = mean(value, na.rm = TRUE)) %>%           # Average by GEOID/year/type
  mutate(share = jobs / sum(jobs, na.rm = TRUE)) %>%
  filter(type == "unemployed") %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(delta_unemployment = share - lag(share, 1)) %>%    # Avg. Unemployment Rate Jan-Oct '20 - Avg. Unemployment Rate Jan-Oct '19
  filter(year == 2020) %>%
  select(GEOID, delta_unemployment)

# Loading COVID-19 confirmed cases and deaths by county
# -> X-var: COVID-19 cases per 10k people

covid_cases <-
  read_csv(file.path("Input","time_series_covid19_confirmed_US.csv")) %>%
  select(GEOID = FIPS, cases = `10/31/20`) %>%                             # Cumulative confirmed cases until end of October
  mutate(GEOID = str_pad(GEOID, width = 5, side = "left", pad = 0)) %>%
  right_join(population, by = "GEOID") %>%
  mutate(cases_per10k = cases / pop * 10000) %>%
  select(GEOID, cases_per10k)

# -> X-var: COVID-19 deaths per 100k people
covid_deaths <-
  read_csv(file.path("Input","time_series_covid19_deaths_US.csv")) %>%
  select(GEOID = FIPS, deaths = `10/31/20`) %>%                            # Cumulative death cases until end of October
  mutate(GEOID = str_pad(GEOID, width = 5, side = "left", pad = 0)) %>%
  right_join(population, by = "GEOID") %>%
  mutate(deaths_per100k = deaths / pop * 100000) %>%
  select(GEOID, deaths_per100k)

# Loading Community Mobility data to Work places by county
# -> X-var: Change in community mobility between cases/deaths peak and baseline (double)
# --------- Baseline     : 30 days mean centered in late February
# --------- Deaths' Peak : 30 days mean centered in mid April
# --------- Cases' Peak  : 30 days mean centered in mid August

mobility_data <-
  read_csv(unzip(file.path("Input","Region_Mobility_Report_CSVs.zip"),
                 "2020_US_Region_Mobility_Report.csv")) %>%
  mutate(stage = case_when(between(date, ymd("2020-02-15"), ymd("2020-03-15")) ~ "baseline",    # Defining relevant time frames
                           between(date, ymd("2020-04-07"), ymd("2020-05-07")) ~ "deaths_peak",
                           between(date, ymd("2020-08-07"), ymd("2020-09-07")) ~ "cases_peak",
                           TRUE ~ "No Relevant")) %>%
  filter(!is.na(census_fips_code), !stage == "No Relevant") %>%
  group_by(GEOID = census_fips_code, stage) %>%
  summarise(work_mobility = mean(workplaces_percent_change_from_baseline, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = GEOID, names_from = stage, values_from = work_mobility) %>%
  mutate(mobility_deaths_peak = deaths_peak - baseline,        # Calculating difference in mobility Deaths' Peak vs. Baseline
         mobility_cases_peak = cases_peak - baseline) %>%      # Calculating difference in mobility Cases'  Peak vs. Baseline
  select(GEOID, starts_with("mobility"))


# Constructing master dataset including Y-variable and the set of constructed X-variables
election_2020_county <-
  left_join(results_2020, population, "GEOID") %>%
  left_join(sex, "GEOID") %>%
  left_join(age, "GEOID") %>%
  left_join(foreign, "GEOID") %>%
  left_join(college_degree, "GEOID") %>%
  left_join(race_ethnicity, "GEOID") %>%
  left_join(hh_income, "GEOID") %>%
  left_join(gini, "GEOID") %>%
  left_join(laus_data, "GEOID") %>%
  left_join(covid_cases, "GEOID") %>%
  left_join(covid_deaths, "GEOID") %>%
  left_join(mobility_data, "GEOID") %>%
  filter(!state == "Alaska")             # Votes (Y-var) in Alaska is based on Electoral Districts,
                                         # then mismatching explanatory variables based on counties

# Saving intermediate data
write_csv(election_2020_county, file.path("Intermediate", "election_2020_county.csv"))
