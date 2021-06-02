rm(list=ls())
options(scipen = 10^5)

library(tidyverse)
library(caret)
library(kableExtra)
library(stargazer)
library(rpart)
library(rpart.plot)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project"

setwd(wd)

##########################################################
###### Linear Regression: 2020 Republican Margin  ########
##########################################################

## Data Preparation

# Defining vector with mobility variables
mobility_vars <- c("mobility_deaths_peak", "mobility_cases_peak")

# Retrieving dataset including Y-variable and the set of constructed X-variables
election_2020 <-
  read_csv(file.path("Intermediate", "election_2020_county.csv")) %>%
  mutate(state = factor(state),                         ## Factor transformation
         pop = log(pop + 1),                            ## Log transformation
         total_votes = log(total_votes + 1)             ## Log transformation
         ) %>%
  select(-c(GEOID, county, age_00_17)) %>%              ## Removing non-relevant variables
  mutate_at(all_of(mobility_vars),                      ## Dealing with NA's: replacing them by 0's 
            ~replace(., is.na(.), 0))

## Performing Regressions

# Null Model: Only Intercept as explanatory variable
lm_null <- lm(gop_margin ~ 1, data = election_2020)

# Full Model: Includes all available explanatory variables
lm_full <- lm(gop_margin ~ . , data = election_2020)

# Statistically Optimal Model: From null to full model adding 1 variable
# until BIC measure is not longer improved

lm_optimal <- step(lm_null, # starting mode
                   scope = formula(lm_full), # largest model
                   direction = "forward",
                   k = log(nrow(election_2020)), # BIC metric
                   trace = 0)

# Conservative Model: Excluding recoded variables
lm_exc_mob <- lm(gop_margin ~ . , data = select(election_2020, -mobility_vars))

## Printing Results

# Labels for explanatory variables
labels <- c("Log(total votes)", "Log(total population)", "Urban/Rural",
            "Female share", "Age 18-24", "Age 25-44", "Age 45-64", "Age 65+",
            "Foreign born share", "College degree share", "Non-Hispanic White",
            "Non-Hispanic Black", "Log(household median income)", "Gini index",
            "$\\Delta$ unemployment rate", "COVID cases x 10k", "COVID deaths x 100k",
            "$\\Delta$ Mobility in deaths' peak","$\\Delta$ Mobility in cases' peak")

# Table of Results
linear_table <- stargazer(lm_exc_mob, lm_full, lm_optimal,
                          type  = "text",
                          header = FALSE,
                          digits = 4,
                          omit = c("state"),
                          omit.labels = "State fixed effects?",
                          omit.yes.no = c("Yes", "No"),
                          no.space = TRUE,
                          df = FALSE,
                          star.cutoffs = c(0.05, 0.01, 0.001),
                          dep.var.labels = c("2020 Republican Margin Vote (pp.)"),
                          column.labels   = c("Without Mobility", "Full", "Optimal"),
                          covariate.labels = labels,
                          style = "qje",
                          title = "Linear Regresion results controllying by state",
                          out = file.path("Output", "Regression_Results.html"))

##########################################################
######## Tree Regression: 2020 Republican Margin  ########
##########################################################

# Performing a simple tree regression: An "If-else" algorithm
tree_fit = rpart(gop_margin ~ .,
             data = select(election_2020, -state),
             minsplit = 50,  # At least 50 observations in a node
             cp = 0.01)      # Minimum threshold for accepting improvements

# Plotting the Decision Tree regression
rpart.plot(tree_fit, type = 1, extra = 1)

