# NUTS2 oanel analysis

# Load libraries
library(tidyverse)
library(broom)
library(kableExtra)
library(magrittr)
library(ggtext)
library(sf)
library(spdep)
library(spatialreg)
library(splm)
library(spdep)
library(plm)
library(janitor)
library(zoo)
library(lmtest)

# Read the dataset
df <- read_csv("Processed_data/df_nuts2.csv")

# Define univariate formula
formula_univariate <- growth_farright_p_perc ~ growth_populism_p_perc + growth_eurosceptic_p_perc

# Define multivariate formula
r_side <- df %>% 
  # select(3, 15, 17:23) %>%
  select(
    gdp, unemployment_rate, elder_rate, fem_rate, migration_rate, pop_density, 
    gdp_lag_4, employment_rate_lag_4, migration_rate_lag_4, 
    migration_unemployment_rate_15_64_year_olds_both_sex_foreign_born_from_outside_eu_27_countries, 
    ) %>% 
  colnames() 

formula_multivariate <- as.formula(paste("growth_eurosceptic_p_perc ~ ", paste(r_side, collapse= "+")))
