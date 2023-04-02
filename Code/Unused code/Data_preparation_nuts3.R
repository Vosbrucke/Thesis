# Data preparation NUTS3

# Load libraries
library(rvest)
library(tidyverse)
library(magrittr)
library(janitor)

# Load european countries codes
eu_countries <- read.csv("Raw_data/countries_eu_inflation.csv")

# Data on employees in regions 
df_employment <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts3/Employment_by_thousands_of_people.csv") %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(unit, geo) %>%
  set_colnames(make_clean_names(colnames(.))) %>% 
  arrange(geo, time_period) %>% 
  ungroup() %>% 
  select(geo, time_period, employment = obs_value)

# Reading population data on feminity and elderly rates
df_pop_groups <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts3/Population_with_groups.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  pivot_wider(names_from = c(sex, age), values_from = obs_value) %>% 
  mutate(
    TOTAL = F_TOTAL + M_TOTAL, 
    GE65 = F_Y_GE65 + M_Y_GE65, 
    ELDER_RATE = GE65 / TOTAL,
    FEM_RATE = M_TOTAL / F_TOTAL * 100
  ) %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  inner_join(df_employment, by = c("geo", "time_period")) %>% 
  mutate(employment_rate = employment * 1000 / total) %>% 
  select(country_code, geo, time_period, population = total, employment, employment_rate, ge65, elder_rate, fem_rate)

# GDP of regions
df_gdp <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts3/GDP.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  inner_join(df_pop_groups, by = c("geo", "time_period")) %>% 
  select(country_code, geo, time_period, gdp = obs_value, population, employment, employment_rate, ge65, elder_rate, fem_rate)

# Migration in regions
df_migration <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts3/Rate_of_migration.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  inner_join(df_gdp, by = c("geo", "time_period")) %>% 
  select(country_code, geo, time_period, gdp, population, employment, employment_rate, ge65, elder_rate, fem_rate, migration_rate = obs_value)

# Population density in regions
df_density <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts3/Pop_density.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  full_join(df_migration, by = c("geo", "time_period")) %>% 
  select(country_code, geo, time_period, gdp, population, employment, employment_rate, ge65, elder_rate, fem_rate, migration_rate, pop_density = obs_value) %>% 
  group_by(geo, time_period) %>%
  filter(row_number() == 1) %>%
  ungroup() %>% 
  group_by(geo) %>%
  # Add lagged variables of economic factors and a social factor
  dplyr::mutate(
    gdp_lag_4 = gdp / dplyr::lag(gdp, n = 4),
    employment_rate_lag_4 = employment_rate / dplyr::lag(employment_rate, n = 4),
    migration_rate_lag_4 = migration_rate / dplyr::lag(migration_rate, n = 4)
  ) %>% 
  # Remove infinite 
  filter(!migration_rate_lag_4 %in% c("-Inf", "Inf")) %>% 
  # Remove unnecessary countries and years
  filter(country_code %in% eu_countries$code) %>% 
  filter(time_period > 2003)

# Elections data frame
df_elections <- read_csv("Processed_data/Extremes_nuts3.csv") %>% 
  filter(!is.na(country)) %>% 
  filter(!year %in% c(2007, 2013)) %>% 
  group_by(nuts2016) %>% 
  # Add a column for growth rate
  dplyr::mutate(
    growth = dplyr::across(dplyr::contains("sum"), ~ ifelse(!is.na(. / dplyr::lag(.)), . / dplyr::lag(.), 0))
  ) %>% 
  ungroup() %>% 
  # Add a column for deviation from the mean
  dplyr::mutate(
    dev_mean = dplyr::across(dplyr::contains("sum"), ~ (. - mean(., na.rm=TRUE)) * 100)
  ) %>% 
  unnest(cols = c(growth, dev_mean), names_sep = "_") %>% 
  # Change 'Inf' values to the new value
  mutate(dplyr::across(dplyr::contains("growth"), ~ ifelse(. == "Inf", sum_eurosceptic, .))) %>% 
  ungroup()

# Join elections data frame with variables
df <- df_elections %>% 
  group_by(country, year) %>% 
  left_join(df_density, by = c("nuts2016" = "geo", "year" = "time_period"))

# Checking how many NAs there are in the data frame per year
df_lagged_na <- function(i) {
  df_lagged %>% 
    filter(year == i) %>% 
    select(!contains("dev")) %>% 
    ungroup() %>% 
    mutate(is_na = across(everything(), is.na)) %>% 
    select(contains("is_na")) %>% 
    dplyr::summarise(colSums(.)) %>% 
    mutate(name = colnames(df_lagged %>% select(!contains("dev")))) %>%
    rename(na_sum = "colSums(.)") %>% 
    select(name, na_sum)
}

# Apply function to find in which years we can find the most missing values
df_lagged_na_res <- lapply(seq(2004, 2019, by = 5), df_lagged_na)

# Apply names for lists
names(df_lagged_na_res) <- paste("Number of missing values for the year of", seq(2004, 2019, by = 5))

# View the list
df_lagged_na_res

write_csv(df, "Processed_data/df.csv")
