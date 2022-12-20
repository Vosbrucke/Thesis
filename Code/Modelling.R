# Modelling 


# Load libraries
library(rvest)
library(tidyverse)
library(magrittr)
library(janitor)
library(rebus)
library(rio)
library(corrplot)
library(broom)


# Load full data set
joined_fully <- read_csv("Processed_data/joined_regional_data.csv")


# Load European countries codes
eu_countries <- read.csv("Raw_data/countries_eu_inflation.csv")


# Calculate correlation
correlation <- joined_fully %>% 
  select(year, geo, gdp_regional, disp_income) %>% 
  group_by(year, geo)


# Correlation with gdp_regional for econ_act_rate is equal 0.59, and for disposable_household_income it is equal to 0.83
correlation %>% 
  filter(!is.na(gdp_regional) && !is.na(disp_income)) %$% 
  cor(gdp_regional, disp_income)


# Create a corrplot
joined_fully %>% 
  drop_na() %>% 
  select(-c(1,2, contains("country"))) %>% 
  cor() %>% 
  corrplot(tl.cex = 0.3, type = "upper")


# Calculate missing values in each column
is_nas <- joined_fully %>% 
  filter(!is.na(country_name)) %>% 
  mutate(is_na = across(everything(), is.na)) %>% 
  select(contains("is_na")) %>% 
  summarise(colSums(.)) %>% 
  mutate(name = colnames(joined_fully)) %>% 
  rename(na_sum = "colSums(.)") %>%
  # In total there are 6563 observations. Remove those columns where there are more than 1641 missing observations (25%)
  filter(na_sum < 1641)

# Pick only columns with lower number of missing values
joined_non_na <- joined_fully %>% 
  select(is_nas$name)


# Load elections data set
elections_pop <- read_csv("Processed_data/elections_pop.csv") %>% 
  filter(year >= 2000)


# Calculate percentage votes given on extreme parties
elections_extremes <- elections_pop %>% 
  mutate(
    vote_perc_farright = farright * vote_perc,
    vote_perc_eurosceptic = eurosceptic * vote_perc, 
    vote_perc_populist = populist * vote_perc
    ) %>% 
  group_by(country, year, nuts2, regionname) %>%
  summarise(
    sum_farright = sum(vote_perc_farright),
    sum_eurosceptic = sum(vote_perc_eurosceptic),
    sum_populist = sum(vote_perc_populist)
    )


# All parties joined
joined_elections_pop <- elections_pop %>% 
  full_join(joined_non_na, by = c("nuts2" = "geo", "year")) %>% 
  arrange(nuts2, year)


# Extremes joined
joined_elections_extremes <- elections_extremes %>% 
  full_join(joined_non_na, by = c("nuts2" = "geo", "year")) %>% 
  filter(year < 2020, country_name != "Turkey") %>%
  arrange(nuts2, year) 

df_lagged <- joined_elections_extremes %>% 
  group_by(nuts2) %>% 
  mutate(
    lag_1_gdp_regional = gdp_regional / lag(gdp_regional),
    lag_2_gdp_regional = gdp_regional / lag(gdp_regional, n = 2),
    lag_3_gdp_regional = gdp_regional / lag(gdp_regional, n = 3),
    lag_4_gdp_regional = gdp_regional / lag(gdp_regional, n = 4),
    lag_1_econ_act_rate = econ_act_rate / lag(econ_act_rate),
    lag_2_econ_act_rate = econ_act_rate / lag(econ_act_rate, n = 2),
    lag_3_econ_act_rate = econ_act_rate / lag(econ_act_rate, n = 3),
    lag_4_econ_act_rate = econ_act_rate / lag(econ_act_rate, n = 4)
    )
  # mutate(lagged = across(.cols = c("gdp_regional", "econ_act_rate", "HRST", "HTC", "participation_in_formal_and_or_non_formal_training_25_to_64_year_olds_population", "FMEDAGEPOP", "MEDAGEPOP", "MMEDAGEPOP"), ~ lag(.x)))

write_csv(df_lagged, "Processed_data/df_to_modelling.csv")

# Modelling preparation
for (i in seq(2004, 2019, by = 5)) {
  df_lagged_model <- df_lagged %>% 
    filter(!is.na(country)) %>% 
    select(-c(country_code, country_name)) %>% 
    filter(year == i)
  
  
  # Create a correlation plot
  cor_tibble <- cor(df_lagged_model[, 8:25], use = "complete.obs")
  
  
  # Create a png file
  png(paste0("Plots/correlation_plot_", i,".png"))
  
  p1 <- {
    corrplot(cor_tibble, method = "color",
             tl.cex = 0.3,
             )
    recordPlot()
  }
  
  # Save the plot
  dev.off()
}

# Prepare a vector of independent variables
r_side <- colnames(df_lagged_model)[8:25]


# Column names to check in formula: sum_farright, sum_eurosceptic, sum_populist
fmla <- as.formula(paste("sum_populist ~ ", paste(r_side, collapse= "+")))


# Make a function to calculate r^2 for linear regression models per year
modelling_per_year <- function(i) {
  lm_model <- lm(fmla, df_lagged %>% 
                   filter(!is.na(country)) %>% 
                   filter(year == i)) 
  glance(summary(lm_model))$r.squared
}


# Apply the function
vector <- sapply(seq(2004, 2019, by = 5), modelling_per_year)
vector


# Visualize the political extreme situation for Hungary
elections_extremes %>% 
  filter(country == "Hungary") %>% 
  ggplot(aes(year, sum_eurosceptic, color = nuts2)) +
  geom_line() 
