# Spatial modelling
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

# Load data frame of elections and other variables
df <- read_csv("Processed_data/df.csv") 

# Load shapefile of regions- it's done so map contains shade of other countries not listed in data
shp_0 <- st_read("Raw_data/NUTS_RG_60M_2016_3035/NUTS_RG_60M_2016_3035.shp")

# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp_0, queen = T)

# Check which regions had no neighbors
which_regions <- data.frame(is_neighbor = lapply(queen_neighbour, sum) > 0)

# Remove regions with no neighbor in shapefile
shp_0 %<>% 
  bind_cols(which_regions) %>% 
  filter(is_neighbor == T)

# Load shapefile for regions- this will be used to join with data frame 
shp <- st_read("Raw_data/NUTS_RG_60M_2016_3035/NUTS_RG_60M_2016_3035.shp") %>% 
  # Filter data only on for nuts 3 level
  filter(LEVL_CODE == 3) %>% 
  filter(NUTS_ID %in% unique(df$nuts2016))

# Retrieve coordinates in matrix form
coordinates <- st_coordinates(shp)

# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp, queen = T)

# Check which regions had no neighbors
which_regions <- data.frame(is_neighbor = lapply(queen_neighbour, sum) > 0)

# Remove regions with no neighbor in shapefile
shp %<>% 
  bind_cols(which_regions) %>% 
  filter(is_neighbor == T)

# Read data with spatial geometry
df_shp <- st_read("Processed_data/df_shp.shp") %>% #, driver = "ESRI Shapefile", append = FALSE, geometry_column = "geometry", stringAsFactors = FALSE)
  # For some reason with sf package two columns get relocated to the end of the data frame
  relocate(c(36, 37), .before = "LEVL_CO")

# Read data with column names (shapefiles are limited to 10 characters- column names are abbreviated)
df_shp_colnames <- read_csv("Processed_data/df_shp_colnames.csv") %>% pull()

# Set correct column names
df_shp %<>% 
  set_colnames(df_shp_colnames)

# Univariate modelling
{
# Univariate regression analysis
formula_univariate <- as.formula(growth_sum_eurosceptic ~
                                  growth_sum_populist)

# Make a function to calculate r^2 for linear regression model per year
lr_modelling <- function(i, formula) {
  lm_model <- lm(formula, df %>% 
                   filter(year == i)) 
  summary(lm_model)
}

# Apply the function
vector_lr <- lapply(seq(2009, 2019, by = 5), lr_modelling, formula = formula_univariate)

# Apply names
names(vector_lr) <- paste("Test for", seq(2009, 2019, by = 5))

# Save results in txt format
sink("Results/lr_univariate.txt")
print(vector_lr)
sink()

# Create a function to check which spatial model should be of the best fit
spatial_modelling <- function(i, formula) {
  df_per_year <- df %>% 
    filter(year == i) %>% 
    na.omit()
  
  shp_func <- shp %>% 
    filter(NUTS_ID %in% unique(df_per_year$nuts2016))
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  shp_func %<>% 
    bind_cols(which_regions) %>% 
    filter(is_neighbor_again == T)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)

  
  # Create spatial weights for neighbors lists
  listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  # Remove regions without neighbors
  df_per_year_2 <- df_per_year %>% 
    filter(nuts2016 %in% shp_func$NUTS_ID)
  
  reg1 <- lm(formula, df_per_year_2) 
  
  # Lagrange Multiplier diagnostics
  lm.LMtests(reg1, listw1, test= "all")
}

# For some reason the function does not work on 2004 year (no neighbor regions found) so this year is, for now- only until it is debugged, removed from the analysis
vector_lm <- lapply(seq(2009, 2019, by = 5), spatial_modelling, formula = formula_univariate)

# Apply names
names(vector_lm) <- paste("Test for", seq(2009, 2019, by = 5))

vector_lm

# Save results in txt format
sink("Results/lm_univariate.txt")
print(lapply(vector_lm, summary))
sink()
}

# Spatial test
{
# Determining if spatial dependency is present
moran_per_year <- function(i) {
  
  # Filter for a given year
  df_shp_year <- df_shp %>% 
    filter(year == i) %>% 
    filter(!is.na(growth_eurosceptic_p_perc))
  
  # Remove regions with no data in them
  shp_func <- shp %>% 
    filter(NUTS_ID %in% unique(df_shp_year$nuts2016))
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)
  
  
  # Create spatial weights for neighbors lists
  listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  # Conduct moran test
  moran.test(df_shp_year$growth_eurosceptic_p_perc, 
             listw1)
  
}

# Apply function
vector_moran <- lapply(seq(2009, 2019, by = 5), moran_per_year)

# Apply names
names(vector_moran) <- paste("Test for", seq(2009, 2019, by = 5))

vector_moran # There is indeed spatial dependency

# Save results in txt format
sink("Results/moran_test.txt")
print(vector_moran)
sink()
}

# Multivariate modelling
{
# Prepare a vector of independent variables
r_side <- df_shp %>% as.data.frame() %>% 
  select(15, 16, 18:26, 28) %>%
  colnames() 

r_side

# Column names to check in formula: sum_farright, sum_eurosceptic, sum_populist
formula_multivariate <- as.formula(paste("growth_eurosceptic_p_perc ~ ", paste(r_side, collapse= "+")))

vector_lm <- lapply(seq(2009, 2019, by = 5), spatial_modelling, formula = formula_multivariate)

# Apply names
names(vector_lm) <- paste("Test for", seq(2009, 2019, by = 5))

# Save results in txt format
sink("Results/lm_multivariate.txt")
print(lapply(vector_lm, summary))
sink()


# Apply the function
vector_lr <- lapply(seq(2009, 2019, by = 5), lr_modelling, formula = formula_multivariate)

# Apply names
names(vector_lr) <- paste("Test for", seq(2009, 2019, by = 5))

# Save results in txt format
sink("Results/lr_multivariate.txt")
print(vector_lr)
sink()
}

# Spatial modelling
{
all_models <- function(i, formula) {
# According to LM tests Spatially Error Model should be the best model 
  df_shp_year <- df_shp %>% 
    ungroup() %>% 
    # unnest(cols = dev_mean, names_sep = "_") %>% 
    filter(year == i)

  df_shp_year <- na.omit(df_shp_year)
  
  shp_func <- shp %>% 
    filter(NUTS_ID %in% unique(df_shp_year$nuts2016))
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  shp_func %<>% 
    bind_cols(which_regions) %>% 
    filter(is_neighbor_again == T)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)
  
  # Create spatial weights for neighbors lists
  listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  # Remove regions without neighhbours
  df <- df_shp_year %>% 
    filter(nuts2016 %in% shp_func$NUTS_ID)

# SLX
reg2 = lmSLX(formula, data = df, listw1) 

summary(reg2)

# SAR
reg3 <- lagsarlm(formula, data = df, listw1) 

summary(reg3)

# SEM
reg4 <- errorsarlm(formula, data=df, listw1) 

tidy(summary(reg4)) %>% arrange(estimate)

glance(summary(reg4))

# With this model only Hausman test can be performed
Hausman.test(reg4) 


# SDEM
reg5 <- errorsarlm(formula, data = df, listw1, etype = "emixed")

summary(reg5)

impacts(reg5,listw=listw1)

summary(impacts(reg5, listw=listw1, R=500),zstats=TRUE)


# SDM
reg6 <- lagsarlm(formula, data = df, listw1, type="mixed") 

summary(reg6)

# MANSKI
reg7 <- sacsarlm(formula, data = df, listw1, type="sacmixed") 

summary(reg7)

# SARAR
reg8 <- sacsarlm(formula, data = df, listw1, type="sac") 

summary(reg8)

# Ultimate test for reduction of complexity- the bigger (up+) log likelihood the better: -783 > -821
LR.Sarlm(reg5, reg3)

# Final decision on spatial regression analysis with all factors
# The Spatial Error Model has the best fit out of all the models. Therefore SDEM is used.
summary(reg5) 

glance(summary(reg5)) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

summary(impacts(reg5,listw=listw1,R=500),zstats=TRUE)
}

vector_spatial_modelling <- sapply(seq(2004, 2019, by = 5), all_models, formula = formula_univariate)

names(vector_spatial_modelling) <- paste("Test for", seq(2004, 2019, by = 5))

vector_spatial_modelling

# Pseudo r square calculations for each models
1-(reg3$SSE/(var(df$growth_sum_eurosceptic)*(length(df$growth_sum_eurosceptic)-1)))

1-(reg4$SSE/(var(df$growth_sum_eurosceptic)*(length(df$growth_sum_eurosceptic)-1)))

1-(reg4$SSE/(var(df$growth_sum_eurosceptic)*(length(df$growth_sum_eurosceptic)-1)))

1-(reg5$SSE/(var(df$growth_sum_eurosceptic)*(length(df$growth_sum_eurosceptic)-1)))

1-(reg6$SSE/(var(df$growth_sum_eurosceptic)*(length(df$growth_sum_eurosceptic)-1)))

1-(reg7$SSE/(var(df$growth_sum_eurosceptic)*(length(df$growth_sum_eurosceptic)-1)))

1-(reg8$SSE/(var(df$growth_sum_eurosceptic)*(length(df$growth_sum_eurosceptic)-1)))

# We would choose the 5th model- SDEM based on it's Akaike Information Criterium and good fit

# Heteroskedastity Breusch-Pagan test
bptest.Sarlm(reg5, studentize = TRUE)

# We can assume that residuals are homoscedastic

# Spatial dependency on the residuals from lm model
df_residuals_OLS <- read_csv("Processed_data/linear_regression_prediction_OLS.csv")

# Join data frame with shp. Delete the counties not present in geospatial data
df_residuals_OLS %<>%
  right_join(shp, by = c("county" = "county", "WOJ.x" = "code"))

# Run moran test for residuals
moran.test(df_residuals_OLS$residuals, listw1)

# Run Monte-Carlo simulation of Moran I
moran.mc(df_residuals_OLS$residuals, listw1, nsim=599)

# 
# # Predict values
# distribution_prediction <- df %>%
#   mutate(
#     predicted_values_SEM = predict(reg4, data = .),
#     residuals = growth_sum_eurosceptic - predicted_values_SEM
#   )
# 
# # Extreme residuals for SEM
# distribution_prediction$residuals %>% summary()
# 
# # Check the residual distribution spatial dependency
# moran.test(distribution_prediction$residuals, listw1)
# 
# # Write csv with predicted values and residuals for SEM
# write_csv(distribution_prediction %>% select(1:4, growth_sum_eurosceptic, predicted_values_SEM, residuals), "Processed_data/spatial_ditribution_prediction_SEM.csv")  
}

# Check correlations in between selected variables
t <- GGally::ggpairs(df_shp %>% ungroup() %>% select(growth_eurosceptic_p_perc, r_side), title = "Correlation in between choosen variables")

ggsave("Plots/Correlations.png", t, dpi = 900, height = 30, width = 30, units = "cm")
