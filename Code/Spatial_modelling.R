# Spatial modelling

# Multivariate analysis
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

# Load modelling data frame- without columns to lag variables on
df_lagged <- read_csv("Processed_data/df_to_modelling.csv") %>% 
  filter(!is.na(country)) 


# Load shapefile for regions
shp <- st_read("Raw_data/NUTS_RG_60M_2016_3035/NUTS_RG_60M_2016_3035.shp") %>% 
  filter(LEVL_CODE == 2) %>% 
  filter(NUTS_ID %in% unique(df_lagged$nuts2))


# Join data frame with shapefile while keeping only those counties that are present in shapefile geometry data
df_lagged %<>% 
  left_join(shp, by = c("nuts2" = "NUTS_ID"))


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

# Remove regions with no neighbor from data frame
df_lagged %<>% 
  right_join(shp, by = c("nuts2" = "NUTS_ID"))


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp, queen = T)


# Create spatial weights for neighbors lists
listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


# Bivariate regression analysis for only significant factors
formula_bivariate <- as.formula(sum_populist ~
                                  econ_act_rate)


# Make a function to model linear regression per year
modelling_per_year <- function(i, formula) {
  reg1 <- lm(formula, df_lagged %>% 
                   filter(year == i)) 
  
  # Check r^2
  glance(summary(reg1))$r.squared
}

# Apply function
vector <- sapply(seq(2004, 2019, by = 5), modelling_per_year, formula = formula_bivariate)
vector


# Make a function to check if spatial dependency is present
moran_per_year <- function(i) {
  
  # Filter for a given year
  df_lagged_year <- df_lagged %>% 
    filter(year == i)
  
  # Remove regions with no data in them
  shp_func <- shp %>% 
    filter(NUTS_ID %in% unique(df_lagged_year$nuts2))
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)
  
  
  # Create spatial weights for neighbors lists
  listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  # Conduct moran test
  moran.test(df_lagged_year %>% 
               pull(sum_populist), 
             listw1)
}

# Apply function
vector_moran <- lapply(seq(2004, 2019, by = 5), moran_per_year)

# Apply names
names(vector_moran) <- paste("Test for", seq(2004, 2019, by = 5))

vector_moran # There is indeed spatial dependency

# Save results in txt format
sink("Results/moran_test.txt")
print(vector_moran)
sink()


# Create a function to check which spatial model should be of the best fit
lm_per_year <- function(i, formula) {
  df_lagged_year <- df_lagged %>% 
    filter(year == i)
  
  shp_func <- shp %>% 
    filter(NUTS_ID %in% unique(df_lagged_year$nuts2))
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)
  
  # Create spatial weights for neighbors lists
  listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  reg1 <- lm(formula, df_lagged_year) 

  # Lagrange Multiplier diagnostics
  lm.LMtests(reg1, listw1, test= "all")
}

# For some reason the function does not work on 2004 year (no neighbor regions found) so this year is, for now- only until it is debugged, removed from the analysis
vector_lm <- lapply(seq(2009, 2019, by = 5), lm_per_year, formula = formula_bivariate)

# Apply names
names(vector_lm) <- paste("Test for", seq(2009, 2019, by = 5))

vector_lm

# Save results in txt format
sink("Results/lm.txt")
print(lapply(vector_lm, summary))
sink()


# Multivariate analysis

# Prepare a vector of independent variables
r_side <- colnames(df_lagged_model)[8:15]

# Column names to check in formula: sum_farright, sum_eurosceptic, sum_populist
formula_multivariate <- as.formula(paste("sum_populist ~ ", paste(r_side, collapse= "+")))

vector_lm <- lapply(seq(2009, 2019, by = 5), lm_per_year, formula = formula_multivariate)

# Apply names
names(vector_lm) <- paste("Test for", seq(2009, 2019, by = 5))


# Save results in txt format
sink("Results/lm_multivariate.txt")
print(lapply(vector_lm, summary))
sink()


# Write the region shapefile
# st_write(SHP_regions, "Processed_data/Shapefiles/SHP_regions.shp", driver = "ESRI Shapefile", append = FALSE)

# Define panel data
panel <- pdata.frame(df_lagged, c("nuts2", "year"))


spml(formula_bivariate, data = panel, listw = listw1, , model = "within", lag = TRUE , spatial.error = "none")

fixed <- plm(formula_bivariate, data = panel, model="within")

random <- plm(formula_bivariate, data = panel, model="random")

# Perform Hausman Test
phtest(fixed, random)

# The model with fixed effects is better suited to the data
slmtest(formula_bivariate, data = panel, listw = listw1, model = "within", test = c("lme", "lml", "rlme", "rlml"))
