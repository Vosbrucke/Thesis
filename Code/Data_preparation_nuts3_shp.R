# Visualizations

# Loading dependencies
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

# Spatial mapping data preparation

# Load data frame of elections and other variables
df <- read_csv("Processed_data/df.csv")

# Load shapefile of regions- it's done so map contains shade of other countries not listed in data
shp_0 <- st_read("Raw_data/NUTS_RG_60M_2016_3035/NUTS_RG_60M_2016_3035.shp")

# Retrieve coordinates in matrix form
coordinates <- st_coordinates(shp_0)

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

# Join data frame with shapefile while keeping only those counties that are present in shapefile geometry data
shp_1 <- shp %>% 
  left_join(df, by = c("NUTS_ID" = "nuts2016"))

# Remove regions with no neighbor from data frame. 
df_shp <- df %>% 
  right_join(shp, by = c("nuts2016" = "NUTS_ID"))

# Write data with spatial geometry
sf::st_write(df_shp, "Processed_data/df_shp.shp") #, driver = "ESRI Shapefile", append = FALSE)

# Write column names (shapefiles are limited to 10 characters- column names in st_write funtion get abbreviated)
write_csv(as.data.frame(colnames(df_shp)), "Processed_data/df_shp_colnames.csv")

# # I will need to impute missing data. For this I will use Hmisc package. The graphs of missing values per country enable to understand what the scope of imputation per country is done
# # Because of the high chance of error countries with the highest percentage of missing value will not be taken into consideration in the imputation and thus in the analysis
# 
# library(Hmisc)
# 
# impute_arg <- aregImpute(~ gdp, data = df_shp %>% filter(year == 2009) %>% pull(gdp), n.impute = 5)
# 
# library(imputeTS)
# 
# df_shp_imputated<- na_interpolation(df_shp %>% filter(year == 2009, country != "Ireland") %>% pull(gdp))
# summary(df_shp)
# summary(df_shp_imputated)
# ggplot_na_imputations(df_shp %>% filter(year == 2009) %>% pull(gdp), df_shp_imputated)
# 
# library(mi)
# 
# imputation <- function(i) {
#   mi(df_shp %>% filter(country == i, year == 2019) %>% as.data.frame() %>% select(r_side), seed = 335)
# }
# 
# test_imputation <- lapply(df_shp$country %>% unique(), imputation)
# summary(mi_data)
