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
library(janitor)


# Load european countries codes
eu_countries <- read.csv("Raw_data/countries_eu_inflation.csv")

# Data on regional GDP to join
# Data on employees in regions- I need to divide it on a population number 
df_to_join <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts3/Employment_by_thousands_of_people.csv") %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  inner_join(eu_countries, by = c("country_code" = "code")) %>% 
  group_by(unit, geo) %>%
  set_colnames(make_clean_names(colnames(.))) %>% 
  arrange(geo, time_period) %>% 
  ungroup() %>% 
  select(geo, time_period, employment = obs_value)

df_pop <- read_csv("/Users/mr.fox/Desktop/Github/Thesis/Raw_data/Nuts3/Population.csv") %>% 
  set_colnames(make_clean_names(colnames(.))) %>% 
  mutate(country_code = str_sub(geo, 1, 2)) %>% 
  inner_join(df_to_join, by = c("geo", "time_period")) %>% 
  mutate(employment_rate = employment * 1000 / obs_value) %>% 
  select(country_code, geo, time_period, obs_value, employment, employment_rate)

# Load modelling data frame- without columns to lag variables on
df_lagged <- read_csv("Processed_data/Extremes_nuts3.csv") %>% 
  filter(!is.na(country)) %>% 
  filter(!year %in% c(2007, 2013)) %>% 
  left_join(df_to_join, by = c("nuts2016" = "geo", "year" = "time_period"))

  


# Load shapefile for all regions for maps
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


# Load shapefile for regions
shp <- st_read("Raw_data/NUTS_RG_60M_2016_3035/NUTS_RG_60M_2016_3035.shp") %>% 
  filter(LEVL_CODE == 3) %>% 
  filter(NUTS_ID %in% unique(df_lagged$nuts2016))


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
shp %<>% 
  left_join(df_lagged, by = c("NUTS_ID" = "nuts2016"))


# Remove regions with no neighbor from data frame
df_lagged %<>% 
  right_join(shp, by = c("nuts2016" = "NUTS_ID"))


# Construct neighbors list from polygon list
queen_neighbour <- poly2nb(shp, queen = T)


# Create spatial weights for neighbors lists
listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)


# Bivariate regression analysis for only significant factors
formula_bivariate <- as.formula(sum_populist.x ~
                                  regional_gdp.x)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "white",
                                     color = "white"),
      panel.background = element_rect(fill = "white",
                                      color = "white"),
      legend.background = element_rect(fill = "white",
                                       color = "white"),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11, color = "#494E4F"),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "#494E4F"),
      plot.title = element_text(size = 15, hjust = 0,
                                color = "#494E4F"),
      # plot.title.position = "plot",
      # plot.caption.position = "plot",
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#494E4F",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

# RUN ALL
{
# Mapping eurosceptism in the selected countries
map_per_year_eu_scept <- function(i) {
ggplot(shp %>% filter(year == i)) +
  geom_sf(data = shp_0 %>% filter(!CNTR_CODE %in% c("FR", "PT")), fill = "#ECECE9", color = "white") +
  geom_sf(aes(fill = sum_eurosceptic), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(limits = c(0,1), option = "magma", direction = -1) +
  labs(
    x = "", 
    y = "", 
    title = paste("Spatial distribution of percentage of votes on eurosceptism parties\nin elections to European Parliament", "in", i),
    ) +
  theme_map() +
  guides(fill = guide_colourbar(title = "Percentage of votes"))
  
  ggsave(paste0("Plots/Election_results_distribution/Eurosceptism_spatial_distribution_in_", i, ".png"), dpi = 900)
}

lapply(seq(2004, 2019, by = 5), map_per_year_eu_scept)

# Mapping farright in the selected countries
map_per_year_farright <- function(i) {
  ggplot(shp %>% filter(year == i)) +
    geom_sf(data = shp_0 %>% filter(!CNTR_CODE %in% c("FR", "PT")), fill = "#ECECE9", color = "white") +
    geom_sf(aes(fill = sum_farright), color = "white", linewidth = 0.1) +
    scale_fill_viridis_c(limits = c(0,1), option = "magma", direction = -1) +
    labs(
      x = "", 
      y = "", 
      title = paste("Spatial distribution of percentage of votes on farright parties\nin elections to European Parliament", "in", i),
    ) +
    theme_map() +
    guides(fill = guide_colourbar(title = "Percentage of votes"))
  
  ggsave(paste0("Plots/Election_results_distribution/Farright_spatial_distribution_in_", i, ".png"), dpi = 900)
}

lapply(seq(2004, 2019, by = 5), map_per_year_farright)

# Mapping populism in the selected countries
map_per_year_populism <- function(i) {
  ggplot(shp %>% filter(year == i)) +
    geom_sf(data = shp_0 %>% filter(!CNTR_CODE %in% c("FR", "PT")), fill = "#ECECE9", color = "white") +
    geom_sf(aes(fill = sum_populist), color = "white", linewidth = 0.1) +
    scale_fill_viridis_c(limits = c(0,1), option = "magma", direction = -1) +
    labs(
      x = "", 
      y = "", 
      title = paste("Spatial distribution of percentage of votes on populist parties\nin elections to European Parliament", "in", i),
    ) +
    theme_map() +
    guides(fill = guide_colourbar(title = "Percentage of votes"))
  
  ggsave(paste0("Plots/Election_results_distribution/Populism_spatial_distribution_in_", i, ".png"), dpi = 900)
}

lapply(seq(2004, 2019, by = 5), map_per_year_populism)
}

ggplot(shp) +
  geom_line(aes(x = year, y = sum_populist, color = NUTS_ID), show.legend = F, alpha = 0.25) +
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~country) +
  scale_color_viridis_d("plasma") +
  labs(x = "", y = "", title = "Percentage of votes on populist parties", subtitle = "Populist identification based on The PopuList ")


pallet <- wesanderson::wes_palette("Darjeeling1", n = 4)
ggplot(shp) +
  geom_violin(aes(x = as.factor(year), y = sum_populist, group = as.factor(year), color = as.factor(year)), fill = "white") +
  geom_violin(aes(x = as.factor(year), y = sum_populist, group = as.factor(year), fill = as.factor(year)), alpha = 0.3) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~country) +
  scale_color_manual(values = pallet) +
  scale_fill_manual(values = pallet)

# Make a function to model linear regression per year
modelling_per_year <- function(i, formula) {
  reg1 <- lm(formula, df_lagged %>% 
                   filter(year.x == i)) 
  
  # Check r^2
  glance(summary(reg1))#$r.squared
}

# Apply function
vector <- sapply(seq(2004, 2019, by = 5), modelling_per_year, formula = formula_bivariate)
vector

i = 2019
# Make a function to check if spatial dependency is present
moran_per_year <- function(i) {
  
  # Filter for a given year
  df_lagged_year <- df_lagged %>% 
    filter(year.x == i)
  
  # Remove regions with no data in them
  shp_func <- shp %>% 
    filter(NUTS_ID %in% unique(df_lagged_year$nuts2016))
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)
  
  
  # Create spatial weights for neighbors lists
  listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  # Conduct moran test
  moran.test(df_lagged_year %>% 
               pull(sum_populist.x), 
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
    filter(year.x == i)
  
  shp_func <- shp %>% 
    filter(NUTS_ID %in% unique(df_lagged_year$nuts2016))
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(shp_func, queen = T)
  
  # Create spatial weights for neighbors lists
  listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
  
  reg1 <- lm(formula_bivariate, df_lagged_year) 

  # Lagrange Multiplier diagnostics
  lm.LMtests(reg1, listw1, test= "all")
}

# For some reason the function does not work on 2004 year (no neighbor regions found) so this year is, for now- only until it is debugged, removed from the analysis
vector_lm <- lapply(seq(2004, 2019, by = 5), lm_per_year, formula = formula_bivariate)

# Apply names
names(vector_lm) <- paste("Test for", seq(2004, 2019, by = 5))

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


spml(formula_bivariate, data = panel, listw = listw1, model = "within", lag = TRUE , spatial.error = "none")

fixed <- plm(formula_bivariate, data = panel, model="within")

random <- plm(formula_bivariate, data = panel, model="random")

# Perform Hausman Test
phtest(fixed, random)

# The model with fixed effects is better suited to the data
slmtest(formula_bivariate, data = panel, listw = listw1, model = "within", test = c("lme", "lml", "rlme", "rlml"))
