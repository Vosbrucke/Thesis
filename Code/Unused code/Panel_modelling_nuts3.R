# Panel analysis
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

# Read data with spatial geometry
df_shp <- st_read("Processed_data/df_shp.shp") %>% #, driver = "ESRI Shapefile", append = FALSE, geometry_column = "geometry", stringAsFactors = FALSE)
  # For some reason with sf package two columns get relocated to the end of the data frame
  relocate(c(36, 37), .before = "LEVL_CO")

# Read data with column names (shapefiles are limited to 10 characters- column names are abbreviated)
df_shp_colnames <- read_csv("Processed_data/df_shp_colnames.csv") %>% pull()

# Set correct column names
df_shp %<>% 
  set_colnames(df_shp_colnames) %>% 
  select(-country_code, -c(29:37), -c(4))

# Create panel data
panel <- pdata.frame(
  df_shp %>% 
    filter(year %in% seq(2009, 2019, by = 5)) %>% 
    filter(!country %in% c("Bulgaria", "Romania", "Croatia")), c("nuts2016", "year")
  ) %>% 
  select(-country) %>% 
  relocate(nuts2016, .before = year)

# Define univariate formula
formula_univariate <- growth_eurosceptic_p_perc ~ growth_populism_p_perc

# Define multivariate formula
r_side <- df_shp %>% as.data.frame() %>% 
  select(13, 14, 16:24, 26) %>%
  colnames() 

formula_multivariate <- as.formula(paste("growth_eurosceptic_p_perc ~ ", paste(r_side, collapse= "+")))


# Make a queen neighborhood list
queen_neighbour <- poly2nb(df_shp %>% filter(year == 2019) %>% filter(!country %in% c("Bulgaria", "Romania", "Croatia")) %>% sf::st_as_sf(), queen = T)

# Create spatial weights for neighbors lists
listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)

# Data management through plm functions, source: https://stackoverflow.com/questions/46525403/spatial-panel-regression-in-r-non-conformable-spatial-weights
plm <- plm(formula_univariate, data = panel, model="pooling")
X <- model.matrix(plm)
y <- pmodel.response(plm)
ind <- attr(plm$model, "index")[, 1]
tind <- attr(plm$model, "index")[, 2]
oo <- order(tind, ind)
X <- X[oo, , drop=FALSE]
y <- y[oo]
ind <- ind[oo]
tind <- tind[oo]
n <- length(unique(ind))
k <- dim(X)[[2]]
t <- max(tapply(X[, 1], ind, length))
nT <- length(ind)

# Check compatibility of weights matrix
if (length(listw1$neighbours) != n) stop("Non conformable spatial weights")

# Univariate analysis
{
# SAR models 
sar_fe1 <- spml(formula_univariate, data = panel, listw = listw1, model = "within", lag = T, effect = "individual", spatial.error = "none")

# Save results in txt format
sink("Results/sar_fe1_univariate.txt")
print(lapply(sar_fe1, summary))
sink()

sar_re1 <- spml(formula_univariate, data = panel, listw = listw1, model = "random", lag = T, effect = "individual", spatial.error = "none")

# Save results in txt format
sink("Results/sar_re1_univariate.txt")
print(lapply(sar_re1, summary))
sink()

sar_fe2 <- spml(formula_univariate, data = panel, listw = listw1, model = "within", lag = T, effect = "twoways", spatial.error = "none")

# Save results in txt format
sink("Results/sar_fe2_univariate.txt")
print(lapply(sar_fe2, summary))
sink()

# Doesn't work
# sar_re2 <- spml(formula_univariate, data = panel, listw = listw1, model = "random", lag = T, effect = "twoways", spatial.error = "none")

# SEM models 
sem_fe1 <- spml(formula_univariate, data = panel, listw = listw1, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/sem_fe1_univariate.txt")
print(lapply(sem_fe1, summary))
sink()

sem_re1 <- spml(formula_univariate, data = panel, listw = listw1, model = "random", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/sem_re1_univariate.txt")
print(lapply(sem_re1, summary))
sink()

sem_fe2 <- spml(formula_univariate, data = panel, listw = listw1, model = "within", lag = T, effect = "twoways", spatial.error = "b")

# Save results in txt format
sink("Results/sem_fe2_univariate.txt")
print(lapply(sem_fe2, summary))
sink()

# Doesn't work
# sem_re2 <- spml(formula_univariate, data = panel, listw = listw1, model = "random", lag = T, effect = "twoways", spatial.error = "b")

# SEMRE models
semre_fe1 <- spreml(formula_univariate, data = panel, w = listw1, errors = "semsrre")

# Save results in txt format
sink("Results/semre_fe1_univariate.txt")
print(lapply(semre_fe1, summary))
sink()

# semre_re1 <- spml(formula_univariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "individual", errors = "semsrre")

# semre_fe2 <- spml(formula_univariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "twoways", errors = "semsrre")

# semre_re2 <- spml(formula_univariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "twoways", errors = "semsrre")

# Tests
slmtest(formula_univariate, data = panel, listw = listw1, model="within",test = c("lme","lml","rlme","rlml"))
slmtest(formula_univariate, data = panel, listw = listw1, model="random",test = c("lme","lml","rlme","rlml"))
slmtest(formula_univariate, data = panel, listw = listw1, model="pooling",test = c("lme","lml","rlme","rlml"))

# There's evidence that the spatial dependence exists (p-value < 0.05). The higher the value of LM component the higher the spatial dependence. It could mean that model with fixed errors suits the best (as expected)

# H0: fixed effect model is consistent
# HA: random effect model is consistent
sphtest(sar_re1, sar_fe1) # It means (as p-value > 0.05) that the model with fixed effects is consistent
summary(sar_fe1)

sphtest(sem_re1, sem_fe1) # It means (as p-value > 0.05) that the model with random effects is consistent
summary(sem_re1)

# Model plm 
fixed <- plm(formula_multivariate, data = panel[which(!is.na(panel$nuts2016)), ], model="within", index = c("nuts2016", "year"))

random <- plm(formula_multivariate, data = panel, model="random")

# Perform Hausman Test
phtest(fixed, random) # p-value is below 0.05- model with fixed effects suits the analysis better (as predicted)

# Perform Breusch-Pagan Test
bptest(growth_eurosceptic_p_perc ~ growth_populism_p_perc, data = panel, studentize=F)
# There is no presence of homoscedasticity (p-value < 0.05). We need to use robust covariance matrix to account for it. Or the other way is to just look at what the p-value of coefficients is- heteroscedasticity means that the error of our analysis can be greater yet if p-value is much below the treshold (0.05 for this work) it won't have a big impact.


# Next steps- checking original coefficients
coeftest(fixed)

# Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC) 

# Check all 
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x)))))

slmtest(formula_multivariate, data = panel, listw = listw1, model = "within", test = c("lme", "lml", "rlme", "rlml"))
}

# Multivariate analysis
{
# Check which regions are unbalanced
# df_regions <- df_shp %>% 
#   filter(year %in% seq(2009, 2019, by = 5)) %>% 
#   filter(!country %in% c("Bulgaria", "Romania", "Croatia")) %>% 
#   na.omit() %>% 
#   group_by(nuts2016) %>% 
#   count() %>% 
#   filter(n > 2) %>% 
#   pull(nuts2016)
# 
# # Remove regions that are not balanced
# df_splm <- df_shp %>% 
#   filter(year %in% seq(2009, 2019, by = 5)) %>% 
#   filter(nuts2016 %in% df_regions)
# 
# # Take all unique region id's
# df_regions <- df_splm %>% 
#   ungroup() %>% 
#   pull(nuts2016) %>% 
#   unique()
# 
# # Make a queen neighbourhood list
# queen_neighbour <- poly2nb(df_splm %>% filter(nuts2016 %in% df_regions, year == 2019) %>% sf::st_as_sf(), queen = T)
# 
# # # Create spatial weights for neighbors lists
# # listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
# 
# # Check which regions had no neighbors
# which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
# 
# 
# # Remove regions with no neighbor in shapefile
# regions_with_neighbours <- tibble(region = df_regions, which_regions) %>% 
#   filter(is_neighbor_again == T) %>% 
#   pull(region)
# 
# # Construct neighbors list from polygon list
# queen_neighbour <- poly2nb(df_splm %>% filter(nuts2016 %in% regions_with_neighbours), queen = T)
# 
# # Create spatial weights for neighbors lists
# listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)
# 
# # Remove regions without neighhbours
# df <- df_shp_year %>% 
#   filter(nuts2016 %in% shp_func$NUTS_ID)
# 
# 

  # Check which regions are unbalanced
  df_regions <- df_shp %>% 
    filter(year %in% seq(2014, 2019, by = 5)) %>% 
    filter(!country %in% c("Bulgaria", "Romania", "Croatia")) %>% 
    # na.omit() %>% 
    group_by(nuts2016) %>% 
    count() %>% 
    filter(n > 1) %>% 
    pull(nuts2016)
  
  # Remove regions that are not balanced
  df_splm <- df_shp %>% 
    filter(year %in% seq(2014, 2019, by = 5)) %>% 
    filter(nuts2016 %in% df_regions) %>% 
    group_by(year, nuts2016) %>% 
    filter(row_number() == 1)
  
  # Define panel data
  panel <- pdata.frame(df_splm, c("nuts2016", "year")) %>% 
    select(-country) %>% 
    relocate(nuts2016, .before = year)
  
  # Make a queen neighborhood list
  queen_neighbour <- poly2nb(df_shp %>% filter(nuts2016 %in% df_regions, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  # t <- sapply(queen_neighbour, function(n){mean(panel$gdp[n], na.rm=TRUE)})
  
  # lists_t <- which(t == "NaN")
  
  # panel$gdp[queen_neighbour[[lists_t]]]
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)

  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_1 <- tibble(region = df_regions, which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region)

  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_1, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_2 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_1], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_1)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_2, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_3 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_2], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_2)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_3, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_4 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_3], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_3)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_4, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_5 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_4], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_4)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_5, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_6 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_5], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_5)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_6, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_7 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_6], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_6)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_7, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_8 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_7], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_7)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_8, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_9 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_8], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_8)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_9, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  
  # Check which regions had no neighbors
  which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  
  # Remove regions with no neighbor in shapefile
  regions_with_neighbours_10 <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_9], which_regions) %>%
    filter(is_neighbor_again == F) %>%
    pull(region) %>% c(., regions_with_neighbours_9)
  
  # Construct neighbors list from polygon list
  queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_10, year == 2019) %>% sf::st_as_sf(), queen = T)
  
  # sapply(queen_neighbour, function(n) {length(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_10, year == 2019) %>% sf::st_as_sf())[n] > 0})
  
  coords <- st_centroid(st_geometry(df_splm %>% filter(year == 2019) %>% sf::st_as_sf()), of_largest_polygon=TRUE)
  rn <- row.names(columbus)
  
  d100to1000 <- dnearneigh(coords, 1, 279000)
  d100to1000
  
  listw1 <- nb2listw(d100to1000, style = "W", zero.policy = TRUE)
  
  regions_t <- df_splm$nuts2016 %>% unique()
  
  df_regions[c(250, 344, 345, 377, 475, 476, 477, 478, 479)]
  
  # Check which regions had no neighbors
  # which_regions <- data.frame(is_neighbor_again = lapply(d100to1000, sum) > 0)
  
  p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))
  p + geom_point(aes(colour = Partic_Per, size = Pop_2001)) +
    geom_text(size = 2, aes(label = name))
  
  
  
  ggplot(data = df_splm, # the input data
         aes(geometry = geometry), fill = "black") +
    geom_sf(color = "black") +
    theme_minimal() # define variables geom_polygon() + # plot the boroughs
    # geom_path(colour="black", lwd=0.05) + # borough borders
    coord_equal() + # fixed x and y scales
    facet_wrap(~ year)

  # 
  # while(length(which_regions[which_regions == F]) > 0) {
  #   
  #   regions_with_neighbours <- c()
  #   
  #   # Construct neighbors list from polygon list
  #   queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours, year == 2019) %>% sf::st_as_sf(), queen = T)
  #   
  #   # Check which regions had no neighbors
  #   which_regions <- data.frame(is_neighbor_again = lapply(queen_neighbour, sum) > 0)
  #   
  #   # Remove regions with no neighbor in shapefile
  #   regions_with_neighbours <- tibble(region = df_regions[!df_regions %in% regions_with_neighbours_3], which_regions) %>%
  #     filter(is_neighbor_again == F) %>%
  #     pull(region) %>% c(., regions_with_neighbours_3)
  #   
  #   # Construct neighbors list from polygon list
  #   queen_neighbour <- poly2nb(df_splm %>% filter(!nuts2016 %in% regions_with_neighbours_4, year == 2019) %>% sf::st_as_sf(), queen = T)
  # }
  
  # Create spatial weights for neighbors lists
  listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)

  # Remove regions without neighhbours
  panel <- panel %>%
    filter(nuts2016 %in% regions_with_neighbours)

  

# Data management through plm functions, source: https://stackoverflow.com/questions/46525403/spatial-panel-regression-in-r-non-conformable-spatial-weights
plm <- plm(formula_multivariate, data = panel, model="pooling")
X <- model.matrix(plm)
y <- pmodel.response(plm)
ind <- attr(plm$model, "index")[, 1]
tind <- attr(plm$model, "index")[, 2]
oo <- order(tind, ind)
X <- X[oo, , drop=FALSE]
y <- y[oo]
ind <- ind[oo]
tind <- tind[oo]
n <- length(unique(ind))
k <- dim(X)[[2]]
t <- max(tapply(X[, 1], ind, length))
nT <- length(ind)

# Check compatibility of weights matrix
if (length(listw1$neighbours) != n) stop("Non conformable spatial weights")

# lag.listw(x = listw1, var = panel$gdp, NAOK = TRUE, zero.policy = TRUE, na.action = na.rm)


# SAR models
sar_fe1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "individual", spatial.error = "none", index = c("nuts2016", "year"))
print(summary(sar_fe1))

sar_re1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "individual", spatial.error = "none")

# Save results in txt format
sink("Results/sar_re1_multivariate.txt")
print(summary(sar_re1))
sink()


sar_fe2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "twoways", spatial.error = "none")
print(summary(sem_fe2))

# Doesn't work
# sar_re2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "twoways", spatial.error = "none")

# SEM models 
sem_fe1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "individual", spatial.error = "b")
print(summary(sem_fe1))
# sem_re1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/sem_re1_multivariate.txt")
print(summary(sem_re1))
sink()

sem_fe2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "twoways", spatial.error = "b")

# Doesn't work
# sem_re2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "twoways", spatial.error = "b")

# SEMRE models
semre_fe1 <- spreml(formula_multivariate, data = panel, w = listw1, errors = "semsrre")

# semre_re1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "individual", errors = "semsrre")

# semre_fe2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "twoways", errors = "semsrre")

# semre_re2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "twoways", errors = "semsrre")

# Save results in txt format
sink("Results/semre_fe1_multivariate.txt")
print(summary(semre_fe1))
sink()


# Tests
slmtest(formula_multivariate, data = panel, listw = listw1, model="within",test = c("lme","lml","rlme","rlml"))
slmtest(formula_multivariate, data = panel, listw = listw1, model="random",test = c("lme","lml","rlme","rlml"))
slmtest(formula_multivariate, data = panel, listw = listw1, model="pooling",test = c("lme","lml","rlme","rlml"))

# There's evidence that the spatial dependence exists (p-value < 0.05). The higher the value of LM component the higher the spatial dependence. It could mean that model with fixed errors suits the best (as expected)

# H0: fixed effect model is consistent
# HA: random effect model is consistent
sphtest(sar_re1, sar_fe1) # It means (as p-value > 0.05) that the model with fixed effects is consistent
summary(sar_fe1)

sphtest(sem_re1, sem_fe1) # It means (as p-value > 0.05) that the model with random effects is consistent
summary(sem_re1)

# Conduct a panel analysis

# SAR models
sar_fe1 <- plm(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "individual", spatial.error = "none")

# Save results in txt format
sink("Results/plm_sar_fe1_multivariate.txt")
print(summary(sar_fe1))
sink()

sar_re1 <- plm(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "individual", spatial.error = "none")

# Save results in txt format
sink("Results/plm_sar_re1_multivariate.txt")
print(summary(sar_re1))
sink()


sar_fe2 <- plm(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "twoways", spatial.error = "none")

# Save results in txt format
sink("Results/plm_sar_fe2_multivariate.txt")
print(summary(sar_fe2))
sink()

# # Doesn't work
# sar_re2 <- plm(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "twoways", spatial.error = "none")

# # Save results in txt format
# sink("Results/plm_sar_re2_multivariate.txt")
# print(summary(sar_re2))
# sink()

# SEM models 
sem_fe1 <- plm(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/plm_sem_fe1_multivariate.txt")
print(summary(sem_fe1))
sink()

sem_re1 <- plm(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/plm_sem_re1_multivariate.txt")
print(summary(sem_re1))
sink()

sem_fe2 <- plm(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "twoways", spatial.error = "b")

# Save results in txt format
sink("Results/plm_sem_fe2_multivariate.txt")
print(summary(sem_fe2))
sink()

# # Doesn't work
# sem_re2 <- plm(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "twoways", spatial.error = "b")

# # Save results in txt format
# sink("Results/plm_sem_re2_multivariate.txt")
# print(summary(sem_re2))
# sink()

# # SEMRE models
# semre_fe1 <- plm(formula_multivariate, data = panel, w = listw1, errors = "semsrre")
# 
# # Save results in txt format
# sink("Results/plm_semre_fe1_multivariate.txt")
# print(summary(semre_fe1))
# sink()

semre_re1 <- plm(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "individual", errors = "semsrre")

# Save results in txt format
sink("Results/plm_semre_re1_multivariate.txt")
print(summary(semre_re1))
sink()

semre_fe2 <- plm(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "twoways", errors = "semsrre")

# Save results in txt format
sink("Results/plm_semre_fe2_multivariate.txt")
print(summary(semre_fe2))
sink()

semre_re2 <- plm(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "twoways", errors = "semsrre")

# Save results in txt format
sink("Results/plm_semre_re2_multivariate.txt")
print(summary(semre_re2))
sink()


# fixed <- plm(formula_multivariate, data = panel, model="within", index = c("nuts2016", "year"))
# 
# random <- plm(formula_multivariate, data = panel, model="random")

# Perform Hausman Test
phtest(fixed, random) # p-value is below 0.05- model with fixed effects suits the analysis better (as predicted)

bptest(growth_eurosceptic_p_perc ~ growth_populism_p_perc, data = panel, studentize=F)
# There is no presence of homoscedasticity (p-value < 0.05). We need to use robust covariance matrix to account for it. Or the other way is to just look at what the p-value of coefficients is- heteroscedasticity means that the error of our analysis can be greater yet if p-value is much below the treshold (0.05 for this work) it won't have a big impact.
}

