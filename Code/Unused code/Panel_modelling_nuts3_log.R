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
df_shp <- st_read("Processed_data/df_shp_log.shp") #, driver = "ESRI Shapefile", append = FALSE, geometry_column = "geometry", stringAsFactors = FALSE)
  # For some reason with sf package two columns get relocated to the end of the data frame
  # relocate(c(36, 37), .before = "LEVL_CO")

# Read data with column names (shapefiles are limited to 10 characters- column names are abbreviated)
df_shp_colnames <- read_csv("Processed_data/df_log_shp_colnames.csv") %>% pull()

# Set correct column names
df_shp %<>% 
  set_colnames(df_shp_colnames) %>% 
  select(-c(31:39), -c(5:7))


# Create panel data
panel <- pdata.frame(df_shp %>% filter(year %in% seq(2009, 2019, by = 5)) %>% filter(!country %in% c("Bulgaria", "Romania", "Croatia")), c("nuts2016", "year")) %>% 
  select(-country) %>% 
  relocate(nuts2016, .before = year)

# Define univariate formula
formula_univariate <- growth_eurosceptic_p_perc ~ growth_populism_p_perc

# Define multivariate formula
r_side <- df_shp %>% as.data.frame() %>% 
  select(14, 15, 17:25, 27) %>%
  colnames() 

formula_multivariate <- as.formula(paste("growth_eurosceptic_p_perc ~ ", paste(r_side, collapse= "+")))


# Make a queen neighbourhood list
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
df_regions <- df_shp %>% 
  filter(year %in% seq(2009, 2019, by = 5)) %>% 
  filter(!country %in% c("Bulgaria", "Romania", "Croatia")) %>% 
  na.omit() %>% 
  group_by(nuts2016) %>% 
  count() %>% 
  filter(n > 2) %>% 
  pull(nuts2016)

# Remove regions that are not balanced
df_splm <- df_shp %>% 
  filter(year %in% seq(2009, 2019, by = 5)) %>% 
  filter(nuts2016 %in% df_regions)

# Define panel data
panel <- pdata.frame(df_splm, c("nuts2016", "year")) %>% 
  select(-country) %>% 
  relocate(nuts2016, .before = year)

# Make a queen neighbourhood list
queen_neighbour <- poly2nb(df_shp %>% filter(nuts2016 %in% df_regions, year == 2019) %>% sf::st_as_sf(), queen = T)

# Create spatial weights for neighbors lists
listw1 <- nb2listw(queen_neighbour, style = "W", zero.policy = TRUE)

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

# SAR models 
# sar_fe1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "individual", spatial.error = "none")

sar_re1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "individual", spatial.error = "none")

# Save results in txt format
sink("Results/sar_re1_multivariate.txt")
print(lapply(sar_re1, summary))
sink()


# sar_fe2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "twoways", spatial.error = "none")

# Doesn't work
# sar_re2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "twoways", spatial.error = "none")

# SEM models 
# sem_fe1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "individual", spatial.error = "b")

sem_re1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "individual", spatial.error = "b")

# Save results in txt format
sink("Results/sem_re1_multivariate.txt")
print(lapply(sem_re1, summary))
sink()

# sem_fe2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "within", lag = T, effect = "twoways", spatial.error = "b")

# Doesn't work
# sem_re2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "random", lag = T, effect = "twoways", spatial.error = "b")

# SEMRE models
semre_fe1 <- spreml(formula_multivariate, data = panel, w = listw1, errors = "semsrre")

# semre_re1 <- spml(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "individual", errors = "semsrre")

# semre_fe2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "twoways", errors = "semsrre")

# semre_re2 <- spml(formula_multivariate, data = panel, listw = listw1, model = "pooling", lag = T, effect = "twoways", errors = "semsrre")

# Save results in txt format
sink("Results/semre_fe1_multivariate.txt")
print(lapply(semre_fe1, summary))
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

fixed <- plm(formula_multivariate, data = panel[which(!is.na(panel$nuts2016)), ], model="within", index = c("nuts2016", "year"))

random <- plm(formula_multivariate, data = panel, model="random")

# Perform Hausman Test
phtest(fixed, random) # p-value is below 0.05- model with fixed effects suits the analysis better (as predicted)

bptest(growth_eurosceptic_p_perc ~ growth_populism_p_perc, data = panel, studentize=F)
# There is no presence of homoscedasticity (p-value < 0.05). We need to use robust covariance matrix to account for it. Or the other way is to just look at what the p-value of coefficients is- heteroscedasticity means that the error of our analysis can be greater yet if p-value is much below the treshold (0.05 for this work) it won't have a big impact.
}