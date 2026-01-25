# initial setup
source("2026 Thesis/R/load_libraries.R")
source("2026 Thesis/R/spatial_models.R")
source("2026 Thesis/R/load_data.R")
source("2026 Thesis/R/visualize_data.R")
source("2026 Thesis/R/transform_data.R")

# load data
data <- load_data(
  # map the countries and elections to be loaded
  country_mapping = data.table::data.table(
    country = c("PL", "PL", "PL"),
    year = c(2023, 2019, 2015),
    region_type = c("NUTS3", "NUTS3", "NUTS3"),
    election_type = c("parliamentary", "parliamentary", "parliamentary")
  )
)

for (key in 1:3) {
  far_right_parties <- c(
    "komitet wyborczy prawo i sprawiedliwość",
    "komitet wyborczy konfedracja wolność i niepodległość",
    "komitet wyborczy polska jest jedna",
    "komitet wyborczy prawica"
  )
  # classify parties into other and far right
  data[[key]]$party_classification <- "other"
  data[[key]][grepl(
    paste(far_right_parties, collapse = "|"), data[[key]]$party_name
  ), "party_classification"] <- "far right"

  # # summarize data by NUTS4 regions and party classification
  # data_sum <- data[[key]] %>%
  #   dplyr::group_by(territory_nuts4_name, geometry, party_classification) %>%
  #   dplyr::summarise(votes = sum(votes, na.rm = TRUE), .groups = 'drop')

  # # visualize spatial data
  # visualize_data(
  #   data_vis = data_sum[data_sum$party_classification == "far right", ],
  #   value_column = "votes",
  #   title = glue(
  #     "Far-Right Parties Electional Results in Poland {data[[key]]$year[1]} Parliamentary Elections"
  #   )
  # )
}

# it doesn't seem there is any significant difference in spatial distribution of Far Right supporters between 2019 and 2023 elections
# actually it rather seems that everything stayed the same but it's difficult to believe, let's make tests for a few selected areas
data[["2023vs2019"]] <- comparison_by_elections(
  data_previous = data[[1]],
  data_current = data[[2]],
  key_previous = "2019",
  key_current = "2023"
)

# visualize the difference in between 2023 and 2019 elections
visualize_data(
  data_vis = data[["2023vs2019"]],
  value_column = "far_right_difference",
  title = "Far-Right Parties Support Difference Between 2023 and 2019 Parliamentary Elections in Poland",
  limits_to_100 = FALSE
)

data[["2023vs2015"]] <- comparison_by_elections(
  data_previous = data[[1]],
  data_current = data[[3]],
  key_previous = "2015",
  key_current = "2023"
)

# visualize the difference in between 2023 and 2015 elections
# we're seeing some patters, around the cities mainly and rural areas being more conservative
visualize_data(
  data_vis = data[["2023vs2015"]],
  value_column = "far_right_difference",
  title = "Far-Right Parties Support Difference Between 2023 and 2015 Parliamentary Elections in Poland",
  limits_to_100 = FALSE
)

data[["2019vs2015"]] <- comparison_by_elections(
  data_previous = data[[2]],
  data_current = data[[3]],
  key_previous = "2015",
  key_current = "2019"
)

# visualize the difference in between 2023 and 2015 elections
# we're seeing some patters, around the cities mainly and rural areas being more conservative
visualize_data(
  data_vis = data[["2019vs2015"]],
  value_column = "far_right_difference",
  title = "Far-Right Parties Support Difference Between 2019 and 2015 Parliamentary Elections in Poland",
  limits_to_100 = FALSE
)

# let's see if population density has any effect on the support for far-right parties
dt_pop <- read_density_xlsx(
  file_name = "PL_2023_NUTS3_density"
)[, territory_nuts4_name := NULL]

# standardize population density variable
dt_pop <- scale_outliers(data = dt_pop, value_columns = "population_density_per_km2", method = "asinh")

visualize_data(
  data_vis = merge(unique(data[[1]][, c("territory_nuts4", "geometry")]), dt_pop, by = "territory_nuts4"),
  value_column = "population_density_per_km2_scaled",
  title = "Population Density (asinh scaled) in Poland 2023",
  limits_to_100 = FALSE
)

# investigate city vs rural areas
dt_age <- read_age_xlsx(
  file_path = "PL_2023_NUTS3_age"
)

dt_age <- dt_age[, .(
  population_city = sum(population_city),
  population = sum(population)
), by = .(territory_nuts4)][, population_city_percentage := (population_city / population) * 100]

visualize_data(
  data_vis = merge(unique(data[[1]][, c("territory_nuts4", "geometry")]), dt_age, by = "territory_nuts4"),
  value_column = "population_city_percentage",
  title = "Population Living in Cities Percentage in Poland 2023",
  limits_to_100 = FALSE
)

# investigate age structure influence on far-right support
dt_age <- read_age_xlsx(
  file_path = "PL_2023_NUTS3_age"
)
dt_age <- data.table::dcast(
  dt_age, territory_nuts4 ~ age_classification,
  value.var = "population_percentage", fun.aggregate = sum
)
names(dt_age)[2:7] <- paste0("group_", gsub("-", "_", names(dt_age)[2:7]))
names(dt_age) <- gsub("+", "plus", names(dt_age), fixed = TRUE)

dt_age[, group_0_44 := group_0_17 + group_18_24 + group_25_34 + group_35_44]
dt_age[, group_0_34 := group_0_17 + group_18_24 + group_25_34]
dt_age[, group_35plus := group_35_44 + group_45_64 + group_65plus]
dt_age[, group_18_34 := group_18_24 + group_25_34]
dt_age[, group_18_44 := group_18_24 + group_25_34 + group_35_44]
dt_age[, group_45plus := group_45_64 + group_65plus]

visualize_data(
  data_vis = merge(unique(data[[1]][, c("territory_nuts4", "geometry")]), dt_age, by = "territory_nuts4"),
  value_column = "group_45plus",
  title = "45+ Population Age Distribution in Poland 2023",
  limits_to_100 = FALSE
)

data_x <- join_covariates_data(
  country = "PL",
  year = 2023,
  data_list = list(
    dt_age = "read_age_xlsx",
    dt_density = "read_density_xlsx",
    dt_migrations_yearly = "read_migrations_yearly_xlsx",
    dt_indicators = "read_indicators_xlsx",
    dt_family_benefits = "read_family_benefits_xlsx",
    dt_revenue = "read_revenue_xlsx",
    dt_unemployment = "read_unemployment_xlsx"
  )
)

data_x[, registration_balance_internal_2011_2023_per_1000 := (registration_balance_internal_2011_2023 / population) * 1000]

visualize_data(
  data_vis = merge(unique(data[[1]][, c("territory_nuts4", "geometry")]), data_x, by = "territory_nuts4"),
  value_column = "registration_balance_internal_2011_2023_per_1000",
  title = "Internal Migration Registration Balance per 1000 in Poland 2023",
  limits_to_100 = FALSE
)

data_x[, registration_abroad_2011_2023_per_1000 := (registration_abroad_2011_2023 / population) * 1000]

visualize_data(
  data_vis = merge(unique(data[[1]][, c("territory_nuts4", "geometry")]), data_x, by = "territory_nuts4"),
  value_column = "registration_abroad_2011_2023_per_1000",
  title = "International Migration Registration Balance per 1000 in Poland 2023",
  limits_to_100 = FALSE
)

data_x_2019 <- join_covariates_data(
  country = "PL",
  year = 2019,
  data_list = list(
    dt_age = "read_age_xlsx",
    dt_density = "read_density_xlsx",
    dt_migrations_yearly = "read_migrations_yearly_xlsx",
    dt_indicators = "read_indicators_xlsx",
    dt_family_benefits = "read_family_benefits_xlsx",
    dt_revenue = "read_revenue_xlsx",
    dt_unemployment = "read_unemployment_xlsx"
  )
)

# decide on the model data variables
data_x[, group_18_44 := group_18_24 + group_25_34 + group_35_44]
data_x[, revenue_gmina_year_2023_per_1000 := revenue_gmina_year_2023 / population * 1000]
data_x[, unemployed_population_year_2023_share := unemployed_population_year_2023 / population]

# check if there are any extreme outliers
summary(data_x$unemployed_population_year_2023_share)
stopifnot(any(data_x$unemployed_population_year_2023_share < 0.3, na.rm = TRUE)) # in case of outliers, stop the process

# scale variables
data_x <- scale_outliers(data_x, "population_density_per_km2", "asinh")
# data_x <- scale_outliers(data_x, "internal_migration_inflow_per_1000", "asinh")
# data_x <- scale_outliers(data_x, "international_migration_inflow_per_1000", "asinh")
data_x <- scale_outliers(data_x, "revenue_gmina_year_2023_per_1000", "asinh")

# narrow down to relevant columns
grepv_vars <- c(
  "territory_nuts4", "population_density_per_km2_scaled", "group_18_44",
  "unemployed_population_year_2023_share", "revenue_gmina_year_2023_per_1000_scaled",
  "registration_balance_internal_2011_2023_per_1000", "registration_abroad_2011_2023_per_1000",
  "share_of_kids_until_17_yo_receiving_family_benefits_2023"
)
variables <- grepv(paste(grepv_vars, collapse = "|"), names(data_x), value = TRUE)

data_x <- data_x[, ..variables]

# impute missing values with median
for (col in names(data_x)) {
  if (any(is.na(data_x[[col]]))) {
    median_value <- median(data_x[[col]], na.rm = TRUE)
    data_x[is.na(get(col)), (col) := median_value]
  }
}

# merge population density and age data with election results
data_model <- merge(
  data[[1]][data[[1]]$party_classification == "far right", ] %>%
    dplyr::group_by(territory_nuts4, territory_nuts4_name, geometry) %>%
    dplyr::summarise(
      votes = sum(votes, na.rm = TRUE), turnout = mean(turnout, na.rm = TRUE),
      .groups = 'drop'
    ),
  data_x,
  by = "territory_nuts4",
  all.x = TRUE
)

# drop any rows with NA geometry
data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]

response_name <- "votes"
covariate_names <- c(
  "turnout", grepv_vars[-1]
)

data_model <- unique(data_model)

# investigate the chosen variables spatially
for (var in covariate_names) {
  print(glue("Visualizing variable: {var}"))
  visualize_data(
    data_vis = data_model,
    value_column = var,
    title = glue("PL 2023 Spatial Distribution of {gsub('_', ' ', var)}"),
    limits_to_100 = FALSE
  )
}

# perform linear regression model as baseline
lm_model <- lm(
  formula = as.formula(
    paste(response_name, paste(covariate_names, collapse = " + "), sep = " ~ ")
  ),
  data = st_drop_geometry(data_model)
)
print("Linear model results:")
print(summary(lm_model))

# prepare to display in README file as linear model results table
knitr::kable(
  summary(lm_model)$coefficients,
  caption = "Linear Model Results for Far-Right Parties Support in Poland 2023 Parliamentary Elections",
  digits = 3
)

# check if there is spatial autocorrelation in the residuals
coords <- sf::st_coordinates(sf::st_centroid(data_model$geometry))

knn_nb <- knearneigh(coords, k = 5)
nb <- knn2nb(knn_nb)
listw <- spdep::nb2listw(nb, style = "W")

moran_test <- spdep::moran.test(
  lm_model$residuals,
  listw = listw,
  zero.policy = TRUE
)

print("Moran's I test for linear model residuals:")
print(moran_test)
data_model$lm_residuals <- lm_model$residuals
visualize_data(
  data_vis = data_model,
  value_column = "lm_residuals",
  title = "PL 2023 Spatial Distribution of Linear Model Residuals",
  limits_to_100 = FALSE
)

# as the residuals show significant spatial autocorrelation I can proceed with spatial models

# run SAR model
sar_model(
  data_model = data_model,
  response_name = response_name,
  covariate_names = covariate_names
)

# run SEM model
sem_model(
  data_model = data_model,
  response_name = response_name,
  covariate_names = covariate_names
)

# run SDEM model
sdem_model(
  data_model = data_model,
  response_name = response_name,
  covariate_names = covariate_names
)

# run spatial models
spatial_random_forest(
  data_model = data_model,
  response_name = response_name,
  covariate_names = covariate_names,
  k = 5
)
