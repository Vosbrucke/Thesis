# source: https://www.r-bloggers.com/2025/06/specialized-r-packages-for-spatial-machine-learning-an-introduction-to-randomforestsgls-spatialrf-and-meteo/
random_forests_gls <- function(data, method) {
  require("RandomForestsGLS")

  coords <- sf::st_coordinates(data)
  temp_response <- data$votes

  data_sf <- sf::st_drop_geometry(data)
  covariate_names <- colnames(data_sf)[2:(ncol(data_sf) - 7)]
  covariate_matrix <- as.matrix(data_sf[, covariate_names])

  set.seed(2025 - 01 - 30)
  train_idx <- sample(1:nrow(coords), floor(nrow(coords) * 0.7))

  estimation_result <- RFGLS_estimate_spatial(
    coords = coords[train_idx, ],
    y = temp_response[train_idx],
    X = covariate_matrix[train_idx, ],
    ntree = 100 # can be adjusted based on computational resources
  )
  print("Estimation result:")
  print(str(estimation_result))

  prediction_result <- RFGLS_predict(
    RFGLS_out = estimation_result,
    Xtest = covariate_matrix[-train_idx, ]
  )
  prediction_result_spatial <- RFGLS_predict_spatial(
    RFGLS_out = estimation_result,
    coords.0 = coords[-train_idx, ],
    Xtest = covariate_matrix[-train_idx, ]
  )


  print("Non-spatial predictions vs. Spatial predictions:")
  plot(prediction_result$predicted, prediction_result_spatial$prediction)
}

k <- 5
spatial_random_forest <- function(data_model, response_name, covariate_names, k = 5) {
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  # Extract coordinates for KNN
  centroids <- sf::st_centroid(data_model$geometry)
  coords <- st_coordinates(centroids)

  # Build neighbors (much faster)
  knn_nb <- knearneigh(coords, k = k)
  nb <- knn2nb(knn_nb)

  # Optionally, create a spatial weights matrix if needed
  # listw <- spdep::nb2listw(nb, style = "W")

  # For spatialRF, create a distance matrix (not adjacency matrix)
  n <- nrow(data_model)
  distance_matrix <- as.matrix(dist(coords))
  
  # Set very small values to a minimum threshold to avoid numerical issues
  distance_matrix[distance_matrix < 0.0001] <- 0.0001

  data_model <- st_drop_geometry(data_model)
  data_model$x <- coords[, 1]
  data_model$y <- coords[, 2]

  # Prepare data for spatialRF - include x, y coordinates
  model_data <- data_model[, c(response_name, covariate_names, "x", "y")]

  rf_spatial_moran <- spatialRF::rf_spatial(
    data = model_data,
    dependent.variable.name = response_name,
    predictor.variable.names = covariate_names,
    distance.matrix = distance_matrix,
    method = "mem.moran.sequential",
    n.cores = 8,
    verbose = FALSE
  )
  print("Spatial Random Forest with Moran's Eigenvector Maps result:")
  print(rf_spatial_moran)

  # the package also comes with variable interactions detection
  interactions <- the_feature_engineer(
    data = data_model,
    dependent.variable.name = response_name,
    predictor.variable.names = covariate_names,
    xy = coords[, c("x", "y")],
    importance.threshold = 0.50, # uses 50% best predictors
    cor.threshold = 0.60, # max corr between interactions and predictors
    seed = 2025 - 01 - 30,
    repetitions = 100,
    verbose = TRUE
  )
  print("Detected variable interactions:")
  print(interactions)

  # model evaluation
  rf_eval <- rf_evaluate(
    model = rf_spatial_moran,
    xy = coords[, c("x", "y")],
    repetitions = 30,
    training.fraction = 0.75,
    metrics = "rmse",
    seed = 2025 - 01 - 30,
    verbose = TRUE
  )
  print("Model evaluation results:")
  print(rf_eval)

  # variable importance
  rf_imp <- rf_importance(
    rf_spatial_moran,
    xy = coords
  )
  print("Variable importance results:")
  print(rf_imp)

  # generate Moran's Eigenvector Maps
  mem1 <- mem(neighbor.matrix = distance_matrix)

  print("Moran's Eigenvector Maps:")
  print(mem1)

  # spatial prediction (if covariates available)
  # pred_srf <- terra::predict(covariates, rf_spatial_moran)
  # terra::plot(pred_srf[[1]])
}

meteo_model <- function(data, method) {
  require("meteo")
  require("sf")

  response_name <- "temp"
  covariate_names <- colnames(data)[2:(ncol(data) - 8)]
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  rfsi_model <- rfsi(
    formula = fo,
    data = data,
    n.obs = 5, # number of nearest observations
    cpus = parallel::detectCores() - 1,
    progress = FALSE,
    importance = "impurity",
    seed = 42,
    num.trees = 250,
    mtry = 5
  )

  print("Meteo RFSi model results:")
  print(rfsi_model)

  rfsi_model_cv <- cv.rfsi(
    formula = fo,
    data = data,
    tgrid = expand.grid(mtry = 3:22),
    tune.type = "LLO", # Leave-Location-Out CV
    k = 5, # number of folds
    seed = 42,
    acc.metric = "RMSE", # R2, CCC, MAE
    output.format = "sf", # "data.frame", # "SpatVector",
    cpus = parallel::detectCores() - 1,
    progress = FALSE,
    importance = "impurity"
  ) # ranger parameter
  print(rfsi_model_cv)

  # model accuracy
  print("RFSi model CV accuracy (RMSE):")
  print(acc.metric.fun(rfsi_model_cv$obs, rfsi_model_cv$pred, "RMSE"))
}

perform_moran_test <- function(data_model, response_name) {

  # exclude empty geometries
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  centroids <- sf::st_centroid(data_model$geometry)
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W")
  moran_test <- spdep::moran.test(
    data_model[[response_name]],
    listw = listw,
    zero.policy = TRUE
  )
  if (moran_test$p.value < 0.05) {
    # color terminal print green for significant autocorrelation
    cat("\033[32mSignificant spatial autocorrelation detected. Moran's I p-value:", round(moran_test$p.value, 5), "\033[39m\n")
  } else {
    # color terminal print red for non-significant autocorrelation
    cat("\033[31mNo significant spatial autocorrelation detected. Moran's I p-value:", round(moran_test$p.value, 5), "\033[39m\n")
  }

  cat("\nMoran's I test results:")
  print(moran_test)
}

# SAR model
sar_model <- function(data_model, response_name, covariate_names) {
  require("spdep")
  require("sf")

  # exclude empty geometries
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  centroids <- sf::st_centroid(data_model$geometry)
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W")

  # prepare formula
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  sar_model <- spatialreg::lagsarlm(
    formula = fo,
    data = data_model,
    listw = listw,
    method = "eigen",
    zero.policy = TRUE
  )
  print("SAR model results:")
  print(summary(sar_model))

  # Calculate spatial impacts (direct, indirect, and total effects)
  print("\nSpatial impacts (marginal effects):")
  sar_impacts <- spatialreg::impacts(sar_model, listw = listw, R = 1000)
  print(summary(sar_impacts, zstats = TRUE))

  # Extract and display key interpretation metrics
  cat("\n=== Interpretation Guide ===\n")
  cat("Rho (spatial lag parameter):", round(sar_model$rho, 4), "\n")
  cat("- Measures strength of spatial dependence\n")
  cat("- Positive = similar values cluster together\n\n")

  cat("Direct effects: impact on the unit itself\n")
  cat("Indirect effects: spillover impact on neighbors\n")
  cat("Total effects: direct + indirect\n")

  # Save model results to table for markdown display
  impacts_summary <- summary(sar_impacts, zstats = TRUE)

  # Extract coefficients and impacts
  results_table <- data.frame(
    Variable = c("rho (spatial lag)", rownames(coef(summary(sar_model)))),
    Coefficient = coef(sar_model),
    Std_Error = c(sqrt(sar_model$rho.se), coef(summary(sar_model))[, "Std. Error"]),
    z_value = c(NA, coef(summary(sar_model))[, "z value"]),
    p_value = c(NA, coef(summary(sar_model))[, "Pr(>|z|)"]),
    Direct_Effect = c(NA, NA, impacts_summary$zmat[, "Direct"]),
    Indirect_Effect = c(NA, NA, impacts_summary$zmat[, "Indirect"]),
    Total_Effect = c(NA, NA, impacts_summary$zmat[, "Total"])
  )

  # Save to CSV
  write.csv(
    results_table,
    file.path("2026 Thesis", "Data Outputs", "Results", "sar_model_results.csv"),
    row.names = FALSE
  )

  cat("\nModel results saved to: Data Outputs/Results/sar_model_results.csv\n")

  cat("\n=== SAR Model Results ===\n")
  kable(results_table, format = "markdown", digits = 3)

  # test the model
  lm_residuals <- residuals(sar_model)
  moran_test_residuals <- spdep::moran.test(
    lm_residuals,
    listw = listw,
    zero.policy = TRUE
  )
  cat("\nMoran's I test for SAR model residuals:")
  print(moran_test_residuals)

  # plot residuals
  plot(lm_residuals, main = "SAR Model Residuals")

  # plot residuals spatially
  data_model$lm_residuals <- lm_residuals
  visualize_data(
    data_vis = data_model,
    value_column = "lm_residuals",
    title = glue("PL 2023 Spatial Distribution of lm_residuals from SAR Model"),
    limits_to_100 = FALSE
  )

  # return the model
  return(sar_model)
}

# SDM model
sdm_model <- function(data_model, response_name, covariate_names) {
  require("spdep")
  require("sf")

  # exclude empty geometries
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  centroids <- sf::st_centroid(data_model$geometry)
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W")

  # prepare formula
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  sdm_model <- spdep::errorsarlm(
    formula = fo,
    data = data_model,
    listw = listw,
    method = "eigen",
    zero.policy = TRUE
  )
  print("SDM model results:")
  print(summary(sdm_model))
  return(sdm_model)
}

# SEM model
sem_model <- function(data_model, response_name, covariate_names) {
  require("spdep")
  require("sf")

  # exclude empty geometries
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  centroids <- sf::st_centroid(data_model$geometry)
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W")

  # prepare formula
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  sem_model <- spdep::errorsarlm(
    formula = fo,
    data = data_model,
    listw = listw,
    method = "eigen",
    zero.policy = TRUE
  )
  print("SEM model results:")
  print(summary(sem_model))
  return(sem_model)
}

# SDEM model
sdem_model <- function(data_model, response_name, covariate_names) {
  require("spdep")
  require("sf")

  # exclude empty geometries
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  centroids <- sf::st_centroid(data_model$geometry)
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W")

  # prepare formula
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  sdem_model <- spdep::errorsarlm(
    formula = fo,
    data = data_model,
    listw = listw,
    method = "eigen",
    zero.policy = TRUE
  )
  print("SDEM model results:")
  print(summary(sdem_model))
  return(sdem_model)
}
