# source: https://www.r-bloggers.com/2025/06/specialized-r-packages-for-spatial-machine-learning-an-introduction-to-randomforestsgls-spatialrf-and-meteo/

# Helper function to compare spatial models with OLS
compare_spatial_models <- function(spatial_model, ols_model, data_model, response_name, listw, model_name = "Spatial") {
  require("knitr")
  
  cat("\n\n=== MODEL FIT COMPARISON ===\n")
  
  # Compare fit metrics
  fit_comparison <- data.frame(
    Model = c("OLS", model_name),
    LogLik = c(logLik(ols_model), logLik(spatial_model)),
    AIC = c(AIC(ols_model), AIC(spatial_model)),
    BIC = c(BIC(ols_model), BIC(spatial_model)),
    Num_Parameters = c(length(coef(ols_model)) + 1, length(coef(spatial_model)) + 2)
  )
  
  # Calculate R-squared
  ols_r2 <- summary(ols_model)$r.squared
  
  # For spatial models, calculate pseudo R-squared
  if("SSE" %in% names(spatial_model)) {
    spatial_r2 <- 1 - (spatial_model$SSE / sum((data_model[[response_name]] - mean(data_model[[response_name]]))^2))
  } else {
    # Alternative calculation if SSE not available
    spatial_r2 <- 1 - (sum(residuals(spatial_model)^2) / sum((data_model[[response_name]] - mean(data_model[[response_name]]))^2))
  }
  
  fit_comparison$R_squared <- c(ols_r2, spatial_r2)
  
  cat("\nModel Fit Comparison:\n")
  print(kable(fit_comparison, format = "markdown", digits = 3))
  
  # Likelihood Ratio Test (Spatial vs OLS)
  lr_stat <- -2 * (logLik(ols_model) - logLik(spatial_model))
  lr_pvalue <- pchisq(lr_stat, df = 1, lower.tail = FALSE)
  cat("\n\nLikelihood Ratio Test (", model_name, "vs OLS):\n", sep = "")
  cat("LR statistic:", round(lr_stat, 3), "\n")
  cat("p-value:", format.pval(lr_pvalue), "\n")
  if(lr_pvalue < 0.05) {
    cat("Conclusion:", model_name, "model is significantly better than OLS\n")
  } else {
    cat("Conclusion: No significant improvement over OLS\n")
  }
  
  # Check residual spatial autocorrelation
  cat("\n\n=== RESIDUAL DIAGNOSTICS ===\n")
  
  # OLS residuals
  ols_residuals <- residuals(ols_model)
  moran_test_ols <- spdep::moran.test(ols_residuals, listw = listw, zero.policy = TRUE)
  cat("\nMoran's I test for OLS residuals:\n")
  cat("Moran I statistic:", round(moran_test_ols$statistic, 4), "\n")
  cat("p-value:", format.pval(moran_test_ols$p.value), "\n")
  
  # Spatial model residuals
  spatial_residuals <- residuals(spatial_model)
  moran_test_spatial <- spdep::moran.test(spatial_residuals, listw = listw, zero.policy = TRUE)
  cat("\nMoran's I test for", model_name, "residuals:\n")
  cat("Moran I statistic:", round(moran_test_spatial$statistic, 4), "\n")
  cat("p-value:", format.pval(moran_test_spatial$p.value), "\n")
  
  if(moran_test_spatial$p.value > 0.05) {
    cat("✓", model_name, "model successfully removed spatial autocorrelation\n")
  } else {
    cat("⚠ Residual spatial autocorrelation still present\n")
  }
  
  # Save comparison table
  write.csv(
    fit_comparison,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0(tolower(gsub(" ", "_", model_name)), "_fit_comparison.csv")),
    row.names = FALSE
  )
  
  cat("\nComparison saved to: Data Outputs/Results/", tolower(gsub(" ", "_", model_name)), "_fit_comparison.csv\n", sep = "")
  
  return(list(
    fit_comparison = fit_comparison,
    lr_test = list(statistic = lr_stat, p.value = lr_pvalue),
    moran_ols = moran_test_ols,
    moran_spatial = moran_test_spatial
  ))
}

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

  sar_model_result <- spatialreg::lagsarlm(
    formula = fo,
    data = data_model,
    listw = listw,
    method = "eigen",
    zero.policy = TRUE
  )
  print("SAR model results:")
  print(summary(sar_model_result))

  # Calculate spatial impacts (direct, indirect, and total effects)
  print("\nSpatial impacts (marginal effects):")
  sar_impacts <- spatialreg::impacts(sar_model_result, listw = listw, R = 1000)
  print(summary(sar_impacts, zstats = TRUE))

  # Extract and display key interpretation metrics
  cat("\n=== Interpretation Guide ===\n")
  cat("Rho (spatial lag parameter):", round(sar_model_result$rho, 4), "\n")
  cat("- Measures strength of spatial dependence\n")
  cat("- Positive = similar values cluster together\n\n")

  cat("Direct effects: impact on the unit itself\n")
  cat("Indirect effects: spillover impact on neighbors\n")
  cat("Total effects: direct + indirect\n")

  # Save model results to table for markdown display
  impacts_summary <- summary(sar_impacts, zstats = TRUE)

  # Extract coefficients and impacts
  results_table <- data.frame(
    Variable = c("rho (spatial lag)", rownames(coef(summary(sar_model_result)))),
    Coefficient = coef(sar_model_result),
    Std_Error = c(sqrt(sar_model_result$rho.se), coef(summary(sar_model_result))[, "Std. Error"]),
    z_value = c(NA, coef(summary(sar_model_result))[, "z value"]),
    p_value = c(NA, coef(summary(sar_model_result))[, "Pr(>|z|)"]),
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
  print(kable(results_table, format = "markdown", digits = 3))

  # Fit OLS model for comparison
  ols_model <- lm(fo, data = data_model)

  # Use helper function for model comparison
  comparison_results <- compare_spatial_models(
    spatial_model = sar_model_result,
    ols_model = ols_model,
    data_model = data_model,
    response_name = response_name,
    listw = listw,
    model_name = "SAR"
  )

  # Plot residuals
  sar_residuals <- residuals(sar_model_result)
  plot(sar_residuals, main = "SAR Model Residuals")

  # Plot residuals spatially
  data_model$sar_residuals <- sar_residuals
  visualize_data(
    data_vis = data_model,
    value_column = "sar_residuals",
    title = glue("Spatial Distribution of SAR Model Residuals"),
    limits_to_100 = FALSE
  )

  # Save model object for further analysis
  saveRDS(
    sar_model_result,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0("sar_model_results_", response_name, ".RDS"))
  )
  cat("\nModel object saved to: Data Outputs/Results/sar_model_results_", response_name, ".RDS\n", sep = "")

  # return the model
  return(sar_model_result)
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

  # SDM includes spatially lagged Y and spatially lagged X variables
  sdm_model_result <- spatialreg::lagsarlm(
    formula = fo,
    data = data_model,
    listw = listw,
    Durbin = TRUE,  # Adds WX terms
    method = "eigen",
    zero.policy = TRUE
  )
  print("SDM (Spatial Durbin Model) results:")
  print("Includes: ρWy + Xβ + WXθ + ε")
  print(summary(sdm_model_result))

  # Calculate spatial impacts (direct, indirect, and total effects)
  print("\nSpatial impacts (marginal effects) for SDM:")
  sdm_impacts <- spatialreg::impacts(sdm_model_result, listw = listw, R = 1000)
  print(summary(sdm_impacts, zstats = TRUE))

  # Extract and display key interpretation metrics
  cat("\n=== SDM Interpretation Guide ===\n")
  cat("Rho (spatial lag parameter):", round(sdm_model_result$rho, 4), "\n")
  cat("- Measures strength of spatial dependence in Y\n\n")

  cat("SDM includes both:\n")
  cat("1. Spatially lagged Y (ρWy): outcome diffusion\n")
  cat("2. Spatially lagged X (WXθ): spillover effects of covariates\n")
  cat("Direct effects: impact on the unit itself\n")
  cat("Indirect effects: spillover impact on neighbors\n")
  cat("Total effects: direct + indirect\n")

  # Save model results to table
  impacts_summary <- summary(sdm_impacts, zstats = TRUE)

  # Extract coefficients and impacts
  results_table <- data.frame(
    Variable = c("rho (spatial lag)", rownames(coef(summary(sdm_model_result)))),
    Coefficient = coef(sdm_model_result),
    Std_Error = c(sqrt(sdm_model_result$rho.se), coef(summary(sdm_model_result))[, "Std. Error"]),
    z_value = c(NA, coef(summary(sdm_model_result))[, "z value"]),
    p_value = c(NA, coef(summary(sdm_model_result))[, "Pr(>|z|)"]),
    Direct_Effect = c(rep(NA, length(coef(sdm_model_result)) - nrow(impacts_summary$zmat)), impacts_summary$zmat[, "Direct"]),
    Indirect_Effect = c(rep(NA, length(coef(sdm_model_result)) - nrow(impacts_summary$zmat)), impacts_summary$zmat[, "Indirect"]),
    Total_Effect = c(rep(NA, length(coef(sdm_model_result)) - nrow(impacts_summary$zmat)), impacts_summary$zmat[, "Total"])
  )

  # Save to CSV
  write.csv(
    results_table,
    file.path("2026 Thesis", "Data Outputs", "Results", "sdm_model_results.csv"),
    row.names = FALSE
  )

  cat("\nModel results saved to: Data Outputs/Results/sdm_model_results.csv\n")

  cat("\n=== SDM Model Results ===\n")
  print(kable(results_table, format = "markdown", digits = 3))

  # Fit OLS model for comparison
  ols_model <- lm(fo, data = data_model)

  # Use helper function for model comparison
  comparison_results <- compare_spatial_models(
    spatial_model = sdm_model_result,
    ols_model = ols_model,
    data_model = data_model,
    response_name = response_name,
    listw = listw,
    model_name = "SDM (Spatial Durbin)"
  )

  # Plot residuals
  sdm_residuals <- residuals(sdm_model_result)
  plot(sdm_residuals, main = "SDM Model Residuals")

  # Plot residuals spatially
  data_model$sdm_residuals <- sdm_residuals
  visualize_data(
    data_vis = data_model,
    value_column = "sdm_residuals",
    title = glue("Spatial Distribution of SDM Model Residuals"),
    limits_to_100 = FALSE
  )

  # Save model object for further analysis
  saveRDS(
    sdm_model_result,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0("sdm_model_results_", response_name, ".RDS"))
  )
  cat("\nModel object saved to: Data Outputs/Results/sdm_model_results_", response_name, ".RDS\n", sep = "")

  return(sdm_model_result)
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

  # SEM only has spatial autocorrelation in errors
  sem_model_result <- spatialreg::errorsarlm(
    formula = fo,
    data = data_model,
    listw = listw,
    method = "eigen",
    zero.policy = TRUE
  )
  print("SEM (Spatial Error Model) results:")
  print("Includes: Xβ + u, where u = λWu + ε")
  print(summary(sem_model_result))

  # Extract and display key interpretation metrics
  cat("\n=== SEM Interpretation Guide ===\n")
  cat("Lambda (spatial error parameter):", round(sem_model_result$lambda, 4), "\n")
  cat("- Measures strength of spatial dependence in errors\n")
  cat("- Positive = errors are spatially correlated (omitted spatial variables)\n\n")
  
  cat("SEM assumes:\n")
  cat("- No diffusion in the outcome variable (no ρWy term)\n")
  cat("- Spatial correlation comes from unobserved factors\n")
  cat("- Coefficients represent direct effects only (no spillovers)\n")

  # Save model results to table
  results_table <- data.frame(
    Variable = c("lambda (spatial error)", rownames(coef(summary(sem_model_result)))),
    Coefficient = c(sem_model_result$lambda, coef(sem_model_result)),
    Std_Error = c(sqrt(sem_model_result$lambda.se), coef(summary(sem_model_result))[, "Std. Error"]),
    z_value = c(NA, coef(summary(sem_model_result))[, "z value"]),
    p_value = c(NA, coef(summary(sem_model_result))[, "Pr(>|z|)"])
  )

  # Save to CSV
  write.csv(
    results_table,
    file.path("2026 Thesis", "Data Outputs", "Results", "sem_model_results.csv"),
    row.names = FALSE
  )

  cat("\nModel results saved to: Data Outputs/Results/sem_model_results.csv\n")

  cat("\n=== SEM Model Results ===\n")
  print(kable(results_table, format = "markdown", digits = 3))

  # Fit OLS model for comparison
  ols_model <- lm(fo, data = data_model)

  # Use helper function for model comparison
  comparison_results <- compare_spatial_models(
    spatial_model = sem_model_result,
    ols_model = ols_model,
    data_model = data_model,
    response_name = response_name,
    listw = listw,
    model_name = "SEM (Spatial Error)"
  )

  # Plot residuals
  sem_residuals <- residuals(sem_model_result)
  plot(sem_residuals, main = "SEM Model Residuals")

  # Plot residuals spatially
  data_model$sem_residuals <- sem_residuals
  visualize_data(
    data_vis = data_model,
    value_column = "sem_residuals",
    title = glue("Spatial Distribution of SEM Model Residuals"),
    limits_to_100 = FALSE
  )

  # Save model object for further analysis
  saveRDS(
    sem_model_result,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0("sem_model_results_", response_name, ".RDS"))
  )
  cat("\nModel object saved to: Data Outputs/Results/sem_model_results_", response_name, ".RDS\n", sep = "")

  return(sem_model_result)
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

  # SDEM includes spatially lagged X variables and spatial error
  sdem_model_result <- spatialreg::errorsarlm(
    formula = fo,
    data = data_model,
    listw = listw,
    Durbin = TRUE,  # Adds WX terms
    method = "eigen",
    zero.policy = TRUE
  )
  print("SDEM (Spatial Durbin Error Model) results:")
  print("Includes: Xβ + WXθ + u, where u = λWu + ε")
  print(summary(sdem_model_result))

  # Calculate spatial impacts (direct, indirect, and total effects)
  print("\nSpatial impacts (marginal effects) for SDEM:")
  sdem_impacts <- spatialreg::impacts(sdem_model_result, listw = listw, R = 1000)
  print(summary(sdem_impacts, zstats = TRUE))

  # Extract and display key interpretation metrics
  cat("\n=== SDEM Interpretation Guide ===\n")
  cat("Lambda (spatial error parameter):", round(sdem_model_result$lambda, 4), "\n")
  cat("- Measures strength of spatial dependence in errors\n\n")

  cat("SDEM includes:\n")
  cat("1. Spatially lagged X (WXθ): spillover effects of covariates\n")
  cat("2. Spatial error (λWu): spatially correlated unobserved factors\n")
  cat("Direct effects: impact on the unit itself\n")
  cat("Indirect effects: spillover impact on neighbors\n")
  cat("Total effects: direct + indirect\n")

  # Save model results to table
  impacts_summary <- summary(sdem_impacts, zstats = TRUE)

  # Extract coefficients and impacts
  results_table <- data.frame(
    Variable = c("lambda (spatial error)", rownames(coef(summary(sdem_model_result)))),
    Coefficient = c(sdem_model_result$lambda, coef(sdem_model_result)),
    Std_Error = c(sqrt(sdem_model_result$lambda.se), coef(summary(sdem_model_result))[, "Std. Error"]),
    z_value = c(NA, coef(summary(sdem_model_result))[, "z value"]),
    p_value = c(NA, coef(summary(sdem_model_result))[, "Pr(>|z|)"]),
    Direct_Effect = c(rep(NA, length(coef(sdem_model_result)) - nrow(impacts_summary$zmat) + 1), impacts_summary$zmat[, "Direct"]),
    Indirect_Effect = c(rep(NA, length(coef(sdem_model_result)) - nrow(impacts_summary$zmat) + 1), impacts_summary$zmat[, "Indirect"]),
    Total_Effect = c(rep(NA, length(coef(sdem_model_result)) - nrow(impacts_summary$zmat) + 1), impacts_summary$zmat[, "Total"])
  )

  # Save to CSV
  write.csv(
    results_table,
    file.path("2026 Thesis", "Data Outputs", "Results", "sdem_model_results.csv"),
    row.names = FALSE
  )

  cat("\nModel results saved to: Data Outputs/Results/sdem_model_results.csv\n")

  cat("\n=== SDEM Model Results ===\n")
  print(kable(results_table, format = "markdown", digits = 3))

  # Fit OLS model for comparison
  ols_model <- lm(fo, data = data_model)

  # Use helper function for model comparison
  comparison_results <- compare_spatial_models(
    spatial_model = sdem_model_result,
    ols_model = ols_model,
    data_model = data_model,
    response_name = response_name,
    listw = listw,
    model_name = "SDEM (Spatial Durbin Error)"
  )

  # Plot residuals
  sdem_residuals <- residuals(sdem_model_result)
  plot(sdem_residuals, main = "SDEM Model Residuals")

  # Plot residuals spatially
  data_model$sdem_residuals <- sdem_residuals
  visualize_data(
    data_vis = data_model,
    value_column = "sdem_residuals",
    title = glue("Spatial Distribution of SDEM Model Residuals"),
    limits_to_100 = FALSE
  )

  # Save model object for further analysis
  saveRDS(
    sdem_model_result,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0("sdem_model_results_", response_name, ".RDS"))
  )
  cat("\nModel object saved to: Data Outputs/Results/sdem_model_results_", response_name, ".RDS\n", sep = "")

  return(sdem_model_result)
}

# Panel SAR Model
panel_sar_model <- function(data_model, response_name, covariate_names, index_vars = c("region_id", "year")) {
  require("plm")
  require("splm")
  require("spdep")
  require("knitr")

  cat("\n\n=== PANEL SAR MODEL ===\n")
  cat("Spatial Autoregressive Panel Model with Fixed Effects\n")
  cat("Model: y_it = ρWy_it + X_it β + α_i + ε_it\n\n")

  # Exclude empty geometries and simplify
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  # Create spatial weights (using first time period for structure)
  centroids <- sf::st_centroid(data_model$geometry)
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  # Prepare formula
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  # Create panel data structure
  data_for_panel <- sf::st_drop_geometry(data_model)
  panel_data <- pdata.frame(data_for_panel, index = index_vars)

  # Fit Panel SAR model
  panel_sar_result <- splm::spml(
    formula = fo,
    data = panel_data,
    listw = listw,
    model = "within",
    effect = "individual",
    lag = TRUE,
    spatial.error = "none"
  )

  cat("\nPanel SAR Model Summary:\n")
  print(summary(panel_sar_result))

  # Extract coefficients
  coef_summary <- summary(panel_sar_result)$CoefTable
  
  # Extract and display key interpretation metrics
  cat("\n=== Panel SAR Interpretation Guide ===\n")
  cat("Rho (spatial lag parameter):", round(panel_sar_result$spat.coef, 4), "\n")
  cat("- Measures strength of spatial dependence across units\n")
  cat("- Positive = similar values cluster together in space\n\n")

  cat("Model includes:\n")
  cat("- Fixed effects (α_i): controls for time-invariant unit-specific factors\n")
  cat("- Spatial lag (ρWy): captures spatial diffusion of the outcome\n")
  cat("- Coefficients represent direct effects (spillovers captured in spatial lag)\n")

  # Create results table
  results_table <- data.frame(
    Variable = c("rho (spatial lag)", rownames(coef_summary)),
    Coefficient = c(panel_sar_result$spat.coef, coef_summary[, "Estimate"]),
    Std_Error = c(NA, coef_summary[, "Std. Error"]),
    t_value = c(NA, coef_summary[, "t-value"]),
    p_value = c(NA, coef_summary[, "Pr(>|t|)"])
  )

  # Save to CSV
  write.csv(
    results_table,
    file.path("2026 Thesis", "Data Outputs", "Results", "panel_sar_model_results.csv"),
    row.names = FALSE
  )

  cat("\nModel results saved to: Data Outputs/Results/panel_sar_model_results.csv\n")

  cat("\n=== Panel SAR Model Results ===\n")
  print(kable(results_table, format = "markdown", digits = 4))

  # Fit pooled OLS for comparison
  pooled_ols <- plm(fo, data = panel_data, model = "pooling")
  
  # Fit fixed effects model without spatial lag for comparison
  fe_model <- plm(fo, data = panel_data, model = "within", effect = "individual")

  # Calculate R-squared manually for panel models
  y_var <- panel_data[[response_name]]
  panel_sar_residuals <- residuals(panel_sar_result)
  ssr_panel_sar <- sum(panel_sar_residuals^2)
  sst <- sum((y_var - mean(y_var, na.rm = TRUE))^2)
  r_squared_panel_sar <- 1 - (ssr_panel_sar / sst)
  
  pooled_residuals <- residuals(pooled_ols)
  ssr_pooled <- sum(pooled_residuals^2)
  r_squared_pooled <- 1 - (ssr_pooled / sst)
  
  fe_residuals <- residuals(fe_model)
  ssr_fe <- sum(fe_residuals^2)
  r_squared_fe <- 1 - (ssr_fe / sst)

  # Extract log-likelihood from splm model (stored in model object)
  ll_panel_sar <- panel_sar_result$logLik
  
  # Calculate log-likelihood manually for plm models
  # LogLik = -n/2 * (log(2*pi) + log(sigma^2) + 1)
  n <- nrow(panel_data)
  sigma2_fe <- ssr_fe / n
  sigma2_pooled <- ssr_pooled / n
  
  ll_fe <- -n/2 * (log(2*pi) + log(sigma2_fe) + 1)
  ll_pooled <- -n/2 * (log(2*pi) + log(sigma2_pooled) + 1)
  
  # Calculate AIC and BIC manually
  k_panel_sar <- length(coef(panel_sar_result)) + 1  # parameters + rho
  k_fe <- length(coef(fe_model)) + 1
  k_pooled <- length(coef(pooled_ols)) + 1
  
  aic_panel_sar <- -2 * ll_panel_sar + 2 * k_panel_sar
  aic_fe <- -2 * ll_fe + 2 * k_fe
  aic_pooled <- -2 * ll_pooled + 2 * k_pooled
  
  bic_panel_sar <- -2 * ll_panel_sar + log(n) * k_panel_sar
  bic_fe <- -2 * ll_fe + log(n) * k_fe
  bic_pooled <- -2 * ll_pooled + log(n) * k_pooled

  cat("\n\n=== MODEL COMPARISON ===\n")
  comparison_table <- data.frame(
    Model = c("Pooled OLS", "Fixed Effects", "Panel SAR"),
    R_squared = c(
      r_squared_pooled,
      r_squared_fe,
      r_squared_panel_sar
    ),
    LogLik = c(
      ll_pooled,
      ll_fe,
      ll_panel_sar
    ),
    AIC = c(
      aic_pooled,
      aic_fe,
      aic_panel_sar
    ),
    BIC = c(
      bic_pooled,
      bic_fe,
      bic_panel_sar
    )
  )
  
  print(kable(comparison_table, format = "markdown", digits = 4))
  
  # Likelihood Ratio Test (Panel SAR vs Fixed Effects)
  lr_stat <- 2 * (ll_panel_sar - ll_fe)
  lr_pvalue <- pchisq(lr_stat, df = 1, lower.tail = FALSE)
  cat("\n\nLikelihood Ratio Test (Panel SAR vs Fixed Effects):\n")
  cat("LR statistic:", round(lr_stat, 3), "\n")
  cat("p-value:", format.pval(lr_pvalue), "\n")
  if(lr_pvalue < 0.05) {
    cat("✓ Panel SAR model is significantly better than Fixed Effects\n")
  } else {
    cat("⚠ No significant improvement over Fixed Effects\n")
  }
  
  cat("\nInterpretation: High R² in panel models is normal with fixed effects.\n")
  cat("Rho =", round(panel_sar_result$spat.coef, 3), "indicates", 
      ifelse(panel_sar_result$spat.coef > 0.5, "strong", "moderate"), 
      "spatial dependence.\n")

  # Residual diagnostics
  panel_residuals <- residuals(panel_sar_result)
  
  cat("\n\n=== RESIDUAL DIAGNOSTICS ===\n")
  cat("Mean of residuals:", round(mean(panel_residuals), 6), "\n")
  cat("SD of residuals:", round(sd(panel_residuals), 4), "\n")
  
  # Plot residuals
  plot(panel_residuals, main = "Panel SAR Model Residuals", ylab = "Residuals", xlab = "Observation")
  abline(h = 0, col = "red", lty = 2)

  # Save model object
  saveRDS(
    panel_sar_result,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0("panel_sar_model_results_", response_name, ".RDS"))
  )
  cat("\nModel object saved to: Data Outputs/Results/panel_sar_model_results_", response_name, ".RDS\n", sep = "")

  return(panel_sar_result)
}

# Panel SDM Model
panel_sdm_model <- function(data_model, response_name, covariate_names, index_vars = c("region_id", "year")) {
  require("plm")
  require("splm")
  require("spdep")
  require("knitr")

  cat("\n\n=== PANEL SDM MODEL ===\n")
  cat("Spatial Durbin Panel Model with Fixed Effects\n")
  cat("Model: y_it = ρWy_it + X_it β + WX_it θ + α_i + ε_it\n\n")

  # Exclude empty geometries and simplify
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  # Create spatial weights
  centroids <- sf::st_centroid(unique(data_model$geometry))
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  # Prepare formula
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  # Create panel data structure
  data_for_panel <- sf::st_drop_geometry(data_model)
  panel_data <- pdata.frame(data_for_panel, index = index_vars)

  # Fit Panel SDM model
  panel_sdm_result <- splm::spml(
    formula = fo,
    data = panel_data,
    listw = listw,
    model = "within",
    effect = "individual",
    lag = TRUE,
    spatial.error = "none",
    Durbin = TRUE  # Adds WX terms
  )

  cat("\nPanel SDM Model Summary:\n")
  print(summary(panel_sdm_result))

  # Extract coefficients
  coef_summary <- summary(panel_sdm_result)$CoefTable
  
  # Extract and display key interpretation metrics
  cat("\n=== Panel SDM Interpretation Guide ===\n")
  cat("Rho (spatial lag parameter):", round(panel_sdm_result$spat.coef, 4), "\n")
  cat("- Measures strength of spatial dependence in Y\n\n")

  cat("Panel SDM includes:\n")
  cat("- Fixed effects (α_i): time-invariant unit-specific factors\n")
  cat("- Spatially lagged Y (ρWy): outcome diffusion across space\n")
  cat("- Spatially lagged X (WXθ): spillover effects of covariates\n")
  cat("Direct effects: impact on the unit itself\n")
  cat("Indirect effects: spillover impact on neighbors\n")

  # Create results table
  results_table <- data.frame(
    Variable = c("rho (spatial lag)", rownames(coef_summary)),
    Coefficient = c(panel_sdm_result$spat.coef, coef_summary[, "Estimate"]),
    Std_Error = c(NA, coef_summary[, "Std. Error"]),
    t_value = c(NA, coef_summary[, "t-value"]),
    p_value = c(NA, coef_summary[, "Pr(>|t|)"])
  )

  # Save to CSV
  write.csv(
    results_table,
    file.path("2026 Thesis", "Data Outputs", "Results", "panel_sdm_model_results.csv"),
    row.names = FALSE
  )

  cat("\nModel results saved to: Data Outputs/Results/panel_sdm_model_results.csv\n")

  cat("\n=== Panel SDM Model Results ===\n")
  print(kable(results_table, format = "markdown", digits = 4))

  # Model comparison
  fe_model <- plm(fo, data = panel_data, model = "within", effect = "individual")

  # Calculate R-squared manually for panel models
  y_var <- panel_data[[response_name]]
  panel_sdm_residuals <- residuals(panel_sdm_result)
  ssr_panel_sdm <- sum(panel_sdm_residuals^2)
  sst <- sum((y_var - mean(y_var, na.rm = TRUE))^2)
  r_squared_panel_sdm <- 1 - (ssr_panel_sdm / sst)
  
  fe_residuals <- residuals(fe_model)
  ssr_fe <- sum(fe_residuals^2)
  r_squared_fe <- 1 - (ssr_fe / sst)

  # Extract log-likelihood from splm model (stored in model object)
  ll_panel_sdm <- panel_sdm_result$logLik
  
  # Calculate log-likelihood manually for plm model
  # LogLik = -n/2 * (log(2*pi) + log(sigma^2) + 1)
  n <- nrow(panel_data)
  sigma2_fe <- ssr_fe / n
  ll_fe <- -n/2 * (log(2*pi) + log(sigma2_fe) + 1)
  
  # Calculate AIC and BIC manually
  k_panel_sdm <- length(coef(panel_sdm_result)) + 1  # parameters + rho
  k_fe <- length(coef(fe_model)) + 1
  
  aic_panel_sdm <- -2 * ll_panel_sdm + 2 * k_panel_sdm
  aic_fe <- -2 * ll_fe + 2 * k_fe
  
  bic_panel_sdm <- -2 * ll_panel_sdm + log(n) * k_panel_sdm
  bic_fe <- -2 * ll_fe + log(n) * k_fe

  cat("\n\n=== MODEL COMPARISON ===\n")
  comparison_table <- data.frame(
    Model = c("Fixed Effects", "Panel SDM"),
    R_squared = c(
      r_squared_fe,
      r_squared_panel_sdm
    ),
    LogLik = c(
      ll_fe,
      ll_panel_sdm
    ),
    AIC = c(
      aic_fe,
      aic_panel_sdm
    ),
    BIC = c(
      bic_fe,
      bic_panel_sdm
    )
  )

  print(kable(comparison_table, format = "markdown", digits = 4))
  
  # Likelihood Ratio Test (Panel SDM vs Fixed Effects)
  lr_stat <- 2 * (ll_panel_sdm - ll_fe)
  # Panel SDM has p spatial lag parameters (1 for rho + k for WX terms)
  df_diff <- k_panel_sdm - k_fe
  lr_pvalue <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
  cat("\n\nLikelihood Ratio Test (Panel SDM vs Fixed Effects):\n")
  cat("LR statistic:", round(lr_stat, 3), "\n")
  cat("Degrees of freedom:", df_diff, "\n")
  cat("p-value:", format.pval(lr_pvalue), "\n")
  if(lr_pvalue < 0.05) {
    cat("✓ Panel SDM model is significantly better than Fixed Effects\n")
  } else {
    cat("⚠ No significant improvement over Fixed Effects\n")
  }
  
  cat("\nInterpretation:\n")
  cat("- High R² (>", round(r_squared_panel_sdm, 2), ") is normal for panel FE models\n")
  cat("- Rho =", round(panel_sdm_result$spat.coef, 3), "indicates", 
      ifelse(panel_sdm_result$spat.coef > 0.5, "strong", "moderate"), 
      "spatial dependence\n")
  cat("- Lower AIC/BIC = better model fit (penalizes complexity)\n")
  cat("- SDM captures both outcome diffusion (ρWy) and covariate spillovers (WXθ)\n")

  # Residual diagnostics
  panel_residuals <- residuals(panel_sdm_result)
  
  cat("\n\n=== RESIDUAL DIAGNOSTICS ===\n")
  cat("Mean of residuals:", round(mean(panel_residuals), 6), "\n")
  cat("SD of residuals:", round(sd(panel_residuals), 4), "\n")
  
  # Plot residuals
  plot(panel_residuals, main = "Panel SDM Model Residuals", ylab = "Residuals", xlab = "Observation")
  abline(h = 0, col = "red", lty = 2)

  # Save model object
  saveRDS(
    panel_sdm_result,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0("panel_sdm_model_results_", response_name, ".RDS"))
  )
  cat("\nModel object saved to: Data Outputs/Results/panel_sdm_model_results_", response_name, ".RDS\n", sep = "")

  return(panel_sdm_result)
}

# Panel SEM Model
panel_sem_model <- function(data_model, response_name, covariate_names, index_vars = c("region_id", "year")) {
  require("plm")
  require("splm")
  require("spdep")
  require("knitr")

  cat("\n\n=== PANEL SEM MODEL ===\n")
  cat("Spatial Error Panel Model with Fixed Effects\n")
  cat("Model: y_it = X_it β + α_i + u_it, where u_it = λWu_it + ε_it\n\n")

  # Exclude empty geometries and simplify
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  # Create spatial weights
  centroids <- sf::st_centroid(unique(data_model$geometry))
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  # Prepare formula
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  # Create panel data structure
  data_for_panel <- sf::st_drop_geometry(data_model)
  panel_data <- pdata.frame(data_for_panel, index = index_vars)

  # Fit Panel SEM model
  panel_sem_result <- splm::spml(
    formula = fo,
    data = panel_data,
    listw = listw,
    model = "within",
    effect = "individual",
    lag = FALSE,
    spatial.error = "b"  # KKP (Kapoor, Kelejian, Prucha) error model
  )

  cat("\nPanel SEM Model Summary:\n")
  print(summary(panel_sem_result))

  # Extract coefficients
  coef_summary <- summary(panel_sem_result)$CoefTable
  
  # Extract and display key interpretation metrics
  cat("\n=== Panel SEM Interpretation Guide ===\n")
  cat("Lambda (spatial error parameter):", round(panel_sem_result$spat.coef, 4), "\n")
  cat("- Measures strength of spatial dependence in errors\n")
  cat("- Positive = errors are spatially correlated\n\n")

  cat("Panel SEM assumes:\n")
  cat("- Fixed effects (α_i): time-invariant unit-specific factors\n")
  cat("- No diffusion in outcome variable (no ρWy term)\n")
  cat("- Spatial correlation comes from unobserved factors\n")
  cat("- Coefficients represent direct effects only\n")

  # Create results table
  results_table <- data.frame(
    Variable = c("lambda (spatial error)", rownames(coef_summary)),
    Coefficient = c(panel_sem_result$spat.coef, coef_summary[, "Estimate"]),
    Std_Error = c(NA, coef_summary[, "Std. Error"]),
    t_value = c(NA, coef_summary[, "t-value"]),
    p_value = c(NA, coef_summary[, "Pr(>|t|)"])
  )

  # Save to CSV
  write.csv(
    results_table,
    file.path("2026 Thesis", "Data Outputs", "Results", "panel_sem_model_results.csv"),
    row.names = FALSE
  )

  cat("\nModel results saved to: Data Outputs/Results/panel_sem_model_results.csv\n")

  cat("\n=== Panel SEM Model Results ===\n")
  print(kable(results_table, format = "markdown", digits = 4))

  # Model comparison
  fe_model <- plm(fo, data = panel_data, model = "within", effect = "individual")

  # Calculate R-squared manually for panel models
  y_var <- panel_data[[response_name]]
  panel_sem_residuals <- residuals(panel_sem_result)
  ssr_panel_sem <- sum(panel_sem_residuals^2)
  sst <- sum((y_var - mean(y_var, na.rm = TRUE))^2)
  r_squared_panel_sem <- 1 - (ssr_panel_sem / sst)
  
  fe_residuals <- residuals(fe_model)
  ssr_fe <- sum(fe_residuals^2)
  r_squared_fe <- 1 - (ssr_fe / sst)

  # Extract log-likelihood from splm model (stored in model object)
  ll_panel_sem <- panel_sem_result$logLik
  
  # Calculate log-likelihood manually for plm model
  # LogLik = -n/2 * (log(2*pi) + log(sigma^2) + 1)
  n <- nrow(panel_data)
  sigma2_fe <- ssr_fe / n
  ll_fe <- -n/2 * (log(2*pi) + log(sigma2_fe) + 1)
  
  # Calculate AIC and BIC manually
  k_panel_sem <- length(coef(panel_sem_result)) + 1  # parameters + lambda
  k_fe <- length(coef(fe_model)) + 1
  
  aic_panel_sem <- -2 * ll_panel_sem + 2 * k_panel_sem
  aic_fe <- -2 * ll_fe + 2 * k_fe
  
  bic_panel_sem <- -2 * ll_panel_sem + log(n) * k_panel_sem
  bic_fe <- -2 * ll_fe + log(n) * k_fe

  cat("\n\n=== MODEL COMPARISON ===\n")
  comparison_table <- data.frame(
    Model = c("Fixed Effects", "Panel SEM"),
    R_squared = c(
      r_squared_fe,
      r_squared_panel_sem
    ),
    LogLik = c(
      ll_fe,
      ll_panel_sem
    ),
    AIC = c(
      aic_fe,
      aic_panel_sem
    ),
    BIC = c(
      bic_fe,
      bic_panel_sem
    )
  )
  
  print(kable(comparison_table, format = "markdown", digits = 4))
  
  # Likelihood Ratio Test (Panel SEM vs Fixed Effects)
  lr_stat <- 2 * (ll_panel_sem - ll_fe)
  lr_pvalue <- pchisq(lr_stat, df = 1, lower.tail = FALSE)
  cat("\n\nLikelihood Ratio Test (Panel SEM vs Fixed Effects):\n")
  cat("LR statistic:", round(lr_stat, 3), "\n")
  cat("p-value:", format.pval(lr_pvalue), "\n")
  if(lr_pvalue < 0.05) {
    cat("✓ Panel SEM model is significantly better than Fixed Effects\n")
  } else {
    cat("⚠ No significant improvement over Fixed Effects\n")
  }
  
  cat("\nInterpretation: Lambda =", round(panel_sem_result$spat.coef, 3), 
      "indicates spatial correlation in unobserved factors.\n")

  # Residual diagnostics
  panel_residuals <- residuals(panel_sem_result)
  
  cat("\n\n=== RESIDUAL DIAGNOSTICS ===\n")
  cat("Mean of residuals:", round(mean(panel_residuals), 6), "\n")
  cat("SD of residuals:", round(sd(panel_residuals), 4), "\n")
  
  # Plot residuals
  plot(panel_residuals, main = "Panel SEM Model Residuals", ylab = "Residuals", xlab = "Observation")
  abline(h = 0, col = "red", lty = 2)

  # Save model object
  saveRDS(
    panel_sem_result,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0("panel_sem_model_results_", response_name, ".RDS"))
  )
  cat("\nModel object saved to: Data Outputs/Results/panel_sem_model_results_", response_name, ".RDS\n", sep = "")

  return(panel_sem_result)
}

# Panel SDEM Model
panel_sdem_model <- function(data_model, response_name, covariate_names, index_vars = c("region_id", "year")) {
  require("plm")
  require("splm")
  require("spdep")
  require("knitr")

  cat("\n\n=== PANEL SDEM MODEL ===\n")
  cat("Spatial Durbin Error Panel Model with Fixed Effects\n")
  cat("Model: y_it = X_it β + WX_it θ + α_i + u_it, where u_it = λWu_it + ε_it\n\n")

  # Exclude empty geometries and simplify
  data_model <- data_model[!sf::st_is_empty(data_model$geometry), ]
  data_model <- sf::st_simplify(data_model)

  # Create spatial weights
  centroids <- sf::st_centroid(unique(data_model$geometry))
  coords <- st_coordinates(centroids)
  knn_nb <- knearneigh(coords, k = 5)
  nb <- knn2nb(knn_nb)
  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  # Prepare formula
  fo <- as.formula(paste(
    response_name,
    "~",
    paste(covariate_names, collapse = " + ")
  ))

  # Create panel data structure
  data_for_panel <- sf::st_drop_geometry(data_model)
  panel_data <- pdata.frame(data_for_panel, index = index_vars)

  # Fit Panel SDEM model
  panel_sdem_result <- splm::spml(
    formula = fo,
    data = panel_data,
    listw = listw,
    model = "within",
    effect = "individual",
    lag = FALSE,
    spatial.error = "b",
    Durbin = TRUE  # Adds WX terms
  )

  cat("\nPanel SDEM Model Summary:\n")
  print(summary(panel_sdem_result))

  # Extract coefficients
  coef_summary <- summary(panel_sdem_result)$CoefTable
  
  # Extract and display key interpretation metrics
  cat("\n=== Panel SDEM Interpretation Guide ===\n")
  cat("Lambda (spatial error parameter):", round(panel_sdem_result$spat.coef, 4), "\n")
  cat("- Measures strength of spatial dependence in errors\n\n")

  cat("Panel SDEM includes:\n")
  cat("- Fixed effects (α_i): time-invariant unit-specific factors\n")
  cat("- Spatially lagged X (WXθ): spillover effects of covariates\n")
  cat("- Spatial error (λWu): spatially correlated unobserved factors\n")
  cat("Direct effects: impact on the unit itself\n")
  cat("Indirect effects: spillover impact on neighbors (from WX)\n")

  # Create results table
  results_table <- data.frame(
    Variable = c("lambda (spatial error)", rownames(coef_summary)),
    Coefficient = c(panel_sdem_result$spat.coef, coef_summary[, "Estimate"]),
    Std_Error = c(NA, coef_summary[, "Std. Error"]),
    t_value = c(NA, coef_summary[, "t-value"]),
    p_value = c(NA, coef_summary[, "Pr(>|t|)"])
  )

  # Save to CSV
  write.csv(
    results_table,
    file.path("2026 Thesis", "Data Outputs", "Results", "panel_sdem_model_results.csv"),
    row.names = FALSE
  )

  cat("\nModel results saved to: Data Outputs/Results/panel_sdem_model_results.csv\n")

  cat("\n=== Panel SDEM Model Results ===\n")
  print(kable(results_table, format = "markdown", digits = 4))

  # Model comparison
  fe_model <- plm(fo, data = panel_data, model = "within", effect = "individual")

  # Calculate R-squared manually for panel models
  y_var <- panel_data[[response_name]]
  panel_sdem_residuals <- residuals(panel_sdem_result)
  ssr_panel_sdem <- sum(panel_sdem_residuals^2)
  sst <- sum((y_var - mean(y_var, na.rm = TRUE))^2)
  r_squared_panel_sdem <- 1 - (ssr_panel_sdem / sst)
  
  fe_residuals <- residuals(fe_model)
  ssr_fe <- sum(fe_residuals^2)
  r_squared_fe <- 1 - (ssr_fe / sst)

  # Extract log-likelihood from splm model (stored in model object)
  ll_panel_sdem <- panel_sdem_result$logLik
  
  # Calculate log-likelihood manually for plm model
  # LogLik = -n/2 * (log(2*pi) + log(sigma^2) + 1)
  n <- nrow(panel_data)
  sigma2_fe <- ssr_fe / n
  ll_fe <- -n/2 * (log(2*pi) + log(sigma2_fe) + 1)
  
  # Calculate AIC and BIC manually
  k_panel_sdem <- length(coef(panel_sdem_result)) + 1  # parameters + lambda
  k_fe <- length(coef(fe_model)) + 1
  
  aic_panel_sdem <- -2 * ll_panel_sdem + 2 * k_panel_sdem
  aic_fe <- -2 * ll_fe + 2 * k_fe
  
  bic_panel_sdem <- -2 * ll_panel_sdem + log(n) * k_panel_sdem
  bic_fe <- -2 * ll_fe + log(n) * k_fe

  cat("\n\n=== MODEL COMPARISON ===\n")
  comparison_table <- data.frame(
    Model = c("Fixed Effects", "Panel SDEM"),
    R_squared = c(
      r_squared_fe,
      r_squared_panel_sdem
    ),
    LogLik = c(
      ll_fe,
      ll_panel_sdem
    ),
    AIC = c(
      aic_fe,
      aic_panel_sdem
    ),
    BIC = c(
      bic_fe,
      bic_panel_sdem
    )
  )
  
  print(kable(comparison_table, format = "markdown", digits = 4))
  
  # Likelihood Ratio Test (Panel SDEM vs Fixed Effects)
  lr_stat <- 2 * (ll_panel_sdem - ll_fe)
  df_diff <- k_panel_sdem - k_fe
  lr_pvalue <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
  cat("\n\nLikelihood Ratio Test (Panel SDEM vs Fixed Effects):\n")
  cat("LR statistic:", round(lr_stat, 3), "\n")
  cat("Degrees of freedom:", df_diff, "\n")
  cat("p-value:", format.pval(lr_pvalue), "\n")
  if(lr_pvalue < 0.05) {
    cat("✓ Panel SDEM model is significantly better than Fixed Effects\n")
  } else {
    cat("⚠ No significant improvement over Fixed Effects\n")
  }
  
  cat("\nInterpretation: SDEM captures covariate spillovers (WX) and spatial error correlation (λWu).\n")

  # Residual diagnostics
  panel_residuals <- residuals(panel_sdem_result)
  
  cat("\n\n=== RESIDUAL DIAGNOSTICS ===\n")
  cat("Mean of residuals:", round(mean(panel_residuals), 6), "\n")
  cat("SD of residuals:", round(sd(panel_residuals), 4), "\n")
  
  # Plot residuals
  plot(panel_residuals, main = "Panel SDEM Model Residuals", ylab = "Residuals", xlab = "Observation")
  abline(h = 0, col = "red", lty = 2)

  # Save model object
  saveRDS(
    panel_sdem_result,
    file.path("2026 Thesis", "Data Outputs", "Results", paste0("panel_sdem_model_results_", response_name, ".RDS"))
  )
  cat("\nModel object saved to: Data Outputs/Results/panel_sdem_model_results_", response_name, ".RDS\n", sep = "")

  return(panel_sdem_result)
}
