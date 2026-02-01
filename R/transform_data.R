comparison_by_elections <- function(
  data_previous, data_current, key_previous, key_current
) {
  data_sum_1 <- as.data.frame(data_previous) %>%
    dplyr::group_by(territory_nuts4, party_classification) %>%
    dplyr::summarise(votes = sum(votes, na.rm = TRUE), .groups = 'drop') %>%
    # turn to wide format based on party classification for easier comparison
    tidyr::pivot_wider(
      names_from = party_classification, values_from = votes, values_fill = 0
    )

  data_sum_2 <- as.data.frame(data_current) %>%
    dplyr::group_by(territory_nuts4, party_classification) %>%
    dplyr::summarise(votes = sum(votes, na.rm = TRUE), .groups = 'drop') %>%
    # turn to wide format based on party classification for easier comparison
    tidyr::pivot_wider(
      names_from = party_classification, values_from = votes, values_fill = 0
    )

  # create a data set to visaulize development in support between elections
  data_comp <- merge(
    data_sum_1[, c("territory_nuts4", "far right", "other")],
    data_sum_2[, c("territory_nuts4", "far right", "other")],
    by = c("territory_nuts4"),
    suffixes = c(paste0("_", key_previous), paste0("_", key_current)),
    all = TRUE
  )

  data_comp$far_right_difference <- data_comp[[
    paste0("far right_", key_current)
  ]] - data_comp[[paste0("far right_", key_previous)]]
  data_comp$other_difference <- data_comp[[
    paste0("other_", key_current)
  ]] - data_comp[[paste0("other_", key_previous)]]

  # add in geometry
  data_comp <- merge(
    unique(data_previous[, c("territory_nuts4", "geometry")]),
    data_comp,
    by = c("territory_nuts4"),
    all.y = TRUE
  )
  return(data_comp)
}

join_covariates_data <- function(
  country = "PL",
  year = 2023,
  data_list = list(
    dt_age = "read_age_xlsx",
    dt_density = "read_density_xlsx",
    # dt_migrations = "read_migrations_xlsx",
    dt_migrations_yearly = "read_migrations_yearly_xlsx",
    dt_indicators = "read_indicators_xlsx",
    dt_family_benefits = "read_family_benefits_xlsx",
    dt_revenue = "read_revenue_xlsx",
    dt_unemployment = "read_unemployment_xlsx",
    dt_partitions = "load_partitions_classification"
  )
) {

  data_l <- list()
  for (data_name in names(data_list)) {
    if (data_name %in% c("dt_density", "dt_age", "dt_migrations")) {
      func <- get(data_list[[data_name]])
      dt <- func(paste0(country, "_", year, "_NUTS3_", gsub("dt_", "", data_name)))

      if (data_name == "dt_age") {
        dt <- data.table::dcast(
          dt, territory_nuts4 ~ age_classification,
          value.var = "population_percentage", fun.aggregate = sum
        )
        names(dt)[2:7] <- paste0("group_", gsub("-", "_", names(dt)[2:7]))
        names(dt) <- gsub("+", "plus", names(dt), fixed = TRUE)
      }

      data_l[[data_name]] <- unique(dt)[, territory_nuts4_name := NULL]
      next
    }
    func <- get(data_list[[data_name]])
    data_l[[data_name]] <- unique(func())[, territory_nuts4_name := NULL]
  }

  # merge all data tables in the list together
  data_full <- Reduce(function(x, y) merge(x, y, by = "territory_nuts4", all = TRUE), data_l)

  # remove any y columns that were created during the merge
  data_full <- data_full[, !grepl("\\.y$", names(data_full)), with = FALSE]

  # rename any x columns to remove the _x suffix
  names(data_full) <- gsub("\\.x$", "", names(data_full))

  summary(data_full)
  return(data_full)
}


#' Apply outlier scaling to specified columns in a data.table/data.frame/sf object
#'
#' Args:
#'   data: data.table, data.frame, or sf object
#'   value_columns: character vector of column names to scale
#'   method: "percentile", "iqr", "winsorize", "log", "sqrt", "cbrt", "asinh", or "none"
#'   suffix: suffix to add to new column names (use "" to overwrite original)
#'
#' Returns:
#'   Modified data with scaled columns
scale_outliers <- function(
  data, value_columns, method = "percentile", suffix = "_scaled"
) {

  data_copy <- data.table::as.data.table(data)

  for (col in value_columns) {
    if (!col %in% names(data_copy)) {
      warning(paste("Column", col, "not found in data. Skipping."))
      next
    }

    values <- data_copy[[col]]
    new_col_name <- if (suffix == "") col else paste0(col, suffix)

    if (method == "percentile") {
      # Cap at 2nd and 98th percentile
      lower_bound <- quantile(values, 0.02, na.rm = TRUE)
      upper_bound <- quantile(values, 0.98, na.rm = TRUE)
      data_copy[[new_col_name]] <- pmax(pmin(values, upper_bound), lower_bound)

    } else if (method == "iqr") {
      # Use IQR method (good for asymmetric distributions)
      q1 <- quantile(values, 0.25, na.rm = TRUE)
      q3 <- quantile(values, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- max(q1 - 1.5 * iqr, min(values, na.rm = TRUE))
      upper_bound <- min(q3 + 1.5 * iqr, max(values, na.rm = TRUE))
      data_copy[[new_col_name]] <- pmax(pmin(values, upper_bound), lower_bound)

    } else if (method == "winsorize") {
      # Winsorize at 1st and 99th percentile
      lower_bound <- quantile(values, 0.01, na.rm = TRUE)
      upper_bound <- quantile(values, 0.99, na.rm = TRUE)
      data_copy[[new_col_name]] <- pmax(pmin(values, upper_bound), lower_bound)

    } else if (method == "log") {
      # Log transformation (strong compression, good for highly skewed data)
      # Uses log1p to handle values near zero, preserves sign for negative values
      data_copy[[new_col_name]] <- sign(values) * log1p(abs(values))

    } else if (method == "sqrt") {
      # Square root transformation (moderate compression, milder than log)
      # Works well for count data and moderately skewed distributions
      data_copy[[new_col_name]] <- sign(values) * sqrt(abs(values))

    } else if (method == "cbrt") {
      # Cube root transformation (gentle compression, works with negative values)
      # Good for mildly skewed data, preserves zeros
      data_copy[[new_col_name]] <- sign(values) * abs(values)^(1/3)

    } else if (method == "asinh") {
      # Inverse hyperbolic sine (similar to log but handles zeros/negatives better)
      # Best for data with zeros and negative values, smooth around zero
      data_copy[[new_col_name]] <- asinh(values)

    } else if (method == "none") {
      data_copy[[new_col_name]] <- values
    }
  }

  # Preserve sf class if input was sf
  if (inherits(data, "sf")) {
    data_copy <- sf::st_as_sf(data_copy)
  }

  return(data_copy)
}
