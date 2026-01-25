load_data <- function(country_mapping, col_mapping) {

  data_input_path <- "2026 Thesis/Data Inputs"

  # map the column names to standardized names
  col_mapping <- list(
    population = c("Mieszkańcy", "Population", "Einwohner"),
    territory_nuts4 = c("TERYT Gminy", "Kod TERYT", "TERYT", "Area", "Fläche"),
    territory_nuts4_name = c("Gmina"),
    territory_nuts3 = c("Powierzchnia", "Area", "Fläche"),
    territory_nuts2 = c("Powierzchnia", "Area", "Fläche"),
    turnout = c("Frekwencja", "Turnout", "Wahlbeteiligung"),
    party_name = c("Komitet", "Party", "Partei", "party")
  )

  data_list <- list()
  for (key in seq_len(nrow(country_mapping))) {

    data_path <- glue("2026 Thesis/Data Outputs/{country_mapping$country[key]}_{country_mapping$year[key]}_{country_mapping$region_type[key]}.RDS")

    if (file.exists(data_path)) {
      message(glue("Reading existing processed data for {country_mapping$country[key]}|{country_mapping$year[key]}|{country_mapping$region_type[key]}"))
      data_shp <- readRDS(data_path)
      data_list[[key]] <- data_shp
      next
    }
    # Load spatial data
    data <- data.table::fread(
      glue("{data_input_path}/{country_mapping$country[key]}_{country_mapping$year[key]}_{country_mapping$region_type[key]}_{country_mapping$election_type[key]}_elections.csv")
    )
    # standardize column names based on mapping
    for (std_name in names(col_mapping)) {
      possible_names <- col_mapping[[std_name]]
      for (name in possible_names) {
        if (name %in% colnames(data)) {
          setnames(data, name, std_name)
          break
        }
      }
    }
    if (key %in% c(1, 2, 3)) {

      # clean data set
      data[, territory_nuts4_name := tolower(territory_nuts4_name)]
      data <- data[territory_nuts4_name != "zagranica"]
      names(data) <- tolower(names(data))
      # turn to long format
      data <- data.table::melt(
        data,
        id.vars = c("territory_nuts4", "territory_nuts4_name", "turnout"),
        measure.vars = patterns("komitet"),
        variable.name = "party_name",
        value.name = "votes"
      )
      # replace commas with dots and convert to numeric
      data[, votes := as.numeric(gsub(",", ".", votes))]
      data[, turnout := as.numeric(gsub(",", ".", turnout))]
    }
    # standardize column names based on mapping
    for (std_name in names(col_mapping)) {
      possible_names <- col_mapping[[std_name]]
      for (name in possible_names) {
        if (name %in% colnames(data)) {
          setnames(data, name, std_name)
          break
        }
      }
    }
    # turn party to lower case
    data[, party_name := tolower(party_name)]

    # add key data
    data <- cbind(
      country_mapping[key, ],
      data
    )

    # join shp data
    shp <- sf::st_read(
      glue("{data_input_path}/shapefiles/{country_mapping$country[key]}_{country_mapping$region_type[key]}.shp"),
      quiet = TRUE
    )

    shp$JPT_KOD_JE <- as.numeric(substr(shp$JPT_KOD_JE, 1, 6))

    data_shp <- merge(
      shp[, c("JPT_KOD_JE", "geometry")],
      data,
      by.x = "JPT_KOD_JE",
      by.y = "territory_nuts4",
      all.y = TRUE,
      all.x = FALSE
    )

    # identify which shp entries have missing geometries
    missing_geom <- which(is.na(sf::st_dimension(data_shp$geometry)))

    missing_geom <- unique(data_shp[missing_geom, ]$JPT_KOD_JE)

    # load a different shp to fill in missing geometries
    # join shp data
    shp <- sf::st_read(
      glue("{data_input_path}/shapefiles/{country_mapping$country[key]}_NUTS4.shp"),
      quiet = TRUE
    )

    shp$JPT_KOD_JE <- as.numeric(substr(shp$JPT_KOD_JE, 1, 6))

    data_shp_missing <- merge(
      shp[shp$JPT_KOD_JE %in% missing_geom, c("JPT_KOD_JE", "geometry")],
      data[territory_nuts4 %in% missing_geom],
      by.x = "JPT_KOD_JE",
      by.y = "territory_nuts4",
      all.y = TRUE,
      all.x = FALSE
    )

    # combine both data_shp
    data_shp <- rbind(
      data_shp[!data_shp$JPT_KOD_JE %in% missing_geom, ],
      data_shp_missing
    )

    # identify which shp entries have missing geometries
    missing_geom <- which(is.na(sf::st_dimension(data_shp$geometry)))

    if (length(missing_geom) > 0) {
      warning(glue(glue("There are still missing geometries in {country_mapping$country[key]}|{country_mapping$year[key]}|{country_mapping$region_type[key]}!")))
    }

    names(data_shp)[names(data_shp) == "JPT_KOD_JE"] <- "territory_nuts4"

    # save processed data
    saveRDS(data_shp, data_path)

    data_list[[key]] <- data_shp
  }

  return(data_list)
}


read_age_xlsx <- function(file_path) {
  # identify file format
  file <- list.files(path = "2026 Thesis/Data Inputs/population", pattern = file_path, full.names = TRUE)

  # check which sheets are available
  data <- lapply(readxl::excel_sheets(file), function(sheet_name) {  
    if (grepl("xlsx", file)) {
      data <- data.table::setDT(suppressMessages(readxl::read_xlsx(
        file, sheet = sheet_name,
        skip = 5
      )))
    } else {
      data <- data.table::setDT(suppressMessages(readxl::read_xls(
        file, sheet = sheet_name,
        skip = 5
      )))
    }
    # restrict columns to relevant ones- very arbitrary but works for now
    data <- data[, c(1:11)]

    # name columns
    names(data) <- c(
      "age_group", "territory_nuts4", "population", "population_male",
      "population_female", "population_city", "population_city_male",
      "population_city_female", "population_rural", "population_rural_male",
      "population_rural_female"
    )
    data <- data[!is.na(age_group)]
    
    # fill in the territory names by what is above it in the excel
    data[, territory_nuts4 := zoo::na.locf(territory_nuts4)]

    # remove any rows inside of age_group that come with characters but keep "70 i więcej" and "[0-9][0-9]   -    [0-9][0-9]" and just values
    data <- data[grepl("70 lat i więcej|^([0-9]{2}   -    [0-9]{2}|[0-9]{1}   -    [0-9]{1})$|[0-9]$|[0-9][0-9]$", age_group)]
    # remove the specific age groups rows to keep only the totals
    data <- data[!grepl("0    -    4|5    -    9|10   -    14|15   -    19|20   -    24|25   -    29|20   -    29|15   -    24", age_group)]
    # print("The following age groups are present in the data:")
    # print(data$age_group |> unique())

    # create age groups
    data[, age_classification := fcase(
      age_group %in% as.character(0:17), "0-17",
      age_group %in% as.character(18:24), "18-24",
      age_group %in% c(as.character(25:29)) | grepl(" 34$", age_group), "25-34",
      grepl("35 |40 ", age_group), "35-44",
      grepl("45 |50 |55 |60 ", age_group), "45-64",
      grepl("65 |70 lat i|70 |75 |80 ", age_group), "65+"
    )]
    if (any(is.na(data$age_classification))) {
      print(unique(data[is.na(age_classification), .(age_group, age_classification)]))
      stop("Some age groups were not classified properly.")
    }
    # turn population data to numeric
    cols <- grepv("population", names(data))
    data[, (cols) := lapply(.SD, function(x) as.numeric(gsub(" ", "", x))), .SDcols = cols]
    
    # aggregate by age classification
    data <- data[,
      lapply(.SD, sum, na.rm = TRUE),
      by = .(territory_nuts4, age_classification), .SDcols = cols
    ]
    # turn territory_nuts4 to numeric
    data[, territory_nuts4 := as.numeric(territory_nuts4)]
    # remove the last number from territory_nuts4
    data[, territory_nuts4 := as.numeric(substr(territory_nuts4, 1, nchar(territory_nuts4) - 1))]

    # create a new metric
    data[, population_city_percentage := (population_city / population) * 100]

    # add percentage within total population for each age group
    data[, population_percentage := (population / sum(population)) * 100, by = .(territory_nuts4)]

    return(data)
  })
  data_full <- rbindlist(data)
  return(data_full)
}
file_name <- "PL_2023_NUTS3_density"
read_density_xlsx <- function(file_name) {
  # identify file format
  file <- list.files(path = "2026 Thesis/Data Inputs/population", pattern = file_name, full.names = TRUE)

  table_mapping <- list(
    "PL_2015_NUTS3_density" = list(sheet = "TABL.26", skip = 11),
    "PL_2019_NUTS3_density" = list(sheet = "tabl. 21", skip = 4),
    "PL_2023_NUTS3_density" = list(sheet = "Tabl. 21", skip = 4),
    "PL_2025_NUTS3_density" = list(sheet = "Tabl. 21", skip = 4)
  )

  # check which sheets are available
  if (grepl("xlsx", file)) {
    data <- data.table::setDT(suppressMessages(readxl::read_xlsx(
      file, sheet = table_mapping[[file_name]]$sheet,
      skip = table_mapping[[file_name]]$skip
    )))
  } else {
    data <- data.table::setDT(suppressMessages(readxl::read_xls(
      file, sheet = table_mapping[[file_name]]$sheet,
      skip = table_mapping[[file_name]]$skip
    )))
  }
  # restrict columns to relevant ones- very arbitrary but works for now
  data <- data[, c(1, 2, 3, 5, 6, 8)]

  # name columns
  names(data) <- c(
    "territory_nuts4", "territory_nuts4_name", "area_ha", "area_km2",
    "population", "population_density_per_km2"
  )

  data <- data[!is.na(territory_nuts4)]
  data[, territory_nuts4_name := tolower(gsub("\\.", "", tolower(territory_nuts4_name)))]
  data[, territory_nuts4 := as.numeric(gsub(" .*", "", territory_nuts4))]

  # summarize by territory_nuts4
  data <- data[, .(
    area_ha = sum(area_ha, na.rm = TRUE),
    area_km2 = sum(area_km2, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ), by = .(territory_nuts4)]

  data[, population_density_per_km2 := population / area_km2]

  return(data)
}
file_name <- "PL_2023_NUTS3_migrations"
read_migrations_xlsx <- function(file_name) {
  # identify file format
  file <- list.files(path = "2026 Thesis/Data Inputs/population", pattern = file_name, full.names = TRUE)

  # check which sheets are available
  if (grepl("xlsx", file)) {
    data <- data.table::setDT(suppressMessages(readxl::read_xlsx(
      file, skip = 8
    )))
  } else {
    data <- data.table::setDT(suppressMessages(readxl::read_xls(
      file, skip = 8
    )))
  }
  # restrict columns to relevant ones- very arbitrary but works for now
  data <- data[, c(1:3, 9, 10, 12, 13, 16:20)]

  # name columns
  names(data) <- c(
    "territory_nuts4_name", "territory_nuts4", "population",
    "internal_migration_inflow", "internal_migration_outflow",
    "international_migration_inflow", "international_migration_outflow",
    "marriages_per_1000", "births_per_1000",
    "deaths_per_1000", "natural_increase_per_1000", "migration_balance_per_1000"
  )

  data <- data[!is.na(territory_nuts4)]

  # clean territory names
  data[, territory_nuts4_name := tolower(gsub("\\.", "", tolower(territory_nuts4_name)))]
  data[, territory_nuts4 := as.numeric(gsub(" .*", "", territory_nuts4))]

  # remove the last number from territory_nuts4
  data[, territory_nuts4 := as.numeric(substr(territory_nuts4, 1, nchar(territory_nuts4) - 1))]

  # turn migration columns to numeric
  data[, international_migration_inflow := as.numeric(international_migration_inflow)]
  data[, international_migration_outflow := as.numeric(international_migration_outflow)]

  # summarize by territory_nuts4
  data <- data[, .(
    population = sum(population, na.rm = TRUE),
    internal_migration_inflow = sum(internal_migration_inflow, na.rm = TRUE),
    internal_migration_outflow = sum(internal_migration_outflow, na.rm = TRUE),
    international_migration_inflow = sum(international_migration_inflow, na.rm = TRUE),
    international_migration_outflow = sum(international_migration_outflow, na.rm = TRUE)
  ), by = .(territory_nuts4)]

  # create per 1000 metrics
  data[, internal_migration_inflow_per_1000 := internal_migration_inflow / population * 1000]
  data[, internal_migration_outflow_per_1000 := internal_migration_outflow / population * 1000]

  # create per 1000 metrics
  data[, international_migration_inflow_per_1000 := international_migration_inflow / population * 1000]
  data[, international_migration_outflow_per_1000 := international_migration_outflow / population * 1000]

  return(data)
}

read_indicators_xlsx <- function() {
  data <- data.table::setDT(suppressMessages(readxl::read_xlsx(
    path = "2026 Thesis/Data Inputs/population/PL_NUTS3_indicators.xlsx",
    sheet = "TABLE",
    skip = 2
  )))

  # restrict columns to relevant ones- very arbitrary but works for now
  data <- data[, c(1, 2, 135:178)]

  # rename columns
  names(data) <- c(
    "territory_nuts4", "territory_nuts4_name",
    paste0("investment_expenditure_share_", 2003:2024),
    paste0("expenditure_k_pln_", 2003:2024)
  )

  data <- data[!is.na(territory_nuts4)]

  # clean territory names
  data[, territory_nuts4_name := tolower(gsub("\\.", "", tolower(territory_nuts4_name)))]
  data[, territory_nuts4 := as.numeric(gsub(" .*", "", territory_nuts4))]

  # remove the last number from territory_nuts4
  data[, territory_nuts4 := as.numeric(substr(territory_nuts4, 1, nchar(territory_nuts4) - 1))]

  # turn numeric columns to numeric
  cols <- names(data)[3:ncol(data)]
  data[, (cols) := lapply(.SD, function(x) as.numeric(gsub(" ", "", x))), .SDcols = cols]

  return(data)
}

read_family_benefits_xlsx <- function() {
  data <- data.table::setDT(suppressMessages(readxl::read_xlsx(
    path = "2026 Thesis/Data Inputs/population/PL_NUTS3_kids_share_benefiting_from_family_benefits.xlsx",
    sheet = "TABLICA",
    skip = 2
  )))

  # rename columns
  names(data) <- c(
    "territory_nuts4", "territory_nuts4_name",
    paste0("share_of_kids_until_17_yo_receiving_family_benefits_", 2011:2024)
  )

  data <- data[!is.na(territory_nuts4)]

  # clean territory names
  data[, territory_nuts4_name := tolower(gsub("\\.", "", tolower(territory_nuts4_name)))]
  data[, territory_nuts4 := as.numeric(gsub(" .*", "", territory_nuts4))]

  # remove the last number from territory_nuts4
  data[, territory_nuts4 := as.numeric(substr(territory_nuts4, 1, nchar(territory_nuts4) - 1))]

  # turn numeric columns to numeric
  cols <- names(data)[3:ncol(data)]
  data[, (cols) := lapply(.SD, function(x) as.numeric(gsub(" ", "", x))), .SDcols = cols]

  return(data)
}

read_revenue_xlsx <- function() {
  data <- data.table::setDT(suppressMessages(readxl::read_xlsx(
    path = "2026 Thesis/Data Inputs/population/PL_NUTS3_revenue_total.xlsx",
    sheet = "TABLE",
    skip = 3
  )))

  data <- data[, 1:24]

  # rename columns
  names(data) <- c(
    "territory_nuts4", "territory_nuts4_name",
    paste0("revenue_gmina_year_", 2003:2024)
  )

  data <- data[!is.na(territory_nuts4)]

  # clean territory names
  data[, territory_nuts4_name := tolower(gsub("\\.", "", tolower(territory_nuts4_name)))]
  data[, territory_nuts4 := as.numeric(gsub(" .*", "", territory_nuts4))]

  # remove the last number from territory_nuts4
  data[, territory_nuts4 := as.numeric(substr(territory_nuts4, 1, nchar(territory_nuts4) - 1))]

  # turn numeric and then summarize by territory_nuts4
  cols <- names(data)[3:ncol(data)]
  data <- data[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), by = .(territory_nuts4), .SDcols = cols]

  return(data)
}

read_unemployment_xlsx <- function() {
  data <- data.table::setDT(suppressMessages(readxl::read_xlsx(
    path = "2026 Thesis/Data Inputs/population/PL_NUTS3_unemployment.xlsx",
    sheet = "TABLE",
    skip = 3
  )))

  data <- data[, 1:16]

  # rename columns
  names(data) <- c(
    "territory_nuts4", "territory_nuts4_name",
    paste0("unemployed_population_year_", 2011:2024)
  )

  data <- data[!is.na(territory_nuts4)]

  # clean territory names
  data[, territory_nuts4_name := tolower(gsub("\\.", "", tolower(territory_nuts4_name)))]
  data[, territory_nuts4 := as.numeric(gsub(" .*", "", territory_nuts4))]

  # remove the last number from territory_nuts4
  data[, territory_nuts4 := as.numeric(substr(territory_nuts4, 1, nchar(territory_nuts4) - 1))]

  # turn numeric and then summarize by territory_nuts4
  cols <- names(data)[3:ncol(data)]
  data <- data[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), by = .(territory_nuts4), .SDcols = cols]

  return(data)
}

read_migrations_yearly_xlsx <- function() {
  data <- data.table::setDT(suppressMessages(readxl::read_xlsx(
    path = "2026 Thesis/Data Inputs/population/PL_NUTS3_migrations_yearly.xlsx",
    sheet = "TABLICA",
    skip = 3
  )))

  data <- data[, c(1:2, 17:44, 59:72)]

  # rename columns
  names(data) <- c(
    "territory_nuts4", "territory_nuts4_name",
    paste0("registration_internal_year_", 2011:2024),
    paste0("registration_from_abroad_year_", 2011:2024),
    paste0("deregistration_internal_year_", 2011:2024)
    # paste0("deregistration_from_abroad_year_", 2011:2024)
  )

  data <- data[!is.na(territory_nuts4)]

  # clean territory names
  data[, territory_nuts4_name := tolower(gsub("\\.", "", tolower(territory_nuts4_name)))]
  data[, territory_nuts4 := as.numeric(gsub(" .*", "", territory_nuts4))]

  # remove the last number from territory_nuts4
  data[, territory_nuts4 := as.numeric(substr(territory_nuts4, 1, nchar(territory_nuts4) - 1))]

  # turn numeric and then summarize by territory_nuts4
  cols <- names(data)[3:ncol(data)]
  data <- data[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), by = .(territory_nuts4), .SDcols = cols]

  cols_registration_internal <- paste0("registration_internal_year_", 2011:2023)
  cols_registration_abroad <- paste0("registration_from_abroad_year_", 2011:2023)
  cols_deregistration_internal <- paste0("deregistration_internal_year_", 2011:2023)
  # cols_deregistration_abroad <- paste0("deregistration_from_abroad_year_", 2011:2023)

  # calculate migartion balance per year until 2023
  data[,
    registration_balance_internal_2011_2023 := rowSums(.SD, na.rm = TRUE),
    .SDcols = c(cols_registration_internal, cols_deregistration_internal)
  ]

  # create an exposure to abroad migration metric
  data[,
    registration_abroad_2011_2023 := rowSums(.SD, na.rm = TRUE),
    .SDcols = cols_registration_abroad
  ]

  # turn numeric and then summarize by territory_nuts4
  cols <- names(data)[3:ncol(data)]
  data <- data[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), by = .(territory_nuts4), .SDcols = cols]

  # choose relevant columns to return
  data <- data[, .(
    territory_nuts4,
    registration_balance_internal_2011_2023,
    registration_abroad_2011_2023
  )]

  return(data)
}
