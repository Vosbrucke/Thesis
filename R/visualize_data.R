visualize_data <- function(
  data_vis, value_column, title = "Election Results Visualization", limits_to_100 = TRUE
) {
  perform_moran_test(data_vis, value_column)
  # data_vis <- data_vis[!sf::st_is_empty(data_vis$geometry), ]
  # data_vis <- sf::st_as_sf(data_vis)
  
  if (limits_to_100) {
    limits <- c(0, 100)
  } else {
    limits <- range(data_vis[[value_column]], na.rm = TRUE)
  }
  
  plot <- ggplot(data_vis) +
    geom_sf(aes_string(fill = value_column)) +
    theme_minimal() +
    # add white background for better visibility
    theme(
      panel.background = element_rect(fill = "white", color = "white"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    ) +
    labs(title = title, fill = value_column) +
    scale_fill_viridis_c(limits = limits, oob = scales::squish)

  ggsave(
    filename = glue::glue("2026 Thesis/Data Outputs/Plots/{gsub(' ', '_', tolower(title))}.png"),
    width = 10,
    height = 10,
    dpi = 400
  )
}

classify_partitions <- function(
  output_file = "2026 Thesis/Data Inputs/partition_classification.csv",
  border_file = NULL  # Optional: path to SVG file or coordinates file
) {
  require("shiny")
  require("leaflet")
  require("leaflet.extras")
  require("sf")
  
  cat("Loading and simplifying shapefile...\n")
  
  # Disable s2 spherical geometry to avoid topology errors
  sf::sf_use_s2(FALSE)
  
  # Load shapefile
  shp <- sf::st_read(
    "2026 Thesis/Data Inputs/shapefiles/PL_NUTS3.shp",
    quiet = TRUE
  )
  
  shp$JPT_KOD_JE <- as.numeric(substr(shp$JPT_KOD_JE, 1, 6))
  
  # Fix any invalid geometries before transformation
  shp <- sf::st_make_valid(shp)
  
  # Simplify geometry first (before transformation for speed)
  shp <- sf::st_simplify(shp, preserveTopology = TRUE, dTolerance = 100)
  
  # Transform to WGS84 for leaflet
  shp_wgs84 <- sf::st_transform(shp, 4326)
  
  # Load border coordinates if provided
  border_lines <- NULL
  if (!is.null(border_file) && file.exists(border_file)) {
    cat("Loading border file...\n")
    # Try to load as shapefile first, then as CSV with coordinates
    if (grepl("\\.(shp|geojson|gpkg)$", border_file, ignore.case = TRUE)) {
      border_lines <- sf::st_read(border_file, quiet = TRUE)
      border_lines <- sf::st_transform(border_lines, 4326)
    } else if (grepl("\\.csv$", border_file, ignore.case = TRUE)) {
      # Expect CSV with columns: line_id, lat, lng, partition
      border_coords <- read.csv(border_file, stringsAsFactors = FALSE)
      if (all(c("lat", "lng") %in% colnames(border_coords))) {
        border_lines <- border_coords
      }
    }
  }
  
  # Initialize partition column if not exists
  if (!"partition" %in% colnames(shp_wgs84)) {
    shp_wgs84$partition <- NA_character_
  }
  
  # Load existing classifications if file exists
  if (file.exists(output_file)) {
    existing <- read.csv(output_file, stringsAsFactors = FALSE)
    shp_wgs84$partition <- existing$partition[match(shp_wgs84$JPT_KOD_JE, existing$JPT_KOD_JE)]
  }
  
  # Define color palette
  partition_colors <- c(
    "Russian" = "#FF6B6B",
    "Prussian" = "#4ECDC4", 
    "Austrian" = "#FFE66D"
  )
  
  # Create Shiny UI
  ui <- fluidPage(
    titlePanel("Classify Polish Partitions (Pre-1918)"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Select Partition Type:"),
        radioButtons(
          "partition_type",
          "",
          choices = c("Russian", "Prussian", "Austrian"),
          selected = "Russian"
        ),
        hr(),
        h4("Selection Mode:"),
        radioButtons(
          "selection_mode",
          "",
          choices = c(
            "Single Click" = "single",
            "Multi-Select" = "multi",
            "Draw Area" = "draw"
          ),
          selected = "draw"
        ),
        conditionalPanel(
          condition = "input.selection_mode == 'multi'",
          actionButton("apply_to_selected", "Apply to Selected", class = "btn-success", style = "width: 100%;"),
          br(), br(),
          actionButton("clear_selection", "Clear Selection", class = "btn-secondary", style = "width: 100%;"),
          br(), br(),
          textOutput("selected_count")
        ),
        conditionalPanel(
          condition = "input.selection_mode == 'draw'",
          p(style = "color: #0066CC;", "Draw a rectangle or polygon on the map to select regions"),
          actionButton("clear_drawing", "Clear Drawing", class = "btn-secondary", style = "width: 100%;")
        ),
        hr(),
        checkboxInput("show_borders", "Show Historical Borders", value = TRUE),
        hr(),
        h4("Instructions:"),
        conditionalPanel(
          condition = "input.selection_mode == 'single'",
          p("Click on regions to classify them immediately")
        ),
        conditionalPanel(
          condition = "input.selection_mode == 'multi'",
          p("Click multiple regions, then 'Apply to Selected'")
        ),
        conditionalPanel(
          condition = "input.selection_mode == 'draw'",
          p("Use toolbar to draw rectangle/polygon over area"),
          p("Regions inside will be classified automatically")
        ),
        hr(),
        actionButton("save_btn", "Save Classifications", class = "btn-primary"),
        br(), br(),
        actionButton("clear_btn", "Clear All", class = "btn-warning"),
        hr(),
        h4("Statistics:"),
        verbatimTextOutput("stats"),
        hr(),
        p("Click 'Save Classifications' when done.")
      ),
      
      mainPanel(
        width = 9,
        leafletOutput("map", height = "800px")
      )
    )
  )
  
  # Create Shiny Server
  server <- function(input, output, session) {
    
    # Reactive values to store data
    rv <- reactiveValues(
      data = shp_wgs84,
      selected = c()  # Store selected region IDs for multi-select mode
    )
    
    # Function to get color for each region
    get_colors <- function(data, selected_ids = NULL) {
      colors <- rep("#CCCCCC", nrow(data))  # Default gray
      
      # Apply partition colors
      for (partition in names(partition_colors)) {
        colors[data$partition == partition] <- partition_colors[[partition]]
      }
      
      # Highlight selected regions in multi-select mode
      if (!is.null(selected_ids) && length(selected_ids) > 0) {
        selected_idx <- which(data$JPT_KOD_JE %in% selected_ids)
        colors[selected_idx] <- "#9900FF"  # Purple for selected
      }
      
      return(colors)
    }
    
    # Display selected count
    output$selected_count <- renderText({
      paste0("Selected: ", length(rv$selected), " regions")
    })
    
    # Render map
    output$map <- renderLeaflet({
      map <- leaflet(rv$data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          layerId = ~JPT_KOD_JE,
          fillColor = get_colors(rv$data),
          fillOpacity = 0.7,
          color = "#000000",
          weight = 1,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~paste0(JPT_NAZWA_, " (", JPT_KOD_JE, ")"),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) %>%
        addDrawToolbar(
          targetGroup = "draw",
          polylineOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          polygonOptions = drawPolygonOptions(
            shapeOptions = drawShapeOptions(
              fillOpacity = 0.2,
              color = "#0066CC",
              weight = 3
            )
          ),
          rectangleOptions = drawRectangleOptions(
            shapeOptions = drawShapeOptions(
              fillOpacity = 0.2,
              color = "#0066CC",
              weight = 3
            )
          ),
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c(partition_colors, "Unclassified" = "#CCCCCC"),
          labels = c(names(partition_colors), "Unclassified"),
          title = "Partition"
        )
      
      # Add historical borders if available
      if (!is.null(border_lines)) {
        if (inherits(border_lines, "sf")) {
          # Add as spatial lines from shapefile/geojson
          map <- map %>%
            addPolylines(
              data = border_lines,
              color = "#FF0000",
              weight = 3,
              opacity = 0.8,
              group = "Historical Borders",
              label = ~ifelse("name" %in% names(border_lines), name, "Border")
            )
        } else if (is.data.frame(border_lines)) {
          # Add as lines from coordinate data
          # Group by line_id if present
          if ("line_id" %in% colnames(border_lines)) {
            for (line_id in unique(border_lines$line_id)) {
              line_data <- border_lines[border_lines$line_id == line_id, ]
              map <- map %>%
                addPolylines(
                  lng = line_data$lng,
                  lat = line_data$lat,
                  color = ifelse("color" %in% colnames(line_data), line_data$color[1], "#FF0000"),
                  weight = 3,
                  opacity = 0.8,
                  group = "Historical Borders",
                  label = ifelse("label" %in% colnames(line_data), line_data$label[1], paste("Border", line_id))
                )
            }
          }
        }
        
        # Add layer control
        map <- map %>%
          addLayersControl(
            overlayGroups = c("Historical Borders"),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
      
      return(map)
    })
    
    # Toggle borders visibility
    observeEvent(input$show_borders, {
      if (!is.null(border_lines)) {
        if (input$show_borders) {
          leafletProxy("map") %>%
            showGroup("Historical Borders")
        } else {
          leafletProxy("map") %>%
            hideGroup("Historical Borders")
        }
      }
    })
    
    # Handle polygon clicks
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      clicked_id <- click$id
      
      if (!is.null(clicked_id) && input$selection_mode != "draw") {
        
        if (input$selection_mode == "multi") {
          # Multi-select mode: toggle selection
          if (clicked_id %in% rv$selected) {
            # Deselect if already selected
            rv$selected <- setdiff(rv$selected, clicked_id)
          } else {
            # Add to selection
            rv$selected <- c(rv$selected, clicked_id)
          }
          
          # Update colors to show selection
          new_colors <- get_colors(rv$data, rv$selected)
          
          leafletProxy("map") %>%
            clearShapes() %>%
            addPolygons(
              data = rv$data,
              layerId = ~JPT_KOD_JE,
              fillColor = new_colors,
              fillOpacity = 0.7,
              color = "#000000",
              weight = 1,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#666",
                fillOpacity = 0.9,
                bringToFront = TRUE
              ),
              label = ~paste0(JPT_NAZWA_, " (", JPT_KOD_JE, ")"),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto"
              )
            )
          
        } else {
          # Single-click mode: apply partition immediately
          idx <- which(rv$data$JPT_KOD_JE == clicked_id)
          if (length(idx) > 0) {
            rv$data$partition[idx] <- input$partition_type
            
            # Update colors
            new_colors <- get_colors(rv$data)
            
            leafletProxy("map") %>%
              clearShapes() %>%
              addPolygons(
                data = rv$data,
                layerId = ~JPT_KOD_JE,
                fillColor = new_colors,
                fillOpacity = 0.7,
                color = "#000000",
                weight = 1,
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = "#666",
                  fillOpacity = 0.9,
                  bringToFront = TRUE
                ),
                label = ~paste0(JPT_NAZWA_, " (", JPT_KOD_JE, ")"),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
                )
              )
          }
        }
      }
    })
    
    # Handle drawn shapes (rectangles and polygons)
    observeEvent(input$map_draw_new_feature, {
      if (input$selection_mode == "draw") {
        feature <- input$map_draw_new_feature
        
        # Convert drawn feature to sf object
        coords <- feature$geometry$coordinates
        
        if (feature$geometry$type == "Polygon") {
          # Extract coordinates from polygon
          coord_matrix <- do.call(rbind, lapply(coords[[1]], function(x) c(x[[1]], x[[2]])))
          drawn_poly <- sf::st_polygon(list(coord_matrix))
        } else if (feature$geometry$type == "Rectangle") {
          # Extract coordinates from rectangle
          coord_matrix <- do.call(rbind, lapply(coords[[1]], function(x) c(x[[1]], x[[2]])))
          drawn_poly <- sf::st_polygon(list(coord_matrix))
        } else {
          return()
        }
        
        # Create sf object with correct CRS
        drawn_sf <- sf::st_sfc(drawn_poly, crs = 4326)
        drawn_sf <- sf::st_sf(geometry = drawn_sf)
        
        # Find regions that intersect with drawn area
        intersects <- sf::st_intersects(rv$data, drawn_sf, sparse = FALSE)
        selected_ids <- rv$data$JPT_KOD_JE[intersects[, 1]]
        
        if (length(selected_ids) > 0) {
          # Apply partition to intersecting regions
          idx <- which(rv$data$JPT_KOD_JE %in% selected_ids)
          rv$data$partition[idx] <- input$partition_type
          
          # Update map
          new_colors <- get_colors(rv$data)
          
          leafletProxy("map") %>%
            clearShapes() %>%
            addPolygons(
              data = rv$data,
              layerId = ~JPT_KOD_JE,
              fillColor = new_colors,
              fillOpacity = 0.7,
              color = "#000000",
              weight = 1,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#666",
                fillOpacity = 0.9,
                bringToFront = TRUE
              ),
              label = ~paste0(JPT_NAZWA_, " (", JPT_KOD_JE, ")"),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto"
              )
            )
          
          # Clear the drawn shape
          leafletProxy("map") %>%
            clearGroup("draw")
        }
      }
    })
    
    # Clear drawing button
    observeEvent(input$clear_drawing, {
      leafletProxy("map") %>%
        clearGroup("draw")
    })
    
    # Apply partition to all selected regions
    observeEvent(input$apply_to_selected, {
      if (length(rv$selected) > 0) {
        # Apply partition to all selected regions
        idx <- which(rv$data$JPT_KOD_JE %in% rv$selected)
        rv$data$partition[idx] <- input$partition_type
        
        # Clear selection
        rv$selected <- c()
        
        # Update map
        new_colors <- get_colors(rv$data)
        
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = rv$data,
            layerId = ~JPT_KOD_JE,
            fillColor = new_colors,
            fillOpacity = 0.7,
            color = "#000000",
            weight = 1,
            highlightOptions = highlightOptions(
              weight = 3,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = ~paste0(JPT_NAZWA_, " (", JPT_KOD_JE, ")"),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          )
      }
    })
    
    # Clear selection
    observeEvent(input$clear_selection, {
      rv$selected <- c()
      
      # Update map to remove selection highlighting
      new_colors <- get_colors(rv$data)
      
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(
          data = rv$data,
          layerId = ~JPT_KOD_JE,
          fillColor = new_colors,
          fillOpacity = 0.7,
          color = "#000000",
          weight = 1,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~paste0(JPT_NAZWA_, " (", JPT_KOD_JE, ")"),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        )
    })
    
    # Reset selection when switching modes
    observeEvent(input$selection_mode, {
      rv$selected <- c()
      
      # Update map
      new_colors <- get_colors(rv$data)
      
      leafletProxy("map") %>%
        clearShapes() %>%
        clearGroup("draw") %>%
        addPolygons(
          data = rv$data,
          layerId = ~JPT_KOD_JE,
          fillColor = new_colors,
          fillOpacity = 0.7,
          color = "#000000",
          weight = 1,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~paste0(JPT_NAZWA_, " (", JPT_KOD_JE, ")"),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        )
    })
    
    # Display statistics
    output$stats <- renderText({
      partitions <- table(rv$data$partition, useNA = "ifany")
      total <- nrow(rv$data)
      classified <- sum(!is.na(rv$data$partition))
      
      paste0(
        "Total Regions: ", total, "\n",
        "Classified: ", classified, "\n",
        "Unclassified: ", total - classified, "\n\n",
        "Russian: ", ifelse("Russian" %in% names(partitions), partitions["Russian"], 0), "\n",
        "Prussian: ", ifelse("Prussian" %in% names(partitions), partitions["Prussian"], 0), "\n",
        "Austrian: ", ifelse("Austrian" %in% names(partitions), partitions["Austrian"], 0)
      )
    })
    
    # Save button
    observeEvent(input$save_btn, {
      # Prepare data for saving
      save_data <- data.frame(
        JPT_KOD_JE = rv$data$JPT_KOD_JE,
        JPT_NAZWA = rv$data$JPT_NAZWA_,
        partition = rv$data$partition,
        stringsAsFactors = FALSE
      )
      
      # Save to CSV
      write.csv(save_data, output_file, row.names = FALSE)
      
      showModal(modalDialog(
        title = "Success!",
        paste0("Classifications saved to: ", output_file),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
    
    # Clear button
    observeEvent(input$clear_btn, {
      showModal(modalDialog(
        title = "Clear All Classifications?",
        "Are you sure you want to clear all partition classifications?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_clear", "Yes, Clear All", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_clear, {
      rv$data$partition <- NA_character_
      
      # Redraw entire map
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(
          data = rv$data,
          layerId = ~JPT_KOD_JE,
          fillColor = "#CCCCCC",
          fillOpacity = 0.7,
          color = "#000000",
          weight = 1,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~paste0(JPT_NAZWA_, " (", JPT_KOD_JE, ")"),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        )
      
      removeModal()
    })
  }
  
  # Run the app
  cat("Launching app...\n")
  shinyApp(ui = ui, server = server)
}
