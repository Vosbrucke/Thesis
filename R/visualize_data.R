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
