#' Create Network Graph using ggraph (Fixed Version)
#'
#' Generate professional network graph visualization using ggraph
#'
#' @param adjacency_matrix Adjacency matrix or network object
#' @param node_attributes Node attributes data frame
#' @param layout Layout algorithm for ggraph
#' @param node_size_var Variable for node sizing
#' @param color_var Variable for node coloring
#' @param title Plot title
#' @return ggplot2 object
#' @export
createNetworkGraph <- function(adjacency_matrix, node_attributes = NULL, 
                              layout = "spring", node_size_var = NULL,
                              color_var = NULL, title = "Network Graph") {
  
  # Convert to igraph object if needed
  if (is.matrix(adjacency_matrix)) {
    graph <- igraph::graph_from_adjacency_matrix(adjacency_matrix, 
                                                mode = "undirected", 
                                                weighted = TRUE)
  } else if (inherits(adjacency_matrix, "igraph")) {
    graph <- adjacency_matrix
  } else {
    stop("adjacency_matrix must be a matrix or igraph object")
  }
  
  # Set layout algorithm
  layout_algo <- switch(layout,
    "spring" = "fr",
    "circle" = "circle", 
    "grid" = "grid",
    "star" = "star",
    "tree" = "tree",
    "kk" = "kk",
    "fr"  # default
  )
  
  # Create base plot
  p <- ggraph::ggraph(graph, layout = layout_algo) +
    ggraph::geom_edge_link(alpha = 0.6, color = "grey70") +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = title,
      subtitle = paste("Layout:", layout)
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      legend.position = "bottom"
    )
  
  # Add nodes with attributes if provided
  if (!is.null(node_attributes)) {
    # Get vertex names
    vertex_names <- igraph::V(graph)$name
    if (is.null(vertex_names)) {
      vertex_names <- as.character(1:igraph::vcount(graph))
    }
    
    # Match node attributes to vertices
    if ("id" %in% names(node_attributes)) {
      match_idx <- match(vertex_names, node_attributes$id)
    } else {
      match_idx <- 1:nrow(node_attributes)
    }
    
    # Prepare data for ggraph
    plot_data <- data.frame(
      vertex_id = vertex_names,
      stringsAsFactors = FALSE
    )
    
    # Add size variable
    if (!is.null(node_size_var) && node_size_var %in% names(node_attributes)) {
      plot_data$node_size <- node_attributes[[node_size_var]][match_idx]
      # Scale sizes
      plot_data$node_size <- scales::rescale(plot_data$node_size, to = c(2, 8))
    } else {
      plot_data$node_size <- 4
    }
    
    # Add color variable
    if (!is.null(color_var) && color_var %in% names(node_attributes)) {
      plot_data$node_color <- as.factor(node_attributes[[color_var]][match_idx])
    } else {
      plot_data$node_color <- factor("default")
    }
    
    # Add labels
    if ("label" %in% names(node_attributes)) {
      plot_data$node_label <- node_attributes$label[match_idx]
    }
    
    # Add node layer with proper aesthetics
    if (!is.null(color_var) && color_var %in% names(node_attributes)) {
      p <- p + ggraph::geom_node_point(
        ggplot2::aes(size = I(plot_data$node_size), color = plot_data$node_color),
        alpha = 0.8
      ) +
      ggplot2::scale_color_brewer(type = "qual", palette = "Set3", name = color_var)
    } else {
      p <- p + ggraph::geom_node_point(
        size = plot_data$node_size,
        color = "steelblue",
        alpha = 0.8
      )
    }
    
    # Add labels if available
    if ("label" %in% names(node_attributes)) {
      p <- p + ggraph::geom_node_text(
        ggplot2::aes(label = plot_data$node_label),
        size = 3, repel = TRUE
      )
    }
    
  } else {
    # Simple nodes without attributes
    p <- p + ggraph::geom_node_point(size = 4, color = "steelblue", alpha = 0.8)
  }
  
  return(p)
}

#' Generate Risk Heatmap (Fixed Version)
generateRiskHeatmap <- function(risk_matrix, labels = NULL, title = "Risk Heatmap",
                               color_palette = "RdYlBu") {
  
  # Convert matrix to long format
  if (is.null(labels)) {
    labels <- if (!is.null(rownames(risk_matrix))) rownames(risk_matrix) else 1:nrow(risk_matrix)
  }
  
  # Create data frame for ggplot
  heatmap_data <- expand.grid(
    x = factor(labels, levels = labels),
    y = factor(labels, levels = rev(labels))
  )
  heatmap_data$value <- as.vector(risk_matrix)
  
  # Create heatmap
  p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_distiller(
      palette = color_palette, 
      trans = "identity",
      na.value = "grey90"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(hjust = 1, size = 10),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.title = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(
      title = title,
      x = "", y = "",
      fill = "Risk Level"
    ) +
    ggplot2::coord_fixed()
  
  # Add text annotations for smaller matrices
  if (length(labels) <= 10) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", value)), 
      size = 3, 
      color = "black",
      fontface = "bold"
    )
  }
  
  return(p)
}

# Risk visualization functions (simplified versions)
plotRiskTimeSeries <- function(risk_series, components, time_window = NULL) {
  
  # Apply time window filter if specified
  if (!is.null(time_window)) {
    risk_series <- risk_series[risk_series$timestamp >= (max(risk_series$timestamp) - time_window), ]
  }
  
  # Reshape data for plotting
  risk_long <- tidyr::pivot_longer(risk_series, 
                                  cols = all_of(components),
                                  names_to = "component",
                                  values_to = "risk_value")
  
  p <- ggplot2::ggplot(risk_long, ggplot2::aes(x = timestamp, y = risk_value, color = component)) +
    ggplot2::geom_line(linewidth = 1.2, alpha = 0.8) +
    ggplot2::scale_color_brewer(palette = "Set2", name = "Risk Component") +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = "Risk Evolution Over Time",
      x = "Date",
      y = "Risk Level"
    )
  
  return(p)
}

plotRiskDecomposition <- function(risk_series, components) {
  
  # Use most recent data point
  latest_risks <- risk_series[nrow(risk_series), components]
  
  decomp_data <- data.frame(
    component = names(latest_risks),
    value = as.numeric(latest_risks)
  )
  
  p <- ggplot2::ggplot(decomp_data, ggplot2::aes(x = reorder(component, value), y = value, fill = value)) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
    ggplot2::scale_fill_gradient2(
      low = "#2E8B57", mid = "#FFD700", high = "#DC143C", 
      midpoint = 0.5, limits = c(0, 1),
      name = "Risk Level"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(
      title = "Current Risk Decomposition",
      x = "Risk Component",
      y = "Risk Level"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", value * 100)), 
                      hjust = -0.1, size = 3, fontface = "bold")
  
  return(p)
}