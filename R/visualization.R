#' @title Advanced Visualization Functions for Network Dynamics
#' @description Comprehensive visualization tools for financial network analysis
#' @author WaveQTE Consortium
#' @docType package
#' @name visualization
NULL

#' Plot Network Dynamics
#'
#' Create comprehensive network dynamics visualizations
#'
#' @param network_dynamics NetworkDynamics object
#' @param plot_type Type of plot to generate
#' @param time_scale Time scale to visualize
#' @param layout Network layout algorithm
#' @param interactive Whether to create interactive plots
#' @return ggplot2 or plotly object
#' @export
plotNetworkDynamics <- function(network_dynamics, plot_type = "network", 
                               time_scale = 1, layout = "spring", 
                               interactive = FALSE) {
  
  if (!inherits(network_dynamics, "NetworkDynamics")) {
    stop("network_dynamics must be a NetworkDynamics object")
  }
  
  # Select appropriate plotting function
  plot_result <- switch(plot_type,
    "network" = plotNetworkGraph(network_dynamics, time_scale, layout),
    "evolution" = plotNetworkEvolution(network_dynamics),
    "metrics" = plotNetworkMetrics(network_dynamics),
    "information_flow" = plotInformationFlow(network_dynamics, time_scale),
    "centrality" = plotCentralityMeasures(network_dynamics, time_scale),
    "clustering" = plotClusteringStructure(network_dynamics, time_scale),
    "heatmap" = plotNetworkHeatmap(network_dynamics, time_scale),
    "3d" = plotNetwork3D(network_dynamics, time_scale),
    stop("Unknown plot type: ", plot_type)
  )
  
  # Make interactive if requested
  if (interactive && !inherits(plot_result, "plotly")) {
    plot_result <- plotly::ggplotly(plot_result)
  }
  
  return(plot_result)
}

#' Visualize Risk Evolution
#'
#' Create visualizations of risk evolution over time
#'
#' @param risk_data SystemicRiskIndex object or risk time series
#' @param components Risk components to visualize
#' @param plot_type Type of risk visualization
#' @param time_window Time window for analysis
#' @return ggplot2 object
#' @export
visualizeRiskEvolution <- function(risk_data, components = "all", 
                                  plot_type = "timeseries", time_window = NULL) {
  
  # Handle different input types
  if (inherits(risk_data, "SystemicRiskIndex")) {
    risk_series <- extractRiskTimeSeries(risk_data)
  } else if (is.data.frame(risk_data)) {
    risk_series <- risk_data
  } else {
    stop("risk_data must be SystemicRiskIndex object or data frame")
  }
  
  # Select components
  if (components == "all") {
    components <- names(risk_series)[!names(risk_series) %in% c("timestamp", "time")]
  }
  
  # Generate appropriate visualization
  plot_result <- switch(plot_type,
    "timeseries" = plotRiskTimeSeries(risk_series, components, time_window),
    "heatmap" = plotRiskHeatmap(risk_series, components, time_window),
    "decomposition" = plotRiskDecomposition(risk_series, components),
    "distribution" = plotRiskDistribution(risk_series, components),
    "correlation" = plotRiskCorrelation(risk_series, components),
    "regime" = plotRiskRegimes(risk_series, components),
    "stress" = plotStressIndicators(risk_series, components),
    "early_warning" = plotEarlyWarningSignals(risk_series, components),
    stop("Unknown plot type: ", plot_type)
  )
  
  return(plot_result)
}

#' Create Network Graph
#'
#' Generate network graph visualization
#'
#' @param adjacency_matrix Adjacency matrix or network object
#' @param node_attributes Node attributes data frame
#' @param edge_attributes Edge attributes data frame
#' @param layout Layout algorithm
#' @param node_size_var Variable for node sizing
#' @param edge_width_var Variable for edge width
#' @param color_var Variable for node coloring
#' @return ggplot2 object
#' @export
createNetworkGraph <- function(adjacency_matrix, node_attributes = NULL, 
                              edge_attributes = NULL, layout = "spring",
                              node_size_var = NULL, edge_width_var = NULL,
                              color_var = NULL) {
  
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
  
  # Calculate layout
  pos <- switch(layout,
    "spring" = igraph::layout_with_fr(graph),
    "circle" = igraph::layout_in_circle(graph),
    "random" = igraph::layout_randomly(graph),
    "grid" = igraph::layout_on_grid(graph),
    "sphere" = igraph::layout_on_sphere(graph),
    igraph::layout_with_fr(graph)
  )
  
  # Extract node and edge data
  node_data <- data.frame(
    id = igraph::V(graph)$name %||% 1:igraph::vcount(graph),
    x = pos[, 1],
    y = pos[, 2]
  )
  
  edge_data <- igraph::as_data_frame(graph, what = "edges")
  
  # Add node positions to edge data
  edge_data$x <- node_data$x[match(edge_data$from, node_data$id)]
  edge_data$y <- node_data$y[match(edge_data$from, node_data$id)]
  edge_data$xend <- node_data$x[match(edge_data$to, node_data$id)]
  edge_data$yend <- node_data$y[match(edge_data$to, node_data$id)]
  
  # Add attributes if provided
  if (!is.null(node_attributes)) {
    node_data <- merge(node_data, node_attributes, by = "id", all.x = TRUE)
  }
  
  if (!is.null(edge_attributes)) {
    edge_data <- merge(edge_data, edge_attributes, by = c("from", "to"), all.x = TRUE)
  }
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = edge_data, 
                         ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                     alpha = if (!is.null(edge_width_var)) get(edge_width_var) else 0.6),
                         color = "gray70") +
    ggplot2::geom_point(data = node_data, 
                       ggplot2::aes(x = x, y = y,
                                   size = if (!is.null(node_size_var)) get(node_size_var) else 3,
                                   color = if (!is.null(color_var)) get(color_var) else "steelblue")) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Network Graph",
                 subtitle = paste("Layout:", layout)) +
    ggplot2::theme(legend.position = "bottom")
  
  # Add node labels if they exist
  if ("label" %in% names(node_data)) {
    p <- p + ggplot2::geom_text(data = node_data, 
                               ggplot2::aes(x = x, y = y, label = label),
                               size = 3, vjust = -0.5)
  }
  
  return(p)
}

#' Generate Risk Heatmap
#'
#' Create heatmap visualization of risk metrics
#'
#' @param risk_matrix Risk matrix or correlation matrix
#' @param labels Row and column labels
#' @param title Plot title
#' @param color_palette Color palette for heatmap
#' @return ggplot2 object
#' @export
generateRiskHeatmap <- function(risk_matrix, labels = NULL, title = "Risk Heatmap",
                               color_palette = "RdYlBu") {
  
  # Convert matrix to long format
  if (is.null(labels)) {
    labels <- if (!is.null(rownames(risk_matrix))) rownames(risk_matrix) else 1:nrow(risk_matrix)
  }
  
  # Create data frame for ggplot
  heatmap_data <- expand.grid(x = labels, y = labels)
  heatmap_data$value <- as.vector(risk_matrix)
  
  # Create heatmap
  p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_tile(color = "white", size = 0.1) +
    ggplot2::scale_fill_distiller(palette = color_palette, trans = "reverse") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = title,
      x = "", y = "",
      fill = "Risk Level"
    ) +
    ggplot2::coord_fixed()
  
  # Add text annotations for values
  if (nrow(heatmap_data) <= 100) {  # Only for smaller matrices
    p <- p + ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), 
                               size = 3, color = "black")
  }
  
  return(p)
}

# Helper Functions for Network Visualization

plotNetworkGraph <- function(network_dynamics, time_scale, layout) {
  # Plot network graph for specific time scale
  
  scale_index <- which(network_dynamics$time_scales == time_scale)
  if (length(scale_index) == 0) {
    stop("Time scale not found in network dynamics")
  }
  
  adjacency_matrix <- network_dynamics$adjacency_matrices[[scale_index]]
  
  # Create node attributes
  node_attributes <- data.frame(
    id = network_dynamics$agent_ids,
    type = sapply(network_dynamics$agents, function(x) x$type),
    centrality = network_dynamics$network_state$centrality[, scale_index],
    stringsAsFactors = FALSE
  )
  
  # Create network graph
  graph <- createNetworkGraph(
    adjacency_matrix = adjacency_matrix,
    node_attributes = node_attributes,
    layout = layout,
    node_size_var = "centrality",
    color_var = "type"
  )
  
  return(graph + ggplot2::labs(
    title = paste("Network Graph - Time Scale:", time_scale, "min"),
    subtitle = paste("Layout:", layout)
  ))
}

plotNetworkEvolution <- function(network_dynamics) {
  # Plot network evolution over time
  
  if (length(network_dynamics$history$timestamps) == 0) {
    stop("No historical data available for network evolution")
  }
  
  # Extract historical metrics
  metrics_df <- network_dynamics$history$network_metrics
  
  # Create time series plot
  p <- ggplot2::ggplot(metrics_df, ggplot2::aes(x = timestamp)) +
    ggplot2::geom_line(ggplot2::aes(y = density, color = "Density"), size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = clustering, color = "Clustering"), size = 1) +
    ggplot2::facet_wrap(~time_scale, scales = "free_y", 
                       labeller = ggplot2::labeller(time_scale = function(x) paste("Scale:", x, "min"))) +
    ggplot2::scale_color_manual(values = c("Density" = "blue", "Clustering" = "red")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Network Evolution Over Time",
      x = "Time",
      y = "Metric Value",
      color = "Metric"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

plotNetworkMetrics <- function(network_dynamics) {
  # Plot current network metrics
  
  metrics_data <- data.frame(
    time_scale = network_dynamics$time_scales,
    density = network_dynamics$network_state$density,
    clustering = network_dynamics$network_state$clustering
  )
  
  # Reshape for plotting
  metrics_long <- tidyr::pivot_longer(metrics_data, 
                                     cols = c(density, clustering),
                                     names_to = "metric",
                                     values_to = "value")
  
  p <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = factor(time_scale), y = value, fill = metric)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Network Metrics by Time Scale",
      x = "Time Scale (minutes)",
      y = "Metric Value",
      fill = "Metric"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

plotInformationFlow <- function(network_dynamics, time_scale) {
  # Plot information flow matrix
  
  scale_index <- which(network_dynamics$time_scales == time_scale)
  if (length(scale_index) == 0) {
    stop("Time scale not found in network dynamics")
  }
  
  flow_matrix <- network_dynamics$network_state$information_flow[, , scale_index]
  
  # Create heatmap
  heatmap <- generateRiskHeatmap(
    risk_matrix = flow_matrix,
    labels = network_dynamics$agent_ids,
    title = paste("Information Flow Matrix - Scale:", time_scale, "min"),
    color_palette = "Blues"
  )
  
  return(heatmap)
}

plotCentralityMeasures <- function(network_dynamics, time_scale) {
  # Plot centrality measures
  
  scale_index <- which(network_dynamics$time_scales == time_scale)
  if (length(scale_index) == 0) {
    stop("Time scale not found in network dynamics")
  }
  
  centrality_data <- data.frame(
    agent_id = network_dynamics$agent_ids,
    centrality = network_dynamics$network_state$centrality[, scale_index],
    type = sapply(network_dynamics$agents, function(x) x$type)
  )
  
  # Sort by centrality
  centrality_data <- centrality_data[order(centrality_data$centrality, decreasing = TRUE), ]
  centrality_data$agent_id <- factor(centrality_data$agent_id, levels = centrality_data$agent_id)
  
  p <- ggplot2::ggplot(centrality_data, ggplot2::aes(x = agent_id, y = centrality, fill = type)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = paste("Centrality Measures - Scale:", time_scale, "min"),
      x = "Agent ID",
      y = "Centrality",
      fill = "Agent Type"
    )
  
  return(p)
}

plotClusteringStructure <- function(network_dynamics, time_scale) {
  # Plot clustering structure
  
  scale_index <- which(network_dynamics$time_scales == time_scale)
  if (length(scale_index) == 0) {
    stop("Time scale not found in network dynamics")
  }
  
  # Get adjacency matrix and detect communities
  adjacency_matrix <- network_dynamics$adjacency_matrices[[scale_index]]
  graph <- igraph::graph_from_adjacency_matrix(adjacency_matrix, 
                                              mode = "undirected", 
                                              weighted = TRUE)
  
  # Community detection
  communities <- igraph::cluster_louvain(graph)
  
  # Create node attributes with community membership
  node_attributes <- data.frame(
    id = network_dynamics$agent_ids,
    community = as.factor(communities$membership),
    type = sapply(network_dynamics$agents, function(x) x$type),
    stringsAsFactors = FALSE
  )
  
  # Create network graph with community coloring
  graph_plot <- createNetworkGraph(
    adjacency_matrix = adjacency_matrix,
    node_attributes = node_attributes,
    layout = "spring",
    color_var = "community"
  )
  
  return(graph_plot + ggplot2::labs(
    title = paste("Clustering Structure - Scale:", time_scale, "min"),
    subtitle = paste("Number of communities:", max(communities$membership))
  ))
}

plotNetworkHeatmap <- function(network_dynamics, time_scale) {
  # Plot network adjacency matrix as heatmap
  
  scale_index <- which(network_dynamics$time_scales == time_scale)
  if (length(scale_index) == 0) {
    stop("Time scale not found in network dynamics")
  }
  
  adjacency_matrix <- network_dynamics$adjacency_matrices[[scale_index]]
  
  heatmap <- generateRiskHeatmap(
    risk_matrix = adjacency_matrix,
    labels = network_dynamics$agent_ids,
    title = paste("Network Adjacency Matrix - Scale:", time_scale, "min"),
    color_palette = "Spectral"
  )
  
  return(heatmap)
}

plotNetwork3D <- function(network_dynamics, time_scale) {
  # Create 3D network visualization
  
  scale_index <- which(network_dynamics$time_scales == time_scale)
  if (length(scale_index) == 0) {
    stop("Time scale not found in network dynamics")
  }
  
  adjacency_matrix <- network_dynamics$adjacency_matrices[[scale_index]]
  graph <- igraph::graph_from_adjacency_matrix(adjacency_matrix, 
                                              mode = "undirected", 
                                              weighted = TRUE)
  
  # 3D layout
  layout_3d <- igraph::layout_with_fr(graph, dim = 3)
  
  # Extract node and edge data
  node_data <- data.frame(
    id = network_dynamics$agent_ids,
    x = layout_3d[, 1],
    y = layout_3d[, 2],
    z = layout_3d[, 3],
    type = sapply(network_dynamics$agents, function(x) x$type)
  )
  
  edge_data <- igraph::as_data_frame(graph, what = "edges")
  
  # Create 3D scatter plot
  p <- plotly::plot_ly(node_data, x = ~x, y = ~y, z = ~z, 
                      color = ~type, type = "scatter3d", mode = "markers",
                      marker = list(size = 8, opacity = 0.8)) %>%
    plotly::layout(
      title = paste("3D Network - Scale:", time_scale, "min"),
      scene = list(
        xaxis = list(title = "X"),
        yaxis = list(title = "Y"),
        zaxis = list(title = "Z")
      )
    )
  
  # Add edges
  for (i in 1:nrow(edge_data)) {
    from_node <- node_data[node_data$id == edge_data$from[i], ]
    to_node <- node_data[node_data$id == edge_data$to[i], ]
    
    p <- p %>% plotly::add_trace(
      x = c(from_node$x, to_node$x),
      y = c(from_node$y, to_node$y),
      z = c(from_node$z, to_node$z),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "gray", width = 2),
      opacity = 0.5,
      showlegend = FALSE
    )
  }
  
  return(p)
}

# Helper Functions for Risk Visualization

extractRiskTimeSeries <- function(risk_data) {
  # Extract time series data from SystemicRiskIndex object
  
  # This would extract historical risk data if available
  # For now, create sample time series
  dates <- seq(Sys.Date() - 30, Sys.Date(), by = "day")
  
  risk_series <- data.frame(
    timestamp = dates,
    systemic_risk = runif(length(dates), 0.1, 0.8),
    network_risk = runif(length(dates), 0.1, 0.6),
    concentration_risk = runif(length(dates), 0.2, 0.7),
    volatility_risk = runif(length(dates), 0.1, 0.5),
    liquidity_risk = runif(length(dates), 0.1, 0.4),
    contagion_risk = runif(length(dates), 0.2, 0.6)
  )
  
  return(risk_series)
}

plotRiskTimeSeries <- function(risk_series, components, time_window) {
  # Plot risk time series
  
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
    ggplot2::geom_line(size = 1, alpha = 0.8) +
    ggplot2::scale_color_brewer(palette = "Set2") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Risk Evolution Over Time",
      x = "Date",
      y = "Risk Level",
      color = "Risk Component"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  # Add risk level zones
  p <- p +
    ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", color = "green", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0.4, linetype = "dashed", color = "orange", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0.6, linetype = "dashed", color = "red", alpha = 0.7) +
    ggplot2::annotate("text", x = min(risk_series$timestamp), y = 0.1, label = "Low Risk", 
                     hjust = 0, color = "green") +
    ggplot2::annotate("text", x = min(risk_series$timestamp), y = 0.3, label = "Moderate Risk", 
                     hjust = 0, color = "orange") +
    ggplot2::annotate("text", x = min(risk_series$timestamp), y = 0.5, label = "High Risk", 
                     hjust = 0, color = "red") +
    ggplot2::annotate("text", x = min(risk_series$timestamp), y = 0.7, label = "Critical Risk", 
                     hjust = 0, color = "darkred")
  
  return(p)
}

plotRiskHeatmap <- function(risk_series, components, time_window) {
  # Plot risk heatmap over time
  
  # Apply time window filter if specified
  if (!is.null(time_window)) {
    risk_series <- risk_series[risk_series$timestamp >= (max(risk_series$timestamp) - time_window), ]
  }
  
  # Reshape data for heatmap
  risk_long <- tidyr::pivot_longer(risk_series, 
                                  cols = all_of(components),
                                  names_to = "component",
                                  values_to = "risk_value")
  
  p <- ggplot2::ggplot(risk_long, ggplot2::aes(x = timestamp, y = component, fill = risk_value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "green", mid = "yellow", high = "red", 
                                 midpoint = 0.5, limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Risk Heatmap Over Time",
      x = "Date",
      y = "Risk Component",
      fill = "Risk Level"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

plotRiskDecomposition <- function(risk_series, components) {
  # Plot risk decomposition
  
  # Use most recent data point
  latest_risks <- risk_series[nrow(risk_series), components]
  
  decomp_data <- data.frame(
    component = names(latest_risks),
    value = as.numeric(latest_risks)
  )
  
  p <- ggplot2::ggplot(decomp_data, ggplot2::aes(x = reorder(component, value), y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Current Risk Decomposition",
      x = "Risk Component",
      y = "Risk Level"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), 
                      hjust = -0.1, size = 3)
  
  return(p)
}

plotRiskDistribution <- function(risk_series, components) {
  # Plot risk distribution
  
  # Reshape data
  risk_long <- tidyr::pivot_longer(risk_series, 
                                  cols = all_of(components),
                                  names_to = "component",
                                  values_to = "risk_value")
  
  p <- ggplot2::ggplot(risk_long, ggplot2::aes(x = risk_value, fill = component)) +
    ggplot2::geom_histogram(alpha = 0.7, bins = 30) +
    ggplot2::facet_wrap(~component, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Risk Distribution by Component",
      x = "Risk Level",
      y = "Frequency"
    ) +
    ggplot2::theme(legend.position = "none")
  
  return(p)
}

plotRiskCorrelation <- function(risk_series, components) {
  # Plot risk correlation matrix
  
  # Calculate correlation matrix
  risk_matrix <- risk_series[, components]
  correlation_matrix <- cor(risk_matrix, use = "complete.obs")
  
  # Create correlation heatmap
  heatmap <- generateRiskHeatmap(
    risk_matrix = correlation_matrix,
    labels = components,
    title = "Risk Component Correlations",
    color_palette = "RdBu"
  )
  
  return(heatmap)
}

plotRiskRegimes <- function(risk_series, components) {
  # Plot risk regimes
  
  # Simple regime classification based on overall risk level
  risk_series$regime <- cut(risk_series$systemic_risk, 
                           breaks = c(0, 0.3, 0.6, 1), 
                           labels = c("Low", "Moderate", "High"))
  
  p <- ggplot2::ggplot(risk_series, ggplot2::aes(x = timestamp, y = systemic_risk, color = regime)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_color_manual(values = c("Low" = "green", "Moderate" = "orange", "High" = "red")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Risk Regimes Over Time",
      x = "Date",
      y = "Systemic Risk Level",
      color = "Risk Regime"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

plotStressIndicators <- function(risk_series, components) {
  # Plot stress indicators
  
  # Calculate stress indicators
  risk_series$stress_indicator <- apply(risk_series[, components], 1, max)
  risk_series$stress_level <- cut(risk_series$stress_indicator, 
                                 breaks = c(0, 0.4, 0.7, 1), 
                                 labels = c("Normal", "Elevated", "High"))
  
  p <- ggplot2::ggplot(risk_series, ggplot2::aes(x = timestamp, y = stress_indicator, fill = stress_level)) +
    ggplot2::geom_area(alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("Normal" = "lightgreen", "Elevated" = "yellow", "High" = "red")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Market Stress Indicators",
      x = "Date",
      y = "Stress Level",
      fill = "Stress Category"
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

plotEarlyWarningSignals <- function(risk_series, components) {
  # Plot early warning signals
  
  # Calculate rolling volatility as early warning signal
  risk_series$volatility_signal <- zoo::rollsd(risk_series$systemic_risk, 5, fill = NA)
  risk_series$trend_signal <- c(NA, diff(risk_series$systemic_risk))
  
  # Create warning flags
  risk_series$warning <- (risk_series$volatility_signal > quantile(risk_series$volatility_signal, 0.8, na.rm = TRUE)) |
                        (risk_series$trend_signal > quantile(risk_series$trend_signal, 0.9, na.rm = TRUE))
  
  p <- ggplot2::ggplot(risk_series, ggplot2::aes(x = timestamp, y = systemic_risk)) +
    ggplot2::geom_line(color = "blue", alpha = 0.7) +
    ggplot2::geom_point(data = risk_series[risk_series$warning & !is.na(risk_series$warning), ],
                       ggplot2::aes(x = timestamp, y = systemic_risk), 
                       color = "red", size = 3, alpha = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Early Warning Signals",
      subtitle = "Red points indicate potential warning signals",
      x = "Date",
      y = "Systemic Risk Level"
    )
  
  return(p)
}