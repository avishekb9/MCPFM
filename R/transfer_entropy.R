#' @title Transfer Entropy Network Calculator
#' @description Parallel computation of transfer entropy networks for financial analysis
#' @author WaveQTE Consortium
#' @docType package
#' @name transferentropy
NULL

#' Calculate Transfer Entropy Network
#'
#' Compute transfer entropy between all pairs of time series with parallel processing
#'
#' @param time_series_data Matrix or data frame with time series in columns
#' @param embedding_dimension Embedding dimension for reconstruction
#' @param time_delay Time delay for embedding
#' @param bandwidth Bandwidth for density estimation
#' @param parallel_cores Number of cores for parallel processing
#' @param significance_level Significance level for statistical testing
#' @return TransferEntropyNetwork object
#' @export
calculateTransferEntropy <- function(time_series_data, embedding_dimension = 3,
                                    time_delay = 1, bandwidth = NULL,
                                    parallel_cores = NULL, significance_level = 0.05) {
  
  # Validate inputs
  if (!is.matrix(time_series_data) && !is.data.frame(time_series_data)) {
    stop("time_series_data must be a matrix or data frame")
  }
  
  # Convert to matrix if needed
  if (is.data.frame(time_series_data)) {
    time_series_data <- as.matrix(time_series_data)
  }
  
  # Check for missing values
  if (any(is.na(time_series_data))) {
    warning("Missing values detected. Consider imputation.")
    time_series_data <- na.omit(time_series_data)
  }
  
  n_series <- ncol(time_series_data)
  n_obs <- nrow(time_series_data)
  series_names <- colnames(time_series_data)
  
  if (is.null(series_names)) {
    series_names <- paste0("Series_", 1:n_series)
  }
  
  # Set up parallel processing
  if (is.null(parallel_cores)) {
    parallel_cores <- min(detectCores() - 1, n_series)
  }
  
  # Register parallel backend
  if (parallel_cores > 1) {
    cl <- makeCluster(parallel_cores)
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
  }
  
  # Calculate optimal bandwidth if not provided
  if (is.null(bandwidth)) {
    bandwidth <- calculateOptimalBandwidth(time_series_data, embedding_dimension)
  }
  
  # Initialize result matrices
  te_matrix <- matrix(0, nrow = n_series, ncol = n_series,
                     dimnames = list(series_names, series_names))
  
  effective_te_matrix <- matrix(0, nrow = n_series, ncol = n_series,
                               dimnames = list(series_names, series_names))
  
  p_values <- matrix(1, nrow = n_series, ncol = n_series,
                    dimnames = list(series_names, series_names))
  
  # Calculate transfer entropy for all pairs
  cat("Calculating transfer entropy network...\n")
  pb <- txtProgressBar(min = 0, max = n_series * (n_series - 1), style = 3)
  progress_counter <- 0
  
  if (parallel_cores > 1) {
    # Parallel computation
    pair_indices <- expand.grid(i = 1:n_series, j = 1:n_series)
    pair_indices <- pair_indices[pair_indices$i != pair_indices$j, ]
    
    results <- foreach(idx = 1:nrow(pair_indices), .combine = rbind,
                      .packages = c("MCPFM")) %dopar% {
      i <- pair_indices$i[idx]
      j <- pair_indices$j[idx]
      
      # Calculate transfer entropy from j to i
      te_result <- computePairwiseTE(time_series_data[, j], time_series_data[, i],
                                    embedding_dimension, time_delay, bandwidth)
      
      # Statistical significance test
      p_value <- bootstrapSignificanceTest(time_series_data[, j], time_series_data[, i],
                                          te_result$te, embedding_dimension, 
                                          time_delay, bandwidth, n_bootstrap = 100)
      
      data.frame(i = i, j = j, te = te_result$te, effective_te = te_result$effective_te,
                p_value = p_value)
    }
    
    # Fill matrices
    for (idx in 1:nrow(results)) {
      i <- results$i[idx]
      j <- results$j[idx]
      te_matrix[i, j] <- results$te[idx]
      effective_te_matrix[i, j] <- results$effective_te[idx]
      p_values[i, j] <- results$p_value[idx]
    }
    
  } else {
    # Sequential computation
    for (i in 1:n_series) {
      for (j in 1:n_series) {
        if (i != j) {
          # Calculate transfer entropy from j to i
          te_result <- computePairwiseTE(time_series_data[, j], time_series_data[, i],
                                        embedding_dimension, time_delay, bandwidth)
          
          te_matrix[i, j] <- te_result$te
          effective_te_matrix[i, j] <- te_result$effective_te
          
          # Statistical significance test
          p_values[i, j] <- bootstrapSignificanceTest(time_series_data[, j], time_series_data[, i],
                                                     te_result$te, embedding_dimension, 
                                                     time_delay, bandwidth, n_bootstrap = 100)
          
          progress_counter <- progress_counter + 1
          setTxtProgressBar(pb, progress_counter)
        }
      }
    }
  }
  
  close(pb)
  
  # Apply significance threshold
  significant_te <- te_matrix
  significant_te[p_values > significance_level] <- 0
  
  # Calculate network metrics
  network_metrics <- calculateTENetworkMetrics(significant_te, series_names)
  
  # Create result object
  result <- list(
    transfer_entropy = te_matrix,
    effective_transfer_entropy = effective_te_matrix,
    significant_transfer_entropy = significant_te,
    p_values = p_values,
    network_metrics = network_metrics,
    parameters = list(
      embedding_dimension = embedding_dimension,
      time_delay = time_delay,
      bandwidth = bandwidth,
      significance_level = significance_level,
      n_series = n_series,
      n_observations = n_obs
    ),
    computation_info = list(
      parallel_cores = parallel_cores,
      computation_time = Sys.time()
    )
  )
  
  class(result) <- c("TransferEntropyNetwork", "list")
  return(result)
}

#' Compute Dynamic Transfer Entropy
#'
#' Calculate time-varying transfer entropy using rolling windows
#'
#' @param time_series_data Matrix of time series data
#' @param window_size Size of rolling window
#' @param step_size Step size for window movement
#' @param embedding_dimension Embedding dimension
#' @param time_delay Time delay for embedding
#' @param parallel_cores Number of cores for parallel processing
#' @return Dynamic transfer entropy results
#' @export
computeDynamicTransferEntropy <- function(time_series_data, window_size = 250,
                                         step_size = 50, embedding_dimension = 3,
                                         time_delay = 1, parallel_cores = NULL) {
  
  n_obs <- nrow(time_series_data)
  n_series <- ncol(time_series_data)
  
  if (window_size >= n_obs) {
    stop("Window size must be smaller than number of observations")
  }
  
  # Calculate number of windows
  n_windows <- floor((n_obs - window_size) / step_size) + 1
  window_centers <- seq(window_size/2, n_obs - window_size/2, by = step_size)
  
  # Initialize results
  dynamic_te <- array(0, dim = c(n_series, n_series, n_windows),
                     dimnames = list(colnames(time_series_data),
                                   colnames(time_series_data),
                                   paste0("Window_", 1:n_windows)))
  
  # Set up parallel processing
  if (is.null(parallel_cores)) {
    parallel_cores <- min(detectCores() - 1, 4)
  }
  
  if (parallel_cores > 1) {
    cl <- makeCluster(parallel_cores)
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
  }
  
  cat("Computing dynamic transfer entropy...\n")
  pb <- txtProgressBar(min = 0, max = n_windows, style = 3)
  
  # Process each window
  if (parallel_cores > 1) {
    window_results <- foreach(w = 1:n_windows, .combine = list,
                             .packages = c("MCPFM")) %dopar% {
      
      start_idx <- (w - 1) * step_size + 1
      end_idx <- start_idx + window_size - 1
      
      window_data <- time_series_data[start_idx:end_idx, ]
      
      # Calculate TE for this window
      te_result <- calculateTransferEntropy(window_data, embedding_dimension, 
                                           time_delay, parallel_cores = 1)
      
      te_result$transfer_entropy
    }
    
    # Store results
    for (w in 1:n_windows) {
      dynamic_te[, , w] <- window_results[[w]]
      setTxtProgressBar(pb, w)
    }
    
  } else {
    # Sequential processing
    for (w in 1:n_windows) {
      start_idx <- (w - 1) * step_size + 1
      end_idx <- start_idx + window_size - 1
      
      window_data <- time_series_data[start_idx:end_idx, ]
      
      # Calculate TE for this window
      te_result <- calculateTransferEntropy(window_data, embedding_dimension, 
                                           time_delay, parallel_cores = 1)
      
      dynamic_te[, , w] <- te_result$transfer_entropy
      
      setTxtProgressBar(pb, w)
    }
  }
  
  close(pb)
  
  # Calculate time-varying network metrics
  dynamic_metrics <- calculateDynamicNetworkMetrics(dynamic_te, window_centers)
  
  result <- list(
    dynamic_transfer_entropy = dynamic_te,
    window_centers = window_centers,
    dynamic_metrics = dynamic_metrics,
    parameters = list(
      window_size = window_size,
      step_size = step_size,
      embedding_dimension = embedding_dimension,
      time_delay = time_delay,
      n_windows = n_windows
    )
  )
  
  class(result) <- c("DynamicTransferEntropy", "list")
  return(result)
}

# Helper Functions

computePairwiseTE <- function(source, target, embedding_dimension, time_delay, bandwidth) {
  # Compute transfer entropy between two time series
  
  # State space reconstruction
  source_embedded <- embedTimeSeries(source, embedding_dimension, time_delay)
  target_embedded <- embedTimeSeries(target, embedding_dimension, time_delay)
  
  # Align series (remove initial points lost to embedding)
  max_delay <- (embedding_dimension - 1) * time_delay
  source_aligned <- source[(max_delay + 1):length(source)]
  target_aligned <- target[(max_delay + 1):length(target)]
  source_embedded_aligned <- source_embedded[(max_delay + 1):nrow(source_embedded), ]
  target_embedded_aligned <- target_embedded[(max_delay + 1):nrow(target_embedded), ]
  
  # Future target values
  target_future <- target_aligned[2:length(target_aligned)]
  source_present <- source_aligned[1:(length(source_aligned) - 1)]
  target_present <- target_aligned[1:(length(target_aligned) - 1)]
  
  # Calculate conditional entropies
  # H(X_t+1 | X_t)
  h_target_future_given_present <- conditionalEntropy(target_future, target_present, bandwidth)
  
  # H(X_t+1 | X_t, Y_t)
  joint_predictors <- cbind(target_present, source_present)
  h_target_future_given_both <- conditionalEntropy(target_future, joint_predictors, bandwidth)
  
  # Transfer entropy: TE = H(X_t+1 | X_t) - H(X_t+1 | X_t, Y_t)
  te <- h_target_future_given_present - h_target_future_given_both
  
  # Effective transfer entropy (normalized)
  h_target_future <- entropy(target_future, bandwidth)
  effective_te <- te / h_target_future
  
  return(list(te = max(te, 0), effective_te = max(effective_te, 0)))
}

embedTimeSeries <- function(series, dimension, delay) {
  # Time series embedding using method of delays
  n <- length(series)
  embedded_length <- n - (dimension - 1) * delay
  
  if (embedded_length <= 0) {
    stop("Time series too short for embedding")
  }
  
  embedded <- matrix(0, nrow = embedded_length, ncol = dimension)
  
  for (i in 1:dimension) {
    start_idx <- (i - 1) * delay + 1
    end_idx <- start_idx + embedded_length - 1
    embedded[, i] <- series[start_idx:end_idx]
  }
  
  return(embedded)
}

conditionalEntropy <- function(y, x, bandwidth) {
  # Estimate conditional entropy H(Y|X) using kernel density estimation
  
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  
  n <- length(y)
  
  if (nrow(x) != n) {
    stop("Dimensions of y and x must match")
  }
  
  # Kernel density estimation
  h_joint <- 0
  h_marginal <- 0
  
  for (i in 1:n) {
    # Joint density p(y_i, x_i)
    joint_density <- 0
    marginal_density <- 0
    
    for (j in 1:n) {
      if (i != j) {
        # Gaussian kernel
        y_kernel <- exp(-0.5 * ((y[i] - y[j]) / bandwidth)^2)
        x_kernel <- exp(-0.5 * sum(((x[i, ] - x[j, ]) / bandwidth)^2))
        
        joint_density <- joint_density + y_kernel * x_kernel
        marginal_density <- marginal_density + x_kernel
      }
    }
    
    joint_density <- joint_density / (n - 1)
    marginal_density <- marginal_density / (n - 1)
    
    if (joint_density > 0 && marginal_density > 0) {
      h_joint <- h_joint - log(joint_density)
      h_marginal <- h_marginal - log(marginal_density)
    }
  }
  
  # Conditional entropy
  conditional_entropy <- (h_joint - h_marginal) / n
  
  return(conditional_entropy)
}

entropy <- function(x, bandwidth) {
  # Estimate entropy using kernel density estimation
  n <- length(x)
  h <- 0
  
  for (i in 1:n) {
    density <- 0
    for (j in 1:n) {
      if (i != j) {
        density <- density + exp(-0.5 * ((x[i] - x[j]) / bandwidth)^2)
      }
    }
    density <- density / (n - 1)
    
    if (density > 0) {
      h <- h - log(density)
    }
  }
  
  return(h / n)
}

calculateOptimalBandwidth <- function(data, embedding_dimension) {
  # Calculate optimal bandwidth using Silverman's rule of thumb
  n <- nrow(data)
  d <- ncol(data)
  
  # Standard deviation of each series
  std_devs <- apply(data, 2, sd)
  
  # Silverman's rule
  bandwidth <- mean(std_devs) * (4 / (3 * n))^(1 / (4 + d))
  
  return(bandwidth)
}

bootstrapSignificanceTest <- function(source, target, observed_te, embedding_dimension,
                                    time_delay, bandwidth, n_bootstrap = 100) {
  # Bootstrap test for transfer entropy significance
  
  # Generate surrogate data by shuffling source series
  bootstrap_tes <- numeric(n_bootstrap)
  
  for (b in 1:n_bootstrap) {
    # Shuffle source series
    source_shuffled <- sample(source)
    
    # Calculate TE with shuffled data
    te_result <- computePairwiseTE(source_shuffled, target, embedding_dimension, 
                                  time_delay, bandwidth)
    bootstrap_tes[b] <- te_result$te
  }
  
  # Calculate p-value
  p_value <- mean(bootstrap_tes >= observed_te)
  
  return(p_value)
}

calculateTENetworkMetrics <- function(te_matrix, series_names) {
  # Calculate network metrics from transfer entropy matrix
  
  # Convert to igraph object (treating as directed weighted network)
  graph <- graph_from_adjacency_matrix(te_matrix, mode = "directed", weighted = TRUE)
  
  # Basic network metrics
  metrics <- list(
    density = edge_density(graph),
    transitivity = transitivity(graph, type = "global"),
    reciprocity = reciprocity(graph),
    assortativity = assortativity_degree(graph),
    avg_path_length = tryCatch(
      average.path.length(graph),
      error = function(e) Inf
    ),
    n_components = components(graph, mode = "weak")$no,
    largest_component_size = max(components(graph, mode = "weak")$csize)
  )
  
  # Node-level metrics
  metrics$in_degree <- degree(graph, mode = "in")
  metrics$out_degree <- degree(graph, mode = "out")
  metrics$betweenness <- betweenness(graph, directed = TRUE)
  metrics$closeness_in <- closeness(graph, mode = "in")
  metrics$closeness_out <- closeness(graph, mode = "out")
  metrics$eigenvector <- eigen_centrality(graph, directed = TRUE)$vector
  
  # Information flow metrics
  metrics$total_information_flow <- sum(te_matrix)
  metrics$information_flow_in <- rowSums(te_matrix)
  metrics$information_flow_out <- colSums(te_matrix)
  metrics$net_information_flow <- metrics$information_flow_out - metrics$information_flow_in
  
  # Systemic importance (information flow centrality)
  metrics$systemic_importance <- (metrics$information_flow_in + metrics$information_flow_out) / 
                                (2 * metrics$total_information_flow)
  
  return(metrics)
}

calculateDynamicNetworkMetrics <- function(dynamic_te, window_centers) {
  # Calculate time-varying network metrics
  
  n_windows <- dim(dynamic_te)[3]
  n_series <- dim(dynamic_te)[1]
  
  # Initialize metric containers
  metrics <- list(
    timestamps = window_centers,
    density = numeric(n_windows),
    total_information_flow = numeric(n_windows),
    avg_clustering = numeric(n_windows),
    max_flow = numeric(n_windows),
    network_concentration = numeric(n_windows),
    systemic_importance = matrix(0, nrow = n_series, ncol = n_windows)
  )
  
  for (w in 1:n_windows) {
    te_window <- dynamic_te[, , w]
    
    # Basic metrics
    metrics$density[w] <- mean(te_window > 0)
    metrics$total_information_flow[w] <- sum(te_window)
    metrics$max_flow[w] <- max(te_window)
    
    # Network concentration (Gini coefficient of flows)
    flows <- as.vector(te_window)
    flows <- flows[flows > 0]
    if (length(flows) > 0) {
      metrics$network_concentration[w] <- gini_coefficient(flows)
    }
    
    # Clustering (transitivity)
    if (sum(te_window > 0) > 0) {
      graph <- graph_from_adjacency_matrix(te_window, mode = "directed", weighted = TRUE)
      metrics$avg_clustering[w] <- transitivity(graph, type = "global")
    }
    
    # Systemic importance
    flow_in <- rowSums(te_window)
    flow_out <- colSums(te_window)
    total_flow <- sum(te_window)
    
    if (total_flow > 0) {
      metrics$systemic_importance[, w] <- (flow_in + flow_out) / (2 * total_flow)
    }
  }
  
  return(metrics)
}

gini_coefficient <- function(x) {
  # Calculate Gini coefficient
  x <- sort(x)
  n <- length(x)
  
  if (n == 0 || sum(x) == 0) {
    return(0)
  }
  
  index <- 1:n
  gini <- (2 * sum(index * x)) / (n * sum(x)) - (n + 1) / n
  
  return(gini)
}