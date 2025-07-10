#' @title Heterogeneous Agent-Based Model
#' @description Implementation of multi-agent financial market simulation
#' @author WaveQTE Consortium
#' @docType package
#' @name agentbasedmodel
NULL

#' Initialize Agent System
#'
#' Create a complete agent-based financial market system
#'
#' @param n_hft Number of high-frequency traders
#' @param n_mm Number of market makers
#' @param n_ii Number of institutional investors
#' @param n_reg Number of regulators
#' @param market_params Market parameters
#' @param simulation_params Simulation parameters
#' @return AgentSystem object
#' @export
initializeAgentSystem <- function(n_hft = 50, n_mm = 20, n_ii = 30, n_reg = 3,
                                 market_params = NULL, simulation_params = NULL) {
  
  # Set default market parameters
  if (is.null(market_params)) {
    market_params <- list(
      initial_price = 100,
      tick_size = 0.01,
      max_position = 1000,
      transaction_cost = 0.001,
      market_impact = 0.01,
      volatility = 0.02,
      drift = 0.0001,
      liquidity = 1000000
    )
  }
  
  # Set default simulation parameters
  if (is.null(simulation_params)) {
    simulation_params <- list(
      time_step = 0.1,  # minutes
      total_time = 1440,  # minutes (1 day)
      random_seed = 42,
      record_frequency = 1,  # record every minute
      shock_probability = 0.001,
      shock_magnitude = 0.05
    )
  }
  
  # Set random seed for reproducibility
  set.seed(simulation_params$random_seed)
  
  # Create agents
  agents <- list()
  agent_id <- 1
  
  # High-Frequency Traders
  for (i in 1:n_hft) {
    agents[[agent_id]] <- createHFTAgent(
      agent_id = agent_id,
      capital_base = runif(1, 1e6, 5e6),
      risk_tolerance = runif(1, 0.7, 0.9),
      speed = runif(1, 0.8, 1.0),
      market_params = market_params
    )
    agent_id <- agent_id + 1
  }
  
  # Market Makers
  for (i in 1:n_mm) {
    agents[[agent_id]] <- createMarketMaker(
      agent_id = agent_id,
      capital_base = runif(1, 5e6, 20e6),
      risk_tolerance = runif(1, 0.5, 0.7),
      inventory_target = 0,
      spread = runif(1, 0.02, 0.05),
      market_params = market_params
    )
    agent_id <- agent_id + 1
  }
  
  # Institutional Investors
  for (i in 1:n_ii) {
    agents[[agent_id]] <- createInstitutionalInvestor(
      agent_id = agent_id,
      capital_base = runif(1, 50e6, 200e6),
      risk_tolerance = runif(1, 0.3, 0.6),
      investment_horizon = runif(1, 60, 480),  # minutes
      fundamental_weight = runif(1, 0.6, 0.9),
      market_params = market_params
    )
    agent_id <- agent_id + 1
  }
  
  # Regulators
  for (i in 1:n_reg) {
    agents[[agent_id]] <- createRegulator(
      agent_id = agent_id,
      monitoring_scope = c("HFT", "MM", "II"),
      intervention_threshold = 0.05,
      circuit_breaker_threshold = 0.10,
      market_params = market_params
    )
    agent_id <- agent_id + 1
  }
  
  # Initialize market state
  market_state <- list(
    current_price = market_params$initial_price,
    price_history = numeric(0),
    volume_history = numeric(0),
    timestamp_history = numeric(0),
    order_book = initializeOrderBook(),
    last_update = 0,
    volatility = market_params$volatility,
    liquidity = market_params$liquidity,
    circuit_breaker_active = FALSE,
    halt_time = 0
  )
  
  # Create agent system
  agent_system <- list(
    agents = agents,
    market_state = market_state,
    market_params = market_params,
    simulation_params = simulation_params,
    network_dynamics = NULL,
    performance_metrics = initializePerformanceMetrics(length(agents)),
    event_log = data.frame(),
    current_time = 0
  )
  
  class(agent_system) <- c("AgentSystem", "list")
  return(agent_system)
}

#' Create High-Frequency Trader Agent
#'
#' Initialize an HFT agent with specific characteristics
#'
#' @param agent_id Unique agent identifier
#' @param capital_base Initial capital
#' @param risk_tolerance Risk tolerance (0-1)
#' @param speed Trading speed factor
#' @param market_params Market parameters
#' @return HFT agent object
#' @export
createHFTAgent <- function(agent_id, capital_base = 1e6, risk_tolerance = 0.8,
                          speed = 1.0, market_params = NULL) {
  
  # Create base MCP agent
  base_agent <- createMCPAgent(
    agent_id = agent_id,
    agent_type = "HFT",
    decision_horizon = 0.1,  # 6 seconds
    risk_tolerance = risk_tolerance,
    capital_base = capital_base
  )
  
  # HFT-specific parameters
  hft_params <- list(
    speed = speed,
    latency = runif(1, 0.001, 0.01),  # milliseconds
    algorithm_type = sample(c("market_making", "momentum", "arbitrage"), 1),
    order_size = runif(1, 100, 1000),
    max_position = capital_base * 0.1,
    profit_target = 0.001,
    stop_loss = 0.002,
    inventory_penalty = 0.01,
    signal_threshold = 0.0001,
    learning_rate = 0.01
  )
  
  # Trading state
  trading_state <- list(
    position = 0,
    cash = capital_base,
    unrealized_pnl = 0,
    realized_pnl = 0,
    active_orders = list(),
    signal_history = numeric(0),
    trade_history = data.frame(),
    last_trade_time = 0,
    inventory_timer = 0,
    risk_exposure = 0
  )
  
  # Performance metrics
  performance <- list(
    total_trades = 0,
    winning_trades = 0,
    total_volume = 0,
    sharpe_ratio = 0,
    max_drawdown = 0,
    latency_stats = list(mean = 0, std = 0),
    fill_rate = 0,
    profit_factor = 0
  )
  
  # Combine all components
  hft_agent <- c(base_agent, list(
    hft_params = hft_params,
    trading_state = trading_state,
    performance = performance
  ))
  
  class(hft_agent) <- c("HFTAgent", "MCPAgent", "list")
  return(hft_agent)
}

#' Create Market Maker Agent
#'
#' Initialize a market maker agent
#'
#' @param agent_id Unique agent identifier
#' @param capital_base Initial capital
#' @param risk_tolerance Risk tolerance (0-1)
#' @param inventory_target Target inventory level
#' @param spread Bid-ask spread
#' @param market_params Market parameters
#' @return Market maker agent object
#' @export
createMarketMaker <- function(agent_id, capital_base = 10e6, risk_tolerance = 0.6,
                             inventory_target = 0, spread = 0.02, market_params = NULL) {
  
  # Create base MCP agent
  base_agent <- createMCPAgent(
    agent_id = agent_id,
    agent_type = "MM",
    decision_horizon = 1,  # 1 minute
    risk_tolerance = risk_tolerance,
    capital_base = capital_base
  )
  
  # Market maker specific parameters
  mm_params <- list(
    spread = spread,
    inventory_target = inventory_target,
    max_inventory = capital_base * 0.05,
    quote_size = runif(1, 500, 2000),
    skew_factor = 0.1,
    adverse_selection_protection = 0.01,
    inventory_penalty = 0.005,
    volatility_adjustment = 0.5,
    quote_frequency = 1,  # seconds
    min_quote_time = 0.1
  )
  
  # Trading state
  trading_state <- list(
    position = 0,
    cash = capital_base,
    unrealized_pnl = 0,
    realized_pnl = 0,
    bid_orders = list(),
    ask_orders = list(),
    inventory = 0,
    last_quote_time = 0,
    total_quoted_volume = 0,
    adverse_selection_cost = 0,
    inventory_cost = 0
  )
  
  # Performance metrics
  performance <- list(
    total_trades = 0,
    bid_fills = 0,
    ask_fills = 0,
    gross_profit = 0,
    net_profit = 0,
    inventory_turnover = 0,
    quote_efficiency = 0,
    market_share = 0
  )
  
  # Combine all components
  mm_agent <- c(base_agent, list(
    mm_params = mm_params,
    trading_state = trading_state,
    performance = performance
  ))
  
  class(mm_agent) <- c("MarketMaker", "MCPAgent", "list")
  return(mm_agent)
}

#' Create Institutional Investor Agent
#'
#' Initialize an institutional investor agent
#'
#' @param agent_id Unique agent identifier
#' @param capital_base Initial capital
#' @param risk_tolerance Risk tolerance (0-1)
#' @param investment_horizon Investment horizon (minutes)
#' @param fundamental_weight Weight on fundamental analysis
#' @param market_params Market parameters
#' @return Institutional investor agent object
#' @export
createInstitutionalInvestor <- function(agent_id, capital_base = 100e6, risk_tolerance = 0.4,
                                       investment_horizon = 240, fundamental_weight = 0.8,
                                       market_params = NULL) {
  
  # Create base MCP agent
  base_agent <- createMCPAgent(
    agent_id = agent_id,
    agent_type = "II",
    decision_horizon = investment_horizon,
    risk_tolerance = risk_tolerance,
    capital_base = capital_base
  )
  
  # Institutional investor specific parameters
  ii_params <- list(
    investment_horizon = investment_horizon,
    fundamental_weight = fundamental_weight,
    technical_weight = 1 - fundamental_weight,
    rebalance_frequency = 60,  # minutes
    max_position = capital_base * 0.3,
    execution_algorithm = sample(c("TWAP", "VWAP", "POV"), 1),
    participation_rate = runif(1, 0.05, 0.20),
    patience = runif(1, 0.5, 0.9),
    fundamental_signal = 0,
    technical_signal = 0
  )
  
  # Trading state
  trading_state <- list(
    position = 0,
    cash = capital_base,
    unrealized_pnl = 0,
    realized_pnl = 0,
    target_position = 0,
    execution_orders = list(),
    last_rebalance_time = 0,
    fundamental_value = market_params$initial_price,
    signal_strength = 0,
    execution_progress = 0
  )
  
  # Performance metrics
  performance <- list(
    total_trades = 0,
    total_volume = 0,
    execution_cost = 0,
    tracking_error = 0,
    information_ratio = 0,
    hit_rate = 0,
    average_holding_period = 0,
    portfolio_turnover = 0
  )
  
  # Combine all components
  ii_agent <- c(base_agent, list(
    ii_params = ii_params,
    trading_state = trading_state,
    performance = performance
  ))
  
  class(ii_agent) <- c("InstitutionalInvestor", "MCPAgent", "list")
  return(ii_agent)
}

#' Create Regulator Agent
#'
#' Initialize a regulator agent
#'
#' @param agent_id Unique agent identifier
#' @param monitoring_scope Types of agents to monitor
#' @param intervention_threshold Threshold for intervention
#' @param circuit_breaker_threshold Threshold for circuit breaker
#' @param market_params Market parameters
#' @return Regulator agent object
#' @export
createRegulator <- function(agent_id, monitoring_scope = c("HFT", "MM", "II"),
                           intervention_threshold = 0.05, circuit_breaker_threshold = 0.10,
                           market_params = NULL) {
  
  # Create base MCP agent
  base_agent <- createMCPAgent(
    agent_id = agent_id,
    agent_type = "REG",
    decision_horizon = 5,  # 5 minutes
    risk_tolerance = 0.0,  # Risk-averse
    capital_base = 0  # No trading capital
  )
  
  # Regulator specific parameters
  reg_params <- list(
    monitoring_scope = monitoring_scope,
    intervention_threshold = intervention_threshold,
    circuit_breaker_threshold = circuit_breaker_threshold,
    monitoring_frequency = 0.1,  # minutes
    investigation_time = 30,  # minutes
    penalty_strength = 0.01,
    transparency_requirement = 0.8,
    systemic_risk_threshold = 0.15,
    coordination_weight = 0.5
  )
  
  # Monitoring state
  monitoring_state <- list(
    active_investigations = list(),
    violations_detected = 0,
    interventions_made = 0,
    circuit_breakers_triggered = 0,
    systemic_risk_level = 0,
    market_stress_indicator = 0,
    last_monitoring_time = 0,
    alert_log = data.frame()
  )
  
  # Performance metrics
  performance <- list(
    detection_rate = 0,
    false_positive_rate = 0,
    intervention_effectiveness = 0,
    market_stability_improvement = 0,
    compliance_rate = 0,
    systemic_risk_reduction = 0
  )
  
  # Combine all components
  reg_agent <- c(base_agent, list(
    reg_params = reg_params,
    monitoring_state = monitoring_state,
    performance = performance
  ))
  
  class(reg_agent) <- c("Regulator", "MCPAgent", "list")
  return(reg_agent)
}

#' Run Agent Simulation
#'
#' Execute the complete agent-based market simulation
#'
#' @param agent_system AgentSystem object
#' @param network_dynamics NetworkDynamics object (optional)
#' @param progress_callback Function to call for progress updates
#' @return Updated AgentSystem with simulation results
#' @export
runAgentSimulation <- function(agent_system, network_dynamics = NULL,
                              progress_callback = NULL) {
  
  if (!inherits(agent_system, "AgentSystem")) {
    stop("agent_system must be an AgentSystem object")
  }
  
  # Set up simulation parameters
  time_step <- agent_system$simulation_params$time_step
  total_time <- agent_system$simulation_params$total_time
  n_steps <- ceiling(total_time / time_step)
  
  # Initialize progress tracking
  if (is.null(progress_callback)) {
    progress_callback <- function(step, total) {
      if (step %% 100 == 0) {
        cat(sprintf("Step %d/%d (%.1f%%)\n", step, total, 100 * step / total))
      }
    }
  }
  
  # Attach network dynamics if provided
  if (!is.null(network_dynamics)) {
    agent_system$network_dynamics <- network_dynamics
  }
  
  # Main simulation loop
  for (step in 1:n_steps) {
    current_time <- (step - 1) * time_step
    agent_system$current_time <- current_time
    
    # Update market state
    agent_system <- updateMarketState(agent_system)
    
    # Process each agent
    for (i in seq_along(agent_system$agents)) {
      agent <- agent_system$agents[[i]]
      
      # Agent-specific processing
      if (inherits(agent, "HFTAgent")) {
        agent_system$agents[[i]] <- processHFTAgent(agent, agent_system)
      } else if (inherits(agent, "MarketMaker")) {
        agent_system$agents[[i]] <- processMarketMaker(agent, agent_system)
      } else if (inherits(agent, "InstitutionalInvestor")) {
        agent_system$agents[[i]] <- processInstitutionalInvestor(agent, agent_system)
      } else if (inherits(agent, "Regulator")) {
        agent_system$agents[[i]] <- processRegulator(agent, agent_system)
      }
    }
    
    # Update network dynamics
    if (!is.null(agent_system$network_dynamics)) {
      agent_system$network_dynamics <- updateNetworkState(
        agent_system$network_dynamics,
        agent_system$market_state
      )
    }
    
    # Record performance metrics
    if (step %% agent_system$simulation_params$record_frequency == 0) {
      agent_system <- recordPerformanceMetrics(agent_system)
    }
    
    # Apply external shocks
    if (runif(1) < agent_system$simulation_params$shock_probability) {
      agent_system <- applyMarketShock(agent_system)
    }
    
    # Progress callback
    progress_callback(step, n_steps)
  }
  
  # Final performance calculations
  agent_system <- calculateFinalPerformance(agent_system)
  
  return(agent_system)
}

# Helper Functions

initializeOrderBook <- function() {
  # Initialize a simple order book structure
  list(
    bids = data.frame(price = numeric(0), volume = numeric(0), 
                     agent_id = character(0), timestamp = numeric(0)),
    asks = data.frame(price = numeric(0), volume = numeric(0),
                     agent_id = character(0), timestamp = numeric(0)),
    last_trade_price = 0,
    last_trade_volume = 0,
    last_trade_time = 0
  )
}

initializePerformanceMetrics <- function(n_agents) {
  # Initialize performance tracking
  list(
    agent_pnl = matrix(0, nrow = n_agents, ncol = 1),
    agent_positions = matrix(0, nrow = n_agents, ncol = 1),
    agent_trades = matrix(0, nrow = n_agents, ncol = 1),
    market_prices = numeric(0),
    market_volumes = numeric(0),
    timestamps = numeric(0),
    volatility_series = numeric(0),
    liquidity_series = numeric(0)
  )
}

updateMarketState <- function(agent_system) {
  # Update market price and other state variables
  
  # Simple random walk with drift
  dt <- agent_system$simulation_params$time_step / 60  # convert to hours
  drift <- agent_system$market_params$drift
  volatility <- agent_system$market_state$volatility
  
  # Price update
  price_change <- drift * dt + volatility * sqrt(dt) * rnorm(1)
  agent_system$market_state$current_price <- 
    agent_system$market_state$current_price * (1 + price_change)
  
  # Update volatility (GARCH-like)
  alpha <- 0.1
  beta <- 0.8
  agent_system$market_state$volatility <- 
    alpha * abs(price_change) + beta * agent_system$market_state$volatility
  
  # Record history
  agent_system$market_state$price_history <- 
    c(agent_system$market_state$price_history, agent_system$market_state$current_price)
  agent_system$market_state$timestamp_history <- 
    c(agent_system$market_state$timestamp_history, agent_system$current_time)
  
  return(agent_system)
}

processHFTAgent <- function(agent, agent_system) {
  # Process HFT agent behavior
  
  # Generate trading signal
  signal <- generateHFTSignal(agent, agent_system)
  
  # Update signal history
  agent$information_state$private_signals <- 
    c(agent$information_state$private_signals, signal)
  
  # Make trading decision
  if (abs(signal) > agent$hft_params$signal_threshold) {
    agent <- executeHFTTrade(agent, agent_system, signal)
  }
  
  # Update performance metrics
  agent <- updateHFTPerformance(agent, agent_system)
  
  return(agent)
}

processMarketMaker <- function(agent, agent_system) {
  # Process market maker behavior
  
  # Update quotes
  agent <- updateMarketMakerQuotes(agent, agent_system)
  
  # Manage inventory
  agent <- manageInventory(agent, agent_system)
  
  # Update performance
  agent <- updateMarketMakerPerformance(agent, agent_system)
  
  return(agent)
}

processInstitutionalInvestor <- function(agent, agent_system) {
  # Process institutional investor behavior
  
  # Update fundamental and technical signals
  agent <- updateInvestmentSignals(agent, agent_system)
  
  # Rebalance portfolio if needed
  if (agent_system$current_time - agent$trading_state$last_rebalance_time >= 
      agent$ii_params$rebalance_frequency) {
    agent <- rebalancePortfolio(agent, agent_system)
  }
  
  # Update performance
  agent <- updateInstitutionalPerformance(agent, agent_system)
  
  return(agent)
}

processRegulator <- function(agent, agent_system) {
  # Process regulator behavior
  
  # Monitor market conditions
  agent <- monitorMarketConditions(agent, agent_system)
  
  # Check for violations
  agent <- checkForViolations(agent, agent_system)
  
  # Intervene if necessary
  if (agent$monitoring_state$systemic_risk_level > agent$reg_params$intervention_threshold) {
    agent_system <- regulatoryIntervention(agent, agent_system)
  }
  
  return(agent)
}

# Additional helper functions would be implemented here
# (generateHFTSignal, executeHFTTrade, updateMarketMakerQuotes, etc.)
# These are simplified for brevity

generateHFTSignal <- function(agent, agent_system) {
  # Generate HFT trading signal based on price movements
  price_history <- agent_system$market_state$price_history
  if (length(price_history) < 2) return(0)
  
  # Simple momentum signal
  returns <- diff(tail(price_history, 10))
  signal <- mean(returns, na.rm = TRUE) * agent$hft_params$speed
  
  # Add noise
  signal <- signal + rnorm(1, 0, 0.001)
  
  return(signal)
}

executeHFTTrade <- function(agent, agent_system, signal) {
  # Execute HFT trade based on signal
  
  # Simplified trade execution
  trade_size <- agent$hft_params$order_size * sign(signal)
  current_price <- agent_system$market_state$current_price
  
  # Update position and cash
  agent$trading_state$position <- agent$trading_state$position + trade_size
  agent$trading_state$cash <- agent$trading_state$cash - trade_size * current_price
  
  # Record trade
  agent$trading_state$last_trade_time <- agent_system$current_time
  agent$performance$total_trades <- agent$performance$total_trades + 1
  
  return(agent)
}

updateMarketMakerQuotes <- function(agent, agent_system) {
  # Update market maker quotes
  
  current_price <- agent_system$market_state$current_price
  spread <- agent$mm_params$spread
  
  # Calculate bid and ask prices
  bid_price <- current_price * (1 - spread / 2)
  ask_price <- current_price * (1 + spread / 2)
  
  # Update quotes (simplified)
  agent$trading_state$last_quote_time <- agent_system$current_time
  
  return(agent)
}

manageInventory <- function(agent, agent_system) {
  # Manage market maker inventory
  
  # Simple inventory management
  if ("trading_state" %in% names(agent)) {
    current_position <- agent$trading_state$position
    target_position <- agent$mm_params$inventory_target
    
    # Adjust quotes based on inventory
    if (abs(current_position - target_position) > agent$mm_params$max_inventory * 0.1) {
      # Need to reduce inventory
      agent$mm_params$spread <- agent$mm_params$spread * 1.1
    }
  }
  
  return(agent)
}

updateHFTPerformance <- function(agent, agent_system) {
  # Update HFT performance metrics
  
  current_price <- agent_system$market_state$current_price
  agent$trading_state$unrealized_pnl <- 
    agent$trading_state$position * current_price + agent$trading_state$cash - agent$capital_base
  
  return(agent)
}

updateMarketMakerPerformance <- function(agent, agent_system) {
  # Update market maker performance
  
  current_price <- agent_system$market_state$current_price
  agent$trading_state$unrealized_pnl <- 
    agent$trading_state$position * current_price + agent$trading_state$cash - agent$capital_base
  
  return(agent)
}

updateInvestmentSignals <- function(agent, agent_system) {
  # Update fundamental and technical signals for institutional investor
  
  # Simplified signal generation
  price_history <- agent_system$market_state$price_history
  if (length(price_history) >= 20) {
    # Technical signal (mean reversion)
    ma_short <- mean(tail(price_history, 5))
    ma_long <- mean(tail(price_history, 20))
    agent$ii_params$technical_signal <- (ma_short - ma_long) / ma_long
  }
  
  # Fundamental signal (random walk around fair value)
  agent$ii_params$fundamental_signal <- rnorm(1, 0, 0.01)
  
  return(agent)
}

rebalancePortfolio <- function(agent, agent_system) {
  # Rebalance institutional investor portfolio
  
  # Calculate target position
  combined_signal <- agent$ii_params$fundamental_weight * agent$ii_params$fundamental_signal +
                    agent$ii_params$technical_weight * agent$ii_params$technical_signal
  
  # Convert to target position
  max_position <- agent$ii_params$max_position
  target_position <- max_position * tanh(combined_signal * 10)
  
  agent$trading_state$target_position <- target_position
  agent$trading_state$last_rebalance_time <- agent_system$current_time
  
  return(agent)
}

updateInstitutionalPerformance <- function(agent, agent_system) {
  # Update institutional investor performance
  
  current_price <- agent_system$market_state$current_price
  agent$trading_state$unrealized_pnl <- 
    agent$trading_state$position * current_price + agent$trading_state$cash - agent$capital_base
  
  return(agent)
}

monitorMarketConditions <- function(agent, agent_system) {
  # Monitor market conditions for regulatory purposes
  
  # Calculate market stress indicators
  price_history <- agent_system$market_state$price_history
  if (length(price_history) >= 10) {
    returns <- diff(tail(price_history, 10))
    volatility <- sd(returns, na.rm = TRUE)
    
    # Update stress indicator
    agent$monitoring_state$market_stress_indicator <- volatility
    agent$monitoring_state$systemic_risk_level <- 
      min(volatility / agent_system$market_params$volatility, 1)
  }
  
  return(agent)
}

checkForViolations <- function(agent, agent_system) {
  # Check for regulatory violations
  
  # Simplified violation detection
  if (agent$monitoring_state$market_stress_indicator > 0.05) {
    agent$monitoring_state$violations_detected <- 
      agent$monitoring_state$violations_detected + 1
  }
  
  return(agent)
}

regulatoryIntervention <- function(agent, agent_system) {
  # Implement regulatory intervention
  
  # Circuit breaker activation
  if (agent$monitoring_state$systemic_risk_level > agent$reg_params$circuit_breaker_threshold) {
    agent_system$market_state$circuit_breaker_active <- TRUE
    agent$monitoring_state$circuit_breakers_triggered <- 
      agent$monitoring_state$circuit_breakers_triggered + 1
  }
  
  return(agent_system)
}

recordPerformanceMetrics <- function(agent_system) {
  # Record current performance metrics
  
  # Update price and volume history
  agent_system$performance_metrics$market_prices <- 
    c(agent_system$performance_metrics$market_prices, agent_system$market_state$current_price)
  agent_system$performance_metrics$timestamps <- 
    c(agent_system$performance_metrics$timestamps, agent_system$current_time)
  
  # Update volatility series
  agent_system$performance_metrics$volatility_series <- 
    c(agent_system$performance_metrics$volatility_series, agent_system$market_state$volatility)
  
  return(agent_system)
}

applyMarketShock <- function(agent_system) {
  # Apply external market shock
  
  shock_magnitude <- agent_system$simulation_params$shock_magnitude
  shock_direction <- sample(c(-1, 1), 1)
  
  # Apply shock to price
  agent_system$market_state$current_price <- 
    agent_system$market_state$current_price * (1 + shock_direction * shock_magnitude)
  
  # Increase volatility temporarily
  agent_system$market_state$volatility <- 
    agent_system$market_state$volatility * 1.5
  
  return(agent_system)
}

calculateFinalPerformance <- function(agent_system) {
  # Calculate final performance metrics for all agents
  
  for (i in seq_along(agent_system$agents)) {
    agent <- agent_system$agents[[i]]
    
    # Calculate final PnL
    current_price <- agent_system$market_state$current_price
    if ("trading_state" %in% names(agent)) {
      final_pnl <- agent$trading_state$position * current_price + 
                   agent$trading_state$cash - agent$capital_base
      
      # Update performance metrics
      if (inherits(agent, "HFTAgent")) {
        agent$performance$sharpe_ratio <- calculateSharpeRatio(agent)
      } else if (inherits(agent, "MarketMaker")) {
        agent$performance$market_share <- calculateMarketShare(agent, agent_system)
      } else if (inherits(agent, "InstitutionalInvestor")) {
        agent$performance$information_ratio <- calculateInformationRatio(agent)
      }
      
      agent_system$agents[[i]] <- agent
    }
  }
  
  return(agent_system)
}

calculateSharpeRatio <- function(agent) {
  # Calculate Sharpe ratio for agent
  if (length(agent$information_state$private_signals) > 1) {
    returns <- diff(agent$information_state$private_signals)
    if (sd(returns) > 0) {
      return(mean(returns) / sd(returns))
    }
  }
  return(0)
}

calculateMarketShare <- function(agent, agent_system) {
  # Calculate market share for market maker
  total_volume <- sum(agent_system$market_state$volume_history, na.rm = TRUE)
  if (total_volume > 0) {
    return(agent$trading_state$total_quoted_volume / total_volume)
  }
  return(0)
}

calculateInformationRatio <- function(agent) {
  # Calculate information ratio for institutional investor
  if (length(agent$information_state$private_signals) > 1) {
    returns <- diff(agent$information_state$private_signals)
    if (sd(returns) > 0) {
      return(mean(returns) / sd(returns))
    }
  }
  return(0)
}