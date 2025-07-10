#' @title Model Context Protocol (MCP) Agent Communication
#' @description Core functions for implementing MCP-based agent communication
#' @author WaveQTE Consortium
#' @docType package
#' @name mcpprotocol
NULL

#' Create MCP Agent
#'
#' Initialize a new MCP agent with specified parameters
#'
#' @param agent_id Unique identifier for the agent
#' @param agent_type Type of agent (HFT, MM, II, REG)
#' @param communication_params List of communication parameters
#' @param decision_horizon Time horizon for decision making (minutes)
#' @param risk_tolerance Risk tolerance level (0-1)
#' @param capital_base Base capital amount
#' @return MCP agent object
#' @export
createMCPAgent <- function(agent_id, agent_type, communication_params = list(),
                          decision_horizon = 5, risk_tolerance = 0.5,
                          capital_base = 1000000) {
  
  # Validate agent type
  valid_types <- c("HFT", "MM", "II", "REG")
  if (!agent_type %in% valid_types) {
    stop("Invalid agent type. Must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  # Set default communication parameters
  default_params <- list(
    max_connections = switch(agent_type,
                           "HFT" = 50,
                           "MM" = 30,
                           "II" = 20,
                           "REG" = 100),
    message_frequency = switch(agent_type,
                             "HFT" = 0.1,  # seconds
                             "MM" = 1.0,
                             "II" = 60.0,
                             "REG" = 300.0),
    information_threshold = switch(agent_type,
                                 "HFT" = 0.01,
                                 "MM" = 0.02,
                                 "II" = 0.05,
                                 "REG" = 0.10),
    trust_decay = 0.95
  )
  
  # Merge with user parameters
  comm_params <- modifyList(default_params, communication_params)
  
  # Create agent structure
  agent <- list(
    id = agent_id,
    type = agent_type,
    communication = comm_params,
    decision_horizon = decision_horizon,
    risk_tolerance = risk_tolerance,
    capital_base = capital_base,
    connections = list(),
    message_queue = list(),
    trust_network = list(),
    information_state = list(
      private_signals = numeric(),
      public_signals = numeric(),
      network_signals = numeric(),
      timestamp = Sys.time()
    ),
    performance_metrics = list(
      messages_sent = 0,
      messages_received = 0,
      successful_transactions = 0,
      information_quality = 0.5
    ),
    created_at = Sys.time(),
    active = TRUE
  )
  
  class(agent) <- c("MCPAgent", "list")
  return(agent)
}

#' Establish MCP Connection
#'
#' Create communication link between two MCP agents
#'
#' @param agent1 First MCP agent
#' @param agent2 Second MCP agent
#' @param connection_strength Initial connection strength (0-1)
#' @param bidirectional Whether connection is bidirectional
#' @return Updated agents with established connection
#' @export
establishMCPConnection <- function(agent1, agent2, connection_strength = 0.5,
                                  bidirectional = TRUE) {
  
  # Validate agents
  if (!inherits(agent1, "MCPAgent") || !inherits(agent2, "MCPAgent")) {
    stop("Both arguments must be MCPAgent objects")
  }
  
  # Check if connection already exists
  if (agent2$id %in% names(agent1$connections)) {
    warning("Connection already exists between agents")
    return(list(agent1 = agent1, agent2 = agent2))
  }
  
  # Calculate connection compatibility
  compatibility <- calculateConnectionCompatibility(agent1, agent2)
  
  # Adjust connection strength based on compatibility
  adjusted_strength <- connection_strength * compatibility
  
  # Create connection metadata
  connection_info <- list(
    target_id = agent2$id,
    target_type = agent2$type,
    strength = adjusted_strength,
    established_at = Sys.time(),
    message_count = 0,
    trust_level = 0.5,
    information_flow = 0.0,
    last_activity = Sys.time()
  )
  
  # Add connection to agent1
  agent1$connections[[agent2$id]] <- connection_info
  
  # Add bidirectional connection if requested
  if (bidirectional) {
    connection_info$target_id <- agent1$id
    connection_info$target_type <- agent1$type
    agent2$connections[[agent1$id]] <- connection_info
  }
  
  return(list(agent1 = agent1, agent2 = agent2))
}

#' Send MCP Message
#'
#' Send message from one agent to another through MCP protocol
#'
#' @param sender_agent Sending agent
#' @param receiver_id ID of receiving agent
#' @param message_content Content of the message
#' @param message_type Type of message (SIGNAL, TRADE, RISK, META)
#' @param priority Message priority (1-10)
#' @return Updated sender agent with message metrics
#' @export
sendMCPMessage <- function(sender_agent, receiver_id, message_content,
                          message_type = "SIGNAL", priority = 5) {
  
  # Validate sender
  if (!inherits(sender_agent, "MCPAgent")) {
    stop("sender_agent must be an MCPAgent object")
  }
  
  # Check if connection exists
  if (!receiver_id %in% names(sender_agent$connections)) {
    stop("No connection exists with receiver ID: ", receiver_id)
  }
  
  # Create message
  message <- list(
    id = generateMessageId(),
    sender_id = sender_agent$id,
    receiver_id = receiver_id,
    type = message_type,
    content = message_content,
    priority = priority,
    timestamp = Sys.time(),
    information_value = calculateInformationValue(message_content, sender_agent),
    encrypted = TRUE
  )
  
  # Update connection activity
  sender_agent$connections[[receiver_id]]$last_activity <- Sys.time()
  sender_agent$connections[[receiver_id]]$message_count <- 
    sender_agent$connections[[receiver_id]]$message_count + 1
  
  # Update sender metrics
  sender_agent$performance_metrics$messages_sent <- 
    sender_agent$performance_metrics$messages_sent + 1
  
  # Add to message queue (simulated)
  sender_agent$message_queue[[length(sender_agent$message_queue) + 1]] <- message
  
  return(sender_agent)
}

#' Receive MCP Message
#'
#' Process incoming message for an MCP agent
#'
#' @param receiver_agent Receiving agent
#' @param message Message object to process
#' @return Updated receiver agent with processed message
#' @export
receiveMCPMessage <- function(receiver_agent, message) {
  
  # Validate receiver
  if (!inherits(receiver_agent, "MCPAgent")) {
    stop("receiver_agent must be an MCPAgent object")
  }
  
  # Validate message
  if (!is.list(message) || is.null(message$sender_id)) {
    stop("Invalid message format")
  }
  
  # Check if connection exists
  if (!message$sender_id %in% names(receiver_agent$connections)) {
    warning("Receiving message from unconnected agent")
    return(receiver_agent)
  }
  
  # Process message based on type
  processed_info <- switch(message$type,
    "SIGNAL" = processSignalMessage(message$content, receiver_agent),
    "TRADE" = processTradeMessage(message$content, receiver_agent),
    "RISK" = processRiskMessage(message$content, receiver_agent),
    "META" = processMetaMessage(message$content, receiver_agent),
    processGenericMessage(message$content, receiver_agent)
  )
  
  # Update information state
  receiver_agent$information_state$network_signals <- 
    c(receiver_agent$information_state$network_signals, processed_info$signal_value)
  receiver_agent$information_state$timestamp <- Sys.time()
  
  # Update trust level based on information quality
  connection <- receiver_agent$connections[[message$sender_id]]
  info_quality <- calculateInformationQuality(processed_info, receiver_agent)
  
  # Update trust with exponential smoothing
  alpha <- 0.1
  connection$trust_level <- alpha * info_quality + (1 - alpha) * connection$trust_level
  receiver_agent$connections[[message$sender_id]] <- connection
  
  # Update receiver metrics
  receiver_agent$performance_metrics$messages_received <- 
    receiver_agent$performance_metrics$messages_received + 1
  
  return(receiver_agent)
}

#' Terminate MCP Connection
#'
#' Safely terminate connection between agents
#'
#' @param agent1 First agent
#' @param agent2_id ID of second agent
#' @return Updated agent with removed connection
#' @export
terminateMCPConnection <- function(agent1, agent2_id) {
  
  # Validate agent
  if (!inherits(agent1, "MCPAgent")) {
    stop("agent1 must be an MCPAgent object")
  }
  
  # Check if connection exists
  if (!agent2_id %in% names(agent1$connections)) {
    warning("No connection exists with agent ID: ", agent2_id)
    return(agent1)
  }
  
  # Remove connection
  agent1$connections[[agent2_id]] <- NULL
  
  return(agent1)
}

# Helper Functions

calculateConnectionCompatibility <- function(agent1, agent2) {
  # Calculate compatibility based on agent types and parameters
  type_compatibility <- switch(paste(agent1$type, agent2$type, sep = "-"),
    "HFT-HFT" = 0.9,
    "HFT-MM" = 0.8,
    "HFT-II" = 0.4,
    "HFT-REG" = 0.6,
    "MM-MM" = 0.8,
    "MM-II" = 0.7,
    "MM-REG" = 0.8,
    "II-II" = 0.6,
    "II-REG" = 0.7,
    "REG-REG" = 0.9,
    0.5  # default
  )
  
  # Adjust for decision horizon compatibility
  horizon_diff <- abs(agent1$decision_horizon - agent2$decision_horizon)
  horizon_penalty <- exp(-horizon_diff / 10)
  
  # Adjust for risk tolerance compatibility
  risk_diff <- abs(agent1$risk_tolerance - agent2$risk_tolerance)
  risk_penalty <- exp(-risk_diff * 2)
  
  compatibility <- type_compatibility * horizon_penalty * risk_penalty
  return(min(max(compatibility, 0), 1))
}

generateMessageId <- function() {
  # Generate unique message ID
  paste0("MCP_", format(Sys.time(), "%Y%m%d_%H%M%S_"), 
         sample(1000:9999, 1))
}

calculateInformationValue <- function(content, agent) {
  # Calculate information value based on content and agent characteristics
  if (is.numeric(content)) {
    return(abs(content) * runif(1, 0.5, 1.5))
  } else {
    return(runif(1, 0.1, 0.9))
  }
}

processSignalMessage <- function(content, agent) {
  # Process signal-type messages
  list(
    signal_value = as.numeric(content),
    relevance = runif(1, 0.3, 1.0),
    confidence = runif(1, 0.5, 0.9)
  )
}

processTradeMessage <- function(content, agent) {
  # Process trade-type messages
  list(
    signal_value = runif(1, -0.1, 0.1),
    relevance = runif(1, 0.6, 1.0),
    confidence = runif(1, 0.7, 0.95)
  )
}

processRiskMessage <- function(content, agent) {
  # Process risk-type messages
  list(
    signal_value = -abs(runif(1, 0, 0.2)),
    relevance = runif(1, 0.8, 1.0),
    confidence = runif(1, 0.6, 0.9)
  )
}

processMetaMessage <- function(content, agent) {
  # Process meta-type messages
  list(
    signal_value = runif(1, -0.05, 0.05),
    relevance = runif(1, 0.2, 0.6),
    confidence = runif(1, 0.3, 0.7)
  )
}

processGenericMessage <- function(content, agent) {
  # Process generic messages
  list(
    signal_value = runif(1, -0.1, 0.1),
    relevance = runif(1, 0.3, 0.8),
    confidence = runif(1, 0.4, 0.8)
  )
}

calculateInformationQuality <- function(processed_info, agent) {
  # Calculate quality of received information
  base_quality <- processed_info$relevance * processed_info$confidence
  
  # Adjust based on agent type sensitivity
  type_adjustment <- switch(agent$type,
    "HFT" = 1.2,
    "MM" = 1.1,
    "II" = 1.0,
    "REG" = 1.3
  )
  
  quality <- base_quality * type_adjustment
  return(min(max(quality, 0), 1))
}