# Store transition probabilities for Standard TMZ therapy
trans_prob_standard <- list(
  stable_to_progressive = 0.0956,
  stable_to_death = 0.0464,
  progressive_to_death = 0.0861
)

# Store transition probabilities for Long-term TMZ therapy
trans_prob_longterm <- list(
  stable_to_progressive = 0.0894,
  stable_to_death = 0.0397,
  progressive_to_death = 0.0689
)

# Store health utilities
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store costs for Standard TMZ therapy
costs_standard <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 350,
  progressive = 2720,
  death = 0
)

# Store costs for Long-term TMZ therapy
costs_longterm <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 2453,
  progressive = 2720,
  death = 0
)

# Define model parameters
model_params <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # in months
)

# Define the number of cycles
n_cycles <- 60

# Initialize Markov traces
markov_trace_standard <- matrix(0, nrow = n_cycles, ncol = 3)
markov_trace_longterm <- matrix(0, nrow = n_cycles, ncol = 3)

# Set initial state distribution (100% in stable state at cycle 1)
markov_trace_standard[1, ] <- c(1, 0, 0)
markov_trace_longterm[1, ] <- c(1, 0, 0)

# Transition matrices for both treatments
transition_matrix_standard <- matrix(c(
  1 - trans_prob_standard$stable_to_progressive - trans_prob_standard$stable_to_death, trans_prob_standard$stable_to_progressive, trans_prob_standard$stable_to_death,
  0, 1 - trans_prob_standard$progressive_to_death, trans_prob_standard$progressive_to_death,
  0, 0, 1
), nrow = 3, byrow = TRUE)

transition_matrix_longterm <- matrix(c(
  1 - trans_prob_longterm$stable_to_progressive - trans_prob_longterm$stable_to_death, trans_prob_longterm$stable_to_progressive, trans_prob_longterm$stable_to_death,
  0, 1 - trans_prob_longterm$progressive_to_death, trans_prob_longterm$progressive_to_death,
  0, 0, 1
), nrow = 3, byrow = TRUE)

# Simulate the Markov process for both treatments
for (t in 2:n_cycles) {
  markov_trace_standard[t, ] <- markov_trace_standard[t - 1, ] %*% transition_matrix_standard
  markov_trace_longterm[t, ] <- markov_trace_longterm[t - 1, ] %*% transition_matrix_longterm
}

# Convert results to a data frame for easier viewing and export
markov_trace_standard_df <- as.data.frame(markov_trace_standard)
colnames(markov_trace_standard_df) <- c("Stable", "Progressive", "Death")
markov_trace_standard_df$Cycle <- 1:n_cycles

markov_trace_longterm_df <- as.data.frame(markov_trace_longterm)
colnames(markov_trace_longterm_df) <- c("Stable", "Progressive", "Death")
markov_trace_longterm_df$Cycle <- 1:n_cycles

# Print the first few rows for verification
head(markov_trace_standard_df)
head(markov_trace_longterm_df)

# Save results to CSV files
write.csv(markov_trace_standard_df, "markov_trace_standard.csv", row.names = FALSE)
write.csv(markov_trace_longterm_df, "markov_trace_longterm.csv", row.names = FALSE)

# Initialize QALMs storage for each cycle
qalm_standard <- numeric(n_cycles)
qalm_longterm <- numeric(n_cycles)

# Define utility decrement rules
progressive_utilities <- rep(health_utilities$progressive, n_cycles)

# Apply decrement to progressive utilities from cycle 3 to cycle 25
for (t in 3:25) {
  progressive_utilities[t] <- progressive_utilities[t - 1] - health_utilities$decrement_per_month
}

# Fix utility at cycle 25 for remaining cycles
progressive_utilities[26:n_cycles] <- progressive_utilities[25]

# Compute QALMs for each cycle
for (t in 1:n_cycles) {
  qalm_standard[t] <- (markov_trace_standard[t, 1] * health_utilities$stable) +
    (markov_trace_standard[t, 2] * progressive_utilities[t])
  
  qalm_longterm[t] <- (markov_trace_longterm[t, 1] * health_utilities$stable) +
    (markov_trace_longterm[t, 2] * progressive_utilities[t])
}

# Compute total QALMs
total_qalm_standard <- sum(qalm_standard)
total_qalm_longterm <- sum(qalm_longterm)

# Create data frames for QALMs
qalm_standard_df <- data.frame(Cycle = 1:n_cycles, QALMs = qalm_standard)
qalm_longterm_df <- data.frame(Cycle = 1:n_cycles, QALMs = qalm_longterm)

# Print results for verification
head(qalm_standard_df)
head(qalm_longterm_df)

# Save results to CSV files
write.csv(qalm_standard_df, "qalm_standard.csv", row.names = FALSE)
write.csv(qalm_longterm_df, "qalm_longterm.csv", row.names = FALSE)

# Print total QALMs
cat("Total QALMs for Standard TMZ therapy:", total_qalm_standard, "\n")
cat("Total QALMs for Long-term TMZ therapy:", total_qalm_longterm, "\n")

# Convert total QALMs to QALYs
total_qaly_standard <- total_qalm_standard / 12
total_qaly_longterm <- total_qalm_longterm / 12

# Print total QALYs for each treatment
cat("Total QALYs for Standard TMZ therapy:", total_qaly_standard, "\n")
cat("Total QALYs for Long-term TMZ therapy:", total_qaly_longterm, "\n")

# Initialize cost storage for each cycle
cost_standard <- numeric(n_cycles)
cost_longterm <- numeric(n_cycles)

# Assign costs based on the cycle structure
for (t in 1:n_cycles) {
  if (t <= 4) {
    # Apply cost for months 1-3 to cycles 1-4
    cost_standard[t] <- markov_trace_standard[t, 1] * costs_standard$stable_1_3 +
      markov_trace_standard[t, 2] * costs_standard$progressive +
      markov_trace_standard[t, 3] * costs_standard$death
    
    cost_longterm[t] <- markov_trace_longterm[t, 1] * costs_longterm$stable_1_3 +
      markov_trace_longterm[t, 2] * costs_longterm$progressive +
      markov_trace_longterm[t, 3] * costs_longterm$death
    
  } else if (t <= 7) {
    # Apply cost for months 4-6 to cycles 5-7
    cost_standard[t] <- markov_trace_standard[t, 1] * costs_standard$stable_4_6 +
      markov_trace_standard[t, 2] * costs_standard$progressive +
      markov_trace_standard[t, 3] * costs_standard$death
    
    cost_longterm[t] <- markov_trace_longterm[t, 1] * costs_longterm$stable_4_6 +
      markov_trace_longterm[t, 2] * costs_longterm$progressive +
      markov_trace_longterm[t, 3] * costs_longterm$death
    
  } else {
    # Apply cost for subsequent months to cycles 8-60
    cost_standard[t] <- markov_trace_standard[t, 1] * costs_standard$stable_subsequent +
      markov_trace_standard[t, 2] * costs_standard$progressive +
      markov_trace_standard[t, 3] * costs_standard$death
    
    cost_longterm[t] <- markov_trace_longterm[t, 1] * costs_longterm$stable_subsequent +
      markov_trace_longterm[t, 2] * costs_longterm$progressive +
      markov_trace_longterm[t, 3] * costs_longterm$death
  }
}

# Compute total costs
total_cost_standard <- sum(cost_standard)
total_cost_longterm <- sum(cost_longterm)

# Create data frames for cost per cycle
cost_standard_df <- data.frame(Cycle = 1:n_cycles, Cost = cost_standard)
cost_longterm_df <- data.frame(Cycle = 1:n_cycles, Cost = cost_longterm)

# Print results for verification
head(cost_standard_df)
head(cost_longterm_df)

# Save results to CSV files
write.csv(cost_standard_df, "cost_standard.csv", row.names = FALSE)
write.csv(cost_longterm_df, "cost_longterm.csv", row.names = FALSE)

# Print total costs
cat("Total Cost for Standard TMZ therapy:", total_cost_standard, "\n")
cat("Total Cost for Long-term TMZ therapy:", total_cost_longterm, "\n")

# Calculate incremental cost
incremental_cost <- total_cost_longterm - total_cost_standard

# Calculate incremental effectiveness (QALMs and QALYs)
incremental_qalm <- total_qalm_longterm - total_qalm_standard
incremental_qaly <- total_qaly_longterm - total_qaly_standard

# Calculate ICERs
icer_qalm <- incremental_cost / incremental_qalm
icer_qaly <- incremental_cost / incremental_qaly

# Format ICERs to show as whole numbers (no decimals)
icer_qalm <- round(icer_qalm, 0)
icer_qaly <- round(icer_qaly, 0)

# Print ICER results
cat("Incremental Cost-Effectiveness Ratio (ICER) per QALM:", icer_qalm, "\n")
cat("Incremental Cost-Effectiveness Ratio (ICER) per QALY:", icer_qaly, "\n")
