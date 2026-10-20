# Store transition probabilities for Standard TMZ therapy
transition_probs_standard <- list(
  stable_to_progressive = 0.0956,
  stable_to_death = 0.0464,
  progressive_to_death = 0.0861
)

# Store transition probabilities for Long-term TMZ therapy
transition_probs_longterm <- list(
  stable_to_progressive = 0.0894,
  stable_to_death = 0.0397,
  progressive_to_death = 0.0689
)

# Store health utility values
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store cost values for Standard TMZ therapy
costs_standard <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 350,
  progressive = 2720,
  death = 0
)

# Store cost values for Long-term TMZ therapy
costs_longterm <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 2453,
  progressive = 2720,
  death = 0
)

# Define model parameters
cycle_length <- 1 # in months
time_horizon <- 60 # in months

# Define number of cycles
num_cycles <- 60

# Initialize Markov trace matrices for both treatments
trace_standard <- matrix(0, nrow = num_cycles, ncol = 3)
trace_longterm <- matrix(0, nrow = num_cycles, ncol = 3)

# Initial distribution: 100% of patients in the stable state
trace_standard[1, ] <- c(1, 0, 0)
trace_longterm[1, ] <- c(1, 0, 0)

# Define transition probability matrices for both treatments
trans_matrix_standard <- matrix(c(
  1 - transition_probs_standard$stable_to_progressive - transition_probs_standard$stable_to_death, transition_probs_standard$stable_to_progressive, transition_probs_standard$stable_to_death,
  0, 1 - transition_probs_standard$progressive_to_death, transition_probs_standard$progressive_to_death,
  0, 0, 1
), nrow = 3, byrow = TRUE)

trans_matrix_longterm <- matrix(c(
  1 - transition_probs_longterm$stable_to_progressive - transition_probs_longterm$stable_to_death, transition_probs_longterm$stable_to_progressive, transition_probs_longterm$stable_to_death,
  0, 1 - transition_probs_longterm$progressive_to_death, transition_probs_longterm$progressive_to_death,
  0, 0, 1
), nrow = 3, byrow = TRUE)

# Compute the Markov trace for both treatments
for (cycle in 2:num_cycles) {
  trace_standard[cycle, ] <- trace_standard[cycle - 1, ] %*% trans_matrix_standard
  trace_longterm[cycle, ] <- trace_longterm[cycle - 1, ] %*% trans_matrix_longterm
}

# Store results in a data frame
markov_trace_df <- data.frame(
  Cycle = 1:num_cycles,
  Stable_Standard = trace_standard[, 1],
  Progressive_Standard = trace_standard[, 2],
  Death_Standard = trace_standard[, 3],
  Stable_LongTerm = trace_longterm[, 1],
  Progressive_LongTerm = trace_longterm[, 2],
  Death_LongTerm = trace_longterm[, 3]
)

# Display the Markov trace
glibrary(ggplot2)
library(reshape2)

markov_trace_melted <- melt(markov_trace_df, id.vars = "Cycle")

# Plot the Markov trace
plot <- ggplot(markov_trace_melted, aes(x = Cycle, y = value, color = variable)) +
  geom_line() +
  labs(title = "Markov Trace for Standard and Long-term TMZ Therapy",
       x = "Cycle (Months)", y = "Proportion of Patients",
       color = "State") +
  theme_minimal()

print(plot)

# Compute QALMs for each cycle
qalms_standard <- numeric(num_cycles)
qalms_longterm <- numeric(num_cycles)

# Utility decrement handling for the progressive state
decrement_start <- 3  # Cycle 3 (index 3 in R)
decrement_end <- 25   # Cycle 25 (index 25 in R)
fixed_utility <- health_utilities$progressive - ((decrement_end - decrement_start + 1) * health_utilities$decrement_per_month)

for (cycle in 1:num_cycles) {
  if (cycle < decrement_start) {
    utility_progressive <- health_utilities$progressive
  } else if (cycle <= decrement_end) {
    utility_progressive <- health_utilities$progressive - ((cycle - decrement_start + 1) * health_utilities$decrement_per_month)
  } else {
    utility_progressive <- fixed_utility
  }
  
  # Calculate QALMs for each cycle
  qalms_standard[cycle] <- (trace_standard[cycle, 1] * health_utilities$stable) +
    (trace_standard[cycle, 2] * utility_progressive)
  qalms_longterm[cycle] <- (trace_longterm[cycle, 1] * health_utilities$stable) +
    (trace_longterm[cycle, 2] * utility_progressive)
}

# Calculate total QALMs
total_qalms_standard <- sum(qalms_standard)
total_qalms_longterm <- sum(qalms_longterm)

# Convert total QALMs to QALYs
total_qalys_standard <- total_qalms_standard / 12
total_qalys_longterm <- total_qalms_longterm / 12

# Store QALMs in a data frame
qalms_df <- data.frame(
  Cycle = 1:num_cycles,
  QALMs_Standard = qalms_standard,
  QALMs_LongTerm = qalms_longterm
)

# Display the QALMs data
glibrary(ggplot2)
qalms_melted <- melt(qalms_df, id.vars = "Cycle")

# Plot the QALMs per cycle
plot_qalms <- ggplot(qalms_melted, aes(x = Cycle, y = value, color = variable)) +
  geom_line() +
  labs(title = "Quality-Adjusted Life Months (QALMs) per Cycle",
       x = "Cycle (Months)", y = "QALMs",
       color = "Treatment") +
  theme_minimal()

print(plot_qalms)

# Output total QALMs and QALYs
print(paste("Total QALMs for Standard TMZ Therapy:", total_qalms_standard))
print(paste("Total QALMs for Long-term TMZ Therapy:", total_qalms_longterm))
print(paste("Total QALYs for Standard TMZ Therapy:", total_qalys_standard))
print(paste("Total QALYs for Long-term TMZ Therapy:", total_qalys_longterm))

# Compute costs for each cycle
costs_standard_cycle <- numeric(num_cycles)
costs_longterm_cycle <- numeric(num_cycles)

for (cycle in 1:num_cycles) {
  if (cycle <= 4) {
    cost_stable_standard <- costs_standard$stable_1_3
    cost_stable_longterm <- costs_longterm$stable_1_3
  } else if (cycle <= 7) {
    cost_stable_standard <- costs_standard$stable_4_6
    cost_stable_longterm <- costs_longterm$stable_4_6
  } else {
    cost_stable_standard <- costs_standard$stable_subsequent
    cost_stable_longterm <- costs_longterm$stable_subsequent
  }
  
  costs_standard_cycle[cycle] <- (trace_standard[cycle, 1] * cost_stable_standard) +
    (trace_standard[cycle, 2] * costs_standard$progressive) +
    (trace_standard[cycle, 3] * costs_standard$death)
  
  costs_longterm_cycle[cycle] <- (trace_longterm[cycle, 1] * cost_stable_longterm) +
    (trace_longterm[cycle, 2] * costs_longterm$progressive) +
    (trace_longterm[cycle, 3] * costs_longterm$death)
}

# Calculate total costs
total_costs_standard <- sum(costs_standard_cycle)
total_costs_longterm <- sum(costs_longterm_cycle)

# Store costs in a data frame
costs_df <- data.frame(
  Cycle = 1:num_cycles,
  Costs_Standard = costs_standard_cycle,
  Costs_LongTerm = costs_longterm_cycle
)

# Display the costs data
glibrary(ggplot2)
costs_melted <- melt(costs_df, id.vars = "Cycle")

# Plot the costs per cycle
plot_costs <- ggplot(costs_melted, aes(x = Cycle, y = value, color = variable)) +
  geom_line() +
  labs(title = "Costs per Cycle",
       x = "Cycle (Months)", y = "Costs",
       color = "Treatment") +
  theme_minimal()

print(plot_costs)

# Output total costs
print(paste("Total Costs for Standard TMZ Therapy:", total_costs_standard))
print(paste("Total Costs for Long-term TMZ Therapy:", total_costs_longterm))

# Compute ICERs
incremental_costs <- total_costs_longterm - total_costs_standard
incremental_qalms <- total_qalms_longterm - total_qalms_standard
incremental_qalys <- total_qalys_longterm - total_qalys_standard

icer_qalms <- round(incremental_costs / incremental_qalms, 0)
icer_qalys <- round(incremental_costs / incremental_qalys, 0)

# Output ICER values
print(paste("ICER (cost per QALM):", icer_qalms))
print(paste("ICER (cost per QALY):", icer_qalys))
