# Step 1: Store model inputs without printing
inputs <- list(
  states = c("stable", "progressive", "death"),
  
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_progression = 0.02
  ),
  
  costs = list(
    standard_tmz = list(
      stable_months_1_3 = 1944,
      stable_months_4_6 = 2453,
      stable_subsequent_months = 350,
      progressive = 2720,
      death = 0
    ),
    long_term_tmz = list(
      stable_months_1_3 = 1944,
      stable_months_4_6 = 2453,
      stable_subsequent_months = 2453,
      progressive = 2720,
      death = 0
    )
  ),
  
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

inputs$design$n_cycles <- with(
  inputs$design,
  time_horizon_months / cycle_length_months
)


# Standalone script: Standard TMZ probabilities for months 1 through 60.
# No objects are printed. No existing script is sourced or modified.
# Interpret the requested formula as 1 - exp(log(y) / t).
# These are the requested transformations, not validated conditional Markov
# transition estimates. OS - PFS is state occupancy, not survival conditional
# on progression; OS and PFS alone do not identify all three transitions.

OS_standard <- c(
  0.988, 0.970, 0.950, 0.913, 0.888, 0.863, 0.813, 0.782, 0.733, 0.675,
  0.650, 0.611, 0.550, 0.525, 0.488, 0.450, 0.430, 0.394, 0.350, 0.325,
  0.313, 0.300, 0.275, 0.265, 0.240, 0.225, 0.213, 0.213, 0.213, 0.213,
  0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188,
  0.094, 0.087, 0.081, 0.075, 0.070, 0.065, 0.060, 0.056, 0.052, 0.048,
  0.045, 0.041, 0.038, 0.036, 0.033, 0.030, 0.028, 0.026, 0.024, 0.022
)

PFS_standard <- c(
  0.978, 0.968, 0.800, 0.738, 0.643, 0.539, 0.493, 0.459, 0.417, 0.368,
  0.314, 0.269, 0.259, 0.228, 0.227, 0.211, 0.197, 0.184, 0.174, 0.157,
  0.146, 0.136, 0.126, 0.107, 0.097, 0.097, 0.097, 0.093, 0.093, 0.093,
  0.071, 0.057, 0.033, 0.033, 0.033, 0.033, 0.024, 0.022, 0.020, 0.018,
  0.016, 0.014, 0.013, 0.011, 0.010, 0.009, 0.008, 0.007, 0.007, 0.006,
  0.005, 0.005, 0.004, 0.004, 0.003, 0.003, 0.003, 0.003, 0.002, 0.002
)

cycle_length_months <- 1
time_horizon_months <- 60
n_cycles <- time_horizon_months / cycle_length_months
t <- seq_len(n_cycles)

stopifnot(
  length(OS_standard) == n_cycles,
  length(PFS_standard) == n_cycles,
  all(is.finite(OS_standard)),
  all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0),
  all(OS_standard - PFS_standard > 0)
)

transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Validate probability bounds without displaying inputs or results.
stopifnot(
  all(as.matrix(transition_probs_standard[-1]) >= 0),
  all(as.matrix(transition_probs_standard[-1]) <= 1),
  all(transition_probs_standard$stable_to_death +
        transition_probs_standard$stable_to_progressive <= 1)
)

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

progressive_to_progressive <- 1 -
  transition_probs_standard$progressive_to_death

# Append to the existing object
transition_probs_standard$stable_to_stable <- stable_to_stable
transition_probs_standard$progressive_to_progressive <- progressive_to_progressive

# Standalone script for Long-term TMZ: 60 monthly cycles.
# No inputs or results are printed; no other script is sourced or modified.
# Requested formula interpreted as 1 - exp(log(y) / t).
# These transformations are not validated conditional Markov transitions.
# In particular, OS - PFS is progressive-state occupancy, not survival
# conditional on progression.

OS_long_term <- c(
  1.000, 1.000, 0.982, 0.930, 0.895, 0.860, 0.825, 0.789, 0.737, 0.702,
  0.667, 0.649, 0.614, 0.579, 0.544, 0.526, 0.491, 0.456, 0.421, 0.404,
  0.368, 0.351, 0.351, 0.351, 0.351, 0.333, 0.333, 0.316, 0.316, 0.296,
  0.296, 0.296, 0.296, 0.296, 0.296, 0.269, 0.241, 0.215, 0.215, 0.215,
  0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215,
  0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215
)

PFS_long_term <- c(
  0.947, 0.912, 0.877, 0.754, 0.699, 0.607, 0.497, 0.478, 0.421, 0.402,
  0.364, 0.325, 0.306, 0.306, 0.306, 0.287, 0.268, 0.268, 0.268, 0.268,
  0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.244,
  0.244, 0.244, 0.244, 0.244, 0.244, 0.244, 0.203, 0.203, 0.203, 0.203,
  0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.152, 0.152, 0.152,
  0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152
)

cycle_length_months <- 1
time_horizon_months <- 60
n_cycles <- time_horizon_months / cycle_length_months
t <- seq_len(n_cycles)

stopifnot(
  length(OS_long_term) == n_cycles,
  length(PFS_long_term) == n_cycles,
  all(is.finite(OS_long_term)),
  all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0),
  all(OS_long_term - PFS_long_term > 0)
)

transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

stopifnot(
  all(as.matrix(transition_probs_long_term[-1]) >= 0),
  all(as.matrix(transition_probs_long_term[-1]) <= 1),
  all(transition_probs_long_term$stable_to_death +
        transition_probs_long_term$stable_to_progressive <= 1)
)

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

progressive_to_progressive <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing object
transition_probs_long_term$stable_to_stable <- stable_to_stable
transition_probs_long_term$progressive_to_progressive <- progressive_to_progressive


# Run after creating transition_probs_standard and transition_probs_long_term
# and appending stable_to_stable and progressive_to_progressive to each.
# This standalone script reuses those in-memory objects without modifying them.
# No data or results are printed. No previous scripts are modified or sourced.
# The requested probability transformations and timing are retained; this does
# not validate their suitability as conditional transitions for HTA modelling.

n_cycles <- 60L
# Assumption: everyone starts stable. Change this vector if required.
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

# Cycle 1 is initial occupancy and has no transition.
# Cycles 2, 3, 4, 5, ..., 60 use probability rows 1, 1, 2, 3, ..., 58.
transition_cycle_used <- c(NA_integer_, pmax(1L, (2L:n_cycles) - 2L))

build_markov_trace <- function(transition_probs, initial, probability_cycles) {
  states <- c("stable", "progressive", "death")
  required <- c("stable_to_stable", "stable_to_progressive", "stable_to_death",
                "progressive_to_progressive", "progressive_to_death")
  tolerance <- 1e-10
  
  if (!all(required %in% names(transition_probs))) {
    stop("Transition object must include all five probabilities, including stay probabilities.")
  }
  lengths <- vapply(transition_probs[required], length, integer(1))
  if (length(unique(lengths)) != 1L || lengths[1] < max(probability_cycles, na.rm = TRUE)) {
    stop("Transition probability columns must have equal lengths and cover all required cycles.")
  }
  if ("cycle" %in% names(transition_probs) &&
      !identical(as.integer(transition_probs$cycle), seq_len(lengths[1]))) {
    stop("Transition probabilities must be ordered consecutively from cycle 1.")
  }
  probabilities <- as.matrix(as.data.frame(transition_probs[required]))
  stopifnot(
    is.numeric(probabilities), all(is.finite(probabilities)),
    all(probabilities >= 0 & probabilities <= 1),
    all(abs(transition_probs$stable_to_stable +
              transition_probs$stable_to_progressive +
              transition_probs$stable_to_death - 1) < tolerance),
    all(abs(transition_probs$progressive_to_progressive +
              transition_probs$progressive_to_death - 1) < tolerance),
    identical(names(initial), states),
    all(is.finite(initial)), all(initial >= 0), abs(sum(initial) - 1) < tolerance
  )
  
  trace <- matrix(0, nrow = length(probability_cycles), ncol = 3L,
                  dimnames = list(NULL, states))
  trace[1L, ] <- initial
  
  for (cycle in 2L:nrow(trace)) {
    k <- probability_cycles[cycle]
    # Rows are origin states; columns are destination states.
    # Progression is irreversible and death is absorbing.
    transition_matrix <- matrix(c(
      transition_probs$stable_to_stable[k],
      transition_probs$stable_to_progressive[k],
      transition_probs$stable_to_death[k],
      0, transition_probs$progressive_to_progressive[k],
      transition_probs$progressive_to_death[k],
      0, 0, 1
    ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
    
    trace[cycle, ] <- as.numeric(trace[cycle - 1L, ] %*% transition_matrix)
  }
  
  stopifnot(
    all(is.finite(trace)),
    all(trace >= -tolerance & trace <= 1 + tolerance),
    all(abs(rowSums(trace) - 1) < tolerance),
    all(diff(trace[, "death"]) >= -tolerance)
  )
  data.frame(cycle = seq_len(nrow(trace)), trace, row.names = NULL)
}

if (!exists("transition_probs_standard") || !exists("transition_probs_long_term")) {
  stop("Create both transition probability objects before running this script.")
}

markov_trace_standard <- build_markov_trace(
  transition_probs_standard, initial_distribution, transition_cycle_used
)
markov_trace_long_term <- build_markov_trace(
  transition_probs_long_term, initial_distribution, transition_cycle_used
)

# Exactly 60 rows: cycle 1 is initial occupancy followed by 59 transitions.
stopifnot(
  nrow(markov_trace_standard) == 60L,
  nrow(markov_trace_long_term) == 60L,
  identical(markov_trace_standard$cycle, 1L:60L),
  identical(markov_trace_long_term$cycle, 1L:60L),
  identical(transition_cycle_used[2L:5L], c(1L, 1L, 2L, 3L)),
  transition_cycle_used[60L] == 58L
)


# Run after creating inputs, markov_trace_standard, and markov_trace_long_term.
# Reuses existing data without redefining or modifying those objects.
# Each trace row contributes one full monthly cycle, including cycle 1.
# No discounting or half-cycle correction is applied.
# The requested decrement is interpreted as a utility decrement of 0.02,
# consistent with the original utility inputs, not 0.02 accrued QALYs/month.
# This is the explicitly requested calendar-cycle schedule for everyone in
# progression; it does not track each patient's time since progression.
# Death contributes zero QALMs.

if (!exists("inputs") || !exists("markov_trace_standard") ||
    !exists("markov_trace_long_term")) {
  stop("Create inputs and both Markov traces before running this script.")
}

calculate_qalms <- function(trace, model_inputs) {
  utilities <- model_inputs$utilities
  cycle_length <- model_inputs$design$cycle_length_months
  stopifnot(
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), 1L:60L),
    length(cycle_length) == 1L, cycle_length == 1,
    length(utilities$stable) == 1L,
    length(utilities$progressive) == 1L,
    length(utilities$decrement_per_consecutive_month_progression) == 1L,
    all(is.finite(unlist(utilities))),
    utilities$decrement_per_consecutive_month_progression >= 0
  )
  proportions <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(proportions)),
    all(proportions >= -1e-10 & proportions <= 1 + 1e-10),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  # Cycles 1-2: 0 decrements. Cycles 3-25: 1-23 decrements.
  # Cycles 26-60: retain exactly 23 decrements.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- utilities$progressive -
    utilities$decrement_per_consecutive_month_progression * decrement_count
  
  results <- data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    progression_decrements = decrement_count,
    stable_utility = utilities$stable,
    progressive_utility = progressive_utility,
    qalm_stable = trace$stable * utilities$stable * cycle_length,
    qalm_progressive = trace$progressive * progressive_utility * cycle_length,
    qalm_death = 0
  )
  results$qalm_total <- results$qalm_stable + results$qalm_progressive +
    results$qalm_death
  results$cumulative_qalms <- cumsum(results$qalm_total)
  
  stopifnot(
    all(decrement_count[1L:2L] == 0L),
    all(decrement_count[3L:25L] == 1L:23L),
    all(decrement_count[26L:60L] == 23L),
    all(progressive_utility[26L:60L] == progressive_utility[25L]),
    all(is.finite(results$qalm_total))
  )
  results
}

qalms_standard <- calculate_qalms(markov_trace_standard, inputs)
qalms_long_term <- calculate_qalms(markov_trace_long_term, inputs)

total_qalms_standard <- sum(qalms_standard$qalm_total)
total_qalms_long_term <- sum(qalms_long_term$qalm_total)

total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display all cycle-level results and both totals; stored values are unrounded.
print(qalms_standard, row.names = FALSE, max = nrow(qalms_standard) * ncol(qalms_standard))
print(qalms_long_term, row.names = FALSE, max = nrow(qalms_long_term) * ncol(qalms_long_term))
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12


# Run after creating inputs, markov_trace_standard, and markov_trace_long_term.
# Reuses existing data without modifying it. Costs are per initial patient.
# Cycle 1 is initial occupancy and is charged a full monthly cost as requested.
# Cycles 2-60 correspond to months 1-59.
# Stable cost bands: cycles 1-4, cycles 5-7, and cycles 8-60.
# Progressive and death costs are applied to occupancy in every cycle.
# No discounting or half-cycle correction is applied.

if (!exists("inputs") || !exists("markov_trace_standard") ||
    !exists("markov_trace_long_term")) {
  stop("Create inputs and both Markov traces before running this script.")
}

calculate_cycle_costs <- function(trace, unit_costs) {
  required_costs <- c("stable_months_1_3", "stable_months_4_6",
                      "stable_subsequent_months", "progressive", "death")
  stopifnot(
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), 1L:60L),
    all(required_costs %in% names(unit_costs)),
    all(vapply(unit_costs[required_costs], length, integer(1)) == 1L)
  )
  values <- unlist(unit_costs[required_costs], use.names = FALSE)
  proportions <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(
    is.numeric(values), all(is.finite(values)), all(values >= 0),
    all(is.finite(proportions)),
    all(proportions >= -1e-10 & proportions <= 1 + 1e-10),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, unit_costs$stable_months_1_3,
    ifelse(trace$cycle <= 7L, unit_costs$stable_months_4_6,
           unit_costs$stable_subsequent_months)
  )
  
  results <- data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = unit_costs$progressive,
    death_unit_cost = unit_costs$death,
    cost_stable = trace$stable * stable_unit_cost,
    cost_progressive = trace$progressive * unit_costs$progressive,
    cost_death = trace$death * unit_costs$death
  )
  results$cost_total <- results$cost_stable + results$cost_progressive +
    results$cost_death
  results$cumulative_cost <- cumsum(results$cost_total)
  
  stopifnot(
    all(results$stable_unit_cost[1L:4L] == unit_costs$stable_months_1_3),
    all(results$stable_unit_cost[5L:7L] == unit_costs$stable_months_4_6),
    all(results$stable_unit_cost[8L:60L] == unit_costs$stable_subsequent_months),
    all(is.finite(results$cost_total))
  )
  results
}

costs_standard <- calculate_cycle_costs(
  markov_trace_standard, inputs$costs$standard_tmz
)
costs_long_term <- calculate_cycle_costs(
  markov_trace_long_term, inputs$costs$long_term_tmz
)

total_cost_standard <- sum(costs_standard$cost_total)
total_cost_long_term <- sum(costs_long_term$cost_total)

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Display every cycle and totals. Stored results retain full precision.
print(costs_standard, row.names = FALSE, max = nrow(costs_standard) * ncol(costs_standard))
print(costs_long_term, row.names = FALSE, max = nrow(costs_long_term) * ncol(costs_long_term))
print(total_costs, row.names = FALSE)


# Incremental comparison: Long-term TMZ minus Standard TMZ.
# Run after the cost and QALM scripts. Existing totals are reused.
# ICERs use full precision; only their displayed values are rounded.

required_objects <- c("total_cost_standard", "total_cost_long_term",
                      "total_qalms_standard", "total_qalms_long_term")
if (!all(vapply(required_objects, exists, logical(1)))) {
  stop("Calculate total costs and total QALMs for both treatments first.")
}
stopifnot(all(vapply(mget(required_objects), function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}, logical(1))))

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICERs are undefined because incremental effectiveness is zero.")
}

icer_qalm <- incremental_cost / incremental_qalms
icer_qaly <- incremental_cost / incremental_qalys

icer_results <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_qalm, icer_qaly)
)

# Print whole numbers without changing the stored ICER precision.
cat("Long-term TMZ versus Standard TMZ\n")
cat("ICER per QALM:", sprintf("%.0f", icer_qalm), "\n")
cat("ICER per QALY:", sprintf("%.0f", icer_qaly), "\n")