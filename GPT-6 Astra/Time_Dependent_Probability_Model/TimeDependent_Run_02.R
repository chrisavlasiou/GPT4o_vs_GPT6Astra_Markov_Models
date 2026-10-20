# Health utility inputs
utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  progression_decrement_per_consecutive_month = 0.02
)

# Monthly cost inputs
costs <- list(
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
)

# Model design
cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- time_horizon_months / cycle_length_months

states <- c("stable", "progressive", "death")


# Standalone script: inputs and requested Standard TMZ calculations.
# No inputs or results are printed. No packages are required.

utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  progression_decrement_per_consecutive_month = 0.02
)

costs <- list(
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
)

cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- as.integer(time_horizon_months / cycle_length_months)
states <- c("stable", "progressive", "death")

# Survival observations are at the end of months 1 through 60.
# Decimal commas in the supplied data are converted to decimal points.
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

stopifnot(
  length(OS_standard) == n_cycles,
  length(PFS_standard) == n_cycles,
  all(is.finite(OS_standard)),
  all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= 1),
  all(OS_standard > PFS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0)
)

t <- seq_len(n_cycles)

# Interpret the requested formula as 1 - exp(log(y) / t),
# i.e. the entire log(y) / t term is in the exponent.
requested_probability <- function(y, t) {
  1 - exp(log(y) / t)
}

transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = requested_probability(OS_standard, t),
  stable_to_progressive = requested_probability(PFS_standard, t),
  progressive_to_death = requested_probability(OS_standard - PFS_standard, t)
)

stopifnot(
  all(is.finite(as.matrix(transition_probs_standard[, -1]))),
  all(as.matrix(transition_probs_standard[, -1]) >= 0),
  all(as.matrix(transition_probs_standard[, -1]) <= 1),
  all(transition_probs_standard$stable_to_death +
        transition_probs_standard$stable_to_progressive <= 1)
)

# Modelling note: these columns implement the requested transformations only.
# OS - PFS is progressive-state occupancy, not conditional survival following
# progression. These transformations alone do not establish a calibrated
# three-state Markov transition model for HTA use.

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

# Assumes no transition from progressive back to stable
progressive_to_progressive <- 1 -
  transition_probs_standard$progressive_to_death

# Append to the existing data frame
transition_probs_standard$stable_to_stable <- stable_to_stable
transition_probs_standard$progressive_to_progressive <- progressive_to_progressive

# Standalone Long-term TMZ transition calculations.
# No packages required; inputs and results are stored without printing.

cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- as.integer(time_horizon_months / cycle_length_months)

# End-of-month observations, months 1 through 60.
# Supplied decimal commas are represented as decimal points in R.
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

stopifnot(
  length(OS_long_term) == n_cycles,
  length(PFS_long_term) == n_cycles,
  all(is.finite(OS_long_term)),
  all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= 1),
  all(OS_long_term > PFS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0)
)

t <- seq_len(n_cycles)

# Interpret the requested formula as 1 - exp(log(y) / t),
# with the whole log(y) / t term inside the exponent.
requested_probability <- function(y, t) {
  1 - exp(log(y) / t)
}

transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = requested_probability(OS_long_term, t),
  stable_to_progressive = requested_probability(PFS_long_term, t),
  progressive_to_death = requested_probability(OS_long_term - PFS_long_term, t)
)

stopifnot(
  all(is.finite(as.matrix(transition_probs_long_term[, -1]))),
  all(as.matrix(transition_probs_long_term[, -1]) >= 0),
  all(as.matrix(transition_probs_long_term[, -1]) <= 1),
  all(transition_probs_long_term$stable_to_death +
        transition_probs_long_term$stable_to_progressive <= 1)
)

# Modelling note: these columns implement the requested transformations only.
# OS - PFS is progressive-state occupancy, not conditional survival following
# progression. These transformations alone do not establish a calibrated
# three-state Markov transition model for HTA use.

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

# Assumes no transition from progressive back to stable
progressive_to_progressive <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing data frame
transition_probs_long_term$stable_to_stable <- stable_to_stable
transition_probs_long_term$progressive_to_progressive <- progressive_to_progressive

# Calculate exactly 60 trace rows using existing transition probability tables.
# Run after creating transition_probs_standard and transition_probs_long_term.
# This script does not recalculate or modify either transition table.
# No packages required; results are stored without printing.

if (!exists("transition_probs_standard") ||
    !exists("transition_probs_long_term")) {
  stop(paste(
    "Load transition_probs_standard and transition_probs_long_term first.",
    "You can source tmz_standard_transitions.R and tmz_long_term_transitions.R."
  ))
}

n_trace_cycles <- 60L
trace_states <- c("stable", "progressive", "death")

# Explicit assumption: the entire cohort starts stable in cycle 1.
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

# Cycle 1 is initial only; cycles 2 and 3 both use probability cycle 1.
# Thereafter trace cycle t uses probability cycle t - 2.
transition_cycle_used <- c(NA_integer_, pmax(1L, (2L:n_trace_cycles) - 2L))

calculate_markov_trace <- function(transition_probs, initial_distribution) {
  required <- c("cycle", "stable_to_death", "stable_to_progressive",
                "progressive_to_death")
  stopifnot(
    all(required %in% names(transition_probs)),
    identical(as.integer(transition_probs$cycle), seq_len(n_trace_cycles)),
    identical(names(initial_distribution), trace_states),
    all(is.finite(initial_distribution)),
    all(initial_distribution >= 0),
    abs(sum(initial_distribution) - 1) < 1e-10
  )
  
  # Use the previously appended stay probabilities when present.
  # Earlier saved scripts contain only the three outgoing probabilities;
  # for those tables, derive the complementary stay probabilities locally.
  stable_stay <- transition_probs$stable_to_stable
  if (is.null(stable_stay)) {
    stable_stay <- 1 - transition_probs$stable_to_death -
      transition_probs$stable_to_progressive
  }
  progressive_stay <- transition_probs$progressive_to_progressive
  if (is.null(progressive_stay)) {
    progressive_stay <- 1 - transition_probs$progressive_to_death
  }
  
  occupancy <- matrix(
    0, nrow = n_trace_cycles, ncol = length(trace_states),
    dimnames = list(NULL, trace_states)
  )
  occupancy[1L, ] <- initial_distribution
  
  for (trace_cycle in 2L:n_trace_cycles) {
    probability_cycle <- transition_cycle_used[trace_cycle]
    i <- match(probability_cycle, transition_probs$cycle)
    
    # Rows = origin state; columns = destination state.
    # Progression is irreversible and death is absorbing.
    transition_matrix <- matrix(c(
      stable_stay[i],
      transition_probs$stable_to_progressive[i],
      transition_probs$stable_to_death[i],
      0,
      progressive_stay[i],
      transition_probs$progressive_to_death[i],
      0, 0, 1
    ), nrow = 3L, byrow = TRUE,
    dimnames = list(trace_states, trace_states))
    
    stopifnot(
      all(is.finite(transition_matrix)),
      all(transition_matrix >= 0 & transition_matrix <= 1),
      all(abs(rowSums(transition_matrix) - 1) < 1e-10)
    )
    occupancy[trace_cycle, ] <-
      occupancy[trace_cycle - 1L, ] %*% transition_matrix
  }
  
  stopifnot(
    nrow(occupancy) == 60L,
    all(is.finite(occupancy)),
    all(occupancy >= -1e-10 & occupancy <= 1 + 1e-10),
    all(abs(rowSums(occupancy) - 1) < 1e-10),
    all(diff(occupancy[, "death"]) >= -1e-10)
  )
  
  data.frame(
    cycle = seq_len(n_trace_cycles),
    transition_cycle_used = transition_cycle_used,
    occupancy,
    row.names = NULL
  )
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_distribution
)

markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_distribution
)

# Trace rows 1:60 contain the initial distribution and 59 transitions.
# Probability cycles 1:58 are used; cycle 1 is used twice.
# Probability cycles 59 and 60 are unused under the requested indexing.
# These traces retain the previously specified probability transformations;
# their calculation does not validate those transformations for HTA use.

# QALMs from existing Markov traces and previously stored utility inputs.
# Run after the input and Markov trace scripts. No input data are redefined.
# The stored decrement is interpreted as 0.02 utility units per month,
# not an absolute loss of 0.02 QALYs (which would equal 0.24 QALMs).
# The requested schedule is by MODEL cycle, not time since individual entry
# into progression: all progressive patients receive the same cycle utility.

required_objects <- c("utilities", "cycle_length_months",
                      "markov_trace_standard", "markov_trace_long_term")
missing_objects <- required_objects[!vapply(
  required_objects, exists, logical(1), inherits = TRUE
)]
if (length(missing_objects) > 0L) {
  stop(paste("Load existing objects first:",
             paste(missing_objects, collapse = ", ")))
}

required_utilities <- c("stable", "progressive",
                        "progression_decrement_per_consecutive_month")
stopifnot(
  all(required_utilities %in% names(utilities)),
  all(is.finite(unlist(utilities[required_utilities]))),
  length(cycle_length_months) == 1L,
  cycle_length_months == 1
)

calculate_qalms <- function(trace) {
  stopifnot(
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), seq_len(60L))
  )
  proportions <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(proportions)),
    all(proportions >= -1e-10 & proportions <= 1 + 1e-10),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  # 0 decrements in cycles 1-2; 1 in cycle 3; 23 in cycles 25-60.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- utilities$progressive -
    decrement_count * utilities$progression_decrement_per_consecutive_month
  
  # Death utility is assumed to be zero.
  # Each of the 60 trace rows contributes one monthly reward, including
  # the initial-state row at cycle 1. No discounting or half-cycle correction.
  stable_qalms <- trace$stable * utilities$stable * cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    cycle_length_months
  death_qalms <- trace$death * 0 * cycle_length_months
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    decrement_count = decrement_count,
    stable_utility = utilities$stable,
    progressive_utility = progressive_utility,
    death_utility = 0,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    cycle_qalms = cycle_qalms,
    cumulative_qalms = cumsum(cycle_qalms)
  )
}

qalms_standard <- calculate_qalms(markov_trace_standard)
qalms_long_term <- calculate_qalms(markov_trace_long_term)

total_qalms_standard <- sum(qalms_standard$cycle_qalms)
total_qalms_long_term <- sum(qalms_long_term$cycle_qalms)

qalms_by_cycle <- data.frame(
  cycle = qalms_standard$cycle,
  progressive_utility = qalms_standard$progressive_utility,
  qalms_standard = qalms_standard$cycle_qalms,
  qalms_long_term = qalms_long_term$cycle_qalms,
  cumulative_qalms_standard = qalms_standard$cumulative_qalms,
  cumulative_qalms_long_term = qalms_long_term$cumulative_qalms
)

total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display every cycle and the treatment totals; full precision is retained
# in the stored objects.
print(qalms_by_cycle, row.names = FALSE, digits = 8, max = 1000)
print(total_qalms, row.names = FALSE, digits = 8)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Per-cycle and total costs using existing traces and cost inputs.
# Run after the input and Markov trace scripts. No input data are redefined.
# Costs are undiscounted, without half-cycle correction, per initial patient.

required_objects <- c("costs", "markov_trace_standard", "markov_trace_long_term")
missing_objects <- required_objects[!vapply(
  required_objects, exists, logical(1), inherits = TRUE
)]
if (length(missing_objects) > 0L) {
  stop(paste("Load existing objects first:",
             paste(missing_objects, collapse = ", ")))
}

calculate_cycle_costs <- function(trace, treatment_costs) {
  required_costs <- c("stable_months_1_3", "stable_months_4_6",
                      "stable_subsequent_months", "progressive", "death")
  stopifnot(
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), seq_len(60L)),
    all(required_costs %in% names(treatment_costs)),
    all(lengths(treatment_costs[required_costs]) == 1L),
    all(is.finite(unlist(treatment_costs[required_costs]))),
    all(unlist(treatment_costs[required_costs]) >= 0)
  )
  proportions <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(proportions)),
    all(proportions >= -1e-10 & proportions <= 1 + 1e-10),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  # Cycle 1 is the initial distribution but receives a full cost reward.
  # Cycle 2 = month 1, cycle 3 = month 2, ..., cycle 60 = month 59.
  # Stable cost bands: cycles 1-4, cycles 5-7, cycles 8-60.
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L,
    treatment_costs$stable_months_1_3,
    ifelse(trace$cycle <= 7L,
           treatment_costs$stable_months_4_6,
           treatment_costs$stable_subsequent_months)
  )
  
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * treatment_costs$progressive
  death_cost <- trace$death * treatment_costs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = treatment_costs$progressive,
    death_unit_cost = treatment_costs$death,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    cycle_cost = cycle_cost,
    cumulative_cost = cumsum(cycle_cost)
  )
}

costs_standard <- calculate_cycle_costs(
  markov_trace_standard, costs$standard_tmz
)
costs_long_term <- calculate_cycle_costs(
  markov_trace_long_term, costs$long_term_tmz
)

total_cost_standard <- sum(costs_standard$cycle_cost)
total_cost_long_term <- sum(costs_long_term$cycle_cost)

costs_by_cycle <- data.frame(
  cycle = costs_standard$cycle,
  month = costs_standard$month,
  stable_unit_cost_standard = costs_standard$stable_unit_cost,
  stable_unit_cost_long_term = costs_long_term$stable_unit_cost,
  cycle_cost_standard = costs_standard$cycle_cost,
  cycle_cost_long_term = costs_long_term$cycle_cost,
  cumulative_cost_standard = costs_standard$cumulative_cost,
  cumulative_cost_long_term = costs_long_term$cumulative_cost
)

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Display all 60 cycles and totals. Rounding affects display only.
costs_by_cycle_display <- costs_by_cycle
costs_by_cycle_display[, -(1:2)] <- round(costs_by_cycle_display[, -(1:2)], 2)
total_costs_display <- total_costs
total_costs_display$total_cost <- round(total_costs_display$total_cost, 2)
print(costs_by_cycle_display, row.names = FALSE, digits = 12, max = 1000)
print(total_costs_display, row.names = FALSE, digits = 12)

# ICERs: Long-term TMZ versus Standard TMZ.
# Run after tmz_qalms.R and tmz_costs.R. Existing totals are reused.

required_objects <- c(
  "total_cost_standard", "total_cost_long_term",
  "total_qalms_standard", "total_qalms_long_term"
)
missing_objects <- required_objects[!vapply(
  required_objects, exists, logical(1), inherits = TRUE
)]
if (length(missing_objects) > 0L) {
  stop(paste("Load existing totals first:",
             paste(missing_objects, collapse = ", ")))
}
for (object_name in required_objects) {
  value <- get(object_name)
  stopifnot(is.numeric(value), length(value) == 1L, is.finite(value))
}

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICERs are undefined because incremental health benefit is zero.")
}

# Retain full precision for calculations; round only the displayed values.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

icer_results <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  outcome = c("Cost per QALM gained", "Cost per QALY gained"),
  icer = c(icer_per_qalm, icer_per_qaly)
)

icer_display <- icer_results
icer_display$icer <- formatC(
  icer_results$icer, format = "f", digits = 0, big.mark = ","
)
print(icer_display, row.names = FALSE, right = FALSE)