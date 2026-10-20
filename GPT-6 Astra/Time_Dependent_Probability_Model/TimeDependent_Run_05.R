# Model inputs (assignments do not print output)
inputs <- list(
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
    states = c("stable", "progressive", "death"),
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

inputs$design$n_cycles <- with(
  inputs$design,
  time_horizon_months / cycle_length_months
)

# Standalone script: Standard TMZ, 60 monthly cycles.
# Implements the requested formula as p = 1 - exp(log(y) / t).
# No input data or calculated probabilities are printed.
# These are formula-based quantities, not validated conditional Markov
# transitions: OS - PFS is progressive-state occupancy, not survival among
# progressed patients. The transformation gives an average probability over
# months 1:t, rather than the conditional probability in month t.

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
t <- seq_len(time_horizon_months / cycle_length_months)

stopifnot(
  length(OS_standard) == length(t),
  length(PFS_standard) == length(t),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(OS_standard - PFS_standard > 0)
)

transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_standard$stable_to_progressive -
  transition_probs_standard$stable_to_death

progressive_to_progressive <- 1 -
  transition_probs_standard$progressive_to_death

# Append to the existing object without printing
transition_probs_standard$stable_to_stable <- stable_to_stable
transition_probs_standard$progressive_to_progressive <- progressive_to_progressive

# Standalone calculation for 60 monthly cycles of long-term TMZ therapy.
# Requested formula interpreted as 1 - exp(log(y) / t).
# Assignments do not print the input data or results.
# Modelling limitation: these formula-based quantities are not validated
# conditional monthly Markov transitions. OS - PFS is progressive-state
# occupancy, not conditional survival among progressed patients.

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
t <- seq_len(time_horizon_months / cycle_length_months)

stopifnot(
  length(OS_long_term) == length(t),
  length(PFS_long_term) == length(t),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(OS_long_term - PFS_long_term > 0)
)

transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# Calculate separately (assuming no return from progressive to stable)
stable_to_stable <- 1 -
  transition_probs_long_term$stable_to_progressive -
  transition_probs_long_term$stable_to_death

progressive_to_progressive <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing object without printing
transition_probs_long_term$stable_to_stable <- stable_to_stable
transition_probs_long_term$progressive_to_progressive <- progressive_to_progressive

# Run after creating transition_probs_standard and transition_probs_long_term
# and appending stable_to_stable and progressive_to_progressive to each.
# Reuses those objects without changing or recalculating their probabilities.
# Assumption: all patients begin stable; death is absorbing and there is no
# return from progressive to stable. Prior probability-method limitations apply.
# Cycle 1 is the initial distribution. Exactly 60 trace rows are produced.

initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- 60L

build_markov_trace <- function(transition_probs, initial_distribution,
                               n_cycles = 60L) {
  states <- c("stable", "progressive", "death")
  required_columns <- c(
    "cycle", "stable_to_stable", "stable_to_progressive", "stable_to_death",
    "progressive_to_progressive", "progressive_to_death"
  )
  if (!all(required_columns %in% names(transition_probs))) {
    stop("Transition object must include cycle and all five transition columns.")
  }
  stopifnot(
    n_cycles == 60L,
    identical(names(initial_distribution), states),
    all(is.finite(initial_distribution)),
    all(initial_distribution >= 0),
    abs(sum(initial_distribution) - 1) < 1e-10
  )
  
  # NA denotes no transition for cycle 1. Cycles 2:60 use rows 1,1,2,...,58.
  probability_cycle <- c(NA_integer_, pmax(1L, seq.int(2L, n_cycles) - 2L))
  if (anyDuplicated(transition_probs$cycle) ||
      !all(probability_cycle[-1L] %in% transition_probs$cycle)) {
    stop("Transition cycle identifiers must be unique and include cycles 1:58.")
  }
  
  trace <- matrix(0, nrow = n_cycles, ncol = 3L,
                  dimnames = list(NULL, states))
  trace[1L, ] <- initial_distribution
  
  for (cycle in seq.int(2L, n_cycles)) {
    index <- match(probability_cycle[cycle], transition_probs$cycle)
    transition_matrix <- matrix(c(
      transition_probs$stable_to_stable[index],
      transition_probs$stable_to_progressive[index],
      transition_probs$stable_to_death[index],
      0,
      transition_probs$progressive_to_progressive[index],
      transition_probs$progressive_to_death[index],
      0, 0, 1
    ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
    
    if (any(!is.finite(transition_matrix)) ||
        any(transition_matrix < 0 | transition_matrix > 1) ||
        any(abs(rowSums(transition_matrix) - 1) > 1e-10)) {
      stop("Invalid transition matrix at probability cycle ",
           probability_cycle[cycle])
    }
    trace[cycle, ] <- as.numeric(trace[cycle - 1L, ] %*% transition_matrix)
  }
  
  stopifnot(
    nrow(trace) == 60L,
    all(is.finite(trace)),
    all(trace >= 0 & trace <= 1 + 1e-10),
    all(abs(rowSums(trace) - 1) < 1e-10)
  )
  data.frame(cycle = seq_len(n_cycles),
             probability_cycle = probability_cycle, trace,
             row.names = NULL)
}

if (!exists("transition_probs_standard") ||
    !exists("transition_probs_long_term")) {
  stop("Create both transition probability objects in this R session first.")
}

markov_trace_standard <- build_markov_trace(
  transition_probs_standard, initial_distribution, n_cycles
)
markov_trace_long_term <- build_markov_trace(
  transition_probs_long_term, initial_distribution, n_cycles
)

# Results remain stored in the two trace objects; nothing is printed.
# Probability cycles 59 and 60 are unused under the requested indexing.


# Run after inputs, markov_trace_standard and markov_trace_long_term exist.
# Reuses existing data; no survival data or trace data are redefined.
# The requested 0.02 is interpreted as a utility decrement, not an absolute
# loss of 0.02 QALYs (which would equal 0.24 QALMs).
# Uses the specified calendar-cycle schedule, not individual time since
# progression. Individual-duration effects require progression cohort tracking.
# Death utility is assumed to be zero. No discounting or half-cycle correction.

if (!exists("inputs") || !exists("markov_trace_standard") ||
    !exists("markov_trace_long_term")) {
  stop("Create inputs and both Markov traces in this R session first.")
}

calculate_qalms <- function(trace, inputs) {
  required_columns <- c("cycle", "stable", "progressive", "death")
  stopifnot(
    all(required_columns %in% names(trace)),
    nrow(trace) == 60L,
    all(trace$cycle == seq_len(60L)),
    inputs$design$cycle_length_months == 1
  )
  proportions <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(proportions)),
    all(proportions >= 0),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  # Counts: 0, 0, 1, 2, ..., 23 (cycle 25), then 23 through cycle 60.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_progression * decrement_count
  
  stable_qalms <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  death_qalms <- trace$death * 0
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    decrement_count = decrement_count,
    stable_utility = inputs$utilities$stable,
    progressive_utility = progressive_utility,
    death_utility = 0,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    cycle_qalms = cycle_qalms,
    cumulative_qalms = cumsum(cycle_qalms)
  )
}

qalms_standard <- calculate_qalms(markov_trace_standard, inputs)
qalms_long_term <- calculate_qalms(markov_trace_long_term, inputs)

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

# Display all 60 cycle results and the totals; keep full precision in objects.
print(qalms_by_cycle, row.names = FALSE, max = nrow(qalms_by_cycle) * ncol(qalms_by_cycle))
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12


# Run after inputs and both 60-cycle Markov traces have been created.
# Reuses existing inputs and traces without redefining them.
# Cycle 1 is charged using the same unit costs as cycles 2-4.
# Costs are undiscounted, with no half-cycle correction, per starting patient.

if (!exists("inputs") || !exists("markov_trace_standard") ||
    !exists("markov_trace_long_term")) {
  stop("Create inputs and both Markov traces in this R session first.")
}

calculate_cycle_costs <- function(trace, unit_costs) {
  stopifnot(
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    nrow(trace) == 60L,
    all(trace$cycle == seq_len(60L))
  )
  proportions <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(proportions)),
    all(proportions >= 0),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, unit_costs$stable_months_1_3,
    ifelse(trace$cycle <= 7L, unit_costs$stable_months_4_6,
           unit_costs$stable_subsequent_months)
  )
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * unit_costs$progressive
  death_cost <- trace$death * unit_costs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = unit_costs$progressive,
    death_unit_cost = unit_costs$death,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    cycle_cost = cycle_cost,
    cumulative_cost = cumsum(cycle_cost)
  )
}

costs_standard <- calculate_cycle_costs(
  markov_trace_standard, inputs$costs$standard_tmz
)
costs_long_term <- calculate_cycle_costs(
  markov_trace_long_term, inputs$costs$long_term_tmz
)

total_cost_standard <- sum(costs_standard$cycle_cost)
total_cost_long_term <- sum(costs_long_term$cycle_cost)

costs_by_cycle <- data.frame(
  cycle = costs_standard$cycle,
  month = costs_standard$month,
  cost_standard = costs_standard$cycle_cost,
  cost_long_term = costs_long_term$cycle_cost,
  cumulative_cost_standard = costs_standard$cumulative_cost,
  cumulative_cost_long_term = costs_long_term$cumulative_cost
)

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Print every cycle and treatment totals; retain unrounded values in objects.
print(costs_by_cycle, row.names = FALSE,
      max = nrow(costs_by_cycle) * ncol(costs_by_cycle))
print(total_costs, row.names = FALSE)


# Incremental comparison: long-term TMZ minus standard TMZ.
# Run after the QALM and cost scripts. Reuses their unrounded totals.
# QALYs are calculated from QALMs so the earlier QALY snippet is optional.

required_objects <- c(
  "total_cost_standard", "total_cost_long_term",
  "total_qalms_standard", "total_qalms_long_term"
)
if (!all(vapply(required_objects, exists, logical(1)))) {
  stop("Run the QALM and cost calculations before this script.")
}
stopifnot(all(vapply(mget(required_objects), function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}, logical(1))))

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Preserve full numeric precision for subsequent calculations.
icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)

# Round only for display, with no decimal places or scientific notation.
icer_display <- data.frame(
  measure = c("Cost per additional QALM", "Cost per additional QALY"),
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
print(icer_display, row.names = FALSE, right = FALSE)