# Step 1: store supplied inputs; sourcing this file produces no output.
# Costs retain the supplied units; currency and price year are unspecified.
# The progression decrement is stored only; its timing is not yet applied.
inputs <- list(
  states = c("stable", "progressive", "death"),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
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

# Standard TMZ: standalone calculation from supplied monthly survival data.
# Run with source("standard_tmz_transition_probabilities.R").
# Objects are assigned silently; no existing script is sourced or modified.

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

t <- seq_len(60L)
stopifnot(
  length(OS_standard) == length(t),
  length(PFS_standard) == length(t),
  all(is.finite(OS_standard)), all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(diff(OS_standard) <= 0), all(diff(PFS_standard) <= 0),
  all(OS_standard - PFS_standard > 0)
)

# Requested formula: 1 - exp(log(y) / t), equivalently 1 - y^(1/t).
# Each row uses that month's cumulative value and cycle number.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# These are the requested transformations, not calibrated conditional
# month-to-month Markov transitions. In particular, OS - PFS represents
# occupancy of the progressed-and-alive state, not its survival curve.
# Transition estimation and model validation are separate subsequent steps.

stable_to_stable_standard <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

progressive_to_progressive_standard <- 1 -
  transition_probs_standard$progressive_to_death

transition_probs_standard$stable_to_stable <- stable_to_stable_standard
transition_probs_standard$progressive_to_progressive <-
  progressive_to_progressive_standard

# Long-term TMZ: standalone calculation from supplied monthly survival data.
# Sourcing this script assigns objects silently and does not modify other scripts.

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

t <- seq_len(60L)
stopifnot(
  length(OS_long_term) == length(t),
  length(PFS_long_term) == length(t),
  all(is.finite(OS_long_term)), all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(diff(OS_long_term) <= 0), all(diff(PFS_long_term) <= 0),
  all(OS_long_term - PFS_long_term > 0)
)

# Requested formula: 1 - exp(log(y) / t), equivalently 1 - y^(1/t).
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

stopifnot(
  all(is.finite(as.matrix(transition_probs_long_term[, -1L]))),
  all(transition_probs_long_term[, -1L] >= 0),
  all(transition_probs_long_term[, -1L] <= 1)
)

# These are the requested transformations, not calibrated conditional
# month-to-month Markov transitions. OS - PFS is occupancy of the
# progressed-and-alive state, not its survival curve.

# Calculate separately
stable_to_stable_long_term <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

progressive_to_progressive_long_term <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing object
transition_probs_long_term$stable_to_stable <- stable_to_stable_long_term
transition_probs_long_term$progressive_to_progressive <-
  progressive_to_progressive_long_term

# Markov traces for Standard and Long-term TMZ: exactly 60 rows, cycles 1:60.
# Run after the two transition-probability scripts, or keep all three scripts
# together and source this file. Existing probability objects are reused.
# No input or transition-probability scripts are modified.

# Locate companion scripts when sourcing from another working directory.
trace_script_directory <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(source_file)) getwd() else dirname(normalizePath(source_file))
})

if (!exists("transition_probs_standard", inherits = TRUE)) {
  source(file.path(trace_script_directory,
                   "standard_tmz_transition_probabilities.R"), local = TRUE)
}
if (!exists("transition_probs_long_term", inherits = TRUE)) {
  source(file.path(trace_script_directory,
                   "long_term_tmz_transition_probabilities.R"), local = TRUE)
}

# Assumption: the full cohort starts stable; no progression-to-stable return.
initial_state_distribution <- c(stable = 1, progressive = 0, death = 0)

calculate_markov_trace <- function(transition_probs, initial_distribution) {
  states <- c("stable", "progressive", "death")
  required_columns <- c("cycle", "stable_to_stable", "stable_to_progressive",
                        "stable_to_death", "progressive_to_progressive",
                        "progressive_to_death")
  stopifnot(
    all(required_columns %in% names(transition_probs)),
    all(states %in% names(initial_distribution))
  )
  initial_distribution <- initial_distribution[states]
  stopifnot(
    all(is.finite(initial_distribution)), all(initial_distribution >= 0),
    abs(sum(initial_distribution) - 1) < 1e-10,
    !anyDuplicated(transition_probs$cycle)
  )
  
  state_proportions <- matrix(0, nrow = 60L, ncol = 3L,
                              dimnames = list(NULL, states))
  state_proportions[1L, ] <- initial_distribution
  probability_cycle_used <- rep(NA_integer_, 60L)
  
  for (trace_cycle in 2:60) {
    probability_cycle <- max(1L, trace_cycle - 2L)
    probability_row <- match(probability_cycle, transition_probs$cycle)
    if (is.na(probability_row)) stop("Missing required probability cycle.")
    p <- transition_probs[probability_row, , drop = FALSE]
    
    # Rows = origin state; columns = destination state.
    transition_matrix <- matrix(c(
      p$stable_to_stable, p$stable_to_progressive, p$stable_to_death,
      0, p$progressive_to_progressive, p$progressive_to_death,
      0, 0, 1
    ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
    
    stopifnot(
      all(is.finite(transition_matrix)),
      all(transition_matrix >= 0 & transition_matrix <= 1),
      all(abs(rowSums(transition_matrix) - 1) < 1e-10)
    )
    state_proportions[trace_cycle, ] <-
      state_proportions[trace_cycle - 1L, ] %*% transition_matrix
    probability_cycle_used[trace_cycle] <- probability_cycle
  }
  
  stopifnot(
    nrow(state_proportions) == 60L,
    all(abs(rowSums(state_proportions) - 1) < 1e-10),
    all(state_proportions >= 0 & state_proportions <= 1 + 1e-10),
    all(diff(state_proportions[, "death"]) >= -1e-10),
    identical(probability_cycle_used, c(NA_integer_, 1L, 1:58))
  )
  
  data.frame(cycle = seq_len(60L), probability_cycle_used,
             state_proportions, row.names = NULL)
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_state_distribution)
markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_state_distribution)

# Both objects contain only cycles 1:60. Cycle 1 is the initial distribution.
# Probability cycles 59 and 60 are unused under the requested lag rule.
# Results inherit the modelling limitations of the supplied probability formula.

# Calculate QALMs using existing inputs and Markov traces; no data redefined.
# Run after the earlier scripts, or keep all scripts in the same directory.
qalm_script_directory <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(source_file)) getwd() else dirname(normalizePath(source_file))
})
if (!exists("inputs", inherits = TRUE)) {
  source(file.path(qalm_script_directory, "tmz_model_inputs.R"), local = TRUE)
}
if (!exists("markov_trace_standard", inherits = TRUE) ||
    !exists("markov_trace_long_term", inherits = TRUE)) {
  source(file.path(qalm_script_directory, "tmz_markov_traces.R"), local = TRUE)
}

# Interpret the supplied 0.02 as a utility decrement, not an absolute loss
# of 0.02 QALYs per month. Utilities multiplied by months yield QALMs.
# Apply the requested calendar-cycle schedule to everyone in progression.
# This is not an individual time-since-progression calculation for new entrants.
calculate_cycle_qalms <- function(trace, inputs) {
  stopifnot(
    nrow(trace) == 60L, identical(trace$cycle, seq_len(60L)),
    inputs$design$cycle_length_months == 1,
    all(abs(rowSums(trace[, c("stable", "progressive", "death")]) - 1) < 1e-10)
  )
  
  # Cycles 1-2: no decrement; cycles 3-25: 1-23 decrements;
  # cycles 26-60: retain the same 23 decrements.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  stable_qalms <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  # Death utility is assumed to be zero.
  death_qalms <- trace$death * 0
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = inputs$utilities$stable,
    progression_decrements = decrement_count,
    progressive_utility = progressive_utility,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    total_qalms = stable_qalms + progressive_qalms + death_qalms
  )
}

qalms_standard <- calculate_cycle_qalms(markov_trace_standard, inputs)
qalms_long_term <- calculate_cycle_qalms(markov_trace_long_term, inputs)

total_qalms_standard <- sum(qalms_standard$total_qalms)
total_qalms_long_term <- sum(qalms_long_term$total_qalms)

qalms_by_cycle <- data.frame(
  cycle = qalms_standard$cycle,
  progressive_utility = qalms_standard$progressive_utility,
  standard_qalms = qalms_standard$total_qalms,
  long_term_qalms = qalms_long_term$total_qalms
)

total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Sum all 60 rows, including the initial distribution in cycle 1, as requested.
# No discounting or half-cycle correction is applied.
print(qalms_by_cycle, row.names = FALSE)
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Calculate per-cycle and total costs from existing inputs and Markov traces.
# Run after the earlier scripts, or keep all scripts in the same directory.
cost_script_directory <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(source_file)) getwd() else dirname(normalizePath(source_file))
})
if (!exists("inputs", inherits = TRUE)) {
  source(file.path(cost_script_directory, "tmz_model_inputs.R"), local = TRUE)
}
if (!exists("markov_trace_standard", inherits = TRUE) ||
    !exists("markov_trace_long_term", inherits = TRUE)) {
  source(file.path(cost_script_directory, "tmz_markov_traces.R"), local = TRUE)
}

calculate_cycle_costs <- function(trace, cost_inputs) {
  stopifnot(
    nrow(trace) == 60L, identical(trace$cycle, seq_len(60L)),
    all(is.finite(as.matrix(trace[, c("stable", "progressive", "death")]))),
    all(as.matrix(trace[, c("stable", "progressive", "death")]) >= 0),
    all(abs(rowSums(trace[, c("stable", "progressive", "death")]) - 1) < 1e-10)
  )
  
  # Cycle 1 is initial occupancy; cycle 2 corresponds to month 1.
  # Charge cycle 1 using the same stable unit cost as cycles 2-4.
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, cost_inputs$stable_months_1_3,
    ifelse(trace$cycle <= 7L, cost_inputs$stable_months_4_6,
           cost_inputs$stable_subsequent_months)
  )
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * cost_inputs$progressive
  death_cost <- trace$death * cost_inputs$death
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = cost_inputs$progressive,
    death_unit_cost = cost_inputs$death,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    total_cost = stable_cost + progressive_cost + death_cost
  )
}

costs_standard <- calculate_cycle_costs(
  markov_trace_standard, inputs$costs$standard_tmz)
costs_long_term <- calculate_cycle_costs(
  markov_trace_long_term, inputs$costs$long_term_tmz)

total_cost_standard <- sum(costs_standard$total_cost)
total_cost_long_term <- sum(costs_long_term$total_cost)

costs_by_cycle <- data.frame(
  cycle = costs_standard$cycle,
  month = costs_standard$month,
  standard_stable_cost = costs_standard$stable_cost,
  standard_progressive_cost = costs_standard$progressive_cost,
  standard_death_cost = costs_standard$death_cost,
  standard_total_cost = costs_standard$total_cost,
  long_term_stable_cost = costs_long_term$stable_cost,
  long_term_progressive_cost = costs_long_term$progressive_cost,
  long_term_death_cost = costs_long_term$death_cost,
  long_term_total_cost = costs_long_term$total_cost
)

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Expected costs per initial patient, summed across all 60 cycles.
# Currency/price year are unspecified; no discounting or half-cycle correction.
# Preserve full precision for totals, rounding only for display.
print(costs_by_cycle, row.names = FALSE)
print(total_costs, row.names = FALSE)

# Incremental cost-effectiveness: Long-term TMZ versus Standard TMZ.
# Reuse existing totals; if absent, load the companion calculation scripts.
icer_script_directory <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(source_file)) getwd() else dirname(normalizePath(source_file))
})
if (!exists("total_cost_standard", inherits = TRUE) ||
    !exists("total_cost_long_term", inherits = TRUE)) {
  invisible(capture.output(source(
    file.path(icer_script_directory, "tmz_costs.R"), local = TRUE)))
}
if (!exists("total_qalms_standard", inherits = TRUE) ||
    !exists("total_qalms_long_term", inherits = TRUE)) {
  invisible(capture.output(source(
    file.path(icer_script_directory, "tmz_qalms.R"), local = TRUE)))
}

# Retain full precision throughout the calculations.
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

stopifnot(
  all(is.finite(c(incremental_cost, incremental_qalms, incremental_qalys)))
)
if (incremental_qalms == 0 || incremental_qalys == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Format only the displayed results with zero decimal places.
icer_results <- data.frame(
  measure = c("Cost per additional QALM", "Cost per additional QALY"),
  ICER = formatC(c(icer_per_qalm, icer_per_qaly),
                 format = "f", digits = 0, big.mark = ",")
)
cat("Long-term TMZ versus Standard TMZ\n")
print(icer_results, row.names = FALSE, right = FALSE)