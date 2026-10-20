# Step 1: Store supplied inputs only; sourcing this script prints no values.
# Currency and price year have not been specified.
# Progression-duration effects will be implemented in a later step.

tmz_inputs <- list(
  states = c("stable", "progressive", "death"),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  costs = list(
    standard_tmz = list(
      stable_months_1_to_3 = 1944,
      stable_months_4_to_6 = 2453,
      stable_subsequent_months = 350,
      progressive = 2720,
      death = 0
    ),
    long_term_tmz = list(
      stable_months_1_to_3 = 1944,
      stable_months_4_to_6 = 2453,
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

# Standard TMZ: standalone input data and requested probability calculations.
# Interpretation of the supplied formula: 1 - exp(log(y) / t).
# t is the cycle number (1 through 60); log() is the natural logarithm.
# This implements the requested mappings, not a calibrated transition model.
# No input values or calculated probabilities are printed.

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
  all(is.finite(OS_standard)),
  all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= 1),
  all(PFS_standard < OS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0)
)

# Standard TMZ: standalone input data and requested probability calculations.
# Interpretation of the supplied formula: 1 - exp(log(y) / t).
# t is the cycle number (1 through 60); log() is the natural logarithm.
# This implements the requested mappings, not a calibrated transition model.
# No input values or calculated probabilities are printed.

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
  all(is.finite(OS_standard)),
  all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= 1),
  all(PFS_standard < OS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0)
)

transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Numerical checks only; these do not establish clinical/model validity.
stopifnot(
  all(is.finite(as.matrix(transition_probs_standard[-1L]))),
  all(transition_probs_standard[-1L] >= 0),
  all(transition_probs_standard[-1L] <= 1),
  all(transition_probs_standard$stable_to_death +
        transition_probs_standard$stable_to_progressive <= 1)
)

# Calculate self-transition probabilities separately.
# Assumption: no transition from progressive back to stable.
stable_to_stable_standard <- 1 - (
  transition_probs_standard$stable_to_death +
    transition_probs_standard$stable_to_progressive
)

progressive_to_progressive_standard <- 1 -
  transition_probs_standard$progressive_to_death

# Append both calculated vectors to the existing data frame.
transition_probs_standard$stable_to_stable <- stable_to_stable_standard
transition_probs_standard$progressive_to_progressive <-
  progressive_to_progressive_standard

stopifnot(
  all(stable_to_stable_standard >= 0 & stable_to_stable_standard <= 1),
  all(progressive_to_progressive_standard >= 0 &
        progressive_to_progressive_standard <= 1),
  all(abs(transition_probs_standard$stable_to_stable +
            transition_probs_standard$stable_to_progressive +
            transition_probs_standard$stable_to_death - 1) < 1e-12),
  all(abs(transition_probs_standard$progressive_to_progressive +
            transition_probs_standard$progressive_to_death - 1) < 1e-12)
)

# Long-term TMZ: standalone inputs and requested probability calculations.
# Formula interpretation: 1 - exp(log(y) / t), with natural logarithms.
# t is the cycle number, from 1 to 60.
# Implements the supplied mappings, not a calibrated transition model.
# Sourcing this script does not print input values or probabilities.

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
  all(is.finite(OS_long_term)),
  all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= 1),
  all(PFS_long_term < OS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0)
)

transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# Numerical checks only; these do not establish clinical/model validity.
stopifnot(
  all(is.finite(as.matrix(transition_probs_long_term[-1L]))),
  all(transition_probs_long_term[-1L] >= 0),
  all(transition_probs_long_term[-1L] <= 1),
  all(transition_probs_long_term$stable_to_death +
        transition_probs_long_term$stable_to_progressive <= 1)
)

# Long-term TMZ: standalone inputs and requested probability calculations.
# Formula interpretation: 1 - exp(log(y) / t), with natural logarithms.
# t is the cycle number, from 1 to 60.
# Implements the supplied mappings, not a calibrated transition model.
# Sourcing this script does not print input values or probabilities.

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
  all(is.finite(OS_long_term)),
  all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= 1),
  all(PFS_long_term < OS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0)
)

transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# Numerical checks only; these do not establish clinical/model validity.
stopifnot(
  all(is.finite(as.matrix(transition_probs_long_term[-1L]))),
  all(transition_probs_long_term[-1L] >= 0),
  all(transition_probs_long_term[-1L] <= 1),
  all(transition_probs_long_term$stable_to_death +
        transition_probs_long_term$stable_to_progressive <= 1)
)

# Calculate self-transition probabilities separately.
# Assumption: no transition from progressive back to stable.
stable_to_stable_long_term <- 1 - (
  transition_probs_long_term$stable_to_death +
    transition_probs_long_term$stable_to_progressive
)

progressive_to_progressive_long_term <- 1 -
  transition_probs_long_term$progressive_to_death

# Append both calculated vectors to the existing data frame.
transition_probs_long_term$stable_to_stable <- stable_to_stable_long_term
transition_probs_long_term$progressive_to_progressive <-
  progressive_to_progressive_long_term

stopifnot(
  all(stable_to_stable_long_term >= 0 & stable_to_stable_long_term <= 1),
  all(progressive_to_progressive_long_term >= 0 &
        progressive_to_progressive_long_term <= 1),
  all(abs(transition_probs_long_term$stable_to_stable +
            transition_probs_long_term$stable_to_progressive +
            transition_probs_long_term$stable_to_death - 1) < 1e-12),
  all(abs(transition_probs_long_term$progressive_to_progressive +
            transition_probs_long_term$progressive_to_death - 1) < 1e-12)
)

# Markov traces for Standard and Long-term TMZ: exactly 60 rows each.
# Reuses the previously calculated transition-probability objects unchanged.
# If those objects are absent, run this script from the directory containing
# the two earlier transition-probability scripts, or set input_dir below.
# Source this script or run it with Rscript; no values are printed.

input_dir <- "."

if (!exists("transition_probs_standard", inherits = TRUE)) {
  standard_inputs <- new.env()
  sys.source(file.path(input_dir, "tmz_standard_transition_probabilities.R"),
             envir = standard_inputs)
  transition_probs_standard <- standard_inputs$transition_probs_standard
}
if (!exists("transition_probs_long_term", inherits = TRUE)) {
  long_term_inputs <- new.env()
  sys.source(file.path(input_dir, "tmz_long_term_transition_probabilities.R"),
             envir = long_term_inputs)
  transition_probs_long_term <- long_term_inputs$transition_probs_long_term
}

# Assumed initial distribution; change here if a different cohort is intended.
initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- 60L

# Cycle 1: initial distribution only.
# Trace cycles 2, 3, 4, 5, ..., 60 use probability cycles 1, 1, 2, 3, ..., 58.
# Thus there are 59 transitions, with no cycle 0 or cycle 61.
transition_cycle_used <- c(NA_integer_, pmax(1L, seq.int(2L, n_cycles) - 2L))

calculate_markov_trace <- function(transition_probs, initial_distribution,
                                   transition_cycle_used) {
  states <- c("stable", "progressive", "death")
  required_columns <- c("cycle", "stable_to_stable", "stable_to_progressive",
                        "stable_to_death", "progressive_to_progressive",
                        "progressive_to_death")
  stopifnot(
    all(required_columns %in% names(transition_probs)),
    !anyDuplicated(transition_probs$cycle),
    identical(names(initial_distribution), states),
    all(is.finite(initial_distribution)),
    all(initial_distribution >= 0),
    abs(sum(initial_distribution) - 1) < 1e-12,
    length(transition_cycle_used) == 60L,
    is.na(transition_cycle_used[1L])
  )
  
  occupancy <- matrix(0, nrow = 60L, ncol = 3L,
                      dimnames = list(NULL, states))
  occupancy[1L, ] <- initial_distribution
  
  for (cycle in seq.int(2L, 60L)) {
    probability_cycle <- transition_cycle_used[cycle]
    row_index <- match(probability_cycle, transition_probs$cycle)
    stopifnot(!is.na(row_index))
    p <- transition_probs[row_index, , drop = FALSE]
    
    # Rows are origin states; columns are destination states.
    # No return from progressive to stable; death is absorbing.
    transition_matrix <- matrix(c(
      p$stable_to_stable, p$stable_to_progressive, p$stable_to_death,
      0, p$progressive_to_progressive, p$progressive_to_death,
      0, 0, 1
    ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
    
    stopifnot(
      all(is.finite(transition_matrix)),
      all(transition_matrix >= 0 & transition_matrix <= 1),
      all(abs(rowSums(transition_matrix) - 1) < 1e-12)
    )
    occupancy[cycle, ] <- as.numeric(
      occupancy[cycle - 1L, ] %*% transition_matrix
    )
  }
  
  stopifnot(
    all(is.finite(occupancy)),
    all(occupancy >= -1e-12 & occupancy <= 1 + 1e-12),
    all(abs(rowSums(occupancy) - 1) < 1e-10),
    all(diff(occupancy[, "death"]) >= -1e-12)
  )
  
  data.frame(
    cycle = seq_len(60L),
    transition_cycle_used = transition_cycle_used,
    stable = occupancy[, "stable"],
    progressive = occupancy[, "progressive"],
    death = occupancy[, "death"]
  )
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_distribution, transition_cycle_used
)
markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_distribution, transition_cycle_used
)

# Explicit checks of the requested cycle mapping and output length.
stopifnot(
  identical(transition_cycle_used, c(NA_integer_, 1L, seq_len(58L))),
  nrow(markov_trace_standard) == 60L,
  nrow(markov_trace_long_term) == 60L,
  identical(markov_trace_standard$cycle, seq_len(60L)),
  identical(markov_trace_long_term$cycle, seq_len(60L))
)

# Calculate per-cycle and total QALMs for both treatments.
# Reuses the earlier input and trace objects; no input data are redefined.
# Run from the folder containing the earlier scripts if objects are not loaded.

if (!exists("tmz_inputs", inherits = TRUE)) {
  source("tmz_model_inputs.R", local = TRUE)
}
if (!exists("markov_trace_standard", inherits = TRUE) ||
    !exists("markov_trace_long_term", inherits = TRUE)) {
  source("tmz_markov_traces.R", local = TRUE)
}

# Implements the requested MODEL-CYCLE schedule, not individual time since
# progression. All progressive patients in a cycle receive the same utility.
# The supplied 0.02 is interpreted as a utility-point decrement. Utilities
# multiplied by months yield QALMs; a QALY is a different unit (12 QALMs).
# Cycles 1-2: zero decrements; cycles 3-25: 1-23 decrements;
# cycles 26-60: retain all 23 decrements. Death utility is assumed to be zero.
# Sum all 60 rows, including the initial cycle, with no discounting or
# half-cycle correction, as requested.

calculate_qalms <- function(trace, inputs) {
  stopifnot(
    identical(trace$cycle, seq_len(60L)),
    all(c("stable", "progressive", "death") %in% names(trace)),
    inputs$design$cycle_length_months == 1,
    all(is.finite(as.matrix(trace[c("stable", "progressive", "death")]))),
    all(as.matrix(trace[c("stable", "progressive", "death")]) >= 0),
    all(abs(rowSums(trace[c("stable", "progressive", "death")]) - 1) < 1e-10)
  )
  
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  stable_qalms <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  death_qalms <- trace$death * 0 * inputs$design$cycle_length_months
  
  result <- data.frame(
    cycle = trace$cycle,
    progressive_utility = progressive_utility,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    total_qalms = stable_qalms + progressive_qalms + death_qalms
  )
  
  stopifnot(
    identical(decrement_count, c(0L, 0L, seq_len(23L), rep(23L, 35L))),
    all(result$progressive_utility[26:60] == result$progressive_utility[25]),
    all(is.finite(result$total_qalms))
  )
  result
}

qalms_standard <- calculate_qalms(markov_trace_standard, tmz_inputs)
qalms_long_term <- calculate_qalms(markov_trace_long_term, tmz_inputs)

total_qalms_standard <- sum(qalms_standard$total_qalms)
total_qalms_long_term <- sum(qalms_long_term$total_qalms)

qalm_totals <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display every cycle for each treatment, followed by the two totals.
# Higher print limits ensure no cycles are omitted from console output.
local({
  old_options <- options(max.print = 10000L)
  on.exit(options(old_options))
  cat("Standard TMZ: QALMs per cycle\n")
  print(qalms_standard, row.names = FALSE, digits = 10)
  cat("\nLong-term TMZ: QALMs per cycle\n")
  print(qalms_long_term, row.names = FALSE, digits = 10)
  cat("\nTotal QALMs across all 60 cycles\n")
  print(qalm_totals, row.names = FALSE, digits = 10)
})

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Per-cycle and total costs for both treatments, reusing existing inputs/traces.
# If objects are absent, run from the folder containing the earlier scripts.
# Costs are per patient, in the supplied unspecified currency, undiscounted.
# Includes the initial distribution (cycle 1) as a full cost-bearing cycle.
# No half-cycle correction is applied.

if (!exists("tmz_inputs", inherits = TRUE)) {
  source("tmz_model_inputs.R", local = TRUE)
}
if (!exists("markov_trace_standard", inherits = TRUE) ||
    !exists("markov_trace_long_term", inherits = TRUE)) {
  source("tmz_markov_traces.R", local = TRUE)
}

calculate_cycle_costs <- function(trace, unit_costs) {
  states <- c("stable", "progressive", "death")
  stopifnot(
    identical(trace$cycle, seq_len(60L)),
    all(states %in% names(trace)),
    all(is.finite(as.matrix(trace[states]))),
    all(as.matrix(trace[states]) >= 0),
    all(abs(rowSums(trace[states]) - 1) < 1e-10),
    all(c("stable_months_1_to_3", "stable_months_4_to_6",
          "stable_subsequent_months", "progressive", "death") %in%
          names(unit_costs)),
    all(is.finite(unlist(unit_costs))),
    all(unlist(unit_costs) >= 0)
  )
  
  # Cycle 1 is initial; cycle 2 = month 1, ..., cycle 60 = month 59.
  # Initial cycle receives the same stable unit cost as months 1-3.
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, unit_costs$stable_months_1_to_3,
    ifelse(trace$cycle <= 7L, unit_costs$stable_months_4_to_6,
           unit_costs$stable_subsequent_months)
  )
  
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * unit_costs$progressive
  death_cost <- trace$death * unit_costs$death
  
  result <- data.frame(
    cycle = trace$cycle,
    month = c(NA_integer_, seq_len(59L)),
    stable_unit_cost = stable_unit_cost,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    total_cost = stable_cost + progressive_cost + death_cost
  )
  
  stopifnot(
    all(result$stable_unit_cost[1:4] == unit_costs$stable_months_1_to_3),
    all(result$stable_unit_cost[5:7] == unit_costs$stable_months_4_to_6),
    all(result$stable_unit_cost[8:60] == unit_costs$stable_subsequent_months),
    all(is.finite(result$total_cost))
  )
  result
}

stopifnot(tmz_inputs$design$cycle_length_months == 1)

costs_standard <- calculate_cycle_costs(
  markov_trace_standard, tmz_inputs$costs$standard_tmz
)
costs_long_term <- calculate_cycle_costs(
  markov_trace_long_term, tmz_inputs$costs$long_term_tmz
)

# Sum all 60 cycles at full precision; rounding is for display only.
total_cost_standard <- sum(costs_standard$total_cost)
total_cost_long_term <- sum(costs_long_term$total_cost)

cost_totals <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  stable_cost = c(sum(costs_standard$stable_cost), sum(costs_long_term$stable_cost)),
  progressive_cost = c(sum(costs_standard$progressive_cost),
                       sum(costs_long_term$progressive_cost)),
  death_cost = c(sum(costs_standard$death_cost), sum(costs_long_term$death_cost)),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

local({
  old_options <- options(max.print = 10000L)
  on.exit(options(old_options))
  cat("Standard TMZ: costs per cycle (month NA = initial distribution)\n")
  print(costs_standard, row.names = FALSE, digits = 10)
  cat("\nLong-term TMZ: costs per cycle (month NA = initial distribution)\n")
  print(costs_long_term, row.names = FALSE, digits = 10)
  cat("\nTotal costs across all 60 cycles\n")
  print(cost_totals, row.names = FALSE, digits = 10)
})

# Incremental cost-effectiveness: Long-term TMZ versus Standard TMZ.
# Reuses full-precision totals; does not redefine input data.
# If totals are absent, run from the folder containing the earlier scripts.

if (!exists("total_cost_standard", inherits = TRUE) ||
    !exists("total_cost_long_term", inherits = TRUE)) {
  invisible(capture.output(source("tmz_costs.R", local = TRUE)))
}
if (!exists("total_qalms_standard", inherits = TRUE) ||
    !exists("total_qalms_long_term", inherits = TRUE)) {
  invisible(capture.output(source("tmz_qalms.R", local = TRUE)))
}

stopifnot(
  length(total_cost_standard) == 1L,
  length(total_cost_long_term) == 1L,
  length(total_qalms_standard) == 1L,
  length(total_qalms_long_term) == 1L,
  all(is.finite(c(total_cost_standard, total_cost_long_term,
                  total_qalms_standard, total_qalms_long_term)))
)

# Convert full-precision QALMs to QALYs before calculating differences.
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

if (incremental_qalms == 0 || incremental_qalys == 0) {
  stop("ICER is undefined because incremental effectiveness is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Keep the numeric results at full precision for subsequent calculations.
icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = rep(incremental_cost, 2L),
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)

# Format only the displayed values: rounded to the nearest whole currency unit.
icer_display <- data.frame(
  measure = c("ICER per QALM", "ICER per QALY"),
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
cat("Long-term TMZ versus Standard TMZ (supplied currency units)\n")
print(icer_display, row.names = FALSE, right = FALSE)