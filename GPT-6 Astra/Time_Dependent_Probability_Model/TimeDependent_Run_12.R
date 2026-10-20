# Step 1: Store supplied inputs. Sourcing this script produces no output.
# Costs retain the supplied units; currency and price year are unspecified.

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

# Standard TMZ: supplied monthly survival data and requested calculations.
# Standalone script; does not read or modify tmz_model.R.
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

t <- seq_len(60L)
stopifnot(
  length(OS_standard) == length(t),
  length(PFS_standard) == length(t),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(OS_standard - PFS_standard > 0)
)

# Requested formula: 1 - exp(log(y) / t); log() is natural logarithm.
# These are the requested transformations, not validated conditional Markov
# transition probabilities. In particular, OS - PFS is state occupancy,
# not a survival function for patients after progression.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Objects are stored without printing.

# Step 2: Calculate remaining-in-state probabilities separately.
# Assumption: patients cannot return from progressive to stable.
stable_to_stable_standard <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

progressive_to_progressive_standard <- 1 -
  transition_probs_standard$progressive_to_death

# Append the separately calculated vectors to the existing data frame.
transition_probs_standard$stable_to_stable <- stable_to_stable_standard
transition_probs_standard$progressive_to_progressive <-
  progressive_to_progressive_standard

# Check probability bounds and outgoing row sums without printing.
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

# Long-term TMZ: monthly survival data and requested transformations.
# Standalone script. Existing scripts are not read or modified.
# Decimal commas in supplied data are represented as decimal points.

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
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(OS_long_term - PFS_long_term > 0)
)

# Requested formula: 1 - exp(log(y) / t); log() is natural logarithm.
# These are the requested transformations, not validated conditional Markov
# transition probabilities. OS - PFS is state occupancy, not a survival
# function for patients after progression.
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# Validate numerical bounds without printing the stored probabilities.
stopifnot(
  all(transition_probs_long_term$stable_to_death >= 0 &
        transition_probs_long_term$stable_to_death <= 1),
  all(transition_probs_long_term$stable_to_progressive >= 0 &
        transition_probs_long_term$stable_to_progressive <= 1),
  all(transition_probs_long_term$progressive_to_death >= 0 &
        transition_probs_long_term$progressive_to_death <= 1)
)

# Calculate remaining-in-state probabilities separately.
# Assumption: patients cannot return from progressive to stable.
stable_to_stable_long_term <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

progressive_to_progressive_long_term <- 1 -
  transition_probs_long_term$progressive_to_death

# Append the separate vectors to the existing data frame.
transition_probs_long_term$stable_to_stable <- stable_to_stable_long_term
transition_probs_long_term$progressive_to_progressive <-
  progressive_to_progressive_long_term

# Check probability bounds and outgoing row sums without printing.
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

# Markov traces for Standard TMZ and Long-term TMZ: exactly 60 cycles.
# Uses the previously created transition probability objects without changing
# their definitions. If absent, loads them from the existing companion scripts.
# Keep this script and both transition probability scripts in the same folder.
# Initial distribution assumption: all patients are stable at cycle 1.

script_directory <- local({
  source_files <- lapply(sys.frames(), function(frame) frame$ofile)
  source_files <- Filter(function(x) !is.null(x), source_files)
  if (length(source_files) > 0L) {
    dirname(normalizePath(tail(source_files, 1L)[[1L]], mustWork = TRUE))
  } else {
    file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(file_arg) > 0L) {
      dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
    } else {
      getwd()
    }
  }
})

if (!exists("transition_probs_standard", inherits = TRUE)) {
  source(file.path(script_directory, "transition_probs_standard.R"), local = TRUE)
}
if (!exists("transition_probs_long_term", inherits = TRUE)) {
  source(file.path(script_directory, "transition_probs_long_term.R"), local = TRUE)
}

initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- 60L

# Cycle 1 has no transition; cycles 2 and 3 both use probability cycle 1.
# For cycles 4 through 60, use probability cycles 2 through 58.
probability_cycle_used <- c(NA_integer_, pmax(1L, (2L:n_cycles) - 2L))

calculate_markov_trace <- function(transition_probs, initial_distribution,
                                   probability_cycle_used) {
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
    length(probability_cycle_used) == 60L,
    is.na(probability_cycle_used[1L])
  )
  
  state_proportions <- matrix(0, nrow = 60L, ncol = 3L,
                              dimnames = list(NULL, states))
  state_proportions[1L, ] <- initial_distribution
  
  for (cycle in 2L:60L) {
    probability_cycle <- probability_cycle_used[cycle]
    row_index <- match(probability_cycle, transition_probs$cycle)
    stopifnot(!is.na(row_index))
    p <- transition_probs[row_index, , drop = FALSE]
    
    # Rows are source states; columns are destination states.
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
    state_proportions[cycle, ] <-
      as.numeric(state_proportions[cycle - 1L, ] %*% transition_matrix)
  }
  
  stopifnot(
    all(is.finite(state_proportions)),
    all(state_proportions >= -1e-12 & state_proportions <= 1 + 1e-12),
    all(abs(rowSums(state_proportions) - 1) < 1e-10),
    all(diff(state_proportions[, "death"]) >= -1e-12)
  )
  data.frame(cycle = seq_len(60L),
             probability_cycle_used = probability_cycle_used,
             state_proportions, row.names = NULL)
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_distribution, probability_cycle_used
)
markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_distribution, probability_cycle_used
)

# Both traces contain cycles 1 through 60 only and are stored without printing.
# These traces implement the previously requested probability transformations;
# numerical validity does not establish clinical calibration to OS and PFS.

# QALMs for both treatments using existing inputs and 60-cycle Markov traces.
# No input data are redefined. Keep companion scripts in the same directory.
# The requested decrement is treated as a 0.02 utility-point decrement per
# month, consistent with the stored input; monthly utility yields QALMs.
# This is a model-cycle schedule, not individual time since progression.

qalm_script_directory <- local({
  source_files <- lapply(sys.frames(), function(frame) frame$ofile)
  source_files <- Filter(function(x) !is.null(x), source_files)
  if (length(source_files) > 0L) {
    dirname(normalizePath(tail(source_files, 1L)[[1L]], mustWork = TRUE))
  } else {
    file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(file_arg) > 0L) {
      dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
    } else {
      getwd()
    }
  }
})

if (!exists("tmz_inputs", inherits = TRUE)) {
  source(file.path(qalm_script_directory, "tmz_model.R"), local = TRUE)
}
if (!exists("markov_trace_standard", inherits = TRUE) ||
    !exists("markov_trace_long_term", inherits = TRUE)) {
  source(file.path(qalm_script_directory, "markov_traces.R"), local = TRUE)
}

calculate_cycle_qalms <- function(trace, inputs) {
  stopifnot(
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), seq_len(60L)),
    inputs$design$cycle_length_months == 1,
    inputs$design$time_horizon_months == 60
  )
  # 0 decrements in cycles 1-2, 1 in cycle 3, ..., 23 in cycle 25.
  # The count stays at 23 for cycles 26-60.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  result <- data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    progressive_decrement_count = decrement_count,
    stable_utility = inputs$utilities$stable,
    progressive_utility = progressive_utility,
    death_utility = 0 # Death contributes no quality-adjusted survival.
  )
  result$stable_qalms <- result$stable * result$stable_utility *
    inputs$design$cycle_length_months
  result$progressive_qalms <- result$progressive *
    result$progressive_utility * inputs$design$cycle_length_months
  result$death_qalms <- result$death * result$death_utility *
    inputs$design$cycle_length_months
  result$total_qalms <- result$stable_qalms + result$progressive_qalms +
    result$death_qalms
  result$cumulative_qalms <- cumsum(result$total_qalms)
  
  stopifnot(
    all(is.finite(result$total_qalms)),
    all(decrement_count[1:2] == 0),
    all(decrement_count[3:25] == seq_len(23L)),
    all(progressive_utility[26:60] == progressive_utility[25])
  )
  result
}

qalms_standard <- calculate_cycle_qalms(markov_trace_standard, tmz_inputs)
qalms_long_term <- calculate_cycle_qalms(markov_trace_long_term, tmz_inputs)

total_qalms_standard <- sum(qalms_standard$total_qalms)
total_qalms_long_term <- sum(qalms_long_term$total_qalms)

qalm_totals <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display all 60 cycles per treatment and the totals; stored values are unrounded.
print(qalms_standard, row.names = FALSE, max = nrow(qalms_standard) * ncol(qalms_standard))
print(qalms_long_term, row.names = FALSE, max = nrow(qalms_long_term) * ncol(qalms_long_term))
print(qalm_totals, row.names = FALSE)

# Totals include every cycle (including the initial distribution at cycle 1).
# No discounting or half-cycle correction is applied.

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Cycle costs for both TMZ treatments using existing inputs and Markov traces.
# Keep this script with tmz_model.R, markov_traces.R and both transition scripts.
# Cycle 1 is the initial distribution and receives a full cycle of costs.

cost_script_directory <- local({
  source_files <- lapply(sys.frames(), function(frame) frame$ofile)
  source_files <- Filter(function(x) !is.null(x), source_files)
  if (length(source_files) > 0L) {
    dirname(normalizePath(tail(source_files, 1L)[[1L]], mustWork = TRUE))
  } else {
    file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(file_arg) > 0L) {
      dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
    } else {
      getwd()
    }
  }
})

if (!exists("tmz_inputs", inherits = TRUE)) {
  source(file.path(cost_script_directory, "tmz_model.R"), local = TRUE)
}
if (!exists("markov_trace_standard", inherits = TRUE) ||
    !exists("markov_trace_long_term", inherits = TRUE)) {
  source(file.path(cost_script_directory, "markov_traces.R"), local = TRUE)
}

calculate_cycle_costs <- function(trace, treatment_costs) {
  stopifnot(
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), seq_len(60L))
  )
  
  # Months 1-3: cycles 1-4 (including the initial distribution).
  # Months 4-6: cycles 5-7. Subsequent months: cycles 8-60.
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L,
    treatment_costs$stable_months_1_to_3,
    ifelse(trace$cycle <= 7L,
           treatment_costs$stable_months_4_to_6,
           treatment_costs$stable_subsequent_months)
  )
  
  result <- data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = treatment_costs$progressive,
    death_unit_cost = treatment_costs$death
  )
  result$stable_cost <- result$stable * result$stable_unit_cost
  result$progressive_cost <- result$progressive * result$progressive_unit_cost
  result$death_cost <- result$death * result$death_unit_cost
  result$total_cost <- result$stable_cost + result$progressive_cost +
    result$death_cost
  result$cumulative_cost <- cumsum(result$total_cost)
  
  stopifnot(
    all(is.finite(result$total_cost)),
    all(result$total_cost >= 0),
    sum(trace$cycle <= 4L) == 4L,
    sum(trace$cycle >= 5L & trace$cycle <= 7L) == 3L,
    sum(trace$cycle >= 8L) == 53L
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

total_cost_standard <- sum(costs_standard$total_cost)
total_cost_long_term <- sum(costs_long_term$total_cost)

cost_totals <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Display all 60 cycles per treatment and totals. Retain full stored precision.
print(costs_standard, row.names = FALSE,
      max = nrow(costs_standard) * ncol(costs_standard))
print(costs_long_term, row.names = FALSE,
      max = nrow(costs_long_term) * ncol(costs_long_term))
print(cost_totals, row.names = FALSE)

# Costs are per initial cohort member, in the supplied monetary units.
# No discounting or half-cycle correction is applied.


# ICERs: Long-term TMZ relative to Standard TMZ.
# Reuses existing totals; loads companion calculation scripts only if needed.
# Keep all model scripts in the same directory.

icer_script_directory <- local({
  source_files <- lapply(sys.frames(), function(frame) frame$ofile)
  source_files <- Filter(function(x) !is.null(x), source_files)
  if (length(source_files) > 0L) {
    dirname(normalizePath(tail(source_files, 1L)[[1L]], mustWork = TRUE))
  } else {
    file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(file_arg) > 0L) {
      dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
    } else {
      getwd()
    }
  }
})

if (!exists("total_cost_standard", inherits = TRUE) ||
    !exists("total_cost_long_term", inherits = TRUE)) {
  invisible(capture.output(
    source(file.path(icer_script_directory, "cost_calculations.R"), local = TRUE)
  ))
}
if (!exists("total_qalms_standard", inherits = TRUE) ||
    !exists("total_qalms_long_term", inherits = TRUE)) {
  invisible(capture.output(
    source(file.path(icer_script_directory, "qalm_calculations.R"), local = TRUE)
  ))
}

# Convert full-precision QALM totals to QALYs.
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

stopifnot(all(is.finite(c(incremental_cost, incremental_qalms,
                          incremental_qalys))))
if (incremental_qalms == 0 || incremental_qalys == 0) {
  stop("ICER is undefined because the incremental health benefit is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Preserve full precision in the numeric ICER objects; format display only.
icer_results <- data.frame(
  comparison = rep("Long-term TMZ versus Standard TMZ", 2L),
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = formatC(c(icer_per_qalm, icer_per_qaly), format = "f",
                 digits = 0, big.mark = ",")
)
print(icer_results, row.names = FALSE, right = FALSE)