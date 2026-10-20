# Step 1: Store supplied inputs. Sourcing this file produces no printed output.
# Currency and price year have not yet been specified.

inputs <- list(
  states = c("stable", "progressive", "death"),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  costs = list(
    standard_tmz = list(
      stable = c(months_1_to_3 = 1944, months_4_to_6 = 2453,
                 subsequent_months = 350),
      progressive = 2720,
      death = 0
    ),
    long_term_tmz = list(
      stable = c(months_1_to_3 = 1944, months_4_to_6 = 2453,
                 subsequent_months = 2453),
      progressive = 2720,
      death = 0
    )
  ),
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

# Standalone script: Standard TMZ, 60 monthly cycles.
# Decimal commas in the supplied data are represented by decimal points in R.
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

# Requested formula interpreted as 1 - exp(log(y) / t).
# log() is the natural logarithm in R. Calculations are vectorised by cycle.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# These implement the requested transformations; OS - PFS is progressive
# state occupancy, not conditional survival after progression. Consequently,
# these transformations alone do not identify the underlying Markov transitions.
# No results are printed automatically.

stable_to_stable_standard <- 1 - (
  transition_probs_standard$stable_to_death +
    transition_probs_standard$stable_to_progressive
)

progressive_to_progressive_standard <- 1 -
  transition_probs_standard$progressive_to_death

transition_probs_standard$stable_to_stable <- stable_to_stable_standard
transition_probs_standard$progressive_to_progressive <-
  progressive_to_progressive_standard

# Standalone script: Long-term TMZ, 60 monthly cycles.
# Decimal commas in the supplied data are represented by decimal points in R.
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

# Requested formula interpreted as 1 - exp(log(y) / t).
# log() is the natural logarithm in R. Calculations are vectorised by cycle.
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# These implement the requested transformations; OS - PFS is progressive
# state occupancy, not conditional survival after progression. Consequently,
# these transformations alone do not identify the underlying Markov transitions.
# No results are printed automatically.

# Calculate separately
stable_to_stable_long_term <- 1 - (
  transition_probs_long_term$stable_to_death +
    transition_probs_long_term$stable_to_progressive
)

progressive_to_progressive_long_term <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing data frame
transition_probs_long_term$stable_to_stable <- stable_to_stable_long_term
transition_probs_long_term$progressive_to_progressive <-
  progressive_to_progressive_long_term

# Markov traces for Standard TMZ and Long-term TMZ: exactly 60 rows.
# Assumption: the entire cohort is stable at the start of cycle 1.
# Reuses the previously calculated transition probability objects.
# If missing, loads them from the existing scripts beside this file.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0L) script_file <- sub("^--file=", "", file_arg[1L])
}
script_dir <- if (is.null(script_file)) getwd() else dirname(normalizePath(script_file))

if (!exists("transition_probs_standard")) {
  source(file.path(script_dir, "transition_probs_standard.R"))
}
if (!exists("transition_probs_long_term")) {
  source(file.path(script_dir, "transition_probs_long_term.R"))
}

initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- 60L

# Cycle 1 is initial only; cycles 2 and 3 both use probability cycle 1.
transition_cycle_used <- c(NA_integer_, pmax(1L, seq.int(2L, n_cycles) - 2L))

calculate_markov_trace <- function(transition_probs, initial_distribution) {
  states <- c("stable", "progressive", "death")
  required <- c("cycle", "stable_to_stable", "stable_to_progressive",
                "stable_to_death", "progressive_to_progressive",
                "progressive_to_death")
  stopifnot(all(required %in% names(transition_probs)),
            !anyDuplicated(transition_probs$cycle),
            all(states %in% names(initial_distribution)),
            all(initial_distribution >= 0),
            abs(sum(initial_distribution) - 1) < 1e-10)
  
  trace <- matrix(0, nrow = n_cycles, ncol = length(states),
                  dimnames = list(NULL, states))
  trace[1L, ] <- initial_distribution[states]
  
  for (cycle in seq.int(2L, n_cycles)) {
    probability_cycle <- transition_cycle_used[cycle]
    row_index <- match(probability_cycle, transition_probs$cycle)
    stopifnot(!is.na(row_index))
    p <- transition_probs[row_index, ]
    
    # Rows = origin state; columns = destination state.
    # Progressive-to-stable is zero and death is absorbing.
    transition_matrix <- matrix(c(
      p$stable_to_stable, p$stable_to_progressive, p$stable_to_death,
      0, p$progressive_to_progressive, p$progressive_to_death,
      0, 0, 1
    ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
    
    stopifnot(all(is.finite(transition_matrix)),
              all(transition_matrix >= 0 & transition_matrix <= 1),
              all(abs(rowSums(transition_matrix) - 1) < 1e-10))
    trace[cycle, ] <- as.numeric(trace[cycle - 1L, ] %*% transition_matrix)
  }
  
  stopifnot(nrow(trace) == 60L, all(abs(rowSums(trace) - 1) < 1e-10))
  data.frame(cycle = seq_len(n_cycles), trace, row.names = NULL)
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_distribution
)
markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_distribution
)

# Results are stored without printing. No cycle 0 or cycle 61 is included.
# These traces inherit the modelling limitations of the supplied probability
# transformations; they are not a validation of those transformations.

# Calculate undiscounted QALMs using existing inputs and Markov traces.
# Run after the earlier scripts, or keep those scripts beside this file.
# No survival data, transition probabilities, or utility inputs are redefined.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0L) script_file <- sub("^--file=", "", file_arg[1L])
}
script_dir <- if (is.null(script_file)) getwd() else dirname(normalizePath(script_file))
if (!exists("inputs")) source(file.path(script_dir, "tmz_model.R"))
if (!exists("markov_trace_standard") || !exists("markov_trace_long_term")) {
  source(file.path(script_dir, "markov_traces.R"))
}

calculate_cycle_qalms <- function(trace, inputs) {
  stopifnot(nrow(trace) == 60L, all(trace$cycle == seq_len(60L)),
            inputs$design$cycle_length_months == 1)
  
  # Cycle 3 has one decrement; cycle 25 has 23 decrements.
  # All subsequent cycles retain those same 23 decrements.
  # The supplied 0.02 is interpreted as a utility decrement (not an annual
  # QALY loss converted to months), consistent with the original utility input.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    decrement_count *
    inputs$utilities$decrement_per_consecutive_month_of_progression
  
  # This is the requested model-cycle schedule, not individual time since
  # progression: new entrants receive the utility assigned to that cycle.
  stable_qalms <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  death_qalms <- trace$death * 0  # Death utility is assumed zero.
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = inputs$utilities$stable,
    progressive_utility = progressive_utility,
    decrement_count = decrement_count,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    qalms = stable_qalms + progressive_qalms + death_qalms
  )
}

qalms_standard <- calculate_cycle_qalms(markov_trace_standard, inputs)
qalms_long_term <- calculate_cycle_qalms(markov_trace_long_term, inputs)
total_qalms_standard <- sum(qalms_standard$qalms)
total_qalms_long_term <- sum(qalms_long_term$qalms)

qalms_results <- rbind(
  data.frame(treatment = "Standard TMZ", qalms_standard),
  data.frame(treatment = "Long-term TMZ", qalms_long_term)
)
total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Each row represents one monthly cycle, including the initial cycle 1.
# No discounting or half-cycle correction is applied.
print(qalms_results, row.names = FALSE, max = nrow(qalms_results) * ncol(qalms_results))
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Per-cycle and total undiscounted costs for both TMZ treatments.
# Reuses existing inputs and traces; no input data are redefined.
# Run after the earlier scripts, or keep them beside this file.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0L) script_file <- sub("^--file=", "", file_arg[1L])
}
script_dir <- if (is.null(script_file)) getwd() else dirname(normalizePath(script_file))
if (!exists("inputs")) source(file.path(script_dir, "tmz_model.R"))
if (!exists("markov_trace_standard") || !exists("markov_trace_long_term")) {
  source(file.path(script_dir, "markov_traces.R"))
}

calculate_cycle_costs <- function(trace, unit_costs) {
  stopifnot(nrow(trace) == 60L, all(trace$cycle == seq_len(60L)))
  
  # Cycle 1 is initial; cycle 2 corresponds to month 1.
  # Cycle 1 receives the same stable cost as cycles 2-4.
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, unit_costs$stable[["months_1_to_3"]],
    ifelse(trace$cycle <= 7L, unit_costs$stable[["months_4_to_6"]],
           unit_costs$stable[["subsequent_months"]])
  )
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * unit_costs$progressive
  death_cost <- trace$death * unit_costs$death
  
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
    total_cost = stable_cost + progressive_cost + death_cost
  )
}

costs_standard <- calculate_cycle_costs(
  markov_trace_standard, inputs$costs$standard_tmz
)
costs_long_term <- calculate_cycle_costs(
  markov_trace_long_term, inputs$costs$long_term_tmz
)
total_cost_standard <- sum(costs_standard$total_cost)
total_cost_long_term <- sum(costs_long_term$total_cost)

cost_results <- rbind(
  data.frame(treatment = "Standard TMZ", costs_standard),
  data.frame(treatment = "Long-term TMZ", costs_long_term)
)
total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Costs are per initial patient, in the currency of the supplied inputs.
# Month 0 labels the initial distribution; its costs are included as requested.
# No discounting or half-cycle correction is applied; no rounding before summing.
print(cost_results, row.names = FALSE, max = nrow(cost_results) * ncol(cost_results))
print(total_costs, row.names = FALSE)

# Incremental comparison: Long-term TMZ minus Standard TMZ.
# Reuses the existing cost and QALM totals without redefining inputs.
# Keep prior scripts beside this file if the totals are not already loaded.
script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0L) script_file <- sub("^--file=", "", file_arg[1L])
}
script_dir <- if (is.null(script_file)) getwd() else dirname(normalizePath(script_file))

# Suppress the detailed tables printed by the prerequisite scripts.
if (!exists("total_cost_standard") || !exists("total_cost_long_term")) {
  invisible(capture.output(source(file.path(script_dir, "calculate_costs.R"))))
}
if (!exists("total_qalms_standard") || !exists("total_qalms_long_term")) {
  invisible(capture.output(source(file.path(script_dir, "calculate_qalms.R"))))
}

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12
incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

stopifnot(is.finite(incremental_cost), is.finite(incremental_qalms),
          is.finite(incremental_qalys), incremental_qalms != 0,
          incremental_qalys != 0)
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Preserve full precision for calculations; round only the displayed results.
icer_results <- data.frame(
  measure = c("ICER per QALM gained", "ICER per QALY gained"),
  ICER = sprintf("%.0f", c(icer_per_qalm, icer_per_qaly))
)
print(icer_results, row.names = FALSE)